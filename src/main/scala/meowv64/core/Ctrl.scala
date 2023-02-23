package meowv64.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import meowv64.debug.DebugModule
import meowv64.exec.ExceptionResult

class StageCtrl extends Bundle {
  val stall = Input(Bool())
  val flush = Output(Bool())
}

object StageCtrl {
  def ctrl() = new StageCtrl
  def stage() = Flipped(new StageCtrl)
}

/** Exception types of RISC-V
  */
object ExType extends ChiselEnum {
  val INSTR_ADDR_MISALIGN = Value(0.U)
  val INSTR_ACCESS_FAULT = Value(1.U)
  val ILLEGAL_INSTR = Value(2.U)
  val BREAKPOINT = Value(3.U)
  val LOAD_ADDR_MISALIGN = Value(4.U)
  val LOAD_ACCESS_FAULT = Value(5.U)
  val STORE_ADDR_MISALIGN = Value(6.U)
  val STORE_ACCESS_FAULT = Value(7.U)
  val U_CALL = Value(8.U)
  val S_CALL = Value(9.U)
  val M_CALL = Value(11.U)
  val INSTR_PAGE_FAULT = Value(12.U)
  val LOAD_PAGE_FAULT = Value(13.U)
  val STORE_PAGE_FAULT = Value(15.U)
}

/** Exception request: None, Exception, MRET/SRET/DRET
  */
object ExReq extends ChiselEnum {
  val none, ex, mret, sret, dret = Value
}

object PrivLevel extends ChiselEnum {
  val U = Value(0.U)
  val S = Value(1.U)
  val H = Value(2.U) // Although we don't support H...
  val M = Value(3.U)
}

class VType(implicit coredef: CoreDef) extends Bundle {
  // from MSB to LSB
  val vill = Bool()
  val reserved = UInt((coredef.XLEN - 9).W)
  val vma = Bool()
  val vta = Bool()
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)

  def floatFmt = {
    MuxLookup(
      vsew,
      0.U,
      Seq(
        1.U -> FloatH.fmt,
        2.U -> FloatS.fmt,
        3.U -> FloatD.fmt
      )
    )
  }
}

class Ctrl(implicit coredef: CoreDef) extends Module {
  val toIF = IO(new Bundle {
    val ctrl = StageCtrl.ctrl()

    val pc = Output(UInt(coredef.XLEN.W))

    /** ICache Reset
      */
    val iRst = Output(Bool())

    /** TLB Reset
      */
    val tlbRst = Output(Bool())

    /** Privilege level
      */
    val priv = Output(PrivLevel())

    /** Allow floating point instructions (mstatus.fs != 0)
      */
    val allowFloat = Output(Bool())
  })

  val br = IO(new Bundle {
    val req = Input(new ExceptionResult()(coredef))
    val tval = Input(UInt(coredef.XLEN.W))
  })

  val toExec = IO(new Bundle {
    val ctrl = StageCtrl.ctrl()

    val retCnt = Input(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))
    val nepc = Input(UInt(coredef.XLEN.W))

    val int = Output(Bool())
    val intAck = Input(Bool())

    val priv = Output(PrivLevel())
    val status = Output(new Status)
    val vState = Output(new VState)
    val debugMode = Output(Bool())
    val frm = Output(UInt(3.W))

    val step = Output(Bool())
    val stepAck = Input(Bool())

    val tlbRst = Output(Bool())

    /** Update fflags & vState
      */
    val updateFFlags = Flipped(Valid(UInt(5.W)))
    val updateVState = Flipped(Valid(new VState))
  })

  val int = IO(Input(new CoreInt))

  val csr = IO(new Bundle {
    val mcycle = new CSRPort(coredef.XLEN)
    val minstret = new CSRPort(coredef.XLEN)
    val mstatus = new CSRPort(coredef.XLEN)
    val mtvec = new CSRPort(coredef.XLEN)
    val mie = new CSRPort(coredef.XLEN)
    val mip = new CSRPort(coredef.XLEN)
    val mepc = new CSRPort(coredef.XLEN)
    val mcause = new CSRPort(coredef.XLEN)
    val mtval = new CSRPort(coredef.XLEN)
    val mcountinhibit = new CSRPort(coredef.XLEN)
    val mideleg = new CSRPort(coredef.XLEN)
    val medeleg = new CSRPort(coredef.XLEN)
    val pmpcfg0 = new CSRPort(coredef.XLEN)
    val pmpcfg2 = new CSRPort(coredef.XLEN)
    val pmpaddr0 = new CSRPort(coredef.XLEN)
    val pmpaddr1 = new CSRPort(coredef.XLEN)
    val mhpmcounter3 = new CSRPort(coredef.XLEN)
    val mhpmevent3 = new CSRPort(coredef.XLEN)

    val sstatus = new CSRPort(coredef.XLEN)
    val stvec = new CSRPort(coredef.XLEN)
    val sie = new CSRPort(coredef.XLEN)
    val sip = new CSRPort(coredef.XLEN)
    val scause = new CSRPort(coredef.XLEN)
    val sepc = new CSRPort(coredef.XLEN)
    val stval = new CSRPort(coredef.XLEN)

    val instret = Output(UInt(64.W))
    val cycle = Output(UInt(64.W))

    val fflags = new CSRPort(coredef.XLEN)
    val frm = new CSRPort(coredef.XLEN)
    val fcsr = new CSRPort(coredef.XLEN)

    val vstart = new CSRPort(coredef.XLEN)
    val vxsat = new CSRPort(coredef.XLEN)
    val vxrm = new CSRPort(coredef.XLEN)
    val vcsr = new CSRPort(coredef.XLEN)
    val vl = new CSRPort(coredef.XLEN)
    val vtype = new CSRPort(coredef.XLEN)
    val vlenb = new CSRPort(coredef.XLEN)

    val dcsr = new CSRPort(coredef.XLEN)
    val dpc = new CSRPort(coredef.XLEN)
    val dscratch0 = new CSRPort(coredef.XLEN)
    val dscratch1 = new CSRPort(coredef.XLEN)
    val tselect = new CSRPort(coredef.XLEN)
  })

  val dm = IO(new CoreToDebugModule)
  val debugMode = RegInit(false.B)
  dm.halted := debugMode
  toExec.debugMode := debugMode

  // Privilege level
  val priv = RegInit(PrivLevel.M)
  toExec.priv := priv

  toIF.ctrl.flush := false.B
  toExec.ctrl.flush := false.B
  toExec.tlbRst := false.B
  toIF.iRst := false.B
  toIF.tlbRst := false.B
  toIF.pc := 0.U
  toIF.priv := priv

  val branch = Wire(Bool())
  val baddr = Wire(UInt(coredef.XLEN.W))

  // xEPC in the front!
  val mepc = RegInit(0.U(coredef.XLEN.W))
  val sepc = RegInit(0.U(coredef.XLEN.W))
  val dpc = RegInit(0.U(coredef.XLEN.W))

  // Next retired instruction
  val nepc = Wire(UInt(coredef.XLEN.W))
  when(br.req.ex === ExReq.mret) {
    nepc := mepc
  }.elsewhen(br.req.ex === ExReq.sret) {
    nepc := sepc
  }.elsewhen(br.req.ex === ExReq.dret) {
    nepc := dpc
  }.otherwise {
    nepc := toExec.nepc
  }

  // Rst comes together with an branch

  // IF control && PC controller
  when(branch) {
    // printf(p"Branched, baddr: ${Hexadecimal(io.baddr)}\n")
    toIF.ctrl.flush := true.B
    toExec.ctrl.flush := true.B

    assert(!toIF.ctrl.stall)
    assert(!toExec.ctrl.stall)

    // pc := alignedPC + (Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM).U
    toIF.pc := baddr
    toIF.iRst := br.req.iRst
    toIF.tlbRst := br.req.tlbRst

    toExec.tlbRst := br.req.tlbRst
  }
  /*
    // printf(p"PC: ${Hexadecimal(io.pc)}\n")
    pc := pc + (Const.INSTR_MIN_WIDTH / 8 * coredef.FETCH_NUM).U
  }
   */

  /*
  printf("Ctrl status:\n")
  printf("================\n")
  printf(p"PC: ${pc}\n")
  printf(p"Stalled: ${stalled}\n")
  when(io.fetch.stall) {
    printf("  Fetch stall")
  }
  when(io.exec.stall) {
    printf("  Exec stall")
  }
  printf("\n")
   */

  val mcycle = RegInit(0.U(coredef.XLEN.W))
  val minstret = RegInit(0.U(coredef.XLEN.W))
  val mcountinhibit = RegInit(0.U(coredef.XLEN.W))

  when(!mcountinhibit(0)) {
    mcycle := mcycle + 1.U
  }

  when(!mcountinhibit(1)) {
    minstret := minstret + toExec.retCnt
  }

  csr.mcycle <> CSRPort.fromReg(coredef.XLEN, mcycle)
  csr.minstret <> CSRPort.fromReg(coredef.XLEN, minstret)
  csr.mcountinhibit <> CSRPort.fromReg(coredef.XLEN, mcountinhibit)

  csr.instret := minstret
  csr.cycle := mcycle

  // xstatus
  val status = RegInit(Status.empty)
  // WPRI fields
  val mwpri = RegInit(0.U(coredef.XLEN.W))
  val swpri = RegInit(0.U(coredef.XLEN.W))
  toExec.status := status

  // when FS=0, trap for float instructions
  toIF.allowFloat := status.fs =/= 0.U

  csr.mstatus.rdata := (
    status.asUInt & Status.mmask
      | mwpri & Status.mwpri
      | Status.hardwired(status).asUInt & ~(Status.mmask | Status.mwpri)
  )

  csr.sstatus.rdata := (
    status.asUInt & Status.smask
      | swpri & Status.swpri
      | Status.hardwired(status).asUInt & ~(Status.smask | Status.swpri)
  )

  when(csr.mstatus.write) {
    status := (
      csr.mstatus.wdata & Status.mmask | status.asUInt & ~Status.mmask
    ).asTypeOf(status)
    mwpri := csr.mstatus.wdata
  }

  when(csr.sstatus.write) {
    status := (
      csr.sstatus.wdata & Status.smask | status.asUInt & ~Status.smask
    ).asTypeOf(status)
    swpri := csr.sstatus.wdata
  }

  // MxDELEG
  val medeleg = RegInit(0.U(coredef.XLEN.W))
  val mideleg = RegInit(0.U(coredef.XLEN.W))
  csr.medeleg <> CSRPort.fromReg(coredef.XLEN, medeleg)
  csr.mideleg <> CSRPort.fromReg(coredef.XLEN, mideleg)

  // PMP
  val pmpcfg0 = RegInit(0.U(coredef.XLEN.W))
  val pmpcfg2 = RegInit(0.U(coredef.XLEN.W))
  val pmpaddr0 = RegInit(0.U(coredef.XLEN.W))
  val pmpaddr1 = RegInit(0.U(coredef.XLEN.W))
  csr.pmpcfg0 <> CSRPort.fromReg(coredef.XLEN, pmpcfg0)
  csr.pmpcfg2 <> CSRPort.fromReg(coredef.XLEN, pmpcfg2)
  csr.pmpaddr0 <> CSRPort.fromReg(coredef.XLEN, pmpaddr0)
  csr.pmpaddr1 <> CSRPort.fromReg(coredef.XLEN, pmpaddr1)

  val mhpmcounter3 = RegInit(0.U(coredef.XLEN.W))
  val mhpmevent3 = RegInit(0.U(coredef.XLEN.W))
  csr.mhpmcounter3 <> CSRPort.fromReg(coredef.XLEN, mhpmcounter3)
  csr.mhpmevent3 <> CSRPort.fromReg(coredef.XLEN, mhpmevent3)

  // xIE, xIP
  val ipStore = RegInit(IntConf.empty)
  val ie = RegInit(IntConf.empty)
  val ip = WireDefault(ipStore)
  ip.external.m := int.meip
  ip.timer.m := int.mtip
  ip.software.m := int.msip
  ip.external.s := int.seip
  // TODO: CSRW SEIP

  val miewpri = RegInit(0.U(coredef.XLEN.W))
  val mipwpri = RegInit(0.U(coredef.XLEN.W))
  val siewpri = RegInit(0.U(coredef.XLEN.W))
  val sipwpri = RegInit(0.U(coredef.XLEN.W))

  csr.mie.rdata := (
    ie.asUInt & IntConf.mmask(false)
      | mwpri & IntConf.mwpri
      | IntConf.hardwired.asUInt & ~(Status.mmask | Status.mwpri)
  )

  csr.sie.rdata := (
    ie.asUInt & IntConf.smask(false)
      | swpri & IntConf.swpri
      | IntConf.hardwired.asUInt & ~(Status.smask | Status.swpri)
  )

  when(csr.mie.write) {
    ie := (
      csr.mie.wdata & IntConf.mmask(false) | ie.asUInt & ~Status.mmask
    ).asTypeOf(ie)
    mwpri := csr.mie.wdata
  }

  when(csr.sie.write) {
    ie := (
      csr.sie.wdata & IntConf.smask(false) | ie.asUInt & ~Status.smask
    ).asTypeOf(ie)
    swpri := csr.sie.wdata
  }

  csr.mip.rdata := (
    ip.asUInt & IntConf.mmask(false)
      | mwpri & IntConf.mwpri
      | IntConf.hardwired.asUInt & ~(Status.mmask | Status.mwpri)
  )

  csr.sip.rdata := (
    ip.asUInt & IntConf.smask(false)
      | swpri & IntConf.swpri
      | IntConf.hardwired.asUInt & ~(Status.smask | Status.swpri)
  )

  when(csr.mip.write) {
    ipStore := (
      csr.mip.wdata & IntConf.mmask(false) | ipStore.asUInt & ~Status.mmask
    ).asTypeOf(ipStore)
    mwpri := csr.mip.wdata
  }

  when(csr.sip.write) {
    ipStore := (
      csr.sip.wdata & IntConf.smask(false) | ipStore.asUInt & ~Status.smask
    ).asTypeOf(ipStore)
    swpri := csr.sip.wdata
  }

  // xEPC
  csr.mepc <> CSRPort.fromReg(coredef.XLEN, mepc)
  csr.sepc <> CSRPort.fromReg(coredef.XLEN, sepc)

  // xtvec, xtval, xcause
  val mtvec = RegInit(0.U(coredef.XLEN.W))
  val mtval = RegInit(0.U(coredef.XLEN.W))
  val mcause = RegInit(0.U(coredef.XLEN.W))
  val stvec = RegInit(0.U(coredef.XLEN.W))
  val stval = RegInit(0.U(coredef.XLEN.W))
  val scause = RegInit(0.U(coredef.XLEN.W))
  csr.mtvec <> CSRPort.fromReg(coredef.XLEN, mtvec)
  csr.mtval <> CSRPort.fromReg(coredef.XLEN, mtval)
  csr.mcause <> CSRPort.fromReg(coredef.XLEN, mcause)
  csr.stvec <> CSRPort.fromReg(coredef.XLEN, stvec)
  csr.stval <> CSRPort.fromReg(coredef.XLEN, stval)
  csr.scause <> CSRPort.fromReg(coredef.XLEN, scause)

  branch := br.req.valid
  baddr := br.req.target

  // fcsr: frm + fflags
  class FCSR extends Bundle {
    // from MSB to LSB
    val frm = UInt(3.W)
    val fflags = UInt(5.W)
  }

  val fcsr = RegInit(0.U.asTypeOf(new FCSR))
  csr.fflags <> CSRPort.fromReg(5, fcsr.fflags)
  csr.frm <> CSRPort.fromReg(3, fcsr.frm)
  csr.fcsr <> CSRPort.fromReg(8, fcsr)
  toExec.frm := fcsr.frm

  // update fflags
  // and set mstatus.fs = 3(dirty)
  when(toExec.updateFFlags.valid) {
    fcsr.fflags := fcsr.fflags | toExec.updateFFlags.bits
    status.fs := 3.U
  }

  // readonly, can only set by vsetvl
  val vState = RegInit(0.U.asTypeOf(new VState))
  csr.vl.rdata := vState.vl
  csr.vtype.rdata := vState.vtype.asUInt
  csr.vlenb.rdata := (coredef.VLEN / 8).U
  toExec.vState := vState

  when(toExec.updateVState.valid) {
    vState := toExec.updateVState.bits
  }

  // vcsr: vxrm + vxsat
  class VCSR extends Bundle {
    // from MSB to LSB
    val vxrm = UInt(2.W)
    val vxsat = UInt(1.W)
  }

  val vcsr = RegInit(0.U.asTypeOf(new VCSR))
  csr.vxrm <> CSRPort.fromReg(2, vcsr.vxrm)
  csr.vxsat <> CSRPort.fromReg(1, vcsr.vxsat)
  csr.vcsr <> CSRPort.fromReg(3, vcsr)

  val vstart = RegInit(0.U(coredef.XLEN.W))
  csr.vstart <> CSRPort.fromReg(coredef.XLEN, vstart)

  val dcsr = RegInit(DCSR.init)
  csr.dcsr.rdata := (
    // debugver=4 mprven=1
    4.U(4.W) ## 0.U(12.W) ## dcsr.ebreakm ## 0.U(1.W) ##
      dcsr.ebreaks ## dcsr.ebreaku ## 0.U(3.U) ##
      dcsr.cause ## 0.U(1.W) ## 1.U(1.W) ## 0.U(1.W) ##
      dcsr.step ## dcsr.prv
  )
  when(csr.dcsr.write) {
    dcsr.ebreakm := csr.dcsr.wdata(15)
    dcsr.ebreaks := csr.dcsr.wdata(13)
    dcsr.ebreaku := csr.dcsr.wdata(12)
    dcsr.cause := csr.dcsr.wdata(8, 6)
    dcsr.step := csr.dcsr.wdata(2)

    // do not update prv when new prv=H(2)
    val new_prv = csr.dcsr.wdata(1, 0)
    when(new_prv =/= 2.U) {
      dcsr.prv := new_prv
    }
  }
  // pass to exec stage
  toExec.step := dcsr.step

  val dscratch0 = RegInit(0.U(coredef.XLEN.W))
  val dscratch1 = RegInit(0.U(coredef.XLEN.W))
  csr.dpc <> CSRPort.fromReg(coredef.XLEN, dpc)
  csr.dscratch0 <> CSRPort.fromReg(coredef.XLEN, dscratch0)
  csr.dscratch1 <> CSRPort.fromReg(coredef.XLEN, dscratch1)
  // Writes of values greater than or equal to the number of supported triggers
  // may result in a different value in this register than what was written.
  val tselect = RegInit(0.U(1.W))
  when(csr.tselect.write) {
    tselect := csr.tselect.wdata(0) ^ 1.U
  }
  csr.tselect.rdata := tselect

  // Interrupts
  // only low 12 bits are valid
  val intMask: UInt = (ie.asUInt & ip.asUInt())(11, 0)
  val intCause = PriorityEncoder(intMask)
  val intDeleg = priv =/= PrivLevel.M && mideleg(intCause)
  val intEnabled = Mux(
    intDeleg,
    priv < PrivLevel.S || status.sie,
    priv < PrivLevel.M || status.mie
  )
  // priority: halt > int
  val intFired =
    intEnabled && intMask
      .asUInt()
      .orR() && ~debugMode // interrupts are masked in debug mode
  val haltFired = dm.haltreq && ~debugMode // debug halt is a special interrupt
  toExec.int := intFired || haltFired
  // from non-debug to debug
  val enterDebugMode = WireInit(haltFired || toExec.stepAck)
  when(enterDebugMode) {
    assert(~debugMode)
  }

  // Exceptions + Interrupts
  val ex =
    (br.req.ex === ExReq.ex) || (toExec.intAck && (intFired || haltFired)) || toExec.stepAck
  val cause = Wire(UInt(coredef.XLEN.W))
  when(
    br.req.ex === ExReq.ex && br.req.exType === ExType.BREAKPOINT &&
      ~debugMode && ((priv === PrivLevel.M && dcsr.ebreakm)
        || (priv === PrivLevel.S && dcsr.ebreaks)
        || (priv === PrivLevel.U && dcsr.ebreaku))
  ) {
    // ebreak, priority 3
    // ebreak in non-debug mode and ebreakm/s/u=1
    cause := 1.U // An ebreak instruction was executed
    // ebreak into debug mode
    enterDebugMode := true.B
  }.elsewhen(haltFired) {
    // priority 1
    cause := 3.U // The debugger requested entry to Debug Mode using haltreq
    assert(enterDebugMode)
  }.elsewhen(toExec.stepAck) {
    // priroity 0
    cause := 4.U // The hart single stepped because step was set.
    assert(enterDebugMode)
  }.elsewhen(intFired) {
    cause := (true.B << (coredef.XLEN - 1)) | intCause
  }.otherwise {
    cause := (false.B << (coredef.XLEN - 1)) | br.req.exType.asUInt()
  }

  // Exception delegated to S-mode
  val delegs = WireDefault(
    Mux(
      intFired,
      intDeleg,
      priv =/= PrivLevel.M && medeleg(cause(62, 0))
    )
  )

  when(priv === PrivLevel.M) {
    // When we are already in M-mode, don't delegate
  }

  val tvecBase = Mux(delegs, stvec, mtvec)
  val tvec = Wire(UInt(coredef.XLEN.W))
  tvec := tvecBase(coredef.XLEN - 1, 2) ## 0.U(2.W)
  when(enterDebugMode) {
    // enter debug mode:
    if (coredef.IN_ROCKET_SYSTEM) {
      // 0x800 debugEntry
      tvec := 0x800.U
    } else {
      // vector = start
      tvec := DebugModule.DM_CODE_REGION_START.U
    }
  }.elsewhen(debugMode) {
    // exception in debug mode
    when(br.req.ex === ExReq.ex && br.req.exType === ExType.BREAKPOINT) {
      // if ebreak in debug mode

      if (coredef.IN_ROCKET_SYSTEM) {
        // 0x800 debugEntry
        tvec := 0x800.U
      } else {
        // vector = start + 0x4
        tvec := (DebugModule.DM_CODE_REGION_START + 0x4).U
      }
    }.otherwise {
      // other exceptions in debug mode

      if (coredef.IN_ROCKET_SYSTEM) {
        // 0x808 debugException
        tvec := 0x808.U
      } else {
        // vector = start + 0x8
        tvec := (DebugModule.DM_CODE_REGION_START + 0x8).U
      }
    }
  }.elsewhen(intFired && tvecBase(1, 0) === 1.U) {
    // Vectored trap
    tvec := (tvecBase(coredef.XLEN - 1, 2) + intCause) ## 0.U(2.W)
  }

  // FIXME: interrupt at MRET
  when(ex) {
    // Branch into mtvec

    branch := true.B
    baddr := tvec

    when(enterDebugMode) {
      assert(!debugMode)
      // enter debug mode
      debugMode := true.B
      dpc := nepc
      dcsr.cause := cause
      dcsr.prv := priv.asUInt
      priv := PrivLevel.M
    }.elsewhen(debugMode) {
      // exception in debug mode
      // do not set cause:
      // cause explains why debug mode was entered
    }.otherwise {
      when(delegs) {
        sepc := nepc
        scause := cause
        stval := br.tval

        status.spie := status.sie
        status.sie := false.B
        status.spp := priv =/= PrivLevel.U

        priv := PrivLevel.S
      } otherwise {
        mepc := nepc
        mcause := cause
        mtval := br.tval

        status.mpie := status.mie
        status.mie := false.B
        status.mpp := priv.asUInt()

        priv := PrivLevel.M
      }
    }
  }.elsewhen(br.req.ex === ExReq.mret) {
    branch := true.B
    baddr := mepc

    status.mie := status.mpie
    status.mpie := true.B
    status.mpp := PrivLevel.U.asUInt()

    priv := status.mpp.asTypeOf(PrivLevel.Type())
  }.elsewhen(br.req.ex === ExReq.sret) {
    branch := true.B
    baddr := sepc

    status.sie := status.spie
    status.spie := true.B
    status.spp := PrivLevel.U.asUInt()

    priv := status.spp.asTypeOf(PrivLevel.Type())
  }.elsewhen(br.req.ex === ExReq.dret) {
    branch := true.B
    baddr := dpc
    debugMode := false.B

    priv := dcsr.prv.asTypeOf(PrivLevel.Type())
  }

  // Avoid Vivado naming collision. Com'on, Xilinx, write *CORRECT* code plz
  override def desiredName: String = "PipeCtrl"
}
