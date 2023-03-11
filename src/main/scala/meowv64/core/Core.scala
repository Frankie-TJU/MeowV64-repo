package meowv64.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.log2Ceil
import meowv64.cache._
import meowv64.exec.Exec
import meowv64.instr._
import meowv64.paging.PTW
import meowv64.reg._
import difftest.DifftestCSRState
import difftest.DifftestArchIntRegState
import difftest.DifftestArchFpRegState
import difftest.DiffCSRStateIO

class CoreInt extends Bundle {
  val meip = Bool()
  val seip = Bool()
  val mtip = Bool()
  val msip = Bool()
}

class CoreFrontend(implicit val coredef: CoreDef) extends Bundle {
  // icache
  val ic = new L1ICPort(coredef.L1I)
  // dcache
  val dc = new L1DCPort(coredef.L1D)
  // uncached inst
  val ui = new L1ICPort(coredef.L1I)
  // uncached data
  val uc = new L1UCPort(coredef.L1D)
}

class CoreDebug(implicit val coredef: CoreDef) extends Bundle {
  val pc = UInt(coredef.XLEN.W)
  val fetchPc = UInt(coredef.XLEN.W)
  val minstret = UInt(coredef.XLEN.W)
  val mcycle = UInt(coredef.XLEN.W)

  // exec
  val iqEmptyMask = UInt(coredef.ISSUE_QUEUES.length.W)
  val iqFullMask = UInt(coredef.ISSUE_QUEUES.length.W)
  val issueNum = UInt(log2Ceil(coredef.ISSUE_NUM + 1).W)
  val issueNumBoundedByROBSize = Bool()
  val issueNumBoundedByLSQSize = Bool()
  val retireNum = UInt(log2Ceil(coredef.ISSUE_NUM + 1).W)
}

class CoreToDebugModule extends Bundle {
  // debug request
  val haltreq = Input(Bool())

  // status
  val halted = Output(Bool())
}

object CoreState extends ChiselEnum {
  val running, halting, halted, resuming = Value
}

class Core(implicit val coredef: CoreDef) extends Module {
  val io = IO(new Bundle {
    val int = Input(new CoreInt)
    val frontend = new CoreFrontend

    val dm = new CoreToDebugModule

    // Debug
    val debug = Output(new CoreDebug)
  })

  assert(
    coredef.FETCH_NUM % 2 == 0,
    "issue num can only be multiples of two, because we need to support compressed instructions"
  )

  val ctrl = Module(new Ctrl)
  ctrl.dm <> io.dm

  // Caches
  val l1i = Module(new L1IC(coredef.L1I))
  val l1d = Module(new L1DC(coredef.L1D))

  l1i.toL2 <> io.frontend.ic
  l1i.toUI <> io.frontend.ui
  l1d.toL2 <> io.frontend.dc

  // TODO: attach DTLB
  val ptw = Module(new PTW)
  l1d.ptw <> ptw.dc

  val fetch = Module(new InstrFetch)
  val bpu = Module(new MicroBTB)
  val ras = Module(new RAS)
  val exec = Module(new Exec)
  val regFiles =
    for (regInfo <- coredef.REG_TYPES) yield {
      // add additional read ports for difftest
      val readPortCount = coredef
        .REG_READ_PORT_COUNT(regInfo.regType) + (if (
                                                   (regInfo == coredef.REG_INT || regInfo == coredef.REG_FLOAT) && coredef.ENABLE_DIFFTEST
                                                 ) { 32 }
                                                 else {
                                                   0
                                                 })
      val reg = Module(
        new RegFile(
          regInfo.width,
          regInfo.physRegs,
          readPortCount,
          coredef.REG_WRITE_PORT_COUNT(regInfo.regType),
          // hardwire x0 to zero
          FIXED_ZERO = regInfo.fixedZero
        )
      )
      reg.suggestName(s"reg_${regInfo.regType}")
    }

  val (csrWriter, csr) = CSR.gen(coredef.XLEN, coredef.HART_ID)

  fetch.toIC <> l1i.toCPU
  fetch.toCtrl <> ctrl.toIF
  fetch.toCore.ptw <> ptw.itlb

  bpu.toExec <> exec.toBPU
  bpu.toFetch <> fetch.toBPU

  ras.toIF <> fetch.toRAS
  ras.toExec.realign.bits := DontCare
  ras.toExec.realign.valid := false.B

  exec.toIF <> fetch.toExec
  for (i <- 0 until coredef.REG_TYPES.length) {
    var readIdx = 0
    var writeIdx = 0

    // read ports
    for ((port, idx) <- coredef.PORTS.zipWithIndex) {
      for ((regType, j) <- port.operandTypes.flatMap(x => x).zipWithIndex) {
        if (regType == coredef.REG_TYPES(i).regType) {
          exec.toRF.readPorts(idx).reader(j).port <> regFiles(i).io.reads(
            readIdx
          )
          readIdx += 1
        }
      }
    }

    // write ports
    for ((port, idx) <- coredef.REG_WRITE_PORTS.zipWithIndex) {
      if (port.regType == coredef.REG_TYPES(i).regType) {
        exec.toRF.writePorts(idx).writer <> regFiles(i).io.writes(writeIdx)
        writeIdx += 1
      }
    }
    println(
      s"Register Type ${coredef.REG_TYPES(i).regType}: ${readIdx}R ${writeIdx}W"
    )
  }
  exec.csrWriter <> csrWriter

  exec.toDC.r <> l1d.mr
  exec.toDC.w <> l1d.w
  exec.toDC.fs <> l1d.fs
  exec.toDC.u <> io.frontend.uc

  exec.toCtrl.ctrl <> ctrl.toExec.ctrl
  exec.toCtrl.tlbRst := ctrl.toExec.tlbRst

  exec.toCore.ptw <> ptw.dtlb

  ctrl.br.req <> exec.toCtrl.branch
  ctrl.br.tval <> exec.toCtrl.tval

  ctrl.toExec.retCnt := exec.toCtrl.retCnt
  ctrl.toExec.nepc := exec.toCtrl.nepc
  ctrl.toExec.int <> exec.toCtrl.int
  ctrl.toExec.intAck := exec.toCtrl.intAck
  ctrl.toExec.priv <> exec.toCtrl.priv
  ctrl.toExec.status <> exec.toCtrl.status
  ctrl.toExec.updateFState <> exec.toCtrl.updateFState
  ctrl.toExec.updateVState <> exec.toCtrl.updateVState
  ctrl.toExec.vState <> exec.toCtrl.vState
  ctrl.toExec.debugMode <> exec.toCtrl.debugMode
  ctrl.toExec.frm <> exec.toCtrl.frm
  ctrl.toExec.step <> exec.toCtrl.step
  ctrl.toExec.stepAck := exec.toCtrl.stepAck

  ctrl.int := io.int

  io.debug.fetchPc := fetch.debug.pc

  // CSR
  CSRHelper.defaults(csr)
  csr.const("mhartid") := coredef.HART_ID.U
  csr.attach("mcycle").connect(ctrl.csr.mcycle)
  csr.attach("minstret").connect(ctrl.csr.minstret)
  csr.attach("mstatus").connect(ctrl.csr.mstatus)
  csr.attach("mtvec").connect(ctrl.csr.mtvec)
  csr.attach("mcause").connect(ctrl.csr.mcause)
  csr.attach("mtval").connect(ctrl.csr.mtval)
  csr.attach("mepc").connect(ctrl.csr.mepc)
  csr.attach("mie").connect(ctrl.csr.mie)
  csr.attach("mip").connect(ctrl.csr.mip)
  csr.attach("mcountinhibit").connect(ctrl.csr.mcountinhibit)
  csr.attach("mideleg").connect(ctrl.csr.mideleg)
  csr.attach("medeleg").connect(ctrl.csr.medeleg)
  csr.attach("pmpaddr0").connect(ctrl.csr.pmpaddr0)
  csr.attach("pmpaddr1").connect(ctrl.csr.pmpaddr1)
  csr.attach("pmpaddr2").connect(ctrl.csr.pmpaddr2)

  csr.attach("sstatus").connect(ctrl.csr.sstatus)
  csr.attach("stvec").connect(ctrl.csr.stvec)
  csr.attach("sie").connect(ctrl.csr.sie)
  csr.attach("sip").connect(ctrl.csr.sip)
  csr.attach("scause").connect(ctrl.csr.scause)
  csr.attach("sepc").connect(ctrl.csr.sepc)
  csr.attach("stval").connect(ctrl.csr.stval)

  csr.attach("fflags").connect(ctrl.csr.fflags)
  csr.attach("frm").connect(ctrl.csr.frm)
  csr.attach("fcsr").connect(ctrl.csr.fcsr)

  csr.attach("vstart").connect(ctrl.csr.vstart)
  csr.attach("vxsat").connect(ctrl.csr.vxsat)
  csr.attach("vxrm").connect(ctrl.csr.vxrm)
  csr.attach("vcsr").connect(ctrl.csr.vcsr)
  csr.attach("vl").connect(ctrl.csr.vl)
  csr.attach("vtype").connect(ctrl.csr.vtype)
  csr.attach("vlenb").connect(ctrl.csr.vlenb)

  csr.attach("dcsr").connect(ctrl.csr.dcsr)
  csr.attach("dpc").connect(ctrl.csr.dpc)
  csr.attach("dscratch0").connect(ctrl.csr.dscratch0)
  csr.attach("dscratch1").connect(ctrl.csr.dscratch1)
  csr.attach("tselect").connect(ctrl.csr.tselect)
  csr.readers("tdata1") := 0.U
  csr.readers("tdata2") := 0.U
  csr.readers("tdata3") := 0.U
  csr.readers("tinfo") := 0.U
  csr.readers("tcontrol") := 0.U

  val mscratch = RegInit(0.U(coredef.XLEN.W))
  csr.attach("mscratch").connect(CSRPort.fromReg(coredef.XLEN, mscratch))
  val sscratch = RegInit(0.U(coredef.XLEN.W))
  csr.attach("sscratch").connect(CSRPort.fromReg(coredef.XLEN, sscratch))

  val satp = RegInit(Satp.empty)
  csr.attach("satp").connect(satp.port)

  csr.readers("cycle") := ctrl.csr.cycle
  csr.readers("instret") := ctrl.csr.instret

  ptw.satp := satp
  fetch.toCore.satp := satp
  exec.toCore.satp := satp

  io.debug.mcycle := ctrl.csr.mcycle.rdata
  io.debug.minstret := ctrl.csr.minstret.rdata
  io.debug.iqEmptyMask := exec.toCore.iqEmptyMask
  io.debug.iqFullMask := exec.toCore.iqFullMask
  io.debug.issueNum := exec.toCore.issueNum
  io.debug.issueNumBoundedByROBSize := exec.toCore.issueNumBoundedByROBSize
  io.debug.issueNumBoundedByLSQSize := exec.toCore.issueNumBoundedByLSQSize
  io.debug.retireNum := exec.toCore.retireNum
  io.debug.pc := exec.toCore.retirePc

  if (coredef.ENABLE_DIFFTEST) {
    val difftestCSR = Module(new DifftestCSRState)
    val difftest = Wire(Output(new DiffCSRStateIO()))
    difftest := DontCare
    difftest.coreid := coredef.HART_ID.U
    difftest.priviledgeMode := ctrl.toExec.priv.asUInt
    difftest.mstatus := csr.readers("mstatus")
    difftest.sstatus := csr.readers("sstatus")
    difftest.mepc := csr.readers("mepc")
    difftest.sepc := csr.readers("sepc")
    difftest.mtval := csr.readers("mtval")
    difftest.stval := csr.readers("stval")
    difftest.mtvec := csr.readers("mtvec")
    difftest.stvec := csr.readers("stvec")
    difftest.mcause := csr.readers("mcause")
    difftest.scause := csr.readers("scause")
    difftest.satp := csr.readers("satp")
    difftest.mip := csr.readers("mip")
    difftest.mie := csr.readers("mie")
    difftest.mscratch := csr.readers("mscratch")
    difftest.sscratch := csr.readers("sscratch")
    difftest.mideleg := csr.readers("mideleg")
    difftest.medeleg := csr.readers("medeleg")
    difftestCSR.io := RegNext(RegNext(difftest))
    difftestCSR.io.clock := clock

    val difftestArchIntReg = Module(new DifftestArchIntRegState)
    difftestArchIntReg.io.clock := clock
    difftestArchIntReg.io.coreid := coredef.HART_ID.U
    for (i <- 0 until 32) {
      val readPortOffset = coredef
        .REG_READ_PORT_COUNT(coredef.REG_INT.regType)
      val physReg = exec.difftest.intCommittedMap(i.U)
      regFiles(0).io.reads(readPortOffset + i).addr := physReg
      difftestArchIntReg.io
        .gpr(i) := regFiles(0).io.reads(readPortOffset + i).data
    }

    val difftestArchFpReg = Module(new DifftestArchFpRegState)
    difftestArchFpReg.io.clock := clock
    difftestArchFpReg.io.coreid := coredef.HART_ID.U
    for (i <- 0 until 32) {
      val readPortOffset = coredef
        .REG_READ_PORT_COUNT(coredef.REG_FLOAT.regType)
      val physReg = exec.difftest.fpCommittedMap(i.U)
      regFiles(1).io.reads(readPortOffset + i).addr := physReg
      difftestArchFpReg.io
        .fpr(i) := regFiles(1).io.reads(readPortOffset + i).data
    }
  }
}
