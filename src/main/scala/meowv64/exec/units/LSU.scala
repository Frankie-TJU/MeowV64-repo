package meowv64.exec.units

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import meowv64.cache._
import meowv64.core.CoreDef
import meowv64.core.ExType
import meowv64.core.PrivLevel
import meowv64.core.Satp
import meowv64.core.SatpMode
import meowv64.core.Status
import meowv64.exec.Retirement
import meowv64.exec._
import meowv64.instr.Decoder
import meowv64.paging._
import meowv64.reg.RegType
import meowv64.util.FlushableQueue

import scala.collection.mutable

/** DelayedMem = Delayed memory access, memory accesses that have side-effects
  * and thus needs to be preformed in-order.
  *
  * All possible access types are stated in the DelayedMemOp enum
  *   - no: No memory access
  *   - s: store
  *   - ul: uncached load
  *   - us: uncached store
  */
object DelayedMemOp extends ChiselEnum {
  val no, s, ul, us = Value
}

/** A (maybe-empty) sequential memory access
  *
  * op: operation addr: address, this is always aligned in log_2(coredef.XLEN)
  * offset: in-line offset len: Bit length sext: Sign extension?
  *
  * Data are stored in the wb field alongside the DelayedMem bundle in execution
  * results
  *
  * @param coredef:
  *   Core definition
  */
class DelayedMem(implicit val coredef: CoreDef) extends Bundle {
  self =>
  val op = DelayedMemOp()
  val wop = DCWriteOp()

  /** Physical address
    */
  val addr = UInt(coredef.XLEN.W)
  val len = DCWriteLen()

  /** Result should be sign extended
    */
  val sext = Bool()
  val data = UInt(coredef.VLEN.W)

  /** This is vse.v
    */
  val isVectorStore = Bool()

  /** For retirement
    */
  val regInfo = coredef.REG_INT
  val writeRd = Bool()
  val rdPhys = UInt(log2Ceil(regInfo.physRegs).W)
  val rdType = RegType()
  val robIndex = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)

  // Written data is shared with wb

  def noop() {
    self := DontCare
    op := DelayedMemOp.no
  }

  def isNoop() = op === DelayedMemOp.no

  def getLSB(raw: UInt): UInt = {
    val ret = Wire(UInt(coredef.XLEN.W))
    ret := DontCare
    when(sext) {
      val sret = Wire(SInt(coredef.XLEN.W))
      sret := DontCare
      switch(len) {
        is(DCWriteLen.B) {
          sret := raw(7, 0).asSInt()
        }
        is(DCWriteLen.H) {
          sret := raw(15, 0).asSInt()
        }
        is(DCWriteLen.W) {
          sret := raw(31, 0).asSInt()
        }
        is(DCWriteLen.D) {
          sret := raw(63, 0).asSInt()
        }
      }

      ret := sret.asUInt()
    }.otherwise {
      switch(len) {
        is(DCWriteLen.B) {
          ret := raw(7, 0)
        }
        is(DCWriteLen.H) {
          ret := raw(15, 0)
        }
        is(DCWriteLen.W) {
          ret := raw(31, 0)
        }
        is(DCWriteLen.D) {
          ret := raw(63, 0)
        }
      }
    }

    ret
  }
}

object LSUReadState extends ChiselEnum {
  val idle, vectorLoad = Value
}

class LSU(implicit val coredef: CoreDef) extends Module with UnitSelIO {
  val regInfo = coredef.REG_INT
  val flush = IO(Input(Bool()))
  val rs = IO(Flipped(new RegisterReadEgress(regInfo)))
  val retire = IO(Output(new Retirement(regInfo)))
  val extras = new mutable.HashMap[String, Data]()

  val vectorReadGroupNum = coredef.VLEN / coredef.XLEN
  val vectorReadReqIndex = RegInit(0.U(log2Ceil(vectorReadGroupNum).W))
  val vectorReadRespIndex = RegInit(0.U(log2Ceil(vectorReadGroupNum).W))
  val vectorReadRespData = RegInit(
    VecInit.fill(vectorReadGroupNum)(0.U(coredef.XLEN.W))
  )
  val inflightVectorReadInstr = Reg(new PipeInstr(regInfo))
  val readState = RegInit(LSUReadState.idle)

  def isUncached(addr: UInt) = addr < BigInt("80000000", 16).U

  val hasNext = rs.instr.valid // TODO: merge into rs.instr
  val next = WireInit(rs.instr.bits)
  switch(readState) {
    is(LSUReadState.idle) {
      when(!rs.instr.valid) {
        next.instr.valid := false.B
      }
    }
    is(LSUReadState.vectorLoad) {
      next := inflightVectorReadInstr
    }
  }

  // FIXME: ValidIO resp
  val toMem = IO(new Bundle {
    val reader = new CoreDCReader
    val writer = new CoreDCWriter(coredef.L1D)
    val uncached = new L1UCPort(coredef.L1D)
  })

  val satp = IO(Input(new Satp))
  val status = IO(Input(new Status))
  val ptw = IO(new TLBExt)
  val tlbRst = IO(Input(Bool()))
  val priv = IO(Input(PrivLevel()))
  // set hasMem in rob for delayed memory ops
  val toExec = IO(new Bundle {
    val valid = Output(Bool())
    val hasMem = Output(Bool())
    val robIndex = Output(UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
  })

  val tlb = Module(new TLB)
  val requiresTranslate = satp.mode =/= SatpMode.bare && (
    priv <= PrivLevel.S
      || status.mprv && (status.mpp =/= PrivLevel.M.asUInt())
  )
  val tlbEffectivePriv =
    Mux(priv === PrivLevel.M, status.mpp.asTypeOf(PrivLevel.M), priv)
  val tlbMode = Wire(TLBLookupMode())
  when(tlbEffectivePriv === PrivLevel.U) {
    tlbMode := TLBLookupMode.U
  }.otherwise {
    tlbMode := Mux(status.sum, TLBLookupMode.both, TLBLookupMode.S)
  }

  val fenceLike = Wire(Bool())

  val tlbRequestModify = WireDefault(false.B)
  tlb.satp := satp
  tlb.ptw <> ptw
  tlb.query.req.valid := requiresTranslate && next.instr.valid && !fenceLike
  tlb.query.req.bits.isModify := tlbRequestModify
  tlb.query.req.bits.mode := tlbMode
  tlb.flush := tlbRst

  val flushed = RegInit(false.B)

  val hasPending = IO(Output(Bool()))
  val release = IO(EnqIO(new DelayedMemResult))

  val pendings = Module(
    new FlushableQueue(new DelayedMem, coredef.INFLIGHT_INSTR_LIMIT)
  )
  hasPending := pendings.io.count > 0.U || pendings.io.enq.fire

  val l2stall = Wire(Bool())
  val l1pass = Wire(Bool())

  pendings.io.flush.get := flush

  // Let's do this without helper

  // Utils
  def isInvalAddr(addr: UInt) = {
    val inval = WireDefault(
      addr(coredef.XLEN - 1, coredef.PADDR_WIDTH).asSInt() =/= addr(
        coredef.PADDR_WIDTH - 1
      ).asSInt()
    )

    when(requiresTranslate) {
      switch(satp.mode) {
        is(SatpMode.sv48) {
          inval := addr(coredef.XLEN - 1, coredef.VADDR_WIDTH)
            .asSInt() =/= addr(coredef.VADDR_WIDTH - 1).asSInt()
        }

        is(SatpMode.sv39) {
          inval := addr(coredef.XLEN - 1, coredef.VADDR_WIDTH - 9)
            .asSInt() =/= addr(coredef.VADDR_WIDTH - 10).asSInt()
        }
      }
    }
    inval
  }

  /** Is this access misaligned?
    *
    * According to ISA, the least two bits of funct3 represents the size of the
    * load/store:
    *   - (L/S)B[U]: 00
    *   - (L/S)H[U]: 01
    *   - (L/S)W[U]: 10
    *   - (L/S)D: 11
    */
  def isMisaligned(offset: UInt, funct3: UInt) = Mux1H(
    UIntToOH(funct3(1, 0)),
    Seq(
      false.B,
      offset(0),
      offset(1, 0).orR,
      offset.orR
    )
  )

  // FIXME: This is incorrect for vle8.v/vse8.v
  val nextInstrIsVLE = WireInit(
    next.instr.instr.op === Decoder.Op("LOAD-FP").ident && next.instr.instr
      .funct3(2) && !next.instr.instr.funct7(1)
  )
  val nextInstrIsVLUXEI = WireInit(
    next.instr.instr.op === Decoder.Op("LOAD-FP").ident && next.instr.instr
      .funct3(2) && next.instr.instr.funct7(1)
  )
  val nextInstrIsVSE = WireInit(
    next.instr.instr.op === Decoder.Op("STORE-FP").ident && next.instr.instr
      .funct3(2)
  )

  val rs2Elements = Wire(Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W)))
  rs2Elements := next.rs2val.asTypeOf(rs2Elements)

  /** Stage 1 state
    */
  assert(coredef.PADDR_WIDTH > coredef.VADDR_WIDTH)
  val rawAddr = Wire(UInt(coredef.XLEN.W))
  when(nextInstrIsVLE) {
    // VLE.V instructions have no imm
    // Compute address from readIndex instead
    rawAddr := next.rs1val + (vectorReadReqIndex << log2Ceil(coredef.XLEN / 8))
  }.elsewhen(nextInstrIsVLUXEI) {
    // VLUXEI.V instructions have no imm
    // Compute address from rs2[readIndex] instead
    rawAddr := next.rs1val + rs2Elements(vectorReadReqIndex)
  }.elsewhen(nextInstrIsVSE) {
    // VSE.V instructions have no imm
    // TODO: handle across page exception
    rawAddr := next.rs1val
  }.otherwise {
    rawAddr := (next.rs1val.asSInt + next.instr.instr.imm).asUInt // We have imm = 0 for R-type instructions
  }
  tlb.query.req.bits.vpn := rawAddr(47, 12)
  // physical address
  val addr = WireDefault(rawAddr)
  when(requiresTranslate) {
    addr := tlb.query.resp.ppn ## rawAddr(11, 0)
  }

  val aligned = addr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(3.W)
  val offset = addr(2, 0)
  val uncached = isUncached(addr) && next.instr.instr.op =/= Decoder
    .Op("AMO")
    .ident // Uncached if < 0x80000000 and is not AMO
  val read = (
    next.instr.instr.op === Decoder.Op("LOAD").ident
      || next.instr.instr.op === Decoder.Op("LOAD-FP").ident
      || next.instr.instr.op === Decoder.Op("AMO").ident && next.instr.instr
        .funct7(6, 2) === Decoder.AMO_FUNC("LR")
  ) && next.instr.valid
  val write = (
    next.instr.instr.op === Decoder.Op("STORE").ident
      || next.instr.instr.op === Decoder.Op("STORE-FP").ident
      || next.instr.instr.op === Decoder.Op("AMO").ident && next.instr.instr
        .funct7(6, 2) =/= Decoder.AMO_FUNC("LR")
  ) && next.instr.valid

  fenceLike := (
    next.instr.instr.op === Decoder.Op("MISC-MEM").ident
      && next.instr.valid
  )

  val invalAddr = isInvalAddr(rawAddr)
  val misaligned = isMisaligned(offset, next.instr.instr.funct3)

  tlbRequestModify := write

  // Maybe we can let flushed request go through
  val canRead = WireDefault(
    read && !uncached && !invalAddr && !misaligned && !flush
  )
  when(requiresTranslate) {
    when(!tlb.query.req.ready) {
      canRead := false.B
    }

    when(tlb.query.resp.fault) {
      canRead := false.B
    }
  }

  toMem.reader.req.bits.reserve := next.instr.instr.op =/= Decoder
    .Op("LOAD")
    .ident && next.instr.instr.op =/= Decoder.Op("LOAD-FP").ident
  toMem.reader.req.bits.addr := Mux(canRead, aligned, 0.U)
  toMem.reader.req.valid := canRead

  when(!next.instr.valid) {
    l1pass := false.B
  }.elsewhen(fenceLike) {
    l1pass := !l2stall
  }.elsewhen(requiresTranslate && !tlb.query.req.ready) {
    l1pass := false.B
  }.elsewhen(canRead) {
    l1pass := toMem.reader.req.ready
  }.otherwise {
    l1pass := !l2stall // Not a DC access, has no waiting side effect, hence we can block indefinitely
  }

  when(l2stall) {
    assert(!toMem.reader.req.ready) // req.ready must be false
  }

  // handle vector load
  // req
  when(canRead && l1pass) {
    switch(readState) {
      is(LSUReadState.idle) {
        when(nextInstrIsVLE || nextInstrIsVLUXEI) {
          readState := LSUReadState.vectorLoad
          vectorReadReqIndex := 1.U
          vectorReadRespIndex := 0.U
          inflightVectorReadInstr := rs.instr.bits
        }
      }
      is(LSUReadState.vectorLoad) {
        vectorReadReqIndex := vectorReadReqIndex + 1.U
        when(vectorReadReqIndex === (vectorReadGroupNum - 1).U) {
          vectorReadReqIndex := 0.U
          readState := LSUReadState.idle
        }
      }
    }
  }
  // resp
  when(readState === LSUReadState.vectorLoad && toMem.reader.resp.valid) {
    vectorReadRespIndex := vectorReadRespIndex + 1.U
    vectorReadRespData(vectorReadRespIndex) := toMem.reader.resp.bits
  }

  rs.instr.ready := l1pass && readState === LSUReadState.idle

  /** Stage 2 state
    */
  val pipeInstr = RegInit(PipeInstr.empty(regInfo))
  val pipeRawAddr = Reg(UInt(coredef.XLEN.W))
  val pipeAddr = Reg(UInt(coredef.XLEN.W))
  val pipeFault = Reg(Bool())
  val pipeDCRead = Reg(Bool())
  val pipeInvalAddr = Reg(Bool())
  val pipeMisaligned = Reg(Bool())
  val pipeRead = Reg(Bool())
  val pipeWrite = Reg(Bool())
  val pipeIsVSE = Reg(Bool())

  l2stall := !toMem.reader.resp.valid && pipeInstr.instr.valid && pipeDCRead
  when(l1pass) {
    pipeFault := tlb.query.req.valid && tlb.query.resp.fault
    pipeInstr := next
    pipeAddr := addr
    pipeRawAddr := rawAddr
    pipeDCRead := toMem.reader.req.fire
    pipeInvalAddr := invalAddr
    pipeMisaligned := misaligned
    pipeRead := read
    pipeWrite := write
    pipeIsVSE := nextInstrIsVSE

    when(!flush) {
      assert(!l2stall)
    }
  }.elsewhen(!l2stall) {
    pipeInstr.instr.valid := false.B
  }

  when(flush) {
    pipeInstr.instr.valid := false.B
    readState := LSUReadState.idle
    vectorReadReqIndex := 0.U
    vectorReadRespIndex := 0.U
  }

  val pipeAligned = pipeAddr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(3.W)
  val pipeOffset = pipeAddr(2, 0)
  val pipeAMO = pipeInstr.instr.instr.op === Decoder.Op("AMO").ident
  val pipeUncached = isUncached(pipeAddr) && !pipeAMO
  val pipeFenceLike = (
    pipeInstr.instr.instr.op === Decoder.Op("MISC-MEM").ident
      && pipeInstr.instr.valid
  )

  retire.valid := pipeInstr.instr.valid
  retire.writeRd := pipeInstr.instr.instr.info.writeRd
  retire.rdType := pipeInstr.instr.instr.getRdType()
  retire.rdPhys := pipeInstr.rdPhys
  retire.robIndex := pipeInstr.robIndex
  when(readState === LSUReadState.vectorLoad) {
    retire.valid := false.B
  }

  /*
   * We should be able to ignore keeping track of flushed here
   * Because if there was an flush, and an corresponding request is running inside DC,
   * then stage 1 requests will never be able to enter stage 2, because DC read ops are in-order
   *
   * If we add MSHR, then we need to keep track of flushed requests
   */
  when(l2stall) {
    retire.valid := false.B
  }

  val shifted =
    toMem.reader.resp.bits >> (pipeOffset << 3) // TODO: use lookup table?
  val signedResult = Wire(SInt(coredef.XLEN.W)).suggestName("signedResult")
  val result = Wire(UInt(coredef.VLEN.W)).suggestName("result")
  shifted.suggestName("shifted")
  result := signedResult.asUInt
  signedResult := DontCare

  switch(pipeInstr.instr.instr.funct3) {
    is(Decoder.MEM_WIDTH_FUNC("B")) { signedResult := shifted(7, 0).asSInt }
    is(Decoder.MEM_WIDTH_FUNC("H")) { signedResult := shifted(15, 0).asSInt }
    is(Decoder.MEM_WIDTH_FUNC("W")) { signedResult := shifted(31, 0).asSInt }
    is(Decoder.MEM_WIDTH_FUNC("D")) { result := shifted }
    is(Decoder.MEM_WIDTH_FUNC("BU")) { result := shifted(7, 0) }
    is(Decoder.MEM_WIDTH_FUNC("HU")) { result := shifted(15, 0) }
    is(Decoder.MEM_WIDTH_FUNC("WU")) { result := shifted(31, 0) }
  }

  // special handling for fld/flw/fsd/fsw/vle.v
  when(
    pipeInstr.instr.instr.op === Decoder.Op("LOAD-FP").ident ||
      pipeInstr.instr.instr.op === Decoder.Op("STORE-FP").ident
  ) {
    // nan boxing
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.MEM_WIDTH_FUNC("W")) {
        result := Fill(32, 1.U) ## shifted(31, 0)
      }
      is(Decoder.MEM_WIDTH_FUNC("D")) { result := shifted }
      is(0.U, 5.U, 6.U, 7.U) {
        // special handling for vle.v
        result := Cat(vectorReadRespData.reverse)
      }
    }
  }

  // Retirement
  val mem = Wire(new DelayedMem)
  mem.noop() // By default
  mem.writeRd := pipeInstr.instr.instr.info.writeRd
  mem.rdPhys := pipeInstr.rdPhys
  mem.rdType := pipeInstr.instr.instr.getRdType()
  mem.robIndex := pipeInstr.robIndex

  retire.info := RetireInfo.vacant(regInfo)
  retire.info.branchTaken := false.B // not a branch instruction
  when(pipeFenceLike) {
    retire.info.wb := DontCare
    when(pipeInstr.instr.instr.funct3 === Decoder.MEM_MISC_FUNC("FENCE.I")) {
      retire.info.exception.ifence(pipeInstr.instr.addr + 4.U)
    }.otherwise {
      retire.info.exception.nofire
    }
  }.elsewhen(pipeInvalAddr) {
    retire.info.wb := pipeRawAddr
    retire.info.exception.ex(
      Mux(
        pipeRead,
        ExType.LOAD_ACCESS_FAULT,
        ExType.STORE_ACCESS_FAULT
      )
    )
  }.elsewhen(pipeFault) {
    retire.info.wb := pipeRawAddr
    retire.info.exception.ex(
      Mux(
        pipeRead,
        ExType.LOAD_PAGE_FAULT,
        ExType.STORE_PAGE_FAULT
      )
    )
  }.elsewhen(pipeMisaligned) {
    retire.info.wb := pipeRawAddr
    retire.info.exception.ex(
      Mux(
        pipeRead,
        ExType.LOAD_ADDR_MISALIGN,
        ExType.STORE_ADDR_MISALIGN
      )
    )
  }.elsewhen(pipeRead && !pipeUncached) {
    retire.info.exception.nofire
    retire.info.wb := result
    when(pipeAMO) { // Must be LR
      mem.op := DelayedMemOp.s
      mem.wop := DCWriteOp.commitLR
      mem.addr := pipeAddr
      mem.data := result
    }
  }.elsewhen(pipeRead) {
    retire.info.exception.nofire

    mem.op := DelayedMemOp.ul
    mem.addr := pipeAddr
    mem.sext := DontCare
    mem.len := DontCare
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.MEM_WIDTH_FUNC("B")) {
        mem.sext := true.B
        mem.len := DCWriteLen.B
      }
      is(Decoder.MEM_WIDTH_FUNC("H")) {
        mem.sext := true.B
        mem.len := DCWriteLen.H
      }
      is(Decoder.MEM_WIDTH_FUNC("W")) {
        mem.sext := true.B
        mem.len := DCWriteLen.W
      }
      is(Decoder.MEM_WIDTH_FUNC("D")) {
        mem.sext := false.B
        mem.len := DCWriteLen.D
      }
      is(Decoder.MEM_WIDTH_FUNC("BU")) {
        mem.sext := false.B
        mem.len := DCWriteLen.B
      }
      is(Decoder.MEM_WIDTH_FUNC("HU")) {
        mem.sext := false.B
        mem.len := DCWriteLen.H
      }
      is(Decoder.MEM_WIDTH_FUNC("WU")) {
        mem.sext := false.B
        mem.len := DCWriteLen.W
      }
    }

    retire.info.wb := DontCare
    mem.data := DontCare
  }.elsewhen(pipeWrite) {
    retire.info.exception.nofire

    mem.len := DontCare
    switch(pipeInstr.instr.instr.funct3) {
      is(Decoder.MEM_WIDTH_FUNC("B")) { mem.len := DCWriteLen.B }
      is(Decoder.MEM_WIDTH_FUNC("H")) { mem.len := DCWriteLen.H }
      is(Decoder.MEM_WIDTH_FUNC("W")) { mem.len := DCWriteLen.W }
      is(Decoder.MEM_WIDTH_FUNC("D")) { mem.len := DCWriteLen.D }
    }
    mem.addr := pipeAddr
    mem.sext := false.B

    when(pipeUncached) {
      mem.op := DelayedMemOp.us
    } otherwise {
      mem.op := DelayedMemOp.s
    }

    when(pipeAMO) {
      mem.wop := MuxLookup(
        pipeInstr.instr.instr.funct7(6, 2),
        DCWriteOp.write,
        Seq(
          Decoder.AMO_FUNC("SC") -> DCWriteOp.cond,
          Decoder.AMO_FUNC("AMOSWAP") -> DCWriteOp.swap,
          Decoder.AMO_FUNC("AMOADD") -> DCWriteOp.add,
          Decoder.AMO_FUNC("AMOXOR") -> DCWriteOp.xor,
          Decoder.AMO_FUNC("AMOAND") -> DCWriteOp.and,
          Decoder.AMO_FUNC("AMOOR") -> DCWriteOp.or,
          Decoder.AMO_FUNC("AMOMIN") -> DCWriteOp.min,
          Decoder.AMO_FUNC("AMOMAX") -> DCWriteOp.max,
          Decoder.AMO_FUNC("AMOMINU") -> DCWriteOp.minu,
          Decoder.AMO_FUNC("AMOMAXU") -> DCWriteOp.maxu
        )
      )
    }.otherwise {
      mem.wop := DCWriteOp.write
    }

    retire.info.wb := DontCare
    // vse.v writes data stored in vs3
    mem.data := Mux(pipeIsVSE, pipeInstr.rs3val, pipeInstr.rs2val)
  }.otherwise {
    // Inval instr?
    retire.info.exception.ex(ExType.ILLEGAL_INSTR)
    retire.info.wb := DontCare
  }

  // when pushing to queue
  // set hasMem in rob
  val push = mem.op =/= DelayedMemOp.no && pipeInstr.instr.valid && !l2stall
  toExec.valid := push
  toExec.hasMem := true.B
  toExec.robIndex := pipeInstr.robIndex
  when(push) {
    retire.valid := false.B
  }

  pendings.io.enq.bits := mem
  pendings.io.enq.valid := push
  assert(pendings.io.enq.ready)

  // Delayed memory ops
  // Pop memory op from pendings, set release.valid to signal completion
  val pendingHead = pendings.io.deq.bits
  toMem.writer.req.bits.wdata := pendingHead.data
  toMem.uncached.wdata := pendingHead.data

  toMem.writer.req.bits.addr := pendingHead.addr
  toMem.uncached.addr := pendingHead.addr

  // toMem.writer.be := pendingHead.be
  toMem.writer.req.bits.len := pendingHead.len
  toMem.uncached.len := pendingHead.len

  toMem.writer.req.valid := false.B
  toMem.writer.req.bits.op := DCWriteOp.write
  toMem.uncached.read := false.B
  toMem.uncached.write := false.B

  when(pendings.io.deq.valid && release.ready) {
    toMem.writer.req.bits.op := pendingHead.wop
    toMem.writer.req.valid := pendingHead.op === DelayedMemOp.s

    toMem.uncached.read := pendingHead.op === DelayedMemOp.ul
    toMem.uncached.write := pendingHead.op === DelayedMemOp.us
  }

  release.bits.data := DontCare
  switch(pendingHead.op) {
    is(DelayedMemOp.s) {
      release.bits.data := toMem.writer.rdata

      when(pendingHead.wop === DCWriteOp.commitLR) {
        release.bits.data := pendingHead.data
      }
    }

    is(DelayedMemOp.ul) {
      release.bits.data := pendingHead.getLSB(toMem.uncached.rdata)
    }
  }

  // FIXME: maybe we can ignore this, because for all such operations, we have erd =/= 0
  release.bits.hasWB := (
    pendingHead.op === DelayedMemOp.ul
      || pendingHead.op === DelayedMemOp.s && (
        pendingHead.wop =/= DCWriteOp.write
      )
  )

  val finished = (
    pendings.io.deq.valid
      && Mux(
        pendingHead.op === DelayedMemOp.s,
        toMem.writer.req.ready,
        !toMem.uncached.stall
      )
  )
  release.valid := finished

  // write to register if WB=1
  // currently, if a delayed memory op is inflight,
  // no read will be issued
  // so we can freely override the write port here
  when(release.fire) {
    retire.info.wb := release.bits.data
    retire.valid := true.B
    retire.writeRd := pendingHead.writeRd
    retire.rdType := pendingHead.rdType
    retire.rdPhys := pendingHead.rdPhys
    retire.robIndex := pendingHead.robIndex
  }

  when(release.fire) {
    pendings.io.deq.deq()
  } otherwise {
    pendings.io.deq.nodeq()
  }

  // check:
  // release.ready remains high until release.fire
  when(RegNext(release.ready && !release.valid, false.B)) {
    assert(release.ready)
  }
}
