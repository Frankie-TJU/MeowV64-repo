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
import meowv64.exec._
import meowv64.instr.Decoder
import meowv64.paging._
import meowv64.reg.RegType
import scala.collection.mutable
import meowv64.instr.Instr

/** DelayedMem = Delayed memory access, memory accesses that have side-effects
  * and thus needs to be preformed in-order.
  *
  * All possible access types are stated in the DelayedMemOp enum
  *   - load: cached load
  *   - s: cached store
  *   - ul: uncached load
  *   - us: uncached store
  */
object DelayedMemOp extends ChiselEnum {
  val load, store, uncachedLoad, uncachedStore, exception = Value
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
  val instr = new Instr()

  val exception = new ExceptionResult()

  /** Physical address
    */
  val addrValid = Bool()
  val addr = UInt(coredef.XLEN.W)
  val len = DCWriteLen()

  /** Result should be sign extended
    */
  val sext = Bool()
  val dataValid = Bool()
  val data = UInt(coredef.VLEN.W)

  /** This is vse.v
    */
  val isVectorStore = Bool()

  /** For retirement
    */
  val regInfo = coredef.REG_INT
  val writeRdEff = Bool()
  val rdPhys = UInt(log2Ceil(regInfo.physRegs).W)
  val rdType = RegType()
  val robIndex = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)

  /** Already written back
    */
  val writeback = Bool()

  // Written data is shared with wb

  def clear() {
    dataValid := false.B
    addrValid := false.B
    exception.valid := false.B
  }

  def canFire = dataValid && addrValid

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

object DelayedMem {
  def empty()(implicit coredef: CoreDef): DelayedMem = {
    val res = Wire(new DelayedMem)
    res := DontCare
    res.dataValid := false.B
    res.addrValid := false.B
    res.exception.valid := false.B
    res
  }
}

object LSUReadState extends ChiselEnum {
  val idle, vectorLoad = Value
}

class SetHasMem(implicit val coredef: CoreDef) extends Bundle {
  val robIndex = Output(UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
}

class LSU(implicit val coredef: CoreDef) extends Module with UnitSelIO {
  val regInfo = coredef.REG_INT
  val flush = IO(Input(Bool()))
  val issue = IO(Flipped(new RegisterReadEgress(regInfo)))
  val retire = IO(new RetirementIO(coredef.LSU_PORT_INFO))
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

  val hasNext = issue.instr.valid // TODO: merge into rs.instr
  val next = WireInit(issue.instr.bits)
  switch(readState) {
    is(LSUReadState.idle) {
      when(!issue.instr.valid) {
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

  // pass fsd/fsw data from FloatToMem
  val toFloat = IO(Flipped(Valid(new FloatToMemReq)))

  val satp = IO(Input(new Satp))
  val status = IO(Input(new Status))
  val ptw = IO(new TLBExt)
  val tlbRst = IO(Input(Bool()))
  val priv = IO(Input(PrivLevel()))

  val DEPTH = coredef.LSQ_DEPTH

  val toExec = IO(new Bundle {
    // set hasMem in rob for delayed memory ops
    val setHasMem = Valid(new SetHasMem())

    // to allocate lsq index
    val lsqIdxBase = Output(UInt(log2Ceil(DEPTH).W))
    val lsqAllocCount = Input(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
    val lsqAllocAccept = Output(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
  })

  val store = RegInit(
    VecInit(Seq.fill(DEPTH)(DelayedMem.empty()))
  )

  val emptyEntries = RegInit(DEPTH.U(log2Ceil(DEPTH + 1).W))
  val head = RegInit(0.U(log2Ceil(DEPTH).W))
  val tail = RegInit(0.U(log2Ceil(DEPTH).W))
  toExec.lsqIdxBase := tail

  // allocation
  tail := tail +% toExec.lsqAllocCount
  toExec.lsqAllocAccept := 0.U
  for (i <- 0 until coredef.ISSUE_NUM) {
    when(emptyEntries > i.U) {
      toExec.lsqAllocAccept := (i + 1).U
    }
  }
  assert(toExec.lsqAllocCount <= toExec.lsqAllocAccept)
  assert(toExec.lsqAllocAccept <= emptyEntries)
  assert(toExec.lsqAllocAccept <= coredef.ISSUE_NUM.U)

  for (i <- 0 until coredef.ISSUE_NUM) {
    val idx = tail +% i.U
    when(toExec.lsqAllocCount > i.U) {
      store(idx).clear()
    }
  }

  // retire
  val retireNum = WireInit(0.U)
  head := head +% retireNum
  emptyEntries := emptyEntries - toExec.lsqAllocCount + retireNum
  assert(emptyEntries <= DEPTH.U)

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

  val release = IO(EnqIO(new Bundle {}))

  val l1pass = Wire(Bool())

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

  // Part 1: compute physical address, check exceptions and save into lsq
  assert(coredef.PADDR_WIDTH > coredef.VADDR_WIDTH)
  val rawAddr = Wire(UInt(coredef.XLEN.W))
  rawAddr := (next.rs1val.asSInt + next.instr.instr.imm).asUInt // We have imm = 0 for R-type instructions
  tlb.query.req.bits.vpn := rawAddr(47, 12)
  // physical address
  val addr = WireDefault(rawAddr)
  when(requiresTranslate) {
    addr := tlb.query.resp.ppn ## rawAddr(11, 0)
  }

  val aligned = addr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(3.W)
  val offset = addr(2, 0)
  val amo = next.instr.instr.op === Decoder.Op("AMO").ident
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

  val fault = WireInit(tlb.query.req.valid && tlb.query.resp.fault)

  val invalAddr = isInvalAddr(rawAddr)
  val misaligned = isMisaligned(offset, next.instr.instr.funct3)

  tlbRequestModify := write

  when(!next.instr.valid) {
    l1pass := false.B
  }.elsewhen(requiresTranslate && !tlb.query.req.ready) {
    l1pass := false.B
  }.otherwise {
    l1pass := true.B
  }

  issue.instr.ready := l1pass

  // set hasMem in rob
  // for side effects
  toExec.setHasMem.valid := false.B
  toExec.setHasMem.bits.robIndex := next.robIndex

  // read store data for fsd/fsw
  when(toFloat.valid) {
    assert(!store(toFloat.bits.lsqIdx).dataValid)
    store(toFloat.bits.lsqIdx).dataValid := true.B
    store(toFloat.bits.lsqIdx).data := toFloat.bits.data
  }

  // save state to lsq
  when(issue.instr.fire) {
    val lsqEntry = store(next.lsqIndex)

    lsqEntry.instr := next.instr.instr
    lsqEntry.writeRdEff := next.instr.instr.writeRdEff()
    lsqEntry.rdPhys := next.rdPhys
    lsqEntry.rdType := next.instr.instr.getRdType()
    lsqEntry.robIndex := next.robIndex

    lsqEntry.addrValid := true.B
    // for fsd/fsw, data is provided by FloatToMem unit
    when(next.instr.instr.op =/= Decoder.Op("STORE-FP").ident) {
      lsqEntry.dataValid := true.B
    }
    when(fenceLike) {
      lsqEntry.op := DelayedMemOp.exception
      when(next.instr.instr.funct3 === Decoder.MEM_MISC_FUNC("FENCE.I")) {
        lsqEntry.exception.ifence(next.instr.addr + 4.U)
      }.otherwise {
        lsqEntry.exception.nofire
      }
    }.elsewhen(invalAddr) {
      lsqEntry.op := DelayedMemOp.exception
      lsqEntry.data := rawAddr
      lsqEntry.exception.ex(
        Mux(
          read,
          ExType.LOAD_ACCESS_FAULT,
          ExType.STORE_ACCESS_FAULT
        )
      )
    }.elsewhen(fault) {
      lsqEntry.op := DelayedMemOp.exception
      lsqEntry.data := rawAddr
      lsqEntry.exception.ex(
        Mux(
          read,
          ExType.LOAD_ACCESS_FAULT,
          ExType.STORE_ACCESS_FAULT
        )
      )
    }.elsewhen(misaligned) {
      lsqEntry.op := DelayedMemOp.exception
      lsqEntry.data := rawAddr
      lsqEntry.exception.ex(
        Mux(
          read,
          ExType.LOAD_ADDR_MISALIGN,
          ExType.STORE_ADDR_MISALIGN
        )
      )
    }.elsewhen(read && !uncached) {
      lsqEntry.op := DelayedMemOp.load
      lsqEntry.addr := addr

      // handle load reserved
      when(amo) {
        // has side effect
        toExec.setHasMem.valid := true.B
        lsqEntry.op := DelayedMemOp.store
        lsqEntry.wop := DCWriteOp.commitLR
      }
    }.elsewhen(read && uncached) {
      // has side effect
      toExec.setHasMem.valid := true.B
      lsqEntry.op := DelayedMemOp.uncachedLoad
      lsqEntry.addr := addr

      switch(next.instr.instr.funct3) {
        is(Decoder.MEM_WIDTH_FUNC("B")) {
          lsqEntry.sext := true.B
          lsqEntry.len := DCWriteLen.B
        }
        is(Decoder.MEM_WIDTH_FUNC("H")) {
          lsqEntry.sext := true.B
          lsqEntry.len := DCWriteLen.H
        }
        is(Decoder.MEM_WIDTH_FUNC("W")) {
          lsqEntry.sext := true.B
          lsqEntry.len := DCWriteLen.W
        }
        is(Decoder.MEM_WIDTH_FUNC("D")) {
          lsqEntry.sext := false.B
          lsqEntry.len := DCWriteLen.D
        }
        is(Decoder.MEM_WIDTH_FUNC("BU")) {
          lsqEntry.sext := false.B
          lsqEntry.len := DCWriteLen.B
        }
        is(Decoder.MEM_WIDTH_FUNC("HU")) {
          lsqEntry.sext := false.B
          lsqEntry.len := DCWriteLen.H
        }
        is(Decoder.MEM_WIDTH_FUNC("WU")) {
          lsqEntry.sext := false.B
          lsqEntry.len := DCWriteLen.W
        }
      }

    }.elsewhen(write) {
      // has side effect
      toExec.setHasMem.valid := true.B
      switch(next.instr.instr.funct3) {
        is(Decoder.MEM_WIDTH_FUNC("B")) { lsqEntry.len := DCWriteLen.B }
        is(Decoder.MEM_WIDTH_FUNC("H")) { lsqEntry.len := DCWriteLen.H }
        is(Decoder.MEM_WIDTH_FUNC("W")) { lsqEntry.len := DCWriteLen.W }
        is(Decoder.MEM_WIDTH_FUNC("D")) { lsqEntry.len := DCWriteLen.D }
      }
      lsqEntry.addr := addr
      lsqEntry.sext := false.B

      when(uncached) {
        lsqEntry.op := DelayedMemOp.uncachedStore
      }.otherwise {
        lsqEntry.op := DelayedMemOp.store
      }

      when(amo) {
        lsqEntry.wop := MuxLookup(
          next.instr.instr.funct7(6, 2),
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
        lsqEntry.wop := DCWriteOp.write
      }

      // for fsd/fsw, data is provided by FloatToMem unit
      when(next.instr.instr.op =/= Decoder.Op("STORE-FP").ident) {
        lsqEntry.data := next.rs2val
      }

    }.otherwise {
      lsqEntry.op := DelayedMemOp.exception
      lsqEntry.exception.ex(
        ExType.ILLEGAL_INSTR
      )
    }
  }

  // Part 2: issue operations from lsq head
  val current = store(head)
  toMem.reader.req.valid := false.B
  toMem.reader.req.bits.addr := current.addr(coredef.PADDR_WIDTH - 1, 3) ## 0.U(
    3.W
  )
  toMem.reader.req.bits.reserve := false.B // TODO
  toMem.writer.req.valid := false.B
  toMem.writer.req.bits.addr := current.addr
  toMem.writer.req.bits.wdata := current.data
  toMem.writer.req.bits.len := current.len
  toMem.writer.req.bits.op := current.wop
  toMem.uncached.addr := current.addr
  toMem.uncached.wdata := current.data
  toMem.uncached.len := current.len
  toMem.uncached.read := false.B
  toMem.uncached.write := false.B
  release.valid := false.B

  // compute write back value from reader
  val shifted =
    toMem.reader.resp.bits >> (current.addr(
      2,
      0
    ) << 3) // TODO: use lookup table?
  val signedResult = Wire(SInt(coredef.XLEN.W)).suggestName("signedResult")
  val result = Wire(UInt(coredef.VLEN.W)).suggestName("result")
  shifted.suggestName("shifted")
  result := signedResult.asUInt
  signedResult := DontCare

  switch(current.instr.funct3) {
    is(Decoder.MEM_WIDTH_FUNC("B")) { signedResult := shifted(7, 0).asSInt }
    is(Decoder.MEM_WIDTH_FUNC("H")) { signedResult := shifted(15, 0).asSInt }
    is(Decoder.MEM_WIDTH_FUNC("W")) { signedResult := shifted(31, 0).asSInt }
    is(Decoder.MEM_WIDTH_FUNC("D")) { result := shifted }
    is(Decoder.MEM_WIDTH_FUNC("BU")) { result := shifted(7, 0) }
    is(Decoder.MEM_WIDTH_FUNC("HU")) { result := shifted(15, 0) }
    is(Decoder.MEM_WIDTH_FUNC("WU")) { result := shifted(31, 0) }
  }

  // special handling for fld/flw
  when(
    current.instr.op === Decoder.Op("LOAD-FP").ident
  ) {
    // nan boxing
    switch(current.instr.funct3) {
      is(Decoder.MEM_WIDTH_FUNC("W")) {
        result := Fill(32, 1.U) ## shifted(31, 0)
      }
      is(Decoder.MEM_WIDTH_FUNC("D")) { result := shifted }
    }
  }

  retire.valid := false.B
  retire.bits := Retirement.empty(regInfo)

  // LSU has higher priority, so always ready
  // we do not wait for retire.ready in LSU
  when(retire.valid) {
    assert(retire.fire)
  }

  val reqSent = RegInit(false.B)
  val advance = WireInit(false.B)
  when(advance) {
    retireNum := 1.U
    head := head +% 1.U
  }

  retire.bits.writeRdEff := current.writeRdEff
  retire.bits.rdPhys := current.rdPhys
  retire.bits.rdType := current.rdType
  retire.bits.robIndex := current.robIndex
  retire.bits.info.wb := result

  when(emptyEntries =/= DEPTH.U && current.canFire) {
    switch(current.op) {
      is(DelayedMemOp.load) {
        toMem.reader.req.valid := ~reqSent
        when(toMem.reader.req.fire) {
          reqSent := true.B
        }

        when(toMem.reader.resp.valid) {
          retire.valid := true.B

          advance := true.B
          reqSent := false.B
        }
      }
      is(DelayedMemOp.uncachedLoad) {
        retire.bits.info.wb := current.getLSB(toMem.uncached.rdata)
        when(release.ready) {
          toMem.uncached.read := true.B
          when(~toMem.uncached.stall) {
            retire.valid := true.B
            release.valid := true.B
            advance := true.B
          }
        }
      }
      is(DelayedMemOp.store) {
        when(release.ready) {
          toMem.writer.req.valid := true.B
          when(toMem.writer.req.fire) {
            release.valid := true.B
            advance := true.B
          }
        }
      }
      is(DelayedMemOp.uncachedStore) {
        when(release.ready) {
          toMem.uncached.write := true.B
          when(~toMem.uncached.stall) {
            release.valid := true.B
            advance := true.B
          }
        }
      }
      is(DelayedMemOp.exception) {
        retire.bits.info.exception := current.exception
        retire.valid := true.B
        when(retire.fire) {
          advance := true.B
        }
      }
    }
  }

  when(flush) {
    head := 0.U
    tail := 0.U
    emptyEntries := DEPTH.U

    reqSent := false.B

    readState := LSUReadState.idle
    vectorReadReqIndex := 0.U
    vectorReadRespIndex := 0.U
  }
}
