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
import meowv64.util.FlushableQueue

import scala.collection.mutable

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

  val satp = IO(Input(new Satp))
  val status = IO(Input(new Status))
  val ptw = IO(new TLBExt)
  val tlbRst = IO(Input(Bool()))
  val priv = IO(Input(PrivLevel()))

  val DEPTH = coredef.LSQ_DEPTH

  val toExec = IO(new Bundle {
    // set hasMem in rob for delayed memory ops
    val valid = Output(Bool())
    val hasMem = Output(Bool())
    val robIndex = Output(UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))

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
  toExec.valid := false.B
  toExec.hasMem := true.B
  toExec.robIndex := next.robIndex

  // save state to lsq
  when(issue.instr.fire) {
    val lsqEntry = store(next.lsqIndex)

    lsqEntry.addrValid := true.B
    lsqEntry.dataValid := true.B
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
    }.elsewhen(read && uncached) {
      // has side effect
      toExec.valid := true.B
      lsqEntry.op := DelayedMemOp.uncachedLoad
      lsqEntry.addr := addr
    }.elsewhen(write) {
      // has side effect
      lsqEntry.op := DelayedMemOp.store
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
  toMem.reader.req.bits := DontCare
  toMem.writer.req.valid := false.B
  toMem.writer.req.bits := DontCare
  toMem.uncached := DontCare
  toMem.uncached.read := false.B
  toMem.uncached.write := false.B
  release.valid := false.B

  retire.valid := false.B
  retire.bits := Retirement.empty(regInfo)

  val advance = WireInit(false.B)
  when(advance) {
    head := head +% 1.U
  }

  retire.bits.writeRdEff := current.writeRdEff
  retire.bits.rdPhys := current.rdPhys
  retire.bits.rdType := current.rdType
  retire.bits.robIndex := current.robIndex

  when(head =/= tail && current.canFire) {
    switch(current.op) {
      is(DelayedMemOp.load) {
        toMem.reader.req.valid := true.B
        when(toMem.reader.req.fire) {
          // TODO: wait for resp
          advance := true.B
        }
      }
      is(DelayedMemOp.store) {
        when(release.ready) {
          release.valid := true.B
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
    readState := LSUReadState.idle
    vectorReadReqIndex := 0.U
    vectorReadRespIndex := 0.U
  }
}
