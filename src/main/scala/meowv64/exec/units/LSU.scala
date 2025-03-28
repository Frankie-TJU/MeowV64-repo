package meowv64.exec.units

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.UIntIsOneOf
import meowv64.cache._
import meowv64.core.CoreDef
import meowv64.core.ExType
import meowv64.core.PrivLevel
import meowv64.core.Satp
import meowv64.core.SatpMode
import meowv64.core.Status
import meowv64.core.VState
import meowv64.exec._
import meowv64.instr.Decoder
import meowv64.instr.Instr
import meowv64.paging._
import meowv64.reg.RegType

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
  val load, uncachedLoad = Value
  val vectorLoad, vectorUncachedLoad, vectorIndexedLoad, loadReserved = Value
  val store, uncachedStore, vectorStore = Value
  val exception = Value
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

  /** Result should be NaN-boxed for float loads
    */
  val nan_boxing = Bool()
  val dataValid = Bool()

  // TODO: move these big registers to a separate queue
  /** Write data, or exception trap value, or original vd
    */
  val data = UInt(coredef.VLEN.W)

  /** Vector load index
    */
  val index = UInt(coredef.VLEN.W)

  /** Vector mask
    */
  val vm = UInt((coredef.VLEN / 8).W)

  /** For retirement
    */
  val regInfo = coredef.REG_VEC
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
          sret := raw(7, 0).asSInt
        }
        is(DCWriteLen.H) {
          sret := raw(15, 0).asSInt
        }
        is(DCWriteLen.W) {
          sret := raw(31, 0).asSInt
        }
        is(DCWriteLen.D) {
          sret := raw(63, 0).asSInt
        }
      }

      ret := sret.asUInt
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

    // nan boxing
    when(nan_boxing) {
      switch(len) {
        is(DCWriteLen.H) {
          ret := Fill(48, 1.U) ## raw(15, 0)
        }
        is(DCWriteLen.W) {
          ret := Fill(32, 1.U) ## raw(31, 0)
        }
        is(DCWriteLen.D) {
          ret := raw
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

class SetHasMem(implicit val coredef: CoreDef) extends Bundle {
  val robIndex = Output(UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
}

class LSU(implicit val coredef: CoreDef) extends Module with UnitSelIO {
  val regInfo = coredef.REG_INT
  val flush = IO(Input(Bool()))
  val issue = IO(Flipped(new RegisterReadEgress(regInfo)))
  val retire = IO(new RetirementIO(coredef.LSU_PORT_INFO))
  val hartId = IO(Input(UInt(32.W)))
  val extras = new mutable.HashMap[String, Data]()

  val vectorMaxBeats = coredef.VLEN / 8
  val vectorReadReqIndex = RegInit(0.U(log2Ceil(vectorMaxBeats + 1).W))
  val vectorReadRespIndex = RegInit(0.U(log2Ceil(vectorMaxBeats + 1).W))
  val vectorReadRespData = RegInit(0.U(coredef.VLEN.W))
  val vectorReadRespDataComb = Wire(UInt(coredef.VLEN.W))
  vectorReadRespDataComb := vectorReadRespData
  vectorReadRespData := vectorReadRespDataComb
  val vectorWriteReqIndex = RegInit(0.U(log2Ceil(vectorMaxBeats + 1).W))
  val vectorWriteData = Wire(Vec(vectorMaxBeats, UInt(coredef.XLEN.W)))

  // match MeowV64Configs.scala
  def isUncached(addr: UInt) =
    addr < BigInt("80000000", 16).U || addr > BigInt("200000000", 16).U

  // add pipeline stage because TLB has long comb path
  val stagedInst = Queue(
    issue.instr,
    entries = 1,
    pipe = false,
    flow = true,
    flush = Some(flush)
  )

  val hasNext = stagedInst.valid // TODO: merge into rs.instr
  val next = WireInit(stagedInst.bits)

  // FIXME: ValidIO resp
  val toMem = IO(new Bundle {
    val reader = new CoreDCReader
    val writer = new CoreDCWriter(coredef.L1D)
    val uncached = new L1UCPort(coredef.L1D)
  })
  val toBuffets = IO(new Bundle {
    val head = Flipped(Decoupled(Vec(32, UInt(8.W))))
  })
  val BUFFETS_FASTPATH = 0x51000000

  // pass fsd/fsw data from FloatToMem
  val toFloat = IO(Flipped(Valid(new FloatToMemReq)))
  // pass vse.v data from FloatToMem
  val toVector = IO(Flipped(Valid(new VectorToMemReq)))

  val satp = IO(Input(new Satp))
  val status = IO(Input(new Status))
  val ptw = IO(new TLBExt)
  val tlbRst = IO(Input(Bool()))
  val priv = IO(Input(PrivLevel()))
  val vState = IO(Input(new VState()))

  val DEPTH = coredef.LSQ_DEPTH

  val toExec = IO(new Bundle {
    // set hasMem in rob for delayed memory ops
    val setHasMem = Valid(new SetHasMem())

    // to allocate lsq index
    val lsqIdxBase = Output(UInt(log2Ceil(DEPTH).W))
    val lsqAllocCount = Input(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
    val lsqAllocAccept = Output(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
    val lsqEmptyEntries = Output(UInt(log2Ceil(DEPTH + 1).W))
  })

  val queue = RegInit(
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
  toExec.lsqEmptyEntries := emptyEntries
  assert(toExec.lsqAllocCount <= toExec.lsqAllocAccept)
  assert(toExec.lsqAllocAccept <= emptyEntries)
  assert(toExec.lsqAllocAccept <= coredef.ISSUE_NUM.U)

  for (i <- 0 until coredef.ISSUE_NUM) {
    val idx = tail +% i.U
    when(toExec.lsqAllocCount > i.U) {
      queue(idx).clear()
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
      || status.mprv && (status.mpp =/= PrivLevel.M.asUInt)
  )
  // Note that, while SUM is ordinarily ignored when not executing in S-mode, it
  // is in effect when MPRV=1 and MPP=S.
  val tlbEffectivePriv =
    Mux(priv === PrivLevel.M, status.mpp.asTypeOf(PrivLevel.M), priv)
  val tlbMode = Wire(TLBLookupMode())
  when(tlbEffectivePriv === PrivLevel.U) {
    tlbMode := TLBLookupMode.U
  }.otherwise {
    // The SUM (permit Supervisor User Memory access) bit modifies the privilege
    // with which S-mode loads and stores access virtual memory. When SUM=0,
    // S-mode memory accesses to pages that are accessible by U-mode (U=1 in
    // Figure 4.18) will fault. When SUM=1, these accesses are permitted.
    tlbMode := Mux(status.sum, TLBLookupMode.both, TLBLookupMode.S)
  }

  val fenceLike = Wire(Bool())

  val tlbRequestModify = WireDefault(false.B)
  tlb.satp := satp
  tlb.ptw <> ptw
  tlb.query.req.valid := requiresTranslate && hasNext && !fenceLike
  when(tlbRequestModify) {
    tlb.query.req.bits.access := TLBAccessMode.W
  }.otherwise {
    tlb.query.req.bits.access := TLBAccessMode.R
  }
  tlb.query.req.bits.mxr := status.mxr
  tlb.query.req.bits.mode := tlbMode
  tlb.flush := tlbRst

  val release = IO(EnqIO(new Bundle {}))

  // check: release ready should be stable until valid
  when(RegNext(release.ready && ~release.valid)) {
    assert(release.ready)
  }

  val l1pass = Wire(Bool())

  // Let's do this without helper

  // Utils
  def isInvalAddr(addr: UInt) = {
    val inval = WireDefault(
      addr(coredef.XLEN - 1, coredef.PADDR_WIDTH).asSInt =/= addr(
        coredef.PADDR_WIDTH - 1
      ).asSInt
    )

    when(requiresTranslate) {
      switch(satp.mode) {
        is(SatpMode.sv48) {
          inval := addr(coredef.XLEN - 1, coredef.VADDR_WIDTH).asSInt =/= addr(
            coredef.VADDR_WIDTH - 1
          ).asSInt
        }

        is(SatpMode.sv39) {
          inval := addr(
            coredef.XLEN - 1,
            coredef.VADDR_WIDTH - 9
          ).asSInt =/= addr(coredef.VADDR_WIDTH - 10).asSInt
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

  def align(addr: UInt) = {
    addr(coredef.PADDR_WIDTH - 1, 3) ##
      0.U(3.W)
  }

  def getBE(addr: UInt, len: DCWriteLen.Type) = {
    val IGNORED_WIDTH = log2Ceil(coredef.L1D.TO_CORE_TRANSFER_BYTES)
    val offset = addr(IGNORED_WIDTH - 1, 0)
    val mask = DCWriteLen.toByteEnable(len)
    val sliced = Wire(UInt(coredef.L1D.TO_CORE_TRANSFER_BYTES.W))
    sliced := mask << offset
    sliced
  }

  // Part 1: compute physical address, check exceptions and save into lsq
  assert(coredef.PADDR_WIDTH > coredef.VADDR_WIDTH)
  // vle.v
  val vectorLoad =
    next.instr.instr.op === Decoder.Op("LOAD-FP").ident &&
      next.instr.instr.mop === 0x0.U &&
      next.instr.instr.funct3.isOneOf(Seq(0.U, 5.U, 6.U, 7.U))
  // vluxei.v
  val vectorIndexedLoad =
    next.instr.instr.op === Decoder.Op("LOAD-FP").ident &&
      next.instr.instr.mop === 0x1.U &&
      next.instr.instr.funct3.isOneOf(Seq(0.U, 5.U, 6.U, 7.U))
  // vse.v
  val vectorStore =
    next.instr.instr.op === Decoder
      .Op("STORE-FP")
      .ident && next.instr.instr.funct3.isOneOf(Seq(0.U, 5.U, 6.U, 7.U))

  val rawAddr = Wire(UInt(coredef.XLEN.W))
  when(vectorLoad || vectorStore || vectorIndexedLoad) {
    // no imm
    // TODO: handle vector across page
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
  val amo = next.instr.instr.op === Decoder.Op("AMO").ident
  val uncached = isUncached(addr) && next.instr.instr.op =/= Decoder
    .Op("AMO")
    .ident // Uncached if < 0x80000000 and is not AMO
  val load = (
    next.instr.instr.op === Decoder.Op("LOAD").ident
      || next.instr.instr.op === Decoder.Op("LOAD-FP").ident
      || next.instr.instr.op === Decoder.Op("AMO").ident && next.instr.instr
        .funct7(6, 2) === Decoder.AMO_FUNC("LR")
  ) && next.instr.valid
  val store = (
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

  tlbRequestModify := store

  when(!next.instr.valid) {
    l1pass := false.B
  }.elsewhen(fenceLike) {
    l1pass := true.B
  }.elsewhen(requiresTranslate && !tlb.query.req.ready) {
    l1pass := false.B
  }.otherwise {
    l1pass := true.B
  }

  stagedInst.ready := l1pass

  // set hasMem in rob
  // for side effects
  toExec.setHasMem.valid := false.B
  toExec.setHasMem.bits.robIndex := next.robIndex

  // read store data for fsd/fsw
  when(toFloat.valid) {
    assert(!queue(toFloat.bits.lsqIdx).dataValid)
    queue(toFloat.bits.lsqIdx).dataValid := true.B
    queue(toFloat.bits.lsqIdx).data := toFloat.bits.data
  }

  // read store data for vse.v or vd for vle.v
  when(toVector.valid) {
    assert(!queue(toVector.bits.lsqIdx).dataValid)
    queue(toVector.bits.lsqIdx).dataValid := true.B
    queue(toVector.bits.lsqIdx).data := toVector.bits.data
    queue(toVector.bits.lsqIdx).index := toVector.bits.index
    queue(toVector.bits.lsqIdx).vm := toVector.bits.vm
  }

  // save state to lsq
  when(stagedInst.fire) {
    val lsqEntry = queue(next.lsqIndex)

    lsqEntry.instr := next.instr.instr
    lsqEntry.writeRdEff := next.instr.instr.writeRdEff()
    lsqEntry.rdPhys := next.rdPhys
    lsqEntry.rdType := next.instr.instr.getRdType()
    lsqEntry.robIndex := next.robIndex

    lsqEntry.addrValid := true.B
    // for ld/lw/fld/flw, data is valid
    // for fsd/fsw/vse.v/vle.v/vluxei.v, data is provided by FloatToMem/VectorToMem unit
    when(
      next.instr.instr.op =/= Decoder.Op("STORE-FP").ident &&
        !vectorLoad && !vectorIndexedLoad
    ) {
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
      // faulting address
      lsqEntry.data := rawAddr
      lsqEntry.exception.ex(
        Mux(
          load,
          ExType.LOAD_ACCESS_FAULT,
          ExType.STORE_ACCESS_FAULT
        )
      )
    }.elsewhen(fault) {
      lsqEntry.op := DelayedMemOp.exception
      // faulting address
      lsqEntry.data := rawAddr
      lsqEntry.exception.ex(
        Mux(
          load,
          ExType.LOAD_PAGE_FAULT,
          ExType.STORE_PAGE_FAULT
        )
      )
    }.elsewhen(misaligned) {
      lsqEntry.op := DelayedMemOp.exception
      // faulting address
      lsqEntry.data := rawAddr
      lsqEntry.exception.ex(
        Mux(
          load,
          ExType.LOAD_ADDR_MISALIGN,
          ExType.STORE_ADDR_MISALIGN
        )
      )
    }.elsewhen(load && !uncached) {
      lsqEntry.addr := addr

      // handle load reserved
      when(amo) {
        // has side effect
        toExec.setHasMem.valid := true.B
        lsqEntry.op := DelayedMemOp.loadReserved
        lsqEntry.wop := DCWriteOp.commitLR
      }.elsewhen(vectorLoad) {
        lsqEntry.op := DelayedMemOp.vectorLoad
      }.elsewhen(vectorIndexedLoad) {
        lsqEntry.op := DelayedMemOp.vectorIndexedLoad
      }.otherwise {
        lsqEntry.op := DelayedMemOp.load
      }
    }.elsewhen(load && uncached) {
      // has side effect
      toExec.setHasMem.valid := true.B
      when(vectorLoad) {
        lsqEntry.op := DelayedMemOp.vectorUncachedLoad
      }.otherwise {
        lsqEntry.op := DelayedMemOp.uncachedLoad
      }
      lsqEntry.addr := addr

      // special handling for fld/flw
      lsqEntry.nan_boxing := next.instr.instr.op === Decoder.Op("LOAD-FP").ident

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

      when(vectorLoad) {
        // read whole 256b = 32B
        lsqEntry.len := DCWriteLen.O
      }

    }.elsewhen(store) {
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
      }.elsewhen(vectorStore) {
        lsqEntry.len := DCWriteLen.D
        lsqEntry.op := DelayedMemOp.vectorStore
      }.otherwise {
        lsqEntry.op := DelayedMemOp.store
      }

      when(amo) {
        lsqEntry.wop := MuxLookup(
          next.instr.instr.funct7(6, 2),
          DCWriteOp.write
        )(
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
      lsqEntry.data := next.instr.instr.raw
      // faulting instr
      lsqEntry.exception.ex(
        ExType.ILLEGAL_INSTR
      )
    }
  }

  // Part 2: issue operations from lsq head
  val current = queue(head)
  toMem.reader.req.valid := false.B
  toMem.reader.req.bits.addr := align(current.addr)
  toMem.reader.req.bits.reserve := false.B // TODO
  toMem.writer.req.valid := false.B
  toMem.writer.req.bits.addr := current.addr
  toMem.writer.req.bits.wdata := current.data
  toMem.writer.req.bits.len := current.len
  toMem.writer.req.bits.be := getBE(current.addr, current.len)
  toMem.writer.req.bits.op := current.wop
  toMem.uncached.addr := current.addr
  toMem.uncached.wdata := current.data
  toMem.uncached.len := current.len
  toMem.uncached.req := L1UCReq.idle
  release.valid := false.B
  toBuffets.head.ready := false.B

  // compute write back value from reader
  val shifted =
    toMem.reader.resp.bits >>
      (current.addr(2, 0) << 3) // TODO: use lookup table?
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
      is(Decoder.MEM_WIDTH_FUNC("H")) {
        result := Fill(48, 1.U) ## shifted(15, 0)
      }
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

  val flushedRead = RegInit(false.B)
  val reqSent = RegInit(false.B)
  val advance = WireInit(false.B)
  when(advance) {
    retireNum := 1.U
  }

  retire.bits.writeRdEff := current.writeRdEff
  retire.bits.rdPhys := current.rdPhys
  retire.bits.rdType := current.rdType
  retire.bits.robIndex := current.robIndex
  retire.bits.info.wb := result

  when(
    current.instr.op === Decoder.Op("LOAD-FP").ident
  ) {
    switch(current.instr.funct3) {
      is(Decoder.MEM_WIDTH_FUNC("H")) {
        retire.bits.info.markFSDirty := true.B
      }
      is(Decoder.MEM_WIDTH_FUNC("W")) {
        retire.bits.info.markFSDirty := true.B
      }
      is(Decoder.MEM_WIDTH_FUNC("D")) {
        retire.bits.info.markFSDirty := true.B
      }
    }
  }

  vectorWriteData := current.data.asTypeOf(vectorWriteData)

  // handle flush between req and resp
  val actualRespValid = WireInit(toMem.reader.resp.valid)
  when(flushedRead && toMem.reader.resp.valid) {
    flushedRead := false.B
    actualRespValid := false.B
  }

  // compute memory access beats from vl
  // beat width is coredef.L1D.TO_CORE_TRANSFER_WIDTH
  // sew=8<<vsew
  val IGNORED_WIDTH = log2Ceil(coredef.L1D.TO_CORE_TRANSFER_BYTES)
  val vectorBeats = Wire(UInt(log2Ceil(vectorMaxBeats + 1).W))
  when(current.op === DelayedMemOp.vectorIndexedLoad) {
    // one beat for each element
    vectorBeats := vState.vl
  }.otherwise {
    // end address = start address + (vl * sew) / 8 = start address + (vl << vsew)
    // beats: ceil((start address offset + vl * sew / 8) / TO_CORE_TRANSFER_BYTES)
    // = (start address offset + (vl << vsew) + TO_CORE_TRANSFER_BYTES - 1) / TO_CORE_TRANSFER_BYTES
    vectorBeats := (current.addr(IGNORED_WIDTH - 1, 0) +
      (vState.vl << vState.vtype.vsew) + (coredef.L1D.TO_CORE_TRANSFER_BYTES - 1).U) >>
      IGNORED_WIDTH
  }

  when(emptyEntries =/= DEPTH.U && current.canFire) {
    switch(current.op) {
      is(DelayedMemOp.load) {
        toMem.reader.req.valid := ~reqSent
        when(toMem.reader.req.fire) {
          reqSent := true.B
        }

        when(actualRespValid) {
          retire.valid := true.B

          advance := true.B
          reqSent := false.B
        }
      }
      is(DelayedMemOp.vectorLoad, DelayedMemOp.vectorIndexedLoad) {
        when(current.op === DelayedMemOp.vectorLoad) {
          // at most two beats
          assert(vectorBeats <= 2.U)
          when(vectorReadReqIndex === 0.U) {
            // first beat
            toMem.reader.req.bits.addr := current.addr
          }.otherwise {
            toMem.reader.req.bits.addr :=
              (current.addr(coredef.PADDR_WIDTH - 1, IGNORED_WIDTH) ##
                0.U(IGNORED_WIDTH.W)) +
                (vectorReadReqIndex << IGNORED_WIDTH)
          }
        }.otherwise {
          // indexed load
          // sew = 8 << vsew
          // shift = sew * reqIndex = reqIndex << 3 << vsew
          val shift = (vectorReadReqIndex << 3) << vState.vtype.vsew
          // mask = (1 << sew) - 1
          val mask = MuxLookup(vState.vtype.vsew, 0.U)(
            Seq(
              0.U -> 0xffL.U, // 8
              1.U -> 0xffffL.U, // 16
              2.U -> 0xffffffffL.U, // 32
              3.U -> ((BigInt(1) << 64) - 1).U // 64
            )
          )
          // TODO: how to handle masked off elements?
          val offset =
            WireInit((current.index >> shift)(coredef.XLEN - 1, 0) & mask)
          toMem.reader.req.bits.addr := current.addr +% offset
        }
        toMem.reader.req.valid := vectorReadReqIndex =/= vectorBeats
        when(toMem.reader.req.fire) {
          vectorReadReqIndex := vectorReadReqIndex + 1.U
          reqSent := true.B
        }

        when(actualRespValid) {
          reqSent := toMem.reader.req.fire
          vectorReadRespIndex := vectorReadRespIndex + 1.U

          when(current.op === DelayedMemOp.vectorLoad) {
            when(vectorReadRespIndex === 0.U) {
              // first beat
              vectorReadRespDataComb := toMem.reader.resp.bits
            }.otherwise {
              // second beat
              vectorReadRespDataComb := vectorReadRespData |
                (toMem.reader.resp.bits <<
                  ((coredef.L1D.TO_CORE_TRANSFER_BYTES.U -
                    current.addr(IGNORED_WIDTH - 1, 0)) << 3))
            }
          }.otherwise {
            // indexed load
            // assume SEW matches instruction
            // so that index count = element count
            for (vsew <- 0 to 3) {
              val width = 8 << vsew
              // handle eew
              when(vState.vtype.vsew === vsew.U) {
                vectorReadRespDataComb := vectorReadRespData |
                  (toMem.reader.resp.bits(width - 1, 0) <<
                    (vectorReadRespIndex << log2Ceil(width)))
              }
            }
          }

          when(vectorReadReqIndex === vectorBeats) {
            retire.valid := true.B

            advance := true.B
            vectorReadRespData := 0.U
            vectorReadReqIndex := 0.U
            vectorReadRespIndex := 0.U
          }
        }

        // handle vm
        val maskVec = WireInit(VecInit.fill(coredef.VLEN / 8)(true.B))
        val mask = Cat(maskVec.reverse)
        for (vsew <- 0 to 3) {
          val width = 8 << vsew
          // handle eew
          when(vState.vtype.vsew === vsew.U) {
            for (lane <- 0 until coredef.VLEN / width) {
              // compute byte mask
              when(
                (~current.vm(lane) &&
                  current.instr.readVm()) || lane.U >= vState.vl
              ) {
                for (j <- 0 until width / 8) {
                  maskVec(lane * width / 8 + j) := false.B
                }
              }
            }
          }
        }

        retire.bits.info.wb := MuxBE(mask, vectorReadRespDataComb, current.data)
      }
      is(DelayedMemOp.uncachedLoad, DelayedMemOp.vectorUncachedLoad) {
        when(current.addr === BUFFETS_FASTPATH.U) {
          when(current.op === DelayedMemOp.uncachedLoad) {
            retire.bits.info.wb := current.getLSB(toBuffets.head.bits.asUInt)
          }.otherwise {
            // vector uncached load
            retire.bits.info.wb := toBuffets.head.bits.asUInt
          }
          when(release.ready && toBuffets.head.valid) {
            retire.valid := true.B
            release.valid := true.B
            advance := true.B
            toBuffets.head.ready := true.B
          }
        }.otherwise {
          when(current.op === DelayedMemOp.uncachedLoad) {
            retire.bits.info.wb := current.getLSB(toMem.uncached.rdata)
          }.otherwise {
            // vector uncached load
            retire.bits.info.wb := toMem.uncached.rdata
          }
          when(release.ready) {
            toMem.uncached.req := L1UCReq.read
            when(~toMem.uncached.stall) {
              retire.valid := true.B
              release.valid := true.B
              advance := true.B
            }
          }
        }
      }
      is(DelayedMemOp.loadReserved) {
        // stage 1: read reserved
        toMem.reader.req.valid := ~reqSent
        toMem.reader.req.bits.reserve := true.B
        when(toMem.reader.req.fire) {
          reqSent := true.B
        }

        // stage 2: commit LR
        when(actualRespValid) {
          retire.valid := true.B
          current.op := DelayedMemOp.store

          reqSent := false.B
        }
      }
      is(DelayedMemOp.store) {
        when(release.ready) {
          toMem.writer.req.valid := true.B
          when(toMem.writer.req.fire) {
            // for amo instructions, write result
            when(
              current.wop =/= DCWriteOp.write && current.wop =/= DCWriteOp.commitLR
            ) {
              retire.valid := true.B
              retire.bits.info.wb := toMem.writer.rdata
            }

            release.valid := true.B
            advance := true.B
          }
        }
      }
      is(DelayedMemOp.vectorStore) {
        val WIDTH = coredef.L1D.TO_CORE_TRANSFER_WIDTH
        assert(vectorBeats <= 2.U)
        val vectorStoreAddr = Reg(UInt(coredef.PADDR_WIDTH.W))
        when(vectorWriteReqIndex === 0.U) {
          // first beat
          toMem.writer.req.bits.addr := current.addr
          toMem.writer.req.bits.wdata := current.data
        }.otherwise {
          // improve timing
          toMem.writer.req.bits.addr := vectorStoreAddr

          // shift vectorData
          toMem.writer.req.bits.wdata := current.data >>
            ((coredef.L1D.TO_CORE_TRANSFER_BYTES.U -
              current.addr(IGNORED_WIDTH - 1, 0)) << 3)
        }

        // compute write be
        // first & last beat is special
        val offset = current.addr(log2Ceil(WIDTH / 8) - 1, 0)
        val fullMask = Fill(WIDTH / 8, 1.U).asUInt
        val firstMask = WireInit(fullMask)
        when(vectorWriteReqIndex === 0.U) {
          firstMask := fullMask << offset
        }

        val lastOffset = offset +
          (vState.vl << vState.vtype.vsew)(log2Ceil(WIDTH / 8) - 1, 0)
        val lastMask = WireInit(fullMask)
        when(vectorWriteReqIndex === vectorBeats - 1.U && lastOffset =/= 0.U) {
          lastMask := fullMask >> ((WIDTH / 8).U - lastOffset)
        }
        toMem.writer.req.bits.be := firstMask & lastMask

        when(release.ready) {
          toMem.writer.req.valid := true.B
          when(toMem.writer.req.fire) {
            vectorWriteReqIndex := vectorWriteReqIndex + 1.U

            // compute next store addr
            vectorStoreAddr :=
              (current.addr(coredef.PADDR_WIDTH - 1, IGNORED_WIDTH) ##
                0.U(IGNORED_WIDTH.W)) +
                ((vectorWriteReqIndex + 1.U) << IGNORED_WIDTH)
            when(vectorWriteReqIndex === vectorBeats - 1.U) {
              release.valid := true.B
              advance := true.B
              vectorWriteReqIndex := 0.U
            }
          }
        }
      }
      is(DelayedMemOp.uncachedStore) {
        when(release.ready) {
          toMem.uncached.req := L1UCReq.write
          when(~toMem.uncached.stall) {
            release.valid := true.B
            advance := true.B
          }
        }
      }
      is(DelayedMemOp.exception) {
        retire.bits.info.exception := current.exception
        retire.bits.info.wb := current.data
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

    // a flush occurred between req and resp
    // NOTE: move this logic to L1 DCache?
    // beware two continuous flush!!!
    when((reqSent || toMem.reader.req.fire) && ~actualRespValid) {
      flushedRead := true.B
    }

    reqSent := false.B

    vectorReadRespData := 0.U
    vectorReadReqIndex := 0.U
    vectorReadRespIndex := 0.U
  }

}
