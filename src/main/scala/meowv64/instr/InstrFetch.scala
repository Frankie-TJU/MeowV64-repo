package meowv64.instr

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import meowv64.cache._
import meowv64.core._
import meowv64.paging._
import meowv64.util._

import Decoder._

/** Fetch exception
  */
object FetchEx extends ChiselEnum {
  val none, invalAddr, pageFault = Value
}

class InstrExt(implicit val coredef: CoreDef) extends Bundle {
  val addr = UInt(coredef.XLEN.W)
  val instr = new Instr

  /** Valid instruction, possibly legal or illegal
    */
  val valid = Bool()
  val fetchEx = FetchEx()

  /** Exception happens on the second half of this instruction
    */
  val acrossPageEx =
    Bool()

  /** Prediction result from BPU
    */
  val pred = new BPUResult

  /** Override prediction result e.g. JAL & JALR
    */
  val overridePred = Bool()

  override def toPrintable: Printable = {
    p"Address: 0x${Hexadecimal(addr)}\n" +
      p"Valid: ${valid}\n" +
      p"${instr}"
  }

  /** Next PC for this instruction
    */
  def npc: UInt = Mux(instr.base === InstrType.C, addr + 2.U, addr + 4.U)

  /** Whether this instruction breaks the instruction flow
    */
  def taken: Bool = overridePred || pred.prediction === BranchPrediction.taken

  /** Illegal instruction
    */
  def illegal =
    fetchEx =/= FetchEx.none || instr.base === InstrType.RESERVED
}

object InstrExt {
  def empty(implicit coredef: CoreDef): InstrExt = {
    val ret = Wire(new InstrExt)

    ret.addr := DontCare
    ret.instr := DontCare
    ret.valid := false.B
    ret.pred := DontCare
    ret.fetchEx := DontCare
    ret.acrossPageEx := DontCare
    ret.overridePred := DontCare

    ret
  }
}

class InstrFetch(implicit val coredef: CoreDef) extends Module {
  val toCtrl = IO(new Bundle {
    val pc = Input(UInt(coredef.XLEN.W))

    val ctrl = StageCtrl.stage()
    val iRst = Input(Bool())
    val tlbRst = Input(Bool())

    /** Privilege level
      */
    val priv = Input(PrivLevel())

    /** Allow floating point instructions (mstatus.fs != 0)
      */
    val allowFloat = Input(Bool())
  })

  val toIC = IO(Flipped(new ICPort(coredef.L1I)))
  val toExec = IO(Flipped(new MultiQueueIO(new InstrExt, coredef.ISSUE_NUM)))

  val toBPU = IO(new Bundle {
    val s1Pc = Output(Valid(UInt(coredef.XLEN.W)))
    val s2Res = Input(
      Vec(
        coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH,
        new BPUResult
      )
    )
  })

  val toRAS = IO(new Bundle {
    val push = Valid(UInt(coredef.XLEN.W))
    val pop = Flipped(Decoupled(UInt(coredef.XLEN.W)))
  })

  val debug = IO(new Bundle {
    val pc = Output(UInt(coredef.XLEN.W))
  })

  val toCore = IO(new Bundle {
    val satp = Input(new Satp)
    val ptw = new TLBExt
  })

  val ICAlign = log2Ceil(coredef.L1I.TO_CORE_TRANSFER_WIDTH / 8)

  // Internal PC counter
  val s1Pc = RegInit(coredef.INIT_VEC.U(coredef.XLEN.W))
  val s1AlignedPc = s1Pc(coredef.XLEN - 1, ICAlign) ## 0.U(ICAlign.W)

  val instPerFetchPacket =
    coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH

  // pc of last cycle
  val s2Pc = RegInit(0.U(coredef.XLEN.W))
  val s2Fault = RegInit(false.B)
  val s2PcOffset = s2Pc(ICAlign - 1, log2Ceil(Const.INSTR_MIN_WIDTH / 8))
  val s2AlignedPc = s2Pc(coredef.XLEN - 1, ICAlign) ## 0.U(ICAlign.W)
  val s2FullMask = WireInit(
    ((1 << instPerFetchPacket) - 1).U(instPerFetchPacket.W)
  )
  val s2OffsetMask = WireInit(s2FullMask << s2PcOffset)
  val s2BrMask = WireInit(s2FullMask)
  val s2Mask = s2OffsetMask & s2BrMask
  val s2LastMask = WireInit(0.U(instPerFetchPacket.W))

  // Actual fetch PC, affected by branch, etc.
  val s1FPc = WireDefault(s1Pc)
  val s1AlignedFPc = s1FPc(coredef.XLEN - 1, ICAlign) ## 0.U(ICAlign.W)
  // fetch packet is successive
  val s1Successive = WireInit(true.B)
  val s1PipeSuccessive = RegInit(true.B)
  val s2Successive = RegInit(false.B)
  s1PipeSuccessive := s1Successive

  s1Pc := s1FPc

  val tlb = Module(new TLB)
  val requiresTranslate =
    toCore.satp.mode =/= SatpMode.bare && toCtrl.priv <= PrivLevel.S
  // TODO: this will cause the flush to be sent one more tick
  val readStalled = toIC.stall || (requiresTranslate && !tlb.query.req.ready)
  val readFire = !readStalled && toIC.read

  // predict fpc from BPU result

  when(!readStalled) {
    s2Pc := s1FPc
    s2Fault := tlb.query.resp.fault
    s2Successive := s1Successive

    when(toIC.read) {
      // If We send an request to IC, step forward PC counter
      s1Pc := s1AlignedFPc + (coredef.L1I.TO_CORE_TRANSFER_WIDTH / 8).U
      s1PipeSuccessive := true.B
    }
  }

  // compute stage 1 fpc from stage 2 BPU result
  // find first taken slot
  s1Successive := s1PipeSuccessive
  for (
    i <-
      (0 until coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH).reverse
  ) {
    val res = toBPU.s2Res(i)
    when(i.U >= s2PcOffset) {
      when(res.prediction === BranchPrediction.taken) {
        s1FPc := res.targetAddress
        s1Successive := false.B

        // branch mask
        s2BrMask := ((1 << (i + 1)) - 1).U
        s2LastMask := (1 << i).U
      }
    }
  }

  tlb.ptw <> toCore.ptw
  tlb.satp := toCore.satp
  tlb.query.req.bits.vpn := s1FPc(47, 12)
  tlb.query.req.valid := requiresTranslate && !toCtrl.ctrl.flush
  tlb.query.req.bits.isModify := false.B
  tlb.query.req.bits.mode := Mux(
    toCtrl.priv === PrivLevel.U,
    TLBLookupMode.U,
    TLBLookupMode.S
  )
  tlb.flush := toCtrl.tlbRst

  // Predict by virtual memory
  toBPU.s1Pc.bits := Mux(
    readFire,
    s1FPc,
    RegNext(toBPU.s1Pc.bits)
  )
  toBPU.s1Pc.valid := readFire
  // TODO: flush BPU on context switch

  // First, push all IC readouts into a queue
  class ICData extends Bundle {
    val data = UInt(coredef.L1I.TO_CORE_TRANSFER_WIDTH.W)

    /** Aligned address of this fetch packet.
      */
    val addr = UInt(coredef.XLEN.W)

    /** Mask of valid 16-bit parts
      */
    val mask = UInt(
      (coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH).W
    )

    /** Last bit of valid 16-bit parts, for BTB predictions
      */
    val last = UInt(
      (coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH).W
    )

    /** Whether this fetch packet is successive, i.e. the next packet of
      * previous one
      */
    val successive = Bool()

    /** BPU prediction result
      */
    val pred =
      Vec(
        coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH,
        new BPUResult
      )

    /** Instruction fetch fault
      */
    val fault = Bool()
  }

  // ICache fetched queue.
  /** FIXME(BPU, meow): Currently, BPU responses are pipelined alongside I$
    * responses into the decode pipeline, and processed at the same cycle of
    * late-predict (which is one-cycle after the instruction's decode) Also, for
    * timing optimization, ICQueue + ICHead induces one cycle delay. This causes
    * *OPTIMAL CORRECT* BHT predictions also introducing two cycles of wasted I$
    * fetch cycle. By design, the *BPU* stated here (BHT + BTB) should function
    * as a next-line predictor. Current implementation is due to:
    *   - Lack of BTB (so we need to wait for decode to know the branch target)
    *   - Lack of RAS operation history table (so we need to wait for decode to
    *     know which instruction is a `call` / `ret`)
    *   - Complications introduced by variable-length instructions, e.g. if we
    *     have a context switch, and a valid BHT entry now cause us to branch at
    *     *NON*-instruction boundary, how do we recover from that? In the next
    *     version of IF impl, we should at least implement BTB and RAS
    *     scratchpad. As of the issues with instruction boundary, worst-case we
    *     can flush the entire BHT at context switch.
    */
  val ICQueueLen = 4;
  val ICQueue = Module(new FlushableQueue(new ICData, ICQueueLen, false, false))
  ICQueue.io.enq.bits.data := toIC.data.bits
  ICQueue.io.enq.bits.addr := s2AlignedPc
  ICQueue.io.enq.bits.mask := s2Mask
  ICQueue.io.enq.bits.last := s2LastMask
  ICQueue.io.enq.bits.pred := toBPU.s2Res
  ICQueue.io.enq.bits.fault := s2Fault
  ICQueue.io.enq.bits.successive := s2Successive
  ICQueue.io.enq.valid := (!toIC.stall && toIC.data.valid) || s2Fault

  // when successive, first instruction must be decodable
  when(ICQueue.io.enq.bits.successive && ICQueue.io.enq.fire) {
    assert(ICQueue.io.enq.bits.mask(0))
  }

  /** In the previous cycle, if there is any instruction causing a late-predict
    * jump. Currently, this is also responsible for handling next-line
    * predictions, which is not optimal. See the comment above.
    */
  val pipeSpecBr = Wire(Bool())

  /** Halt ICache fetch for current cycle due to IF control signals. We cannot
    * stall I$ from throwing fetched instruction out, so we need to keep enough
    * space to store all of them.
    */
  val haltIC =
    (ICQueue.io.count >= (ICQueueLen - 1).U) && !toCtrl.ctrl.flush && !pipeSpecBr
  // I$ Fetch address
  val icAddr = WireDefault(s1FPc)
  val icRead = WireDefault(!haltIC)
  when(requiresTranslate) {
    icAddr := tlb.query.resp.ppn ## s1AlignedFPc(11, 0)
    icRead := (!haltIC && tlb.query.req.ready) && !tlb.query.resp.fault
  }
  toIC.read := icRead && !toCtrl.ctrl.flush
  toIC.addr := icAddr
  toIC.rst := toCtrl.ctrl.flush && toCtrl.iRst

  val ICHead = Module(new FlushableSlot(new ICData, true, false))
  ICHead.io.enq <> ICQueue.io.deq
  ICHead.io.deq.nodeq() // Default

  val headPtr = RegInit(0.U(log2Ceil(coredef.L1I.TO_CORE_TRANSFER_WIDTH / 16).W))

  val decodeVec = Wire(
    Vec(coredef.L1I.TO_CORE_TRANSFER_WIDTH * 2 / 16, UInt(16.W))
  )
  decodeVec := (ICQueue.io.deq.bits.data ## ICHead.io.deq.bits.data)
    .asTypeOf(decodeVec)
  val joinedVec = Wire(
    Vec(coredef.L1I.TO_CORE_TRANSFER_WIDTH * 2 / 16 - 1, UInt(32.W))
  )
  for ((v, i) <- joinedVec.zipWithIndex) {
    v := decodeVec(i + 1) ## decodeVec(i)
  }
  // Scala ++ works in reverse order (little endian you may say?)
  val joinedPred = VecInit(ICHead.io.deq.bits.pred ++ ICQueue.io.deq.bits.pred)
  val joinedMask = ICQueue.io.deq.bits.mask ## ICHead.io.deq.bits.mask
  val joinedLast = ICQueue.io.deq.bits.last ## ICHead.io.deq.bits.last

  val decodable = Wire(Vec(coredef.FETCH_NUM, Bool()))
  val decodePtr = Wire(
    Vec(
      coredef.FETCH_NUM + 1,
      UInt(log2Ceil(coredef.L1I.TO_CORE_TRANSFER_WIDTH * 2 / 16).W)
    )
  )
  val decoded = Wire(Vec(coredef.FETCH_NUM, new InstrExt))
  dontTouch(decoded)
  val decodedRASPush = Wire(Vec(coredef.FETCH_NUM, Bool()))
  val decodedRASPop = Wire(Vec(coredef.FETCH_NUM, Bool()))
  decodePtr(0) := headPtr

  for (i <- 0 until coredef.FETCH_NUM) {
    // this instruction spans ICHead and ICQueue
    val overflowed =
      decodePtr(i) >= (coredef.L1I.TO_CORE_TRANSFER_WIDTH / 16 - 1).U

    when(!overflowed) {
      decodable(i) := ICHead.io.deq.valid
    }.otherwise {
      decodable(i) := ICHead.io.deq.valid &&
        ICQueue.io.deq.valid
    }

    val raw = joinedVec(decodePtr(i))
    val (instr, isInstr16) = raw.parseInstr(toCtrl.allowFloat)

    when(isInstr16) {
      decodePtr(i + 1) := decodePtr(i) + 1.U
    } otherwise {
      decodePtr(i + 1) := decodePtr(i) + 2.U
    }

    decoded(i).instr := instr
    val addr = ICHead.io.deq.bits.addr + (decodePtr(i) << log2Ceil(
      (Const.INSTR_MIN_WIDTH / 8)
    ))
    val acrossPage =
      !isInstr16 && addr(12, log2Ceil(Const.INSTR_MIN_WIDTH / 8)).andR()
    decoded(i).addr := addr

    // If an instruction span across the page border:
    // We need to consider fault/inval addr from both the previous page and the next page
    // This is done by checking if the ppn of the instruction is the same as the ppn of
    // ICHead

    decoded(i).fetchEx := FetchEx.none
    decoded(i).acrossPageEx := false.B
    assume(coredef.XLEN != coredef.VADDR_WIDTH)
    val headAddr = ICHead.io.deq.bits.addr
    val isInvalAddr = WireDefault(
      // Fetch cannot be uncached. We are also ignoring tlb.query.uncached
      headAddr(coredef.XLEN - 1, coredef.PADDR_WIDTH).asSInt() =/= headAddr(
        coredef.PADDR_WIDTH - 1
      ).asSInt()
    )

    when(requiresTranslate) {
      switch(toCore.satp.mode) {
        is(SatpMode.sv48) {
          isInvalAddr := headAddr(coredef.XLEN - 1, coredef.VADDR_WIDTH)
            .asSInt() =/= headAddr(coredef.VADDR_WIDTH - 1).asSInt()
        }

        is(SatpMode.sv39) {
          isInvalAddr := headAddr(coredef.XLEN - 1, coredef.VADDR_WIDTH - 9)
            .asSInt() =/= headAddr(coredef.VADDR_WIDTH - 10).asSInt()
        }
      }
    }

    when(ICHead.io.deq.bits.fault) {
      decoded(i).fetchEx := FetchEx.pageFault
    }.elsewhen(acrossPage && ICQueue.io.deq.bits.fault) {
      decoded(i).fetchEx := FetchEx.pageFault
      decoded(i).acrossPageEx := true.B
    }.elsewhen(overflowed && ICQueue.io.deq.bits.fault) {
      // handle case for example:
      // ICHead is at 0x1ff8, ICQueue is at 0x2000
      // decoded(0) is at 0x1ffc, decoded(1) is at 0x2000
      decoded(i).fetchEx := FetchEx.pageFault
    }.elsewhen(isInvalAddr) {
      decoded(i).fetchEx := FetchEx.invalAddr
    }

    decoded(i).valid := true.B
    val pred = joinedPred(decodePtr(i + 1) - 1.U)
    decoded(i).pred := pred
    decoded(i).overridePred := false.B

    decodedRASPop(i) := false.B
    decodedRASPush(i) := false.B

    val targetAddress = (decoded(i).instr.imm
      +% decoded(i).addr.asSInt()).asUInt()
    decoded(i).pred.isBr := false.B
    when(instr.op === Decoder.Op("JAL").ident) {
      // force predict branch to be taken if it was not predicted in BPU
      when(
        pred.prediction =/= BranchPrediction.taken || pred.targetAddress =/= targetAddress
      ) {
        decoded(i).overridePred := true.B
        // compute target address for JAL instruction
        // will be written to BPU later
        decoded(i).pred.targetAddress := targetAddress
      }
      decodedRASPush(i) := instr.rd === 1.U || instr.rd === 5.U
    }.elsewhen(instr.op === Decoder.Op("JALR").ident) {
      decodedRASPush(i) := instr.rd === 1.U || instr.rd === 5.U
      val doPop =
        (instr.rs1 === 1.U || instr.rs1 === 5.U) && instr.rs1 =/= instr.rd
      decodedRASPop(i) := doPop

      when(doPop) {
        // force predict branch to be taken
        decoded(i).overridePred := toRAS.pop.valid
      }
      decoded(i).pred.targetAddress := toRAS.pop.bits
    }.elsewhen(instr.op === Decoder.Op("BRANCH").ident) {
      when(instr.imm < 0.S && pred.prediction === BranchPrediction.missed) {
        // for backward branches
        // when BHT missed,
        // force predict branch to be taken
        decoded(i).overridePred := true.B
      }
      // compute target address for BRANCH instructions
      // will be written to BPU later
      decoded(i).pred.targetAddress := (decoded(i).instr.imm
        +% decoded(i).addr.asSInt()).asUInt()
      decoded(i).pred.isBr := true.B
    }.otherwise {
      decoded(i).pred.valid := false.B
    }

    when(
      pred.prediction === BranchPrediction.taken
    ) {
      // TODO predicted as a branch/jal, but actually not
    }
  }

  val issueFifo = Module(
    new MultiQueue(
      new InstrExt,
      coredef.ISSUE_FIFO_DEPTH,
      coredef.FETCH_NUM,
      coredef.ISSUE_NUM
    )
  )

  /** How many instructions are actually issued after decode slot `i-1` Be
    * careful about the subscript mismatch
    */
  val steppings = Wire(
    Vec(coredef.FETCH_NUM + 1, UInt(log2Ceil(coredef.FETCH_NUM + 1).W))
  )

  /** Cancel the issuing of this and the following instructions (similar to
    * `break` in C)
    */
  val brokens = Wire(Vec(coredef.FETCH_NUM + 1, Bool()))
  steppings(0) := 0.U
  brokens(0) := false.B
  for (i <- (0 until coredef.FETCH_NUM)) {
    brokens(i + 1) := Mux(
      (i + 1).U <= issueFifo.writer.accept && decodable(i),
      brokens(i),
      true.B
    )
    if (i > 0) {
      // instruction flow interrupts
      when(decoded(i - 1).taken) {
        brokens(i + 1) := true.B
      }
    }
    // masked instruction
    when(~joinedMask(decodePtr(i))) {
      brokens(i + 1) := true.B
    }
    steppings(i + 1) := Mux(brokens(i + 1), steppings(i), (i + 1).U)
  }

  val stepping = steppings(coredef.FETCH_NUM)

  /** RAS push/pop FIXME(meow): move this to early predict
    */
  toRAS.pop.ready := stepping =/= 0.U && decodedRASPop(stepping -% 1.U)
  toRAS.push.valid := stepping =/= 0.U && decodedRASPush(stepping -% 1.U)
  toRAS.push.bits := decoded(stepping -% 1.U).npc

  val nHeadPtr = decodePtr(stepping) // Expanded
  headPtr := nHeadPtr // Trim

  assume(nHeadPtr.getWidth == headPtr.getWidth + 1)
  assert(stepping <= issueFifo.writer.accept)

  // TODO: finished ICHead, or encountered a taken branch
  when(
    nHeadPtr.head(1)(0) ||
      ~ICHead.io.deq.bits.mask(headPtr) ||
      ~ICHead.io.deq.bits.mask(nHeadPtr)
  ) {
    // ICQueue head is finished, go to next fetch packet
    ICHead.io.deq.deq()
  }

  // if not successive, reset head ptr from mask
  when(!ICQueue.io.deq.bits.successive && ICQueue.io.deq.fire) {
    headPtr := PriorityEncoder(ICQueue.io.deq.bits.mask)
  }

  toExec <> issueFifo.reader
  issueFifo.writer.view := decoded
  issueFifo.writer.cnt := stepping

  toCtrl.ctrl.stall := haltIC || toIC.stall

  val pendingIRst = RegInit(false.B)
  val pendingTLBRst = RegInit(false.B)
  val pendingFlush = RegInit(false.B)

  ICQueue.io.flush.get := false.B
  ICHead.io.flush.get := false.B
  issueFifo.flush := false.B

  /** Speculative branch FIXME(meow): move BHT to early predict
    */
  val pipeStepping = RegNext(stepping)
  val pipeTaken = RegNext(VecInit(decoded.map(_.overridePred)))
  val pipeSpecBrTargets = RegNext(VecInit(decoded.map(_.pred.targetAddress)))
  val pipeSpecBrMask = pipeTaken.zipWithIndex.map({ case (taken, idx) =>
    idx.U < pipeStepping && taken
  })
  val pipeSpecBrTarget = MuxLookup(
    true.B,
    0.U,
    pipeSpecBrMask.zip(pipeSpecBrTargets)
  )
  pipeSpecBr := VecInit(pipeSpecBrMask).asUInt.orR && RegNext(
    !toCtrl.ctrl.flush && !pipeSpecBr,
    false.B
  )

  when(pipeSpecBr) {
    ICQueue.io.flush.get := true.B
    ICHead.io.flush.get := true.B
    // Do not push into issue fifo in this cycle
    issueFifo.writer.cnt := 0.U
    // Do not commit RAS
    toRAS.push.valid := false.B
    toRAS.pop.ready := false.B

    s1FPc := pipeSpecBrTarget
    s1Successive := false.B
    s2Fault := false.B
    headPtr := pipeSpecBrTarget(
      ICAlign - 1,
      log2Ceil(Const.INSTR_MIN_WIDTH / 8)
    )

    pendingIRst := false.B
    pendingTLBRst := toCtrl.tlbRst

    when(readStalled) {
      pendingFlush := true.B
    }
  }

  /** Flushing
    */

  when(toCtrl.ctrl.flush) {
    // External flushing, wait for one tick
    // This is to ensure priv level and other environment are set up correctly
    issueFifo.flush := true.B
    ICQueue.io.flush.get := true.B
    ICHead.io.flush.get := true.B
    toCtrl.ctrl.stall := false.B

    val ICAlign = log2Ceil(coredef.L1I.TO_CORE_TRANSFER_WIDTH / 8)
    // Set pc directly, because we are waiting for one tick
    s1Pc := toCtrl.pc
    s1FPc := toCtrl.pc
    s1Successive := false.B
    s2Fault := false.B
    headPtr := toCtrl.pc(ICAlign - 1, log2Ceil(Const.INSTR_MIN_WIDTH / 8))

    pendingIRst := toCtrl.iRst
    pendingTLBRst := toCtrl.tlbRst

    when(readStalled) {
      pendingFlush := true.B
    }
  }

  // Flush on IC stall tick
  when(pendingFlush) {
    toIC.rst := pendingIRst
    // it might assert tlbRst when pendingFlush is true
    tlb.flush := toCtrl.tlbRst | pendingTLBRst
    ICQueue.io.enq.noenq()
    when(!toIC.stall) { // TLB will flush within one tick
      pendingFlush := false.B
    }
  }

  // TODO: asserts that decodable is shaped like [true, ..., true, false, ..., false] when there is no BPU

  debug.pc := s1FPc
}
