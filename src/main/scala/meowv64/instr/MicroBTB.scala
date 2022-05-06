package meowv64.instr

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import meowv64.core._

/** BHT prediction type: branch taken, branch not taken, or missing in BHT
  */
object BranchPrediction extends ChiselEnum {
  val taken, notTaken, missed = Value
}

class MicroBTBEntry(implicit val coredef: CoreDef) extends Bundle {
  val OFFSET_WIDTH = log2Ceil(coredef.L1I.TO_CORE_TRANSFER_WIDTH)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)

  val TAG_WIDTH = coredef.VADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  /** Tag equals && valid -> match
    */
  val tag = UInt(TAG_WIDTH.W)
  val valid = Bool()

  /** Branch type: b* or jal?
    *
    * If jal, it is always taken.
    */
  val isBr = Bool()

  /** BHT history counter
    */
  val history = UInt(coredef.BHT_WIDTH.W)

  /** Target address if branch is taken.
    *
    * If necessary, store offset instead of target address here.
    */
  val targetAddress = UInt(coredef.XLEN.W)

  /** Compute BPUResult
    */
  def taken(tag: UInt) = {
    val ret = Wire(new BPUResult())
    ret.valid := valid && this.tag === tag
    ret.isBr := isBr
    // if jal, always taken
    // otherwise, check msb of history counter
    ret.taken := !this.isBr || history(coredef.BHT_WIDTH - 1)
    ret.targetAddress := targetAddress
    ret.history := history
    ret
  }
}

class BPUResult(implicit val coredef: CoreDef) extends Bundle {
  val OFFSET_WIDTH = log2Ceil(coredef.L1I.TO_CORE_TRANSFER_BYTES)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)
  val TAG_WIDTH = coredef.VADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  /** This is a valid prediction
    */
  val valid = Bool()

  /** This branch is taken
    */
  val taken = Bool()

  /** Branch type: b* or jal?
    *
    * If jal, it is always taken.
    */
  val isBr = Bool()

  /** Predicted target address
    */
  val targetAddress = UInt(coredef.XLEN.W)

  /** BHT history counter.
    *
    * Metadata to pass with pipeline.
    */
  val history = UInt(coredef.BHT_WIDTH.W)

  def prediction = Mux(
    valid,
    Mux(
      taken,
      BranchPrediction.taken,
      BranchPrediction.notTaken
    ),
    BranchPrediction.missed
  )

  def up(tag: UInt): MicroBTBEntry = {
    val ret = Wire(new MicroBTBEntry())

    ret.valid := true.B
    ret.tag := tag
    ret.isBr := isBr
    ret.targetAddress := targetAddress

    when(this.valid) {
      // saturating add
      ret.history := Mux(
        this.history.andR(),
        (-1).S(coredef.BHT_WIDTH.W).asUInt,
        this.history + 1.U
      )
    }.otherwise {
      ret.history := 1.U(1.W) ## 0.U((coredef.BHT_WIDTH - 1).W)
    }

    ret
  }

  def down(tag: UInt): MicroBTBEntry = {
    val ret = Wire(new MicroBTBEntry())

    ret.valid := true.B
    ret.tag := tag
    ret.isBr := isBr
    ret.targetAddress := targetAddress

    when(this.valid) {
      // saturating sub
      ret.history := Mux(
        this.history.orR(),
        this.history - 1.U,
        0.U
      )
    }.otherwise {
      ret.history := 0.U(1.W) ## (-1).S((coredef.BHT_WIDTH - 1).W).asUInt
    }

    ret
  }

  /** Update BHT prediction history
    *
    * @param taken
    * @param tag
    * @return
    */
  def update(taken: Bool, tag: UInt) = Mux(taken, this.up(tag), this.down(tag))
}

object BPUResult {
  def empty(implicit coredef: CoreDef) = {
    val res = Wire(new BPUResult)
    res.valid := false.B
    res.history := 0.U
    res.targetAddress := 0.U

    res
  }

}

/** Micro Branch Target Buffer. It only considers branch instructions and jal.
  */
class MicroBTB(implicit val coredef: CoreDef) extends Module {
  val toFetch = IO(new Bundle {

    /** the address (pc) of the query branch */
    val s1Pc =
      Input(Valid(UInt(coredef.VADDR_WIDTH.W)))
    // for each possible branch instruction
    // return a BPUResult
    // it has one cycle delay
    val s2Res = Output(
      Valid(
        Vec(
          coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH,
          new BPUResult
        )
      )
    )
  })

  val toExec = IO(new Bundle {

    /** Update BPU based on execution result
      */
    val valid = Input(Bool())
    val lpc = Input(UInt(coredef.XLEN.W)) // Only register on the last slot
    val hist = Input(new BPUResult)
    val taken = Input(Bool())
  })

  val INLINE_COUNT = coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH
  val OFFSET_WIDTH = log2Ceil(coredef.L1I.TO_CORE_TRANSFER_BYTES)
  val INDEX_WIDTH = log2Ceil(coredef.BHT_SIZE)
  val INDEX_OFFSET_WIDTH = OFFSET_WIDTH + INDEX_WIDTH
  val TAG_WIDTH = coredef.VADDR_WIDTH - OFFSET_WIDTH - INDEX_WIDTH

  def getIndex(addr: UInt) = addr(INDEX_OFFSET_WIDTH - 1, OFFSET_WIDTH)
  def getTag(addr: UInt) = addr(coredef.VADDR_WIDTH - 1, INDEX_OFFSET_WIDTH)
  def getOffset(addr: UInt) =
    addr(OFFSET_WIDTH - 1, log2Ceil(Const.INSTR_MIN_WIDTH / 8))
  def toAligned(addr: UInt) = getTag(addr) ## getIndex(addr) ## 0.U(
    OFFSET_WIDTH.W
  ) // The input address should be aligned anyway

  // memory blackbox does not support nested aggregate data type
  val btbEntries =
    SyncReadMem(
      coredef.BHT_SIZE,
      Vec(INLINE_COUNT, UInt((new MicroBTBEntry).getWidth.W))
    )

  val doingReset = RegInit(true.B)
  val resetCnt = RegInit(0.U(log2Ceil(coredef.BHT_SIZE).W))

  // Prediction part
  val s1Tag = getTag(toFetch.s1Pc.bits)
  val s2Tag = RegNext(s1Tag)
  val s2Readout = btbEntries.read(getIndex(toFetch.s1Pc.bits))
  // one cycle delay
  toFetch.s2Res.valid := RegNext(toFetch.s1Pc.valid)
  when(toFetch.s2Res.valid) {
    toFetch.s2Res.bits := VecInit(
      s2Readout.map(_.asTypeOf(new MicroBTBEntry).taken(s2Tag))
    )
  }.otherwise {
    toFetch.s2Res.bits := 0.U.asTypeOf(toFetch.s2Res.bits)
  }

  // valid is stale when in reset state
  when(doingReset) {
    toFetch.s2Res.valid := false.B
    for (res <- toFetch.s2Res.bits) {
      res.valid := false.B
    }
  }

  // Update part

  // Write bypass: if two updates to the same entry is close,
  // the state might be stale and the first update is silently overridden.
  // Record the history of recent writes, and bypass the state if available.
  val wrBypassPc = RegInit(
    VecInit.fill(coredef.BPU_WRITE_BYPASS_COUNT)(0.U(coredef.XLEN.W))
  )
  val wrBypassState = RegInit(
    VecInit.fill(coredef.BPU_WRITE_BYPASS_COUNT)(0.U(coredef.BHT_WIDTH.W))
  )
  val wrBypassWriteIdx = RegInit(
    0.U(log2Ceil(coredef.BPU_WRITE_BYPASS_COUNT).W)
  )

  val wrBypassHitVec = wrBypassPc
    .map({ case pc => pc === toExec.lpc })
  val wrBypassHit = wrBypassHitVec.reduce(_ || _)
  val wrBypassHitIdx = PriorityEncoder(wrBypassHitVec)

  val updateTag = getTag(toExec.lpc)
  val updateOffset = getOffset(toExec.lpc)
  assert(
    updateOffset.getWidth == log2Ceil(
      coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH
    )
  )

  // Consider write bypass
  val history = WireInit(toExec.hist)
  when(wrBypassHit) {
    val bypass = wrBypassState(wrBypassHitIdx)
    history.history := bypass
  }

  val updated = history.update(toExec.taken, updateTag)
  val updateMask = UIntToOH(updateOffset).asBools()

  val we = updateMask.map(bit => {
    val ret = WireDefault(bit)
    when(!toExec.valid) {
      ret := false.B
    }

    when(doingReset) {
      ret := true.B
    }

    ret
  })

  val init = Wire(new MicroBTBEntry)
  init.isBr := false.B
  init.history := 0.U
  init.tag := 0.U
  init.targetAddress := 0.U
  init.valid := false.B

  val waddr = Mux(doingReset, resetCnt, getIndex(toExec.lpc))
  val data = VecInit(
    Seq.fill(coredef.L1I.TO_CORE_TRANSFER_WIDTH / Const.INSTR_MIN_WIDTH)(
      Mux(doingReset, init, updated)
    )
  )

  btbEntries.write(
    waddr,
    data.asTypeOf(Vec(INLINE_COUNT, UInt((new MicroBTBEntry).getWidth.W))),
    we
  )

  // Save to write bypass buffer
  when(toExec.valid) {
    when(wrBypassHit) {
      // update in-place
      wrBypassState(wrBypassHitIdx) := updated.history
    }.otherwise {
      wrBypassPc(wrBypassWriteIdx) := toExec.lpc
      wrBypassState(wrBypassWriteIdx) := updated.history

      when(wrBypassWriteIdx === (coredef.BPU_WRITE_BYPASS_COUNT - 1).U) {
        wrBypassWriteIdx := 0.U
      }.otherwise {
        wrBypassWriteIdx := wrBypassWriteIdx + 1.U
      }
    }
  }

  when(doingReset) {
    resetCnt := resetCnt +% 1.U
    when(resetCnt.andR()) {
      doingReset := false.B
    }
  }
}
