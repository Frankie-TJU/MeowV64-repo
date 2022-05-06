package meowv64.cache

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.util.log2Ceil
import meowv64.debug.DebugModule

class CoreICPort(val opts: L1Opts) extends Bundle {
  val read = Flipped(Decoupled(UInt(opts.ADDR_WIDTH.W)))
  val rst = Input(Bool())

  val data = Valid(UInt(opts.TO_CORE_TRANSFER_WIDTH.W)) // Data delay is 1 cycle
}

object S2State extends ChiselEnum {
  val rst, idle, refill, refilled = Value
}

// TODO: Use sram ip
class L1IC(opts: L1Opts) extends Module {
  val toCPU = IO(new CoreICPort(opts))
  val toL2 = IO(new L1ICPort(opts))
  val toDM = IO(new L1ICPort(opts))

  toCPU.data := DontCare
  toL2.read.bits := DontCare
  toL2.read.valid := false.B
  toDM.read.bits := DontCare
  toDM.read.valid := false.B

  val IGNORED_WIDTH = log2Ceil(opts.TO_CORE_TRANSFER_BYTES)

  val validArray = RegInit(0.U((opts.LINE_PER_ASSOC * opts.ASSOC).W))
  val tagArray =
    Mem(opts.LINE_PER_ASSOC, Vec(opts.ASSOC, UInt(opts.TAG_WIDTH.W)))
  // memory blackbox does not support nested aggregate data type
  val dataArray = SyncReadMem(
    opts.LINE_PER_ASSOC,
    Vec(
      opts.ASSOC,
      UInt((opts.TRANSFER_COUNT * opts.TO_CORE_TRANSFER_WIDTH).W)
    )
  )

  val writerAddr = Wire(UInt(opts.INDEX_WIDTH.W))
  val writerTag = Wire(UInt(opts.TAG_WIDTH.W))
  val writerData = Wire(
    Vec(opts.TRANSFER_COUNT, UInt(opts.TO_CORE_TRANSFER_WIDTH.W))
  )
  val writerMask = Wire(Vec(opts.ASSOC, Bool()))

  tagArray.write(
    writerAddr,
    VecInit(Seq.fill(opts.ASSOC)(writerTag)),
    writerMask
  )
  dataArray.write(
    writerAddr,
    VecInit(
      Seq.fill(opts.ASSOC)(
        writerData.asTypeOf(
          UInt((opts.TRANSFER_COUNT * opts.TO_CORE_TRANSFER_WIDTH).W)
        )
      )
    ),
    writerMask
  )

  writerAddr := DontCare
  writerTag := DontCare
  writerData := DontCare
  writerMask := VecInit(Seq.fill(opts.ASSOC)(false.B))

  def getTransferOffset(addr: UInt) = addr(opts.OFFSET_WIDTH - 1, IGNORED_WIDTH)
  def getIndex(addr: UInt) = opts.getIndex(addr)
  def getTag(addr: UInt) = opts.getTag(addr)
  def toAligned(addr: UInt) =
    getTag(addr) ## getIndex(addr) ## 0.U(opts.OFFSET_WIDTH.W)
  def addrInDM(addr: UInt) =
    (DebugModule.DM_CODE_REGION_START).U <= addr && addr < (DebugModule.DM_CODE_REGION_START + DebugModule.DM_CODE_REGION_SIZE).U

  // Stage 1, tag fetch, data fetch
  val pipeRead = RegInit(false.B)
  val pipeRst = RegInit(false.B)
  val pipeAddr = RegInit(0.U(opts.ADDR_WIDTH.W))

  val readingAddr = Wire(UInt(opts.ADDR_WIDTH.W))
  when(toCPU.read.ready) {
    readingAddr := toCPU.read.bits
  }.otherwise {
    readingAddr := pipeAddr
  }
  val readValid = Wire(Vec(opts.ASSOC, Bool()))
  for (i <- 0 until opts.ASSOC) {
    readValid(i) := validArray(Cat(i.U, getIndex(readingAddr)))
  }
  val readouts = tagArray.read(getIndex(readingAddr))
  val dataReadouts = dataArray
    .read(getIndex(readingAddr))
    .asTypeOf(
      Vec(
        opts.ASSOC,
        Vec(opts.TRANSFER_COUNT, UInt(opts.TO_CORE_TRANSFER_WIDTH.W))
      )
    )
  val hitMap = VecInit(
    readouts.zip(readValid).map { case (tag, valid) =>
      valid && tag === getTag(readingAddr)
    }
  )

  val pipeReadValid = RegNext(readValid)
  val pipeReadouts = RegNext(readouts)
  val pipeHitMap = VecInit(
    pipeReadouts.zip(pipeReadValid).map { case (tag, valid) =>
      valid && tag === getTag(pipeAddr)
    }
  )

  // Stage 2, data mux, refilling, reset
  val state = RegInit(S2State.rst)
  val nstate = Wire(S2State())

  // when reset, hitMap contains X
  when(state =/= S2State.rst) {
    // at most one hit
    assert(PopCount(hitMap) <= 1.U)
  }

  val rand = chisel3.util.random.LFSR(8)
  val victim = RegInit(0.U(log2Ceil(opts.ASSOC)))

  nstate := state
  state := nstate

  val stalled = nstate =/= S2State.idle
  toCPU.read.ready := ~stalled
  toCPU.data.valid := false.B
  toCPU.data.bits := DontCare

  when(toCPU.read.ready) {
    pipeRead := toCPU.read.valid
    pipeAddr := toCPU.read.bits
    pipeRst := toCPU.rst
  }

  val waitBufAddr = RegInit(0.U(opts.ADDR_WIDTH.W))
  val waitBufFull = RegInit(false.B)
  val pipeOutput = Reg(toCPU.data.bits.cloneType)

  switch(state) {
    is(S2State.rst) {
      validArray := 0.U

      // Omit current request
      toCPU.data.valid := false.B
      when(pipeRead) {
        nstate := S2State.refill
      }.otherwise {
        nstate := S2State.idle
      }
    }

    is(S2State.idle) {
      val rdata = Mux1H(
        dataReadouts.zipWithIndex.map({ case (line, idx) =>
          pipeHitMap(idx) -> line
        })
      )

      when(pipeRst) {
        nstate := S2State.rst

        validArray := 0.U
      }.elsewhen(pipeRead) {
        when(pipeHitMap.asUInt().orR) {
          toCPU.data.valid := true.B
          toCPU.data.bits := rdata(getTransferOffset(pipeAddr))
        }.otherwise {
          nstate := S2State.refill
        }
      }.otherwise {
        toCPU.data.valid := false.B
        nstate := S2State.idle
      }
    }

    is(S2State.refill) {
      // arbiter between L2 and DM
      val addr = toAligned(pipeAddr)
      toL2.read.bits := addr
      toDM.read.bits := addr
      val stall = Wire(Bool())
      val data = Wire(UInt((opts.TO_L2_TRANSFER_WIDTH).W))
      when(addrInDM(addr)) {
        toDM.read.valid := true.B
        stall := toDM.stall
        data := toDM.data
      }.otherwise {
        toL2.read.valid := true.B
        stall := toL2.stall
        data := toL2.data
      }

      when(!stall) {
        val victim = rand(opts.ASSOC_IDX_WIDTH - 1, 0)
        val mask = UIntToOH(victim)

        val dataView = data.asTypeOf(writerData)

        writerAddr := getIndex(pipeAddr)
        writerTag := getTag(pipeAddr)
        writerMask := mask.asBools()
        writerData := dataView

        validArray := validArray.bitSet(Cat(victim, writerAddr), true.B)

        pipeOutput := dataView(getTransferOffset(pipeAddr))

        nstate := S2State.refilled
      }
    }

    is(S2State.refilled) {
      nstate := S2State.idle

      toCPU.data.bits := pipeOutput
      toCPU.data.valid := true.B
    }
  }
}
