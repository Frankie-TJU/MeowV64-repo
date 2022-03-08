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

class ILine(val opts: L1Opts) extends Bundle {
  val tag = UInt(opts.TAG_WIDTH.W)
  val valid = Bool()
}

object ILine {
  def default(opts: L1Opts): ILine = {
    val ret = Wire(new ILine(opts))
    ret.tag := DontCare
    ret.valid := false.B

    ret
  }
}

// TODO: Change to xpm_tdpmem
class L1IC(opts: L1Opts) extends Module {
  val toCPU = IO(new CoreICPort(opts))
  val toL2 = IO(new L1ICPort(opts))
  val toDM = IO(new L1ICPort(opts))

  toCPU.data := DontCare
  toL2.read.bits := DontCare
  toL2.read.valid := false.B
  toDM.read.bits := DontCare
  toDM.read.valid := false.B

  val IGNORED_WIDTH = log2Ceil(opts.TO_CORE_TRANSFER_WIDTH / 8)

  val directories = Mem(opts.LINE_PER_ASSOC, Vec(opts.ASSOC, new ILine(opts)))
  val stores = SyncReadMem(
    opts.LINE_PER_ASSOC,
    Vec(
      opts.ASSOC,
      Vec(opts.TRANSFER_COUNT, UInt(opts.TO_CORE_TRANSFER_WIDTH.W))
    )
  )

  val writerAddr = Wire(UInt(opts.INDEX_WIDTH.W))
  val writerDir = Wire(new ILine(opts))
  val writerData = Wire(
    Vec(opts.TRANSFER_COUNT, UInt(opts.TO_CORE_TRANSFER_WIDTH.W))
  )
  val writerMask = Wire(Vec(opts.ASSOC, Bool()))

  directories.write(
    writerAddr,
    VecInit(Seq.fill(opts.ASSOC)(writerDir)),
    writerMask
  )
  stores.write(
    writerAddr,
    VecInit(Seq.fill(opts.ASSOC)(writerData)),
    writerMask
  )

  writerAddr := DontCare
  writerDir := DontCare
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
  val readouts = directories.read(getIndex(readingAddr))
  val dataReadouts = stores.read(getIndex(readingAddr))
  val hitMap = VecInit(
    readouts.map(r => r.valid && r.tag === getTag(readingAddr))
  )
  val pipeReadouts = RegNext(readouts)
  val pipeHitMap = VecInit(
    pipeReadouts.map(r => r.valid && r.tag === getTag(pipeAddr))
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

  val rstCnt = RegInit(0.U(opts.INDEX_WIDTH.W))
  val rstLine = Wire(new ILine(opts))
  rstLine.tag := 0.U
  rstLine.valid := false.B

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
      rstCnt := rstCnt +% 1.U

      writerAddr := rstCnt
      writerDir := ILine.default(opts)
      writerMask := VecInit(Seq.fill(opts.ASSOC)(true.B))

      when(rstCnt.andR()) {
        // Omit current request
        toCPU.data.valid := false.B

        when(pipeRead) {
          nstate := S2State.refill
        }.otherwise {
          nstate := S2State.idle
        }
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
        rstCnt := 0.U
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
        val written = Wire(new ILine(opts))
        written.tag := getTag(pipeAddr)
        written.valid := true.B

        val victim = rand(opts.ASSOC_IDX_WIDTH - 1, 0)
        val mask = UIntToOH(victim)

        val dataView = data.asTypeOf(writerData)

        writerAddr := getIndex(pipeAddr)
        writerDir := written
        writerMask := mask.asBools()
        writerData := dataView

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
