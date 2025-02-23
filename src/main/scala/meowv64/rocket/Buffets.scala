package meowv64.rocket

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.diplomacy.SimpleDevice
import freechips.rocketchip.diplomacy.TransferSizes
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.regmapper.RegFieldDesc
import freechips.rocketchip.tilelink.TLManagerNode
import freechips.rocketchip.tilelink.TLMessages
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.tilelink.TLSlaveParameters
import freechips.rocketchip.tilelink.TLSlavePortParameters

case class BuffetsConfig(
    memoryBase: BigInt,
    memorySize: BigInt,
    configBase: BigInt,
    beatBytes: Int
)

class Buffets(val config: BuffetsConfig)(implicit p: Parameters)
    extends LazyModule {

  // to core
  val slaveNode = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        Seq(
          TLSlaveParameters.v1(
            address =
              List(AddressSet(config.memoryBase, config.memorySize - 1)),
            supportsGet = TransferSizes(1, config.beatBytes),
            fifoId = Some(0)
          )
        ),
        beatBytes = config.beatBytes
      )
    )
  )

  // configuration
  val device = new SimpleDevice("buffets", Seq("custom,buffets"))

  val registerNode = TLRegisterNode(
    address = Seq(AddressSet(config.configBase, 0xffff)),
    device = device,
    beatBytes = config.beatBytes
  )

  lazy val module = new BuffetsModuleImp(this)
}

object BuffetsState extends ChiselEnum {
  val sIdle, sReading, sReadDone, sWriting, sWriteDone, sPushing = Value
}

object Buffets {
  // addresses
  def HEAD = 0x00
  def TAIL = 0x20
  def SIZE = 0x40
  def EMPTY = 0x60
  def SHRINK = 0x80

  // performance counters
  def PERF_BYTES_PUSHED = 0x1000
  def PERF_COUNT_PUSHED = 0x1020
  def PERF_BYTES_POPPED = 0x1040
  def PERF_COUNT_POPPED = 0x1060
  def PERF_BYTES_READ = 0x1080
  def PERF_COUNT_READ = 0x10a0
  def PERF_COUNT_READ_STALL_CYCLES = 0x10c0
  def PERF_COUNT_PUSH_STALL_CYCLES = 0x10e0
}

class BuffetsModuleImp(outer: Buffets) extends LazyModuleImp(outer) {
  val config = outer.config
  val ingress = IO(
    Flipped(Decoupled(new AddressGenerationEgress(config.beatBytes)))
  )
  val fastpath = IO(new Bundle {
    val head = Decoupled(Vec(config.beatBytes, UInt(8.W)))
  })

  val words = config.memorySize / config.beatBytes

  val data = SyncReadMem(
    words,
    Vec(config.beatBytes, UInt(8.W))
  )

  // memory port
  val enable = WireInit(false.B)
  val write = WireInit(false.B)
  val addr = Wire(UInt(log2Up(words).W))
  val readData = Wire(Vec(config.beatBytes, UInt(8.W)))
  val writeData = Wire(Vec(config.beatBytes, UInt(8.W)))
  val writeMask = Wire(Vec(config.beatBytes, Bool()))

  readData := 0.U.asTypeOf(readData)
  writeData := 0.U.asTypeOf(writeData)
  writeMask := 0.U.asTypeOf(writeMask)
  addr := 0.U

  when(enable) {
    val port = data(addr)
    when(write) {
      for (i <- 0 until config.beatBytes) {
        when(writeMask(i)) {
          port(i) := writeData(i)
        }
      }
    }.otherwise {
      readData := port
    }
  }

  // buffets
  val head = RegInit(0.U(log2Ceil(config.memorySize).W))
  val tail = RegInit(0.U(log2Ceil(config.memorySize).W))
  val size = RegInit(0.U(log2Ceil(config.memorySize + 1).W))
  val empty = RegInit(config.memorySize.U(log2Ceil(config.memorySize + 1).W))

  // performance counters
  // number of bytes pushed to buffets
  val bytesPushed = RegInit(0.U(64.W))
  // number of times when data is pushed to buffets
  val countPushed = RegInit(0.U(64.W))
  // number of bytes popped from buffets
  val bytesPopped = RegInit(0.U(64.W))
  // number of times when data is popped from buffets
  val countPopped = RegInit(0.U(64.W))
  // number of bytes read from buffets
  val bytesRead = RegInit(0.U(64.W))
  // number of times when data is read from buffets
  val countRead = RegInit(0.U(64.W))
  // number of cycles when read is stalled
  val countReadStallCycles = RegInit(0.U(64.W))
  // number of cycles when push is stalled
  val countPushStallCycles = RegInit(0.U(64.W))

  val shrinkIO = Wire(Decoupled(UInt(log2Ceil(config.memorySize + 1).W)))
  val shrinkQueue = Queue(shrinkIO)
  shrinkQueue.ready := false.B

  fastpath.head.bits := data(
    head(
      log2Ceil(config.memorySize) - 1,
      log2Ceil(config.beatBytes)
    )
  )

  outer.registerNode.regmap(
    Buffets.HEAD -> Seq(
      RegField(
        head.getWidth,
        head,
        RegFieldDesc("head", "head pointer")
      )
    ),
    Buffets.TAIL -> Seq(
      RegField(
        tail.getWidth,
        tail,
        RegFieldDesc("tail", "tail pointer")
      )
    ),
    Buffets.SIZE -> Seq(
      RegField(
        size.getWidth,
        size,
        RegFieldDesc("size", "valid bytes")
      )
    ),
    Buffets.EMPTY -> Seq(
      RegField(
        empty.getWidth,
        empty,
        RegFieldDesc("empty", "empty bytes")
      )
    ),
    Buffets.SHRINK -> Seq(
      RegField.w(
        log2Ceil(config.memorySize + 1),
        shrinkIO,
        RegFieldDesc("shrink", "shrink bytes")
      )
    ),
    Buffets.PERF_BYTES_PUSHED -> Seq(
      RegField(
        bytesPushed.getWidth,
        bytesPushed,
        RegFieldDesc("bytesPushed", "number of bytes pushed to buffets")
      )
    ),
    Buffets.PERF_COUNT_PUSHED -> Seq(
      RegField(
        countPushed.getWidth,
        countPushed,
        RegFieldDesc(
          "countPushed",
          "number of times when data is pushed to buffets"
        )
      )
    ),
    Buffets.PERF_BYTES_POPPED -> Seq(
      RegField(
        bytesPopped.getWidth,
        bytesPopped,
        RegFieldDesc("bytesPopped", "number of bytes popped from buffets")
      )
    ),
    Buffets.PERF_COUNT_POPPED -> Seq(
      RegField(
        countPopped.getWidth,
        countPopped,
        RegFieldDesc(
          "countPopped",
          "number of times when data is popped from buffets"
        )
      )
    ),
    Buffets.PERF_BYTES_READ -> Seq(
      RegField(
        bytesRead.getWidth,
        bytesRead,
        RegFieldDesc("bytesRead", "number of bytes read from buffets")
      )
    ),
    Buffets.PERF_COUNT_READ -> Seq(
      RegField(
        countRead.getWidth,
        countRead,
        RegFieldDesc(
          "countRead",
          "number of times when data is read from buffets"
        )
      )
    ),
    Buffets.PERF_COUNT_READ_STALL_CYCLES -> Seq(
      RegField(
        countReadStallCycles.getWidth,
        countReadStallCycles,
        RegFieldDesc(
          "countReadStallCycles",
          "number of cycles when read is stalled"
        )
      )
    ),
    Buffets.PERF_COUNT_PUSH_STALL_CYCLES -> Seq(
      RegField(
        countPushStallCycles.getWidth,
        countPushStallCycles,
        RegFieldDesc(
          "countPushStallCycles",
          "number of cycles when push is stalled"
        )
      )
    )
  )

  val state = RegInit(BuffetsState.sIdle)

  val (slave, slave_edge) = outer.slaveNode.in(0)

  val req = Queue(slave.a)
  val newData = Reg(UInt((config.beatBytes * 8).W))
  val pushLen = Reg(UInt(log2Ceil(config.beatBytes + 1).W))
  val currentReq = Reg(slave.a.bits.cloneType)
  val currentAddr = Reg(UInt(log2Ceil(config.memorySize).W))

  fastpath.head.valid := state === BuffetsState.sIdle && size >= config.beatBytes.U

  ingress.ready := false.B
  req.ready := false.B
  slave.d.valid := false.B
  switch(state) {
    is(BuffetsState.sIdle) {
      when(fastpath.head.fire) {
        head := head + config.beatBytes.U
        size := size - config.beatBytes.U
        empty := empty + config.beatBytes.U
      }.elsewhen(ingress.valid && empty > ingress.bits.len) {
        ingress.ready := true.B
        state := BuffetsState.sPushing
        pushLen := ingress.bits.len
        newData := ingress.bits.data

        bytesPushed := bytesPushed + ingress.bits.len
        countPushed := countPushed + 1.U
      }.elsewhen(shrinkQueue.valid) {
        shrinkQueue.ready := true.B

        val shrink = shrinkQueue.bits
        head := head + shrink
        size := size - shrink
        empty := empty + shrink

        bytesPopped := bytesPopped + shrink
        countPopped := countPopped + 1.U
      }.elsewhen(req.valid) {
        // accept when ready
        val offset = req.bits.address(log2Ceil(config.memorySize) - 1, 0)
        when(offset + (1.U << req.bits.size) <= size) {
          req.ready := true.B
          currentReq := req.bits
          currentAddr := req.bits.address + head
          when(req.bits.opcode === TLMessages.Get) {
            state := BuffetsState.sReading

            bytesRead := bytesRead + (1.U << req.bits.size)
            countRead := countRead + 1.U
          }.elsewhen(req.bits.opcode === TLMessages.PutFullData) {
            state := BuffetsState.sWriting
          }.otherwise {
            assert(false.B)
          }
        }
      }
    }
    is(BuffetsState.sReading) {
      enable := true.B
      write := false.B
      addr := currentAddr(
        log2Ceil(config.memorySize) - 1,
        log2Ceil(config.beatBytes)
      )
      state := BuffetsState.sReadDone
    }
    is(BuffetsState.sReadDone) {
      enable := true.B
      slave.d.valid := true.B
      // right shift to put data in LSB
      // left shift for TileLink alignment
      val offset = currentAddr(log2Ceil(config.beatBytes) - 1, 0)
      val reqOffset = currentReq.address(log2Ceil(config.beatBytes) - 1, 0)
      val actualData =
        readData.asTypeOf(
          slave.d.bits.data
        ) >> (offset << 3.U) << (reqOffset << 3.U)
      slave.d.bits := slave_edge.AccessAck(
        currentReq,
        actualData
      )

      when(slave.d.ready) {
        state := BuffetsState.sIdle
      }
    }
    is(BuffetsState.sWriting) {
      enable := true.B
      write := true.B
      writeData := newData.asTypeOf(writeData)
      state := BuffetsState.sWriteDone
    }
    is(BuffetsState.sWriteDone) {
      slave.d.valid := true.B
      slave.d.bits := slave_edge.AccessAck(currentReq)

      when(slave.d.ready) {
        state := BuffetsState.sIdle
      }
    }
    is(BuffetsState.sPushing) {
      // save pushData
      enable := true.B
      write := true.B

      // compute mask
      // TODO: cross line pushing
      val tailInLine = tail(log2Ceil(config.beatBytes) - 1, 0)
      addr := tail(tail.getWidth - 1, log2Ceil(config.beatBytes))
      for (i <- 0 until config.beatBytes) {
        when(tailInLine <= i.U && i.U < tailInLine + pushLen) {
          writeMask(i) := true.B
        }
      }
      writeData := (newData << (tailInLine << 3.U)).asTypeOf(writeData)

      tail := tail + pushLen
      size := size + pushLen
      empty := empty - pushLen
      state := BuffetsState.sIdle
    }
  }

  when(ingress.valid && !ingress.ready) {
    // push is stalled
    countPushStallCycles := countPushStallCycles + 1.U
  }

  when(req.valid && !req.ready) {
    // read is stalled
    countReadStallCycles := countReadStallCycles + 1.U
  }
}
