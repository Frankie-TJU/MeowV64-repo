package meowv64.rocket

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ChiselEnum
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
    address = Seq(AddressSet(config.configBase, 0xfff)),
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
}

class BuffetsModuleImp(outer: Buffets) extends LazyModuleImp(outer) {
  val config = outer.config
  val ingress = IO(
    Flipped(Decoupled(new AddressGenerationEgress(config.beatBytes)))
  )

  val words = config.memorySize / config.beatBytes

  val data = SyncReadMem(
    words,
    Vec(config.beatBytes, UInt(8.W))
  )

  // memory port
  val enable = WireInit(false.B)
  val write = WireInit(false.B)
  val addr = Wire(UInt(log2Up(words).W))
  val readData = Reg(Vec(config.beatBytes, UInt(8.W)))
  val writeData = Wire(Vec(config.beatBytes, UInt(8.W)))
  val writeMask = Wire(Vec(config.beatBytes, Bool()))

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

  val shrinkIO = Wire(Decoupled(UInt(log2Ceil(config.memorySize + 1).W)))
  val shrinkQueue = Queue(shrinkIO)
  shrinkQueue.ready := false.B

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
    )
  )

  val state = RegInit(BuffetsState.sIdle)

  val (slave, slave_edge) = outer.slaveNode.in(0)

  val req = Queue(slave.a)
  val newData = Reg(UInt((config.beatBytes * 8).W))
  val pushLen = Reg(UInt(log2Ceil(config.beatBytes + 1).W))
  val currentReq = Reg(slave.a.bits.cloneType)

  ingress.ready := false.B
  req.ready := false.B
  slave.d.valid := false.B
  switch(state) {
    is(BuffetsState.sIdle) {
      when(ingress.valid) {
        ingress.ready := true.B
        state := BuffetsState.sPushing
        pushLen := ingress.bits.len
        newData := ingress.bits.data

        tail := tail + ingress.bits.len
        size := size + ingress.bits.len
        empty := empty - ingress.bits.len
      }.elsewhen(shrinkQueue.valid) {
        shrinkQueue.ready := true.B

        val shrink = shrinkQueue.bits
        head := head + shrink
        size := size - shrink
        empty := empty + shrink
      }.elsewhen(req.valid) {
        // accept when ready
        val offset = req.bits.address(log2Ceil(config.memorySize) - 1, 0)
        when(offset + (1.U << req.bits.size) <= size) {
          req.ready := true.B
          currentReq := req.bits
          when(req.bits.opcode === TLMessages.Get) {
            state := BuffetsState.sReading
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
      state := BuffetsState.sReadDone
    }
    is(BuffetsState.sReadDone) {
      slave.d.valid := true.B
      slave.d.bits := slave_edge.AccessAck(currentReq, readData.asTypeOf(slave.d.bits.data))

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
      writeData := newData.asTypeOf(writeData)

      // compute mask
      for (i <- 0 until config.beatBytes) {
        when(i.U < pushLen) {
          writeMask(i) := true.B
        }
      }

      state := BuffetsState.sIdle
    }
  }
}
