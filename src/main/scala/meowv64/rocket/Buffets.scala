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
  val sIdle, sReading, sWriting, sShrinking, sPushing = Value
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
  val egress = IO(
    Flipped(Decoupled(new AddressGenerationEgress(config.beatBytes)))
  )

  val words = config.memorySize / config.beatBytes

  val data = SyncReadMem(
    words,
    UInt((config.beatBytes * 8).W)
  )

  // memory port
  val enable = WireInit(false.B)
  val write = WireInit(false.B)
  val addr = Wire(UInt(log2Up(words).W))
  val readData = Wire(UInt((config.beatBytes * 8).W))
  val writeData = Wire(UInt((config.beatBytes * 8).W))
  readData := DontCare
  when(enable) {
    val port = data(addr)
    when(write) {
      port := writeData
    }.otherwise {
      readData := port
    }
  }

  // buffets
  val head = RegInit(0.U(log2Ceil(config.memorySize).W))
  val tail = RegInit(0.U(log2Ceil(config.memorySize).W))
  val size = RegInit(0.U(log2Ceil(config.memorySize + 1).W))
  val empty = RegInit(config.memorySize.U(log2Ceil(config.memorySize + 1).W))

  val shrinkIO = Decoupled(UInt(log2Ceil(config.memorySize + 1).W))

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

  val (slave, slave_edge) = outer.slaveNode.out(0)

  val req = Queue(slave.a)
  val pushData = Reg(UInt((config.beatBytes * 8).W))

  egress.ready := false.B
  req.ready := false.B
  switch(state) {
    is(BuffetsState.sIdle) {
      when(egress.valid) {
        egress.ready := true.B
        state := BuffetsState.sPushing
        pushData := egress.bits.data

        tail := tail + egress.bits.len
        size := size + egress.bits.len
        empty := empty - egress.bits.len
      }.elsewhen(req.valid) {
        // accept when ready
        val offset = req.bits.address(log2Ceil(config.memorySize) - 1, 0)
        when(offset + (1.U << req.bits.size) <= size) {
          req.ready := true.B
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
    }
    is(BuffetsState.sWriting) {
      enable := true.B
      write := true.B
    }
    is(BuffetsState.sShrinking) {
      val shrink = 0.U
      head := head + shrink
      size := size - shrink
      empty := empty + shrink
    }
    is(BuffetsState.sPushing) {
      // save pushData
      state := BuffetsState.sIdle
    }
  }
}
