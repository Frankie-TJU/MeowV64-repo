package meowv64.rocket

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.diplomacy.SimpleDevice
import freechips.rocketchip.diplomacy.TransferSizes
import freechips.rocketchip.tilelink.TLManagerNode
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.tilelink.TLSlaveParameters
import freechips.rocketchip.tilelink.TLSlavePortParameters
import chisel3.util._
import chisel3._
import chisel3.experimental.ChiselEnum

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
  val empty = RegInit(config.memorySize.U(log2Ceil(config.memorySize + 1).W))

  val state = RegInit(BuffetsState.sIdle)

  val (slave, slave_edge) = outer.slaveNode.out(0)

  switch(state) {
    is(BuffetsState.sIdle) {
      egress.ready := true.B
      slave.a.ready := true.B
    }
    is(BuffetsState.sReading) {}
    is(BuffetsState.sWriting) {}
    is(BuffetsState.sShrinking) {}
    is(BuffetsState.sPushing) {}
  }
}
