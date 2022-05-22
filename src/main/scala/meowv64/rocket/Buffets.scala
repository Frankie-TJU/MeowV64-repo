package meowv64.rocket

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink.TLManagerNode
import freechips.rocketchip.tilelink.TLSlavePortParameters
import freechips.rocketchip.tilelink.TLSlaveParameters
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.TransferSizes
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.diplomacy.SimpleDevice
import freechips.rocketchip.tilelink.TLClientNode
import freechips.rocketchip.tilelink.TLMasterPortParameters
import freechips.rocketchip.tilelink.TLMasterParameters
import freechips.rocketchip.diplomacy.IdRange

case class BuffetsConfig(
    memoryBase: BigInt,
    memorySize: BigInt,
    configBase: BigInt,
    beatBytes: Int
)

class Buffets(config: BuffetsConfig)(implicit p: Parameters)
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

  // to memory
  val masterNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v2(
            name = "buffets-master",
            sourceId = IdRange(0, 1)
          )
        )
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {}
}
