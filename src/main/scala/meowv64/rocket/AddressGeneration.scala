package meowv64.rocket

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ChiselEnum
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.diplomacy.SimpleDevice
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.regmapper.RegFieldDesc
import freechips.rocketchip.regmapper.RegFieldGroup
import freechips.rocketchip.tilelink.TLClientNode
import freechips.rocketchip.tilelink.TLMasterParameters
import freechips.rocketchip.tilelink.TLMasterPortParameters
import freechips.rocketchip.tilelink.TLRegisterNode

case class AddressGenerationConfig(
    configBase: BigInt,
    beatBytes: Int,
    configInstWords: Int = 16
)

object AddressGeneration {
  def STATUS = 0x00
  def CONTROL = 0x10
  def INSTS = 0x20
}

object AddressGenerationState extends ChiselEnum {
  val sIdle = Value
}

class AddressGeneration(config: AddressGenerationConfig)(implicit p: Parameters)
    extends LazyModule {

  // configuration
  val device = new SimpleDevice("addrgen", Seq("custom,addrgen"))

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
            name = "meowv64-addrgen",
            sourceId = IdRange(0, 1)
          )
        )
      )
    )
  )
  lazy val module = new LazyModuleImp(this) {
    val configInsts = Reg(Vec(config.configInstWords, UInt(32.W)))
    val state = RegInit(AddressGenerationState.sIdle)
    val control = WireInit(0.U(32.W))

    registerNode.regmap(
      AddressGeneration.STATUS -> Seq(
        RegField.r(
          32,
          state.asUInt,
          RegFieldDesc("status", "current status of address generation unit")
        )
      ),
      AddressGeneration.CONTROL -> Seq(
        RegField.w(
          32,
          control,
          RegFieldDesc("control", "control address generation unit")
        )
      ),
      AddressGeneration.INSTS -> RegFieldGroup(
        "config_insts",
        Some("Saves the configuration instructions"),
        configInsts.zipWithIndex.map({ case (x, i) =>
          RegField(
            32,
            x,
            RegFieldDesc(s"config_insts_$i", s"configuration instructions $i")
          )
        }),
        false
      )
    )
  }
}
