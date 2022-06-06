package meowv64

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import chiseltest._
import freechips.rocketchip.diplomacy.LazyModule
import chipsalliance.rocketchip.config.Parameters
import meowv64.rocket.AddressGeneration
import meowv64.rocket.AddressGenerationConfig
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink.TLXbar
import freechips.rocketchip.tilelink.TLRAM
import freechips.rocketchip.tilelink.TLClientNode
import freechips.rocketchip.tilelink.TLMasterPortParameters
import freechips.rocketchip.tilelink.TLMasterParameters
import freechips.rocketchip.diplomacy.IdRange
import meowv64.rocket.MeowV64BaseConfig

class AddressGenerationTestHarness(implicit p: Parameters) extends LazyModule {
  val beatBytes = 32
  val dut = LazyModule(
    new AddressGeneration(
      AddressGenerationConfig(
        configBase = BigInt(0x60000000L),
        beatBytes = beatBytes
      )
    )
  )
  val xbar = LazyModule(new TLXbar)
  val ram = LazyModule(
    new TLRAM(address = AddressSet(0x0, 0xffff), beatBytes = beatBytes)
  )

  val externalNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v2(
            name = "external",
            sourceId = IdRange(0, 1)
          )
        )
      )
    )
  )

  ram.node := xbar.node
  dut.registerNode := xbar.node
  xbar.node := dut.masterNode
  xbar.node := externalNode

  lazy val module = new LazyModuleImp(this) {
    externalNode.makeIOs()
  }
}

class AddressGenerationSpec
    extends AnyFlatSpec
    with Matchers
    with ChiselScalatestTester {
  behavior of "AddressGeneration"

  implicit val p: Parameters = new MeowV64BaseConfig()

  it should s"run successfully" in {
    test(
      LazyModule(new AddressGenerationTestHarness()).module
    )
      .withAnnotations(Simulator.getAnnotations()) { dut =>
        dut.clock.step(16)
      }
  }
}
