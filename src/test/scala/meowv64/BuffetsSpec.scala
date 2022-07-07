package meowv64

import chisel3._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import chiseltest._
import freechips.rocketchip.diplomacy.LazyModule
import chipsalliance.rocketchip.config.Parameters
import meowv64.rocket.Buffets
import meowv64.rocket.BuffetsConfig
import meowv64.rocket.AddressGenerationEgress
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink.TLXbar
import freechips.rocketchip.tilelink.TLRAM
import freechips.rocketchip.tilelink.TLClientNode
import freechips.rocketchip.tilelink.TLMasterPortParameters
import freechips.rocketchip.tilelink.TLMasterParameters
import freechips.rocketchip.diplomacy.IdRange
import meowv64.rocket.MeowV64BaseConfig
import freechips.rocketchip.tilelink.TLMessages

class BuffetsTestHarness(implicit p: Parameters) extends LazyModule {
  val beatBytes = 32
  val config =
    BuffetsConfig(
      memoryBase = BigInt(0x80000000L),
      memorySize = BigInt(32),
      configBase = BigInt(0x60000000L),
      beatBytes = beatBytes
    )
  val dut = LazyModule(
    new Buffets(config)
  )
  val xbar = LazyModule(new TLXbar)

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

  dut.registerNode := xbar.node
  dut.slaveNode := xbar.node
  xbar.node := externalNode

  lazy val module = new BuffetsTestHarnessModuleImp(this)
}

class BuffetsTestHarnessModuleImp(
    val outer: BuffetsTestHarness
) extends LazyModuleImp(outer) {
  val external_io = outer.externalNode.makeIOs()

  val (external, external_edge) = outer.externalNode.out(0)

  val ingress = IO(
    Flipped(Decoupled(new AddressGenerationEgress(outer.beatBytes)))
  )
  outer.dut.module.ingress <> ingress
}

class BuffetsSpec
    extends AnyFlatSpec
    with Matchers
    with ChiselScalatestTester {
  behavior of "Buffets"

  implicit val p: Parameters = new MeowV64BaseConfig()

  it should s"run successfully" in {
    test(
      LazyModule(new BuffetsTestHarness()).module
    )
      .withAnnotations(Simulator.getAnnotations()) { dut =>
        dut.clock.step(16)
        val tl = dut.external_io(0)

        def write(addr: BigInt, data: BigInt) = {
          tl.a.bits.opcode.poke(TLMessages.PutFullData)
          tl.a.bits.param.poke(0.U)
          tl.a.bits.size.poke(2.U)
          tl.a.bits.source.poke(0.U)

          val beatBytes = 0x20
          tl.a.bits.address.poke(addr.U)

          var mask = BigInt("f", 16)
          mask = mask << (addr.toInt % beatBytes)
          tl.a.bits.mask.poke(mask)
          tl.a.bits.data.poke((data << ((addr.toInt % beatBytes).toInt * 8)).U)

          tl.a.bits.corrupt.poke(0.U)
          tl.a.valid.poke(true.B)
          tl.d.ready.poke(true.B)
          dut.clock.step()

          while (tl.a.ready.peek.litToBoolean == false) {
            dut.clock.step()
          }
          tl.a.valid.poke(false.B)
          dut.clock.step()
        }

        val base = 0x60000000L

        dut.ingress.valid.poke(false.B)
      }
  }
}
