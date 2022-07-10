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
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink.TLXbar
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

class BuffetsSpec extends AnyFlatSpec with Matchers with ChiselScalatestTester {
  behavior of "Buffets"

  implicit val p: Parameters = new MeowV64BaseConfig()

  it should s"run successfully" in {
    test(
      LazyModule(new BuffetsTestHarness()).module
    )
      .withAnnotations(Simulator.getAnnotations()) { dut =>
        dut.clock.step(16)
        val tl = dut.external_io(0)

        tl.a.valid.poke(false.B)
        tl.d.ready.poke(false.B)

        def read(addr: BigInt): BigInt = {
          tl.a.bits.opcode.poke(TLMessages.Get)
          tl.a.bits.param.poke(0.U)
          tl.a.bits.size.poke(2.U)
          tl.a.bits.source.poke(0.U)

          val beatBytes = 0x20
          tl.a.bits.address.poke(addr.U)

          var mask = BigInt("f", 16)
          mask = mask << (addr % beatBytes).toInt
          tl.a.bits.mask.poke(mask)

          tl.a.bits.corrupt.poke(0.U)
          tl.a.valid.poke(true.B)

          while (tl.a.ready.peek.litToBoolean == false) {
            dut.clock.step()
          }

          dut.clock.step()
          tl.a.valid.poke(false.B)

          tl.d.ready.poke(true.B)
          while (tl.d.valid.peek.litToBoolean == false) {
            dut.clock.step()
          }
          var data = tl.d.bits.data.peek.litValue
          dut.clock.step()
          tl.d.ready.poke(false.B)

          data = data >> ((addr.toInt % beatBytes).toInt * 8)

          data
        }

        def pushData(data: BigInt, len: Int) {
          dut.ingress.bits.data.poke(data.U)
          dut.ingress.bits.len.poke(len.U)
          dut.ingress.valid.poke(true.B)

          while (dut.ingress.ready.peek.litToBoolean == false) {
            dut.clock.step()
          }
          dut.clock.step()

          dut.ingress.valid.poke(false.B)
        }

        val base = 0x80000000L

        dut.ingress.valid.poke(false.B)

        pushData(0x12345678, 4)
        pushData(0x11112222, 4)
        pushData(0x3333444455556666L, 8)

        assert(read(base) == 0x12345678L)
        assert(read(base + 0x4) == 0x11112222L)
        assert(read(base + 0x8) == 0x55556666L)
        assert(read(base + 0xc) == 0x33334444L)
      }
  }
}
