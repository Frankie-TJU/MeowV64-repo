package meowv64

import chisel3._
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
import freechips.rocketchip.tilelink.TLMessages
import meowv64.rocket.AddressGenerationEgress
import chisel3.util.Decoupled

class AddressGenerationTestHarness(implicit p: Parameters) extends LazyModule {
  val beatBytes = 32
  val config = 
      AddressGenerationConfig(
        configBase = BigInt(0x60000000L),
        beatBytes = beatBytes
      )
  val dut = LazyModule(
    new AddressGeneration(config
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

  lazy val module = new AddressGenerationTestHarnessModuleImp(this)
}

class AddressGenerationTestHarnessModuleImp(
    outer: AddressGenerationTestHarness
) extends LazyModuleImp(outer) {
  val external_io = outer.externalNode.makeIOs()

  val (external, external_edge) = outer.externalNode.out(0)

  val egress = IO(outer.dut.module.egress.cloneType)
  egress <> outer.dut.module.egress
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
          tl.a.bits.data.poke((data << ((addr.toInt % beatBytes) * 8)).U)

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

        dut.egress.ready.poke(true.B)

        // write some data to memory
        write(0x1000, 0x1234)
        write(0x1004, 0x5678)

        write(base + AddressGeneration.ITERATIONS, 8)
        // strided
        val stride = 8
        val bytes = 8
        val config = (stride << AddressGeneration.CONFIG_STRIDE) | (bytes << AddressGeneration.CONFIG_BYTES)
        write(base + AddressGeneration.INSTS, config)
        // addr = 0x00001000
        write(base + AddressGeneration.INSTS + 0x4, 0x0000)
        write(base + AddressGeneration.INSTS + 0x8, 0x1000)
        write(base + AddressGeneration.CONTROL, 1)

        dut.clock.step(16)
      }
  }
}
