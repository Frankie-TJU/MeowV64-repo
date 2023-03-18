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

class AddressGenerationTestHarness(implicit p: Parameters) extends LazyModule {
  val beatBytes = 32
  val config =
    AddressGenerationConfig(
      configBase = BigInt(0x60000000L),
      beatBytes = beatBytes
    )
  val dut = LazyModule(
    new AddressGeneration(config)
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
    val outer: AddressGenerationTestHarness
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

        dut.egress.ready.poke(true.B)

        // strided
        if (true) {
          // write some data to memory
          write(0x1000, 0x12345678)
          write(0x1004, 0x56781234)

          // strided read 8 bytes
          val stride = 8
          val bytes = 8
          val config =
            (stride << AddressGeneration.CONFIG_STRIDE) | (bytes << AddressGeneration.CONFIG_BYTES)
          write(base + AddressGeneration.INSTS, config)
          // addr = 0x00001000
          write(base + AddressGeneration.INSTS + 0x4, 0x0000)
          write(base + AddressGeneration.INSTS + 0x8, 0x1000)
          write(base + AddressGeneration.ITERATIONS, 1)
          // start
          write(base + AddressGeneration.CONTROL, 1)

          fork {
            dut.egress.ready.poke(true.B)
            while (!dut.egress.valid.peek.litToBoolean) {
              dut.clock.step()
            }

            dut.egress.valid.expect(true.B)
            val data = dut.egress.bits.data.peekInt()
            assert(data == BigInt("5678123412345678", 16))
            dut.egress.bits.len.expect(8.U)
          }.joinAndStep(dut.clock)

          dut.clock.step(16)
        }

        // indexed
        if (true) {
          // write some data to memory
          // index
          write(0x2000, 0x3)
          write(0x2004, 0x2)
          write(0x2008, 0x1)
          write(0x200c, 0x0)
          // value
          write(0x3000, 0x00)
          write(0x3004, 0x10)
          write(0x3008, 0x20)
          write(0x300c, 0x30)

          val opcode: Long = 1
          val stride: Long = 4
          val bytes: Long = 4
          val indexedShift: Long = 2
          val config: Long =
            (opcode << AddressGeneration.CONFIG_OPCODE) | (indexedShift << AddressGeneration.CONFIG_INDEXED_SHIFT) |
              (stride << AddressGeneration.CONFIG_STRIDE) | (bytes << AddressGeneration.CONFIG_BYTES)
          write(base + AddressGeneration.INSTS, config)
          // addr = 0x00002000
          write(base + AddressGeneration.INSTS + 0x4, 0x0000)
          write(base + AddressGeneration.INSTS + 0x8, 0x2000)
          // indexedBase = 0x00003000
          write(base + AddressGeneration.INSTS + 0xc, 0x0000)
          write(base + AddressGeneration.INSTS + 0x10, 0x3000)
          write(base + AddressGeneration.ITERATIONS, 4)
          // start
          write(base + AddressGeneration.CONTROL, 1)

          for (expected <- Seq(0x30, 0x20, 0x10, 0x00)) {
            dut.egress.ready.poke(true.B)
            while (!dut.egress.valid.peek.litToBoolean) {
              dut.clock.step()
            }

            dut.egress.valid.expect(true.B)
            val data = dut.egress.bits.data.peekInt()
            assert(data == expected)
            dut.egress.bits.len.expect(4.U)
            dut.clock.step(1)
          }

          dut.clock.step(16)
        }
      }
  }
}
