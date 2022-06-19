package meowv64.rocket

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
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
import freechips.rocketchip.tilelink.TLMasterToSlaveTransferSizes
import freechips.rocketchip.diplomacy.TransferSizes

case class AddressGenerationConfig(
    configBase: BigInt,
    beatBytes: Int,
    configInstWords: Int = 16,
    maxIterations: Int = 32,
    maxInflights: Int = 4,
    addrWidth: Int = 64,
    bytesWidth: Int = 7,
    strideWidth: Int = 8
) {
  // power of two
  assert((maxInflights & (maxInflights - 1)) == 0)
}

object AddressGeneration {
  // addresses
  def STATUS = 0x00
  def CONTROL = 0x20
  def ITERATIONS = 0x40
  def INSTS = 0x60

  // config
  def CONFIG_OPCODE = 31
  def CONFIG_BYTES = 20
  def CONFIG_STRIDE = 0
}

object AddressGenerationState extends ChiselEnum {
  val sIdle, sWorking, sFinishing = Value
}

object AddressGenerationOp extends ChiselEnum {
  val STRIDED, INDEXED = Value
}

class AddressGenerationInflight(config: AddressGenerationConfig)
    extends Bundle {
  val valid = Bool()

  // params
  val op = AddressGenerationOp()
  val bytes = UInt(config.bytesWidth.W)

  // progress
  val recv = UInt(config.bytesWidth.W)

  // current TileLink request
  val req = Bool()
  val reqAddr = UInt(config.addrWidth.W)
  val reqLgSize = UInt(log2Ceil(config.beatBytes).W)
}

object AddressGenerationInflight {
  def empty(config: AddressGenerationConfig) = {
    val res = Wire(new AddressGenerationInflight(config))
    res.valid := false.B

    res.op := AddressGenerationOp.STRIDED
    res.bytes := 0.U

    res.recv := 0.U

    res.req := false.B
    res.reqAddr := 0.U
    res.reqLgSize := 0.U
    res
  }
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
            sourceId = IdRange(0, config.maxInflights),
            emits = TLMasterToSlaveTransferSizes(
              get = TransferSizes(1, config.beatBytes)
            )
          )
        )
      )
    )
  )
  lazy val module = new LazyModuleImp(this) {
    val configInsts = RegInit(VecInit.fill(config.configInstWords)(0.U(32.W)))
    val currentInstIndex = RegInit(0.U(log2Up(config.configInstWords).W))
    val state = RegInit(AddressGenerationState.sIdle)
    val control = WireInit(0.U(32.W))
    val iterations = RegInit(0.U(log2Up(config.maxIterations).W))
    val currentIteration = RegInit(0.U(log2Up(config.maxIterations + 1).W))
    val inflights = RegInit(
      VecInit.fill(config.maxInflights)(AddressGenerationInflight.empty(config))
    )

    val head = RegInit(0.U(log2Ceil(config.maxInflights).W))
    val tail = RegInit(0.U(log2Ceil(config.maxInflights).W))

    val (master, master_edge) = masterNode.out(0)

    registerNode.regmap(
      AddressGeneration.STATUS -> Seq(
        RegField.r(
          32,
          state.asUInt,
          RegFieldDesc("state", "current state of address generation unit")
        )
      ),
      AddressGeneration.CONTROL -> Seq(
        RegField.w(
          32,
          control,
          RegFieldDesc("control", "control address generation unit")
        )
      ),
      AddressGeneration.ITERATIONS -> Seq(
        RegField(
          iterations.getWidth,
          iterations,
          RegFieldDesc("iterations", "number of iterations")
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

    switch(state) {
      is(AddressGenerationState.sIdle) {
        when(control(0)) {
          when(iterations =/= 0.U) {
            state := AddressGenerationState.sWorking
          }
        }
      }
      is(AddressGenerationState.sWorking) {
        val currentInst = configInsts(currentInstIndex)
        val arg1 = configInsts(currentInstIndex + 1.U)
        val arg2 = configInsts(currentInstIndex + 2.U)
        val currentOpcode = currentInst(31, 31)
        val currentBytes = currentInst(AddressGeneration.CONFIG_BYTES + config.bytesWidth, AddressGeneration.CONFIG_BYTES)
        val currentStride = currentInst(AddressGeneration.CONFIG_STRIDE + config.strideWidth, AddressGeneration.CONFIG_STRIDE)
        val full = tail +% 1.U === head
        when(currentInst === 0.U) {
          // next loop
          when(currentIteration + 1.U === iterations) {
            currentIteration := 0.U
            state := AddressGenerationState.sFinishing
          }.otherwise {
            currentIteration := currentIteration + 1.U
          }
          currentInstIndex := 0.U
        }.otherwise {
          when(~full) {
            inflights(tail).valid := true.B

            // params
            inflights(tail).op := AddressGenerationOp.safe(currentOpcode)._1
            val base = arg1 ## arg2
            inflights(tail).bytes := currentBytes

            // progress
            inflights(tail).recv := 0.U

            // initial TileLink request
            inflights(tail).req := true.B
            inflights(tail).reqAddr := base + currentStride * currentIteration
            // ceil currentBytes up
            when(currentBytes <= config.beatBytes.U) {
              inflights(tail).reqLgSize := Log2(currentBytes - 1.U) + 1.U
            }.otherwise {
              inflights(tail).reqLgSize := log2Ceil(config.beatBytes).U
            }

            tail := tail + 1.U
            currentInstIndex := currentInstIndex + 3.U
          }
        }
      }
      is(AddressGenerationState.sFinishing) {
        when(head === tail) {
          state := AddressGenerationState.sIdle
        }
      }
    }

    // send requests
    val reqMask = inflights.map(_.req)
    val reqIndex = PriorityEncoder(reqMask)
    master.a.valid := false.B
    when(reqMask.reduce(_ || _)) {
      val inflight = inflights(reqIndex)

      master.a.bits := master_edge.Get(reqIndex, inflight.reqAddr, inflight.reqLgSize)._2
      master.a.valid := true.B
      when(master.a.fire) {
        inflight.req := false.B
      }
    }

    // recv data
    master.d.ready := true.B
    when(master.d.fire) {
      val inflight = inflights(master.d.bits.source)
      val recvBytes = (1.U << master.d.bits.size)
      val newRecv = inflight.recv + recvBytes
      inflight.recv := newRecv

      when(inflight.bytes <= newRecv) {
        // done
        inflight.valid := false.B
      }.otherwise {
        // next beat
        inflight.req := true.B
        inflight.reqAddr := inflight.reqAddr + recvBytes
      }
    }

    when(head =/= tail) {
      val inflight = inflights(head)
      when(!inflight.valid) {
        head := head +% 1.U
      }
    }
  }
}
