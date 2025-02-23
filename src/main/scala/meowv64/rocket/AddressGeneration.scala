package meowv64.rocket

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.diplomacy.SimpleDevice
import freechips.rocketchip.diplomacy.TransferSizes
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.regmapper.RegFieldDesc
import freechips.rocketchip.regmapper.RegFieldGroup
import freechips.rocketchip.tilelink.TLClientNode
import freechips.rocketchip.tilelink.TLMasterParameters
import freechips.rocketchip.tilelink.TLMasterPortParameters
import freechips.rocketchip.tilelink.TLMasterToSlaveTransferSizes
import freechips.rocketchip.tilelink.TLRegisterNode

case class AddressGenerationConfig(
    configBase: BigInt,
    beatBytes: Int,
    configInstWords: Int = 16,
    maxInflights: Int = 4,
    addrWidth: Int = 64
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

  // performance counters
  def PERF_BYTES_READ = 0x1000
  def PERF_COUNT_READ = 0x1020
  def PERF_BYTES_EGRESS = 0x1040
  def PERF_COUNT_INST = 0x1060
  def PERF_COUNT_INDEXED = 0x1080
  def PERF_COUNT_STRIDED = 0x10a0
  def PERF_COUNT_ACTIVE = 0x10c0
  def PERF_COUNT_FULL = 0x10e0

  // config
  // opcode[31:27]
  def CONFIG_OPCODE = 27
  def CONFIG_OPCODE_WIDTH = 5
  // rs1[26:25]
  def CONFIG_RS1 = 25
  def CONFIG_RS1_WIDTH = 2
  // rd[24:23]
  def CONFIG_RD = 23
  def CONFIG_RD_WIDTH = 2
  // bytes[19:13]
  def CONFIG_BYTES = 13
  def CONFIG_BYTES_WIDTH = 7
  // rs2[22:21]
  def CONFIG_RS2 = 21
  def CONFIG_RS2_WIDTH = 2
  // indexedShift[12:10]
  def CONFIG_INDEXED_SHIFT = 10
  def CONFIG_INDEXED_SHIFT_WIDTH = 3
  // stride[9:0]
  def CONFIG_STRIDE = 0
  def CONFIG_STRIDE_WIDTH = 10
  // addr[24:0]
  def CONFIG_ADDR = 0
  def CONFIG_ADDR_WIDTH = 25
  // imm[23:0]
  def CONFIG_IMM = 0
  def CONFIG_IMM_WIDTH = 23

  def REG_COUNT = 4
  def REG_WIDTH = 32
}

object AddressGenerationState extends ChiselEnum {
  val sIdle, sWorking, sWaitingForLoad, sFinishing = Value
}

object AddressGenerationOp extends ChiselEnum {
  val LOOP, STRIDED, INDEXED, LOAD, ADD, ADDI = Value
}

class AddressGenerationInflight(config: AddressGenerationConfig)
    extends Bundle {
  val valid = Bool()
  val done = Bool()

  // params
  val op = AddressGenerationOp()
  val bytes = UInt(AddressGeneration.CONFIG_BYTES_WIDTH.W)
  val indexedBase = UInt(config.addrWidth.W)
  val indexedShift = UInt(AddressGeneration.CONFIG_INDEXED_SHIFT_WIDTH.W)

  // progress
  val recv = UInt(AddressGeneration.CONFIG_BYTES_WIDTH.W)
  val data = UInt(((1 << AddressGeneration.CONFIG_BYTES_WIDTH) * 8).W)
  val index = UInt(((1 << AddressGeneration.CONFIG_BYTES_WIDTH) * 8).W)
  val gotIndex = Bool()

  // current TileLink request
  val req = Bool()
  val reqAddr = UInt(config.addrWidth.W)
  val reqLgSize = UInt(log2Ceil(config.beatBytes).W)
}

class AddressGenerationEgress(beatBytes: Int) extends Bundle {
  val data = UInt((beatBytes * 8).W)
  val len = UInt(log2Ceil(beatBytes + 1).W)
}

object AddressGenerationInflight {
  def empty(config: AddressGenerationConfig) = {
    val res = Wire(new AddressGenerationInflight(config))
    res.valid := false.B
    res.done := false.B

    res.op := AddressGenerationOp.STRIDED
    res.bytes := 0.U
    res.indexedBase := 0.U
    res.indexedShift := 0.U

    res.recv := 0.U
    res.data := 0.U
    res.index := 0.U
    res.gotIndex := false.B

    res.req := false.B
    res.reqAddr := 0.U
    res.reqLgSize := 0.U
    res
  }
}

class AddressGeneration(val config: AddressGenerationConfig)(implicit
    p: Parameters
) extends LazyModule {

  // configuration
  val device = new SimpleDevice("addrgen", Seq("custom,addrgen"))

  val registerNode = TLRegisterNode(
    address = Seq(AddressSet(config.configBase, 0xffff)),
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

  lazy val module = new AddressGenerationModuleImp(this)
}

class AddressGenerationModuleImp(outer: AddressGeneration)
    extends LazyModuleImp(outer) {
  val config = outer.config

  val egress = IO(Decoupled(new AddressGenerationEgress(config.beatBytes)))

  val configInsts = RegInit(VecInit.fill(config.configInstWords)(0.U(32.W)))
  val currentInstIndex = RegInit(0.U(log2Up(config.configInstWords).W))
  val state = RegInit(AddressGenerationState.sIdle)
  val control = WireInit(0.U(32.W))
  val iterations = RegInit(0.U(AddressGeneration.REG_WIDTH.W))
  val regs = RegInit(
    VecInit.fill(AddressGeneration.REG_COUNT)(
      0.U(AddressGeneration.REG_WIDTH.W)
    )
  )
  val inflights = RegInit(
    VecInit.fill(config.maxInflights)(AddressGenerationInflight.empty(config))
  )

  // add wrapper to handle zero reg
  def readRegs(index: UInt): UInt = {
    val res = Wire(UInt(AddressGeneration.REG_WIDTH.W))
    when(index === (AddressGeneration.REG_COUNT - 1).U) {
      res := 0.U
    }.otherwise {
      res := regs(index)
    }
    res
  }

  def writeRegs(index: UInt, data: UInt) = {
    when(index =/= (AddressGeneration.REG_COUNT - 1).U) {
      regs(index) := data
    }
  }

  val head = RegInit(0.U(log2Ceil(config.maxInflights).W))
  val tail = RegInit(0.U(log2Ceil(config.maxInflights).W))
  val full = tail +% 1.U === head

  val (master, master_edge) = outer.masterNode.out(0)

  // register destination when waiting for load
  val waitingForLoadRD = RegInit(0.U(AddressGeneration.CONFIG_RD_WIDTH.W))

  // performance counters
  // number of bytes read from memory
  val bytesRead = RegInit(0.U(64.W))
  // number of transactions to read from memory
  val countRead = RegInit(0.U(64.W))
  // number of bytes sent to buffets
  val bytesEgress = RegInit(0.U(64.W))
  // number of instructions executed
  val countInst = RegInit(0.U(64.W))
  // number of indexed instructions executed
  val countIndexed = RegInit(0.U(64.W))
  // number of strided instructions executed
  val countStrided = RegInit(0.U(64.W))
  // number of cycles when address generation is active
  val countActive = RegInit(0.U(64.W))
  // number of cycles when inflight queue is full
  val countFull = RegInit(0.U(64.W))

  outer.registerNode.regmap(
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
    ),
    AddressGeneration.PERF_BYTES_READ -> Seq(
      RegField(
        bytesRead.getWidth,
        bytesRead,
        RegFieldDesc("bytesRead", "number of bytes read from memory")
      )
    ),
    AddressGeneration.PERF_COUNT_READ -> Seq(
      RegField(
        countRead.getWidth,
        countRead,
        RegFieldDesc("countRead", "number of transactions to read from memory")
      )
    ),
    AddressGeneration.PERF_BYTES_EGRESS -> Seq(
      RegField(
        bytesEgress.getWidth,
        bytesEgress,
        RegFieldDesc("bytesEgress", "number of bytes sent to buffets")
      )
    ),
    AddressGeneration.PERF_COUNT_INST -> Seq(
      RegField(
        countInst.getWidth,
        countInst,
        RegFieldDesc("countInst", "number of instructions executed")
      )
    ),
    AddressGeneration.PERF_COUNT_INDEXED -> Seq(
      RegField(
        countIndexed.getWidth,
        countIndexed,
        RegFieldDesc("countIndexed", "number of indexed instructions executed")
      )
    ),
    AddressGeneration.PERF_COUNT_STRIDED -> Seq(
      RegField(
        countStrided.getWidth,
        countStrided,
        RegFieldDesc("countStrided", "number of strided instructions executed")
      )
    ),
    AddressGeneration.PERF_COUNT_ACTIVE -> Seq(
      RegField(
        countActive.getWidth,
        countActive,
        RegFieldDesc(
          "countActive",
          "number of cycles when address generation is active"
        )
      )
    ),
    AddressGeneration.PERF_COUNT_FULL -> Seq(
      RegField(
        countFull.getWidth,
        countFull,
        RegFieldDesc(
          "countFull",
          "number of cycles when inflight queue is full"
        )
      )
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
      val arg3 = configInsts(currentInstIndex + 3.U)
      val arg4 = configInsts(currentInstIndex + 4.U)

      // see fields in BUFFETS.md
      val currentOpcode = AddressGenerationOp.safe(currentInst(29, 27))._1
      val currentBytes = currentInst(
        AddressGeneration.CONFIG_BYTES + AddressGeneration.CONFIG_BYTES_WIDTH - 1,
        AddressGeneration.CONFIG_BYTES
      )
      val currentStride = currentInst(
        AddressGeneration.CONFIG_STRIDE + AddressGeneration.CONFIG_STRIDE_WIDTH - 1,
        AddressGeneration.CONFIG_STRIDE
      )
      val currentIndexedShift = currentInst(
        AddressGeneration.CONFIG_INDEXED_SHIFT + AddressGeneration.CONFIG_INDEXED_SHIFT_WIDTH - 1,
        AddressGeneration.CONFIG_INDEXED_SHIFT
      )
      val currentRS1 = currentInst(
        AddressGeneration.CONFIG_RS1 + AddressGeneration.CONFIG_RS1_WIDTH - 1,
        AddressGeneration.CONFIG_RS1
      )
      val currentRS2 = currentInst(
        AddressGeneration.CONFIG_RS2 + AddressGeneration.CONFIG_RS2_WIDTH - 1,
        AddressGeneration.CONFIG_RS2
      )
      val currentRD = currentInst(
        AddressGeneration.CONFIG_RD + AddressGeneration.CONFIG_RD_WIDTH - 1,
        AddressGeneration.CONFIG_RD
      )
      val currentIMM = currentInst(
        AddressGeneration.CONFIG_IMM + AddressGeneration.CONFIG_IMM_WIDTH - 1,
        AddressGeneration.CONFIG_IMM
      )
      val currentAddr = currentInst(
        AddressGeneration.CONFIG_ADDR + AddressGeneration.CONFIG_ADDR_WIDTH - 1,
        AddressGeneration.CONFIG_ADDR
      )

      switch(currentOpcode) {
        is(AddressGenerationOp.LOOP) {
          // loop instruction
          // regs[rs1] + 1 == iterations
          when(readRegs(currentRS1) + 1.U === iterations) {
            // stop and regs[rs1] = 0
            writeRegs(currentRS1, 0.U)
            state := AddressGenerationState.sFinishing
            currentInstIndex := 0.U
          }.otherwise {
            // regs[rs1] ++
            writeRegs(currentRS1, readRegs(currentRS1) + 1.U)
            // goto addr
            currentInstIndex := currentAddr
          }
          countInst := countInst + 1.U
        }
        is(
          AddressGenerationOp.STRIDED,
          AddressGenerationOp.INDEXED,
          AddressGenerationOp.LOAD
        ) {
          // strided/indexed/load
          when(~full) {
            countInst := countInst + 1.U

            inflights(tail) := AddressGenerationInflight.empty(config)
            inflights(tail).valid := true.B
            inflights(tail).done := false.B

            // params
            inflights(tail).op := currentOpcode
            inflights(tail).bytes := currentBytes
            val base = arg1 ## arg2
            val indexedBase = arg3 ## arg4
            inflights(tail).indexedBase := indexedBase
            inflights(tail).indexedShift := currentIndexedShift
            inflights(tail).data := 0.U

            // progress
            inflights(tail).recv := 0.U

            // initial TileLink request
            inflights(tail).req := true.B
            inflights(tail).reqAddr := base + currentStride * readRegs(
              currentRS1
            )

            // ceil currentBytes up
            when(currentBytes <= config.beatBytes.U) {
              inflights(tail).reqLgSize := Log2(currentBytes - 1.U) + 1.U
            }.otherwise {
              inflights(tail).reqLgSize := log2Ceil(config.beatBytes).U
            }

            tail := tail + 1.U
            when(currentOpcode === AddressGenerationOp.STRIDED) {
              // strided load
              countStrided := countStrided + 1.U
              currentInstIndex := currentInstIndex + 3.U
            }.elsewhen(currentOpcode === AddressGenerationOp.INDEXED) {
              // indexed load
              countIndexed := countIndexed + 1.U
              currentInstIndex := currentInstIndex + 5.U
            }.elsewhen(currentOpcode === AddressGenerationOp.LOAD) {
              // load to reg
              currentInstIndex := currentInstIndex + 3.U
              waitingForLoadRD := currentRD
              // return to sWorking when data is read
              state := AddressGenerationState.sWaitingForLoad
            }.otherwise {
              assert(false.B)
            }
          }
        }
        is(AddressGenerationOp.ADD) {
          // regs[rd] = regs[rs1] + regs[rs2]
          writeRegs(currentRD, readRegs(currentRS1) + readRegs(currentRS2))
          currentInstIndex := currentInstIndex + 1.U
        }
        is(AddressGenerationOp.ADDI) {
          // regs[rd] = regs[rs1] + simm
          writeRegs(
            currentRD,
            (readRegs(currentRS1).asSInt + currentIMM.asSInt).asUInt
          )
          currentInstIndex := currentInstIndex + 1.U
        }
      }
    }
    is(AddressGenerationState.sFinishing) {
      when(head === tail) {
        state := AddressGenerationState.sIdle
      }
    }
  }

  when(full) {
    countFull := countFull + 1.U
  }

  when(state =/= AddressGenerationState.sIdle) {
    countActive := countActive + 1.U
  }

  // send requests
  val reqMask = inflights.map(_.req)
  val reqIndex = PriorityEncoder(reqMask)
  master.a.valid := false.B
  when(reqMask.reduce(_ || _)) {
    val inflight = inflights(reqIndex)

    master.a.bits := master_edge
      .Get(reqIndex, inflight.reqAddr, inflight.reqLgSize)
      ._2
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
    val offset = inflight.reqAddr(log2Up(config.beatBytes) - 1, 0)
    val shift = offset << 3.U

    bytesRead := bytesRead + recvBytes
    countRead := countRead + 1.U

    when(inflight.op === AddressGenerationOp.STRIDED) {
      // strided
      when(inflight.bytes <= newRecv) {
        // done
        inflight.done := true.B
      }.otherwise {
        // next beat
        inflight.req := true.B
        inflight.reqAddr := inflight.reqAddr + recvBytes
      }
      inflight.data := master.d.bits.data >> shift
    }.elsewhen(inflight.op === AddressGenerationOp.INDEXED) {
      // indexed
      when(!inflight.gotIndex) {
        inflight.gotIndex := true.B
        inflight.index := master.d.bits.data
        val index = Wire(UInt(32.W))
        index := master.d.bits.data >> shift

        // first indexed access
        // indexedShift = 2 -> uint32_t data[]
        // indexedShift = 3 -> uint64_t data[]
        inflight.req := true.B
        inflight.reqAddr := inflight.indexedBase +
          (index << inflight.indexedShift)
        when(inflight.bytes === 4.U) {
          inflight.reqLgSize := 2.U // 4 bytes
        }.otherwise {
          assert(inflight.bytes === 8.U)
          inflight.reqLgSize := 3.U // 8 bytes
        }
        inflight.recv := 0.U
      }.otherwise {
        val data = Wire(UInt(64.W))
        when(inflight.bytes === 4.U) {
          data := (master.d.bits.data >> shift)(31, 0)
        }.otherwise {
          assert(inflight.bytes === 8.U)
          data := master.d.bits.data >> shift
        }
        inflight.data := data
        // done
        inflight.done := true.B
      }
    }.elsewhen(inflight.op === AddressGenerationOp.LOAD) {
      // load data to register
      assert(state === AddressGenerationState.sWaitingForLoad)
      state := AddressGenerationState.sWorking
      writeRegs(waitingForLoadRD, master.d.bits.data >> shift)
      inflight.done := true.B
    }.otherwise {
      assert(false.B)
    }
  }

  // egress
  egress.valid := false.B
  egress.bits.data := 0.U
  egress.bits.len := 0.U

  when(head =/= tail) {
    val inflight = inflights(head)
    when(inflight.done) {
      when(inflight.op === AddressGenerationOp.LOAD) {
        // do nothing
        head := head +% 1.U
        inflight.valid := false.B
      }.otherwise {
        // send to buffets
        egress.valid := true.B
        egress.bits.data := inflight.data
        egress.bits.len := inflight.bytes
        when(egress.fire) {
          bytesEgress := bytesEgress + inflight.bytes
          inflight.valid := false.B
          head := head +% 1.U
        }
      }
    }
  }
}
