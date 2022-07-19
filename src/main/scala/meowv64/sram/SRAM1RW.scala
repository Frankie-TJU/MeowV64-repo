package meowv64.sram

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.Dependency

// `depth` elements
// each element has `width` bits
sealed abstract class SRAM1RWBlockType(val width: Int, val depth: Int) {
  def addressWidth = log2Up(depth)
  def moduleName = s"sram_1rw_${depth}X${width}"
  def moduleFile = moduleName + ".v"
}

object SRAM1RWBlockType {
  case object SRAM1RW_16_128 extends SRAM1RWBlockType(16, 128)
  case object SRAM1RW_64_128 extends SRAM1RWBlockType(64, 128)
}

class SRAM1RWBlackBox(blockType: SRAM1RWBlockType)
    extends BlackBox
    with HasBlackBoxResource {

  val io = IO(new Bundle {
    val CLK = Input(Clock())
    // Read+Write Port
    val CEN = Input(Bool())
    val WEN = Input(Bool())
    val A = Input(UInt(blockType.addressWidth.W))
    val D = Input(UInt(blockType.width.W))
    val Q = Output(UInt(blockType.width.W))
    // other signals
    val EMA = Input(UInt(3.W))
    val EMAW = Input(UInt(2.W))
    val RET1N = Input(Bool()) // retention input
  })

  override def desiredName: String = blockType.moduleName

  // addResource(s"/sram/${blockType.moduleName}.v")
}

// use several sram blocks to create a `width` x `depth` sram
// 1 read & 1 write
class SRAM1RWMemInner(
    width: Int,
    depth: Int,
    blockType: SRAM1RWBlockType,
    name: String
) extends Module {
  require(
    width % blockType.width == 0 && depth % blockType.depth == 0,
    s"required size ${width} * ${depth} not using whole SRAM1RW ${blockType.width} * ${blockType.depth}"
  )

  private val widthConcat = width / blockType.width
  private val depthConcat = depth / blockType.depth

  // read + write
  val RW0_clk = IO(Input(Clock()))
  val RW0_addr = IO(Input(UInt(log2Ceil(depth).W)))
  val RW0_en = IO(Input(Bool()))
  val RW0_wmode = IO(Input(Bool()))
  val RW0_wdata = IO(Input(UInt(width.W)))
  val RW0_rdata = IO(Output(UInt(width.W)))

  val writeData =
    RW0_wdata.asTypeOf(Vec(widthConcat, UInt(blockType.width.W)))
  val blockAddr = RW0_addr >> blockType.addressWidth
  // initialize SRAM1RW blackbox, connect ports
  val sramArray = for (i <- 0 until widthConcat) yield {
    for (j <- 0 until depthConcat) yield {
      val sram = Module(new SRAM1RWBlackBox(blockType))
      sram.suggestName(s"${name}_sram_${i}_${j}")
      val selected =
        blockAddr === j.asUInt
      // assume R0_clk is the same as W0_clk
      sram.io.CLK := RW0_clk
      // Read+Write Port (A)
      sram.io.CEN := ~(RW0_en && selected)
      sram.io.A := RW0_addr // truncate here
      sram.io.WEN := ~RW0_wmode
      sram.io.D := writeData(i)
      // other signals
      sram.io.RET1N := true.B // disable retain mode
      // TODO: investigate about EMA option of SRAM1RW block
      sram.io.EMA := 7.U
      sram.io.EMAW := 3.U

      sram
    }
  }

  // select read data
  val lastReadBlockAddr = RegNext(blockAddr)
  private val readData = Wire(Vec(widthConcat, UInt(blockType.width.W)))
  for ((data, i) <- readData zipWithIndex) {
    val dataToSelect = for ((block, j) <- sramArray(i) zipWithIndex) yield {
      (
        lastReadBlockAddr === j.asUInt,
        block.io.Q
      )
    }
    data := Mux1H(dataToSelect)
  }

  RW0_rdata := readData.asUInt
}

// use several sram blocks to create a `width` x `depth` sram
// 1 read & 1 write
// handle masking
class SRAM1RWMem(
    width: Int,
    depth: Int,
    maskGran: Int,
    blockType: SRAM1RWBlockType,
    name: String
) extends Module {
  private val widthConcat = width / maskGran

  // read + write
  val RW0_clk = IO(Input(Clock()))
  val RW0_addr = IO(Input(UInt(log2Ceil(depth).W)))
  val RW0_en = IO(Input(Bool()))
  val RW0_wmode = IO(Input(Bool()))
  val RW0_wmask = IO(Input(UInt(widthConcat.W)))
  val RW0_wdata = IO(Input(UInt(width.W)))
  val RW0_rdata = IO(Output(UInt(width.W)))

  // create sram for each mask
  private val innerWidth =
    (maskGran + blockType.width - 1) / blockType.width * blockType.width

  val writeData =
    RW0_wdata.asTypeOf(Vec(widthConcat, UInt(maskGran.W)))
  val readData = Wire(Vec(widthConcat, UInt(maskGran.W)))
  val sramArray = for (i <- 0 until widthConcat) yield {
    val sram = Module(
      new SRAM1RWMemInner(innerWidth, depth, blockType, s"${name}_sram_${i}")
    )
    sram.RW0_clk := RW0_clk
    sram.RW0_addr := RW0_addr
    sram.RW0_en := RW0_en
    sram.RW0_wmode := RW0_wmode && RW0_wmask(i)
    readData(i) := sram.RW0_rdata
    sram.RW0_wdata := writeData(i)
  }
  RW0_rdata := readData.asUInt
}

object SRAM1RWMem extends App {
  new ChiselStage().execute(
    Array("-X", "verilog", "-o", s"data_ext.v"),
    Seq(
      ChiselGeneratorAnnotation(() =>
        new SRAM1RWMem(
          256,
          2048,
          8,
          SRAM1RWBlockType.SRAM1RW_16_128,
          "data_ext"
        )
      ),
      RunFirrtlTransformAnnotation(Dependency(PrefixModulesPass)),
      ModulePrefix("data_ext", "SRAM1RWMem"),
    )
  )
  new ChiselStage().execute(
    Array("-X", "verilog", "-o", s"cc_dir_ext.v"),
    Seq(
      ChiselGeneratorAnnotation(() =>
        new SRAM1RWMem(
          152,
          2048,
          19,
          SRAM1RWBlockType.SRAM1RW_16_128,
          "cc_dir_ext"
        )
      ),
      RunFirrtlTransformAnnotation(Dependency(PrefixModulesPass)),
      ModulePrefix("cc_dir_ext", "SRAM1RWMem"),
    )
  )
}
