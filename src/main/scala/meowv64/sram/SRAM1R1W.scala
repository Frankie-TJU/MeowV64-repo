package meowv64.sram

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.Dependency

// `depth` elements
// each element has `width` bits
sealed abstract class SRAM1R1WBlockType(val width: Int, val depth: Int) {
  def addressWidth = log2Up(depth)
  def moduleName = s"sram_1r1w_${depth}X${width}"
  def moduleFile = moduleName + ".v"
}

object SRAM1R1WBlockType {
  case object SRAM1R1W_128_32 extends SRAM1R1WBlockType(128, 32)
  case object SRAM1R1W_128_64 extends SRAM1R1WBlockType(128, 64)
}

class SRAM1R1WBlackBox(blockType: SRAM1R1WBlockType)
    extends BlackBox
    with HasBlackBoxResource {

  val io = IO(new Bundle {
    val CLK = Input(Clock())
    // Read Port (A)
    val CENA = Input(Bool())
    val AA = Input(UInt(blockType.addressWidth.W))
    val QA = Output(UInt(blockType.width.W))
    // Write Port (B)
    val CENB = Input(Bool())
    val AB = Input(UInt(blockType.addressWidth.W))
    val DB = Input(UInt(blockType.width.W))
    // other signals
    val STOV = Input(UInt(1.W))
    val STOVAB = Input(UInt(1.W))
    val EMA = Input(UInt(3.W))
    val EMAW = Input(UInt(2.W))
    val EMAS = Input(UInt(1.W))
    val EMAP = Input(UInt(1.W))
    val RET1N = Input(Bool()) // retention input
  })

  override def desiredName: String = blockType.moduleName

  // addResource(s"/sram/${blockType.moduleName}.v")
}

// use several sram blocks to create a `width` x `depth` sram
// 1 read & 1 write
class SRAM1R1WMemInner(
    width: Int,
    depth: Int,
    blockType: SRAM1R1WBlockType,
    name: String
) extends Module {
  require(
    width % blockType.width == 0 && depth % blockType.depth == 0,
    s"required size ${width} * ${depth} not using whole SRAM1R1W ${blockType.width} * ${blockType.depth}"
  )

  private val widthConcat = width / blockType.width
  private val depthConcat = depth / blockType.depth

  // read
  val R0_clk = IO(Input(Clock()))
  val R0_addr = IO(Input(UInt(log2Ceil(depth).W)))
  val R0_en = IO(Input(Bool()))
  val R0_data = IO(Output(UInt(width.W)))

  // write
  val W0_clk = IO(Input(Clock()))
  val W0_addr = IO(Input(UInt(log2Ceil(depth).W)))
  val W0_en = IO(Input(Bool()))
  val W0_data = IO(Input(UInt(width.W)))

  val writeData =
    W0_data.asTypeOf(Vec(widthConcat, UInt(blockType.width.W)))
  val readBlockAddr = R0_addr >> blockType.addressWidth
  val writeBlockAddr = W0_addr >> blockType.addressWidth
  // initialize SRAM1R1W blackbox, connect ports
  val sramArray = for (i <- 0 until widthConcat) yield {
    for (j <- 0 until depthConcat) yield {
      val sram = Module(new SRAM1R1WBlackBox(blockType))
      sram.suggestName(s"${name}_sram_${i}_${j}")
      val readSelected =
        readBlockAddr === j.asUInt
      val writeSelected =
        writeBlockAddr === j.asUInt
      // assume R0_clk is the same as W0_clk
      sram.io.CLK := R0_clk
      // Read Port (A)
      sram.io.CENA := ~(R0_en && readSelected)
      sram.io.AA := R0_addr // truncate here
      // Write Port (B)
      sram.io.CENB := ~(W0_en && writeSelected)
      sram.io.AB := W0_addr // truncate here
      sram.io.DB := writeData(i)
      // other signals
      sram.io.RET1N := true.B // disable retain mode
      // TODO: investigate about EMA option of SRAM1R1W block
      sram.io.EMA := 7.U
      sram.io.EMAW := 3.U
      sram.io.EMAS := 1.U
      sram.io.EMAP := 2.U
      sram.io.STOV := WireDefault(0.U)
      sram.io.STOVAB := WireDefault(0.U)

      sram
    }
  }

  // select read data
  val lastReadBlockAddr = RegNext(readBlockAddr)
  private val readData = Wire(Vec(widthConcat, UInt(blockType.width.W)))
  for ((data, i) <- readData zipWithIndex) {
    val dataToSelect = for ((block, j) <- sramArray(i) zipWithIndex) yield {
      (
        lastReadBlockAddr === j.asUInt,
        block.io.QA
      )
    }
    data := Mux1H(dataToSelect)
  }

  R0_data := readData.asUInt
}

// use several sram blocks to create a `width` x `depth` sram
// 1 read & 1 write
// handle masking
class SRAM1R1WMem(
    width: Int,
    depth: Int,
    maskGran: Int,
    blockType: SRAM1R1WBlockType,
    name: String
) extends Module {
  private val widthConcat = width / maskGran

  // read
  val R0_clk = IO(Input(Clock()))
  val R0_addr = IO(Input(UInt(log2Ceil(depth).W)))
  val R0_en = IO(Input(Bool()))
  val R0_data = IO(Output(UInt(width.W)))

  // write
  val W0_clk = IO(Input(Clock()))
  val W0_addr = IO(Input(UInt(log2Ceil(depth).W)))
  val W0_en = IO(Input(Bool()))
  val W0_data = IO(Input(UInt(width.W)))
  val W0_mask = IO(Input(UInt(widthConcat.W)))

  // create sram for each mask
  private val innerWidth =
    (maskGran + blockType.width - 1) / blockType.width * blockType.width

  val writeData =
    W0_data.asTypeOf(Vec(widthConcat, UInt(maskGran.W)))
  val readData = Wire(Vec(widthConcat, UInt(maskGran.W)))
  val sramArray = for (i <- 0 until widthConcat) yield {
    val sram = Module(
      new SRAM1R1WMemInner(innerWidth, depth, blockType, s"${name}_sram_${i}")
    )
    sram.R0_clk := R0_clk
    sram.R0_addr := R0_addr
    sram.R0_en := R0_en
    readData(i) := sram.R0_data

    sram.W0_clk := W0_clk
    sram.W0_addr := W0_addr
    sram.W0_en := W0_en && W0_mask(i)
    sram.W0_data := writeData(i)
  }
  R0_data := readData.asUInt
}

object SRAM1R1WMem extends App {
  new ChiselStage().execute(
    Array("-X", "verilog", "-o", s"btbEntries_ext.v"),
    Seq(
      ChiselGeneratorAnnotation(() =>
        new SRAM1R1WMem(
          420,
          32,
          105,
          SRAM1R1WBlockType.SRAM1R1W_128_32,
          "btbEntries_ext"
        )
      ),
      RunFirrtlTransformAnnotation(Dependency(PrefixModulesPass)),
      ModulePrefix("btbEntries_ext", "SRAM1R1WMem")
    )
  )
  new ChiselStage().execute(
    Array("-X", "verilog", "-o", s"icDataArray_ext.v"),
    Seq(
      ChiselGeneratorAnnotation(() =>
        new SRAM1R1WMem(
          512,
          32,
          256,
          SRAM1R1WBlockType.SRAM1R1W_128_32,
          "icDataArray_ext"
        )
      ),
      RunFirrtlTransformAnnotation(Dependency(PrefixModulesPass)),
      ModulePrefix("icDataArray_ext", "SRAM1R1WMem")
    )
  )
}
