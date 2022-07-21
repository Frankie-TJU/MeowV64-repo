package meowv64.sram

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.Dependency

// `depth` elements
// each element has `width` bits
sealed abstract class SRAM1RWMaskedBlockType(val width: Int, val depth: Int) {
  def addressWidth = log2Up(depth)
  def moduleName = s"sram_1rw_${depth}X${width}_mask"
  def moduleFile = moduleName + ".v"
}

object SRAM1RWMaskedBlockType {
  case object SRAM1RWMasked_64_128 extends SRAM1RWMaskedBlockType(64, 128)
}

class SRAM1RWMaskedBlackBox(blockType: SRAM1RWMaskedBlockType)
    extends BlackBox
    with HasBlackBoxResource {

  val io = IO(new Bundle {
    val CLK = Input(Clock())
    // Read+Write Port
    val CEN = Input(Bool())
    val GWEN = Input(Bool())
    val A = Input(UInt(blockType.addressWidth.W))
    val D = Input(UInt(blockType.width.W))
    val WEN = Input(UInt(blockType.width.W))
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
class SRAM1RWMaskedMemInner(
    width: Int,
    depth: Int,
    blockType: SRAM1RWMaskedBlockType,
    name: String
) extends Module {
  require(
    width % blockType.width == 0 && depth % blockType.depth == 0,
    s"required size ${width} * ${depth} not using whole SRAM1RWMasked ${blockType.width} * ${blockType.depth}"
  )

  private val widthConcat = width / blockType.width
  private val depthConcat = depth / blockType.depth

  // read + write
  val RW0_clk = IO(Input(Clock()))
  val RW0_addr = IO(Input(UInt(log2Ceil(depth).W)))
  val RW0_en = IO(Input(Bool()))
  val RW0_wmode = IO(Input(Bool()))
  val RW0_wmask = IO(Input(UInt(width.W)))
  val RW0_wdata = IO(Input(UInt(width.W)))
  val RW0_rdata = IO(Output(UInt(width.W)))

  val writeData =
    RW0_wdata.asTypeOf(Vec(widthConcat, UInt(blockType.width.W)))
  val writeMask =
    RW0_wmask.asTypeOf(Vec(widthConcat, UInt(blockType.width.W)))
  val blockAddr = RW0_addr >> blockType.addressWidth
  // initialize SRAM1RWMasked blackbox, connect ports
  val sramArray = for (i <- 0 until widthConcat) yield {
    for (j <- 0 until depthConcat) yield {
      val sram = Module(new SRAM1RWMaskedBlackBox(blockType))
      sram.suggestName(s"${name}_sram_${i}_${j}")
      val selected =
        blockAddr === j.asUInt
      // assume R0_clk is the same as W0_clk
      sram.io.CLK := RW0_clk
      // Read+Write Port (A)
      sram.io.CEN := ~(RW0_en && selected)
      sram.io.A := RW0_addr // truncate here
      sram.io.GWEN := ~RW0_wmode
      sram.io.WEN := ~writeMask(i)
      sram.io.D := writeData(i)
      // other signals
      sram.io.RET1N := true.B // disable retain mode
      // TODO: investigate about EMA option of SRAM1RWMasked block
      sram.io.EMA := 7.U
      sram.io.EMAW := 3.U

      sram
    }
  }

  // select read data
  withClock(RW0_clk) {
    val lastReadBlockAddr = RegNext(blockAddr)
    val readData = Wire(Vec(widthConcat, UInt(blockType.width.W)))
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
}

// use several sram blocks to create a `width` x `depth` sram
// 1 read & 1 write
// handle masking
class SRAM1RWMaskedMem(
    width: Int,
    depth: Int,
    maskGran: Int,
    blockType: SRAM1RWMaskedBlockType,
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

  // create a larger sram
  private val innerWidth =
    (width + blockType.width - 1) / blockType.width * blockType.width

  val sram = Module(
    new SRAM1RWMaskedMemInner(
      innerWidth,
      depth,
      blockType,
      s"${name}_sram"
    )
  )
  sram.RW0_clk := RW0_clk
  sram.RW0_addr := RW0_addr
  sram.RW0_en := RW0_en
  sram.RW0_wmode := RW0_wmode
  val masks = Wire(Vec(width, Bool()))
  for (i <- 0 until width) {
    masks(i) := RW0_wmask(i / maskGran)
  }
  sram.RW0_wmask := masks.asUInt
  RW0_rdata := sram.RW0_rdata
  sram.RW0_wdata := RW0_rdata
}

object SRAM1RWMaskedMem extends App {
  new ChiselStage().execute(
    Array("-X", "verilog", "-o", s"data_ext.v"),
    Seq(
      ChiselGeneratorAnnotation(() =>
        new SRAM1RWMaskedMem(
          256,
          2048,
          8,
          SRAM1RWMaskedBlockType.SRAM1RWMasked_64_128,
          "data_ext"
        )
      ),
      RunFirrtlTransformAnnotation(Dependency(PrefixModulesPass)),
      ModulePrefix("data_ext", "SRAM1RWMaskedMem")
    )
  )
  new ChiselStage().execute(
    Array("-X", "verilog", "-o", s"cc_dir_ext.v"),
    Seq(
      ChiselGeneratorAnnotation(() =>
        new SRAM1RWMaskedMem(
          152,
          2048,
          19,
          SRAM1RWMaskedBlockType.SRAM1RWMasked_64_128,
          "cc_dir_ext"
        )
      ),
      RunFirrtlTransformAnnotation(Dependency(PrefixModulesPass)),
      ModulePrefix("cc_dir_ext", "SRAM1RWMaskedMem")
    )
  )
}
