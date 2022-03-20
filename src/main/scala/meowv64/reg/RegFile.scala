package meowv64.reg

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.BitPat
import chisel3.util.log2Ceil

object RegType extends ChiselEnum {
  val integer, float, vector = Value

  implicit def bitpat(op: RegType.Type): BitPat =
    BitPat(op.litValue.U(getWidth.W))
}

class RegReader(val WIDTH: Int, val DEPTH: Int) extends Bundle {
  val addr = Output(UInt(log2Ceil(DEPTH).W))
  val data = Input(UInt(WIDTH.W))
}

class RegWriter(val WIDTH: Int, val DEPTH: Int) extends Bundle {
  // this is required since float register 0 is not wired to zero
  val valid = Output(Bool())
  val addr = Output(UInt(log2Ceil(DEPTH).W))
  val data = Output(UInt(WIDTH.W))
}

// Standard Registers
// TODO: support multi port
class RegFile(
    WIDTH: Int,
    DEPTH: Int,
    READ_COUNT: Int,
    WRITE_COUNT: Int,
    FIXED_ZERO: Boolean = true
) extends Module {
  val io = IO(new Bundle {
    val reads = Vec(READ_COUNT, Flipped(new RegReader(WIDTH, DEPTH)))
    val writes = Flipped(Vec(WRITE_COUNT, new RegWriter(WIDTH, DEPTH)))
  })

  val regs = SyncReadMem(DEPTH, UInt(WIDTH.W))
  val lastWrites = RegNext(io.writes)

  // read has one cycle delay
  for (read <- io.reads) {
    val addr = RegNext(read.addr)
    val readData = regs.read(read.addr)
    when(addr === 0.U && FIXED_ZERO.B) {
      // x0 is hard wired to zero
      read.data := 0.U
    }.otherwise {
      // bypass read & write in one cycle
      read.data := readData
      for (writes <- lastWrites) {
        when(writes.valid && writes.addr === addr) {
          read.data := writes.data
        }
      }
    }
  }

  for (write <- io.writes) {
    when(write.valid) {
      regs.write(write.addr, write.data)
    }
  }

  /*
  printf("RegFile status: \n")
  printf("================\n")
  for(i <- (0 until COUNT)) {
    val prefix = ("x" + i).reverse.padTo(3, ' ').reverse
    printf(p" | $prefix: 0x${Hexadecimal(regs(i.U))}")
    if(i % 4 == 3) printf(" |\n")
  }
   */
}
