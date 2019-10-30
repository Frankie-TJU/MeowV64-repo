package cache

import chisel3._
import _root_.data._

class DCachePort(val ADDR_WIDTH: Int, val DATA_LEN: Int) extends Bundle {
  val addr = Input(UInt(ADDR_WIDTH.W))
  val read = Input(Bool())
  val write = Input(Bool())
  val wdata = Input(UInt(DATA_LEN.W))

  val be = Input(UInt((DATA_LEN/8).W))

  val axi = new AXI(DATA_LEN, ADDR_WIDTH)

  val stall = Output(Bool())
  val pause = Input(Bool())

  val rdata = Output(UInt(DATA_LEN.W))
  val vacant = Output(Bool())
}

class DCache(ADDR_WIDTH: Int, DATA_LEN: Int) extends Module {
  val io = IO(new DCachePort(ADDR_WIDTH, DATA_LEN))

  val inner = Module(new Passthrough(ADDR_WIDTH, DATA_LEN))

  inner.io <> io
}
