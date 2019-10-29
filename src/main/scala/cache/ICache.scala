package cache

import chisel3._
import _root_.data._

class ICachePort(val ADDR_WIDTH: Int, val DATA_LEN: Int) extends Bundle {
  val addr = Input(UInt(ADDR_WIDTH.W))
  val read = Input(Bool())

  val axi = new AXI(8)

  val stall = Output(Bool())
  val pause = Input(Bool())
  val flush = Input(Bool()) // Branch missperdict, flushing all running requests

  val data = Output(UInt(DATA_LEN.W)) // Data delay is 1 cycle
  val vacant = Output(Bool())
}

// TODO: Change to xpm_tdpmem
class ICache(ADDR_WIDTH: Int, DATA_LEN: Int) extends Module {
  val io = IO(new ICachePort(ADDR_WIDTH, DATA_LEN))

  val inner = Module(new Passthrough(ADDR_WIDTH, DATA_LEN))

  inner.io.addr <> io.addr
  inner.io.read <> io.read
  inner.io.axi <> io.axi
  inner.io.stall <> io.stall
  inner.io.pause <> io.pause
  inner.io.rdata <> io.data
  inner.io.vacant <> io.vacant

  inner.io.write := false.B
  inner.io.wdata := DontCare
  inner.io.be := DontCare

  // FIXME: make passthrough supports flush
  io.flush <> DontCare
}
