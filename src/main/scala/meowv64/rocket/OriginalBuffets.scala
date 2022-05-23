package meowv64.rocket

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid

case class OriginalBuffetsConfig(
    idxWidth: Int = 8,
    dataWidth: Int = 32,
    separateWritePorts: Boolean = true,
    supportsUpdate: Boolean = true,
    readReqFifoDepth: Int = 8,
    updateFifoDepth: Int = 8,
    readRespFifoDepth: Int = 8,
    pushFifoDepth: Int = 8,
    scoreboardSize: Int = 8
) {
  def size = (1 << idxWidth) - 1
}

class OriginalBuffetsRead(config: OriginalBuffetsConfig) extends Bundle {
  val idx = UInt(config.idxWidth.W)
  val will_update = Bool()
  val is_shrink = Bool()
}

// https://github.com/cwfletcher/buffets/blob/master/dut/buffet.v
class OriginalBuffets(config: OriginalBuffetsConfig) extends Module {
  val credit = IO(Decoupled(UInt(config.idxWidth.W)))
  // Fill
  val push_data = IO(Flipped(Decoupled(UInt(config.dataWidth.W))))
  // Read & Shrink
  val read_idx = IO(Flipped(new OriginalBuffetsRead(config)))
  val read_data = IO(Decoupled(UInt(config.dataWidth.W)))
  // Update
  val update_idx = IO(Flipped(Valid(UInt(config.idxWidth.W))))
  val update_data = IO(Flipped(Valid(UInt(config.idxWidth.W))))
  val update_ready = IO(Output(Bool()))
  val update_receive_ack = IO(Output(Bool()))
}
