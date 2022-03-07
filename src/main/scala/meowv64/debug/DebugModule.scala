package meowv64.debug

import chisel3._
import chisel3.util.Decoupled

class DebugModuleReq extends Bundle {
  val address = UInt(7.W)
  val data = UInt(32.W)
  val isRead = Bool()
}

class DebugModuleResp extends Bundle {
  val data = UInt(32.W)
  val fail = Bool()
}

class DebugModuleInterface extends Bundle {
  val req = Flipped(Decoupled(new DebugModuleReq))
  val resp = Decoupled(new DebugModuleResp)
}

class DebugModule extends Module {
  val io = IO(new Bundle {
    val dmi = new DebugModuleInterface()
  })

  io.dmi.req.ready := true.B
  io.dmi.resp.valid := false.B
  io.dmi.resp.bits.data := 0.U
  io.dmi.resp.bits.fail := false.B
}
