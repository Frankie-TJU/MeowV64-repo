package meowv64.debug

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import meowv64.system.SystemDef

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

object DebugModuleState extends ChiselEnum {
  val idle, req, resp = Value
}

class DMControl extends Bundle {
  val haltreq = Bool()
  val resumereq = Bool()
  val hartreset = Bool()
  val ackhavereset = Bool()
  val ackunavail = Bool()
  val hasel = Bool()
  val hartsello = UInt(10.W)
  val hartselhi = UInt(10.W)
  val setkeepalive = Bool()
  val clrkeepalive = Bool()
  val setresethaltreq = Bool()
  val clrresethaltreq = Bool()
  val ndmreset = Bool()
  val dmactive = Bool()
}

class DMStatus extends Bundle {
  val ndmresetpending = Bool()
  val stickyunavail = Bool()
  val impebreak = Bool()
  val zero = UInt(2.W)
  val allhavereset = Bool()
  val anyhavereset = Bool()
  val allresumeack = Bool()
  val anyresumeack = Bool()
  val allnonexistent = Bool()
  val anynonexistent = Bool()
  val allunavail = Bool()
  val anyunavail = Bool()
  val allrunning = Bool()
  val anyrunning = Bool()
  val allhalted = Bool()
  val anyhalted = Bool()
  val authenticated = Bool()
  val authbusy = Bool()
  val hasresethaltreq = Bool()
  val confstrptrvalid = Bool()
  val version = UInt(4.W)
}

class DebugModule(implicit sDef: SystemDef) extends Module {
  val io = IO(new Bundle {
    val dmi = new DebugModuleInterface()
  })

  io.dmi.req.ready := false.B
  io.dmi.resp.valid := false.B

  val state = RegInit(DebugModuleState.idle)
  val curReq = Reg(new DebugModuleReq())
  val curResp = Reg(new DebugModuleResp())
  io.dmi.resp.bits := curResp

  // dmcontrol registers
  val hartreset = RegInit(false.B)
  val hasel = RegInit(false.B)
  val hartsel = RegInit(0.U(log2Ceil(sDef.CORE_COUNT)))
  val ndmreset = RegInit(false.B)
  val dmactive = RegInit(false.B)

  val done = WireInit(false.B)
  switch(state) {
    is(DebugModuleState.idle) {
      io.dmi.req.ready := true.B
      when(io.dmi.req.fire) {
        curReq := io.dmi.req.bits
        state := DebugModuleState.req
      }
    }
    is(DebugModuleState.req) {
      switch(curReq.address) {
        is(0x10.U) {
          // dmcontrol
          when(curReq.isRead) {
            val resp = WireInit(0.U.asTypeOf(new DMControl))
            resp.hartreset := hartreset
            resp.hasel := hasel
            val hartselExtended = Wire(UInt(20.W))
            hartselExtended := hartsel
            resp.hartsello := hartselExtended(9, 0)
            resp.hartselhi := hartselExtended(19, 10)
            resp.ndmreset := ndmreset
            resp.dmactive := dmactive

            curResp.data := resp.asUInt
            curResp.fail := false.B
          }.otherwise {
            val req = Wire(new DMControl)
            req := curReq.data.asTypeOf(req)

            hartreset := req.hartreset
            hasel := req.hasel
            hartsel := Cat(req.hartselhi, req.hartsello)
            ndmreset := req.ndmreset
            dmactive := req.dmactive
          }
          done := true.B
        }
        is(0x11.U) {
          // dmstatus
          when(curReq.isRead) {
            val resp = WireInit(0.U.asTypeOf(new DMStatus))
            resp.version := 3.U // debug 1.0

            curResp.data := resp.asUInt
            curResp.fail := false.B
          }
          done := true.B
        }
      }
      when(done) {
        state := DebugModuleState.resp
      }
    }
    is(DebugModuleState.resp) {
      io.dmi.resp.valid := true.B
      when(io.dmi.resp.fire) {
        state := DebugModuleState.idle
      }
    }
  }
}
