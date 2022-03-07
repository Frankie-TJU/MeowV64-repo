package meowv64.debug

import chisel3._
import chisel3.experimental._
import chisel3.util._
import freechips.rocketchip.util.AsyncQueue

// Follows the implementation of SpinalHDL JtagTap
// See JtagTap.scala and JtagTapinstructions.scala from SpinalHDL

class Jtag extends Bundle {
  val tck = Input(Bool())
  val trstn = Input(Bool())
  val tms = Input(Bool())
  val tdi = Input(Bool())
  val tdo = Output(Bool())
}

object JtagState extends ChiselEnum {
  val reset, idle = Value
  val ir_select, ir_capture, ir_shift, ir_exit1, ir_pause, ir_exit2, ir_update =
    Value
  val dr_select, dr_capture, dr_shift, dr_exit1, dr_pause, dr_exit2, dr_update =
    Value
}

class JtagTap(val instWidth: Int) extends Module {
  val io = IO(new Bundle {
    val jtag = new Jtag
    val dmi = Flipped(new DebugModuleInterface)
  })

  val jtagClock = io.jtag.tck.asClock
  val jtagReset = ~io.jtag.trstn

  // req queue
  val reqQueue = Module(new AsyncQueue(new DebugModuleReq))
  reqQueue.io.enq_clock := jtagClock
  reqQueue.io.enq_reset := jtagReset
  reqQueue.io.deq_clock := clock
  reqQueue.io.deq_reset := reset
  reqQueue.io.deq <> io.dmi.req

  // resp queue
  val respQueue = Module(new AsyncQueue(new DebugModuleResp))
  respQueue.io.enq_clock := clock
  respQueue.io.enq_reset := reset
  respQueue.io.deq_clock := jtagClock
  respQueue.io.deq_reset := jtagReset
  respQueue.io.enq <> io.dmi.resp

  withClockAndReset(jtagClock, jtagReset) {
    val state = RegInit(JtagState.reset)
    val nextState = WireInit(state)

    switch(state) {
      is(JtagState.reset) {
        nextState := Mux(io.jtag.tms, JtagState.reset, JtagState.idle)
      }
      is(JtagState.idle) {
        nextState := Mux(io.jtag.tms, JtagState.dr_select, JtagState.idle)
      }
      is(JtagState.ir_select) {
        nextState := Mux(io.jtag.tms, JtagState.reset, JtagState.ir_capture)
      }
      is(JtagState.ir_capture) {
        nextState := Mux(io.jtag.tms, JtagState.ir_exit1, JtagState.ir_shift)
      }
      is(JtagState.ir_shift) {
        nextState := Mux(io.jtag.tms, JtagState.ir_exit1, JtagState.ir_shift)
      }
      is(JtagState.ir_exit1) {
        nextState := Mux(io.jtag.tms, JtagState.ir_update, JtagState.ir_pause)
      }
      is(JtagState.ir_pause) {
        nextState := Mux(io.jtag.tms, JtagState.ir_exit2, JtagState.ir_pause)
      }
      is(JtagState.ir_exit2) {
        nextState := Mux(io.jtag.tms, JtagState.ir_update, JtagState.ir_shift)
      }
      is(JtagState.ir_update) {
        nextState := Mux(io.jtag.tms, JtagState.dr_select, JtagState.idle)
      }
      is(JtagState.dr_select) {
        nextState := Mux(io.jtag.tms, JtagState.ir_select, JtagState.dr_capture)
      }
      is(JtagState.dr_capture) {
        nextState := Mux(io.jtag.tms, JtagState.dr_exit1, JtagState.dr_shift)
      }
      is(JtagState.dr_shift) {
        nextState := Mux(io.jtag.tms, JtagState.dr_exit1, JtagState.dr_shift)
      }
      is(JtagState.dr_exit1) {
        nextState := Mux(io.jtag.tms, JtagState.dr_update, JtagState.dr_pause)
      }
      is(JtagState.dr_pause) {
        nextState := Mux(io.jtag.tms, JtagState.dr_exit2, JtagState.dr_pause)
      }
      is(JtagState.dr_exit2) {
        nextState := Mux(io.jtag.tms, JtagState.dr_update, JtagState.dr_shift)
      }
      is(JtagState.dr_update) {
        nextState := Mux(io.jtag.tms, JtagState.dr_select, JtagState.idle)
      }
    }
    state := nextState

    val inst = RegInit(0.U(instWidth.W))
    val instShift = RegInit(0.U(instWidth.W))
    val bypass = RegNext(io.jtag.tdi)
    val tdoUnbuffered = WireInit(bypass)
    val tdoDr = WireInit(false.B)
    val tdoIr = instShift(0)
    // bypass: all zeros, all ones
    val isBypass = inst.andR || (~inst.orR)

    withClock((~io.jtag.tck).asClock) {
      io.jtag.tdo := RegNext(tdoUnbuffered)
    }

    switch(state) {
      is(JtagState.ir_capture) {
        // 01
        instShift := 1.U
      }
      is(JtagState.ir_shift) {
        instShift := Cat(io.jtag.tdi, instShift) >> 1
        tdoUnbuffered := tdoIr
      }
      is(JtagState.ir_update) {
        inst := instShift
      }
      is(JtagState.dr_shift) {
        instShift := Cat(io.jtag.tdi, instShift) >> 1
        when(isBypass) {
          tdoUnbuffered := bypass
        }.otherwise {
          tdoUnbuffered := tdoDr
        }
      }
    }

    def map(ctrl: JtagTapInstructionCtrl, instId: Int) = {
      ctrl.tdi := io.jtag.tdi
      ctrl.enable := inst === instId.U
      ctrl.capture := state === JtagState.dr_capture
      ctrl.shift := state === JtagState.dr_shift
      ctrl.update := state === JtagState.dr_update
      ctrl.reset := state === JtagState.reset
      when(ctrl.enable) {
        tdoDr := ctrl.tdo
      }
    }

    // idcode
    val idcodeId = 1
    val idcode = Module(new JtagTapInstructionIdcode(0x12222001L))
    map(idcode.ctrl, idcodeId)

    // dtmcs & dmi
    val dtm = Module(new JtagDTM())
    dtm.toDM.req <> reqQueue.io.enq
    dtm.toDM.resp <> respQueue.io.deq
    map(dtm.ctrlDTMCS, 0x10)
    map(dtm.ctrlDMI, 0x11)

    // default inst: idcode
    when(state === JtagState.reset) {
      inst := idcodeId.U
    }
  }

}

class JtagTapInstructionCtrl extends Bundle {
  val tdi = Output(Bool())
  val enable = Output(Bool())
  val capture = Output(Bool())
  val shift = Output(Bool())
  val update = Output(Bool())
  val reset = Output(Bool())
  val tdo = Input(Bool())
}

class JtagTapInstructionIdcode(val id: BigInt) extends Module {
  val ctrl = IO(Flipped(new JtagTapInstructionCtrl()))
  val shifter = RegInit(0.U(32.W))

  when(ctrl.enable) {
    when(ctrl.shift) {
      shifter := Cat(ctrl.tdi, shifter) >> 1
    }
    when(ctrl.capture) {
      // read
      shifter := id.U
    }
  }

  ctrl.tdo := shifter(0)
}

class DTMCS extends Bundle {
  val dmihardreset = Bool()
  val dmireset = Bool()
  val zero = Bool()
  val idle = UInt(3.W)
  val dmistat = UInt(2.W)
  val abits = UInt(6.W)
  val version = UInt(4.W)
}

class DMI extends Bundle {
  val address = UInt(7.W)
  val data = UInt(32.W)
  val op = UInt(2.W)
}

object JtagDTMState extends ChiselEnum {
  val idle, req, resp = Value
}

/** Device Transport Module
  */
class JtagDTM extends Module {
  val ctrlDTMCS = IO(Flipped(new JtagTapInstructionCtrl()))
  val ctrlDMI = IO(Flipped(new JtagTapInstructionCtrl()))
  val toDM = IO(Flipped(new DebugModuleInterface))

  val state = RegInit(JtagDTMState.idle)
  val lastRes = RegInit(0.U(2.W)) // sticky op in dmi
  val lastRespData = RegInit(0.U(32.W))

  // DTMCS logic
  val idle = WireInit(0.U(3.W)) // this is a hint, not idle state
  val shifterDTMCS = RegInit(0.U(32.W))
  when(ctrlDTMCS.enable) {
    when(ctrlDTMCS.shift) {
      shifterDTMCS := Cat(ctrlDTMCS.tdi, shifterDTMCS) >> 1
    }
    when(ctrlDTMCS.update) {
      // write
      val dtmcs = Wire(new DTMCS())
      dtmcs := shifterDTMCS.asTypeOf(dtmcs)
      when(dtmcs.dmireset) {
        // clear lastRes
        lastRes := 0.U
      }
    }
    when(ctrlDTMCS.capture) {
      // read
      val dtmcs = Wire(new DTMCS())
      dtmcs.dmihardreset := false.B
      dtmcs.dmireset := false.B
      dtmcs.zero := false.B
      dtmcs.idle := idle
      dtmcs.dmistat := lastRes
      dtmcs.abits := 7.U
      dtmcs.version := 1.U // debug spec 1.0

      shifterDTMCS := dtmcs.asUInt
    }
  }

  ctrlDTMCS.tdo := shifterDTMCS(0)

  // DMI logic
  val currentDMReq = Reg(new DebugModuleReq)
  toDM.req.valid := false.B
  toDM.req.bits := currentDMReq

  val currentDMResp = Reg(new DebugModuleResp)
  toDM.resp.ready := false.B
  when(toDM.resp.fire) {
    currentDMResp := toDM.resp.bits
  }

  val shifterDMI = RegInit(0.U(new DMI().getWidth.W))
  when(ctrlDMI.enable) {
    when(ctrlDMI.shift) {
      shifterDMI := Cat(ctrlDMI.tdi, shifterDMI) >> 1
    }
    when(ctrlDMI.update) {
      // write
      val dmi = Wire(new DMI())
      dmi := shifterDMI.asTypeOf(dmi)
      when(lastRes === 0.U) {
        // good to go
        when(state === JtagDTMState.idle) {
          // read/write
          when(dmi.op === 1.U || dmi.op === 2.U) {
            // fire
            currentDMReq.address := dmi.address
            currentDMReq.data := dmi.data
            currentDMReq.isRead := dmi.op === 1.U
            state := JtagDTMState.req
          }
        }
      }
    }
    when(ctrlDMI.capture) {
      // read
      val dmi = Wire(new DMI())
      dmi.address := 0.U
      dmi.data := lastRespData
      when(state =/= JtagDTMState.idle || lastRes === 3.U) {
        // busy
        dmi.op := 3.U
        lastRes := 3.U
      }.otherwise {
        // fail or success
        dmi.op := lastRes
      }

      shifterDMI := dmi.asUInt
    }
  }

  ctrlDMI.tdo := shifterDMI(0)

  switch(state) {
    is(JtagDTMState.req) {
      toDM.req.valid := true.B
      when(toDM.req.fire) {
        state := JtagDTMState.resp
      }
    }
    is(JtagDTMState.resp) {
      toDM.resp.ready := true.B
      when(toDM.resp.fire) {
        when(lastRes === 0.U) {
          // do not change if lastRes is set
          lastRes := Mux(toDM.resp.bits.fail, 2.U, 0.U)
        }
        lastRespData := toDM.resp.bits.data
        state := JtagDTMState.idle
      }
    }
  }
}
