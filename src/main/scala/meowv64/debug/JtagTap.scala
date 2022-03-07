package meowv64.debug

import chisel3._
import chisel3.util._
import chisel3.experimental._

// Follows the implementation of SpinalHDL JtagTap
// See JtagTap.scala and JtagTapinstructions.scala from SpinalHDL

class Jtag extends Bundle {
  val tck = Input(Bool())
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
  })

  withClock(io.jtag.tck.asClock) {
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
    val idcode = Module(new JtagTapInstructionIdcode(0xffffffffL))
    map(idcode.ctrl, idcodeId)

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

  when(ctrl.enable && ctrl.shift) {
    shifter := Cat(ctrl.tdi, shifter) >> 1
  }

  when(ctrl.capture) {
    shifter := id.U
  }

  ctrl.tdo := shifter(0)
}
