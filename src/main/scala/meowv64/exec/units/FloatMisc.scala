package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.core.FloatH
import meowv64.core.FloatS
import meowv64.exec._
import meowv64.instr.Decoder

class FloatMiscExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.XLEN.W)

  val fflags = UInt(5.W)
}

/** Handles instructions: FSGNJ,
  */
class FloatMisc(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new FloatMiscExt, coredef.REG_FLOAT) {

  def map(
      stage: Int,
      pipe: PipeInstr,
      last_ext: Option[FloatMiscExt]
  ): (FloatMiscExt, Bool) = {
    val ext = Wire(new FloatMiscExt)
    ext.res := 0.U
    ext.fflags := 0.U

    val rs1Value = FloatS.unbox(pipe.rs1val, coredef.XLEN)
    val rs2Value = FloatS.unbox(pipe.rs2val, coredef.XLEN)

    when(
      pipe.instr.instr.funct5 ===
        Decoder.FP_FUNC("FSGNJ")
    ) {
      // sign injection
      when(pipe.instr.instr.funct7(1, 0) === FloatS.fmt) {
        when(pipe.instr.instr.funct3 === 0.U) {
          // FSGNJ.S
          ext.res := Fill(32, 1.U) ## rs2Value(31) ## rs1Value(30, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 1.U) {
          // FSGNJN.S
          ext.res := Fill(32, 1.U) ## (~rs2Value(31)) ## rs1Value(30, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 2.U) {
          // FSGNJX.S
          ext.res := Fill(32, 1.U) ##
            (rs2Value(31) ^ rs1Value(31)) ## rs1Value(30, 0)
        }
      }.elsewhen(pipe.instr.instr.funct7(1, 0) === FloatH.fmt) {
        when(pipe.instr.instr.funct3 === 0.U) {
          // FSGNJ.H
          ext.res := Fill(48, 1.U) ## rs2Value(15) ## rs1Value(14, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 1.U) {
          // FSGNJN.H
          ext.res := Fill(48, 1.U) ## (~rs2Value(15)) ## rs1Value(14, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 2.U) {
          // FSGNJX.H
          ext.res := Fill(48, 1.U) ##
            (rs2Value(15) ^ rs1Value(15)) ## rs1Value(14, 0)
        }
      }.otherwise {
        when(pipe.instr.instr.funct3 === 0.U) {
          // FSGNJ.D
          ext.res := pipe.rs2val(63) ## pipe.rs1val(62, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 1.U) {
          // FSGNJN.D
          ext.res := (~pipe.rs2val(63)) ## pipe.rs1val(62, 0)
        }.elsewhen(pipe.instr.instr.funct3 === 2.U) {
          // FSGNJX.D
          ext.res := (pipe.rs2val(63) ^ pipe.rs1val(63)) ## pipe.rs1val(62, 0)
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: FloatMiscExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))

    // result
    info.wb := ext.res.asUInt

    // fflags
    info.markFSDirty := true.B
    info.updateFFlags := true.B
    info.fflags := ext.fflags

    info
  }

  init()
}
