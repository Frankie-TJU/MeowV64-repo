package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.core.FloatH
import meowv64.core.FloatS
import meowv64.exec._
import meowv64.instr.Decoder

class FloatToIntExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.XLEN.W)

  val fflags = UInt(5.W)
}

/** Handles instructions: FCLASS.S, FCLASS.D, FMX.X.D
  */
class FloatToInt(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new FloatToIntExt, coredef.REG_FLOAT) {

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[FloatToIntExt]
  ): (FloatToIntExt, Bool) = {
    val ext = Wire(new FloatToIntExt)
    ext.res := 0.U
    ext.fflags := 0.U

    val rs1Values =
      coredef.FLOAT_TYPES.map(f => f.unbox(pipe.rs1val, coredef.XLEN))

    when(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FMV.X.H/W/D"
      ) && pipe.instr.instr.funct3 === 0.U
    ) {
      when(pipe.instr.instr.funct7(1, 0) === FloatS.fmt) {
        // fmv.x.w
        // sign extension
        // do not consider nan boxing here
        ext.res := Fill(32, pipe.rs1val(31)) ## pipe.rs1val(31, 0)
      }.elsewhen(pipe.instr.instr.funct7(1, 0) === FloatH.fmt) {
        // fmv.x.h
        // sign extension
        // do not consider nan boxing here
        ext.res := Fill(48, pipe.rs1val(15)) ## pipe.rs1val(15, 0)
      }.otherwise {
        // fmv.x.d
        ext.res := pipe.rs1val
      }
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FCLASS"
      ) && pipe.instr.instr.funct3 === 1.U
    ) {
      for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
        when(pipe.instr.instr.fmt === float.fmt) {
          val sign = rs1Values(idx)(float.exp + float.sig - 1)
          val exp = rs1Values(idx)(float.exp + float.sig - 2, float.sig - 1)
          val sig = rs1Values(idx)(float.sig - 2, 0)

          val expZero = exp === 0.U
          val expMax = exp.andR
          val isZero = expZero && sig === 0.U
          val isSubnormal = expZero && sig =/= 0.U
          val isNormal = exp.orR && ~exp.andR
          val isInf = expMax && sig === 0.U
          val isNaN = expMax && sig.orR
          val isSNaN = isNaN && sig(float.sig - 2) === false.B
          val isQNaN = isNaN && sig(float.sig - 2) === true.B

          ext.res := Cat(
            isQNaN,
            isSNaN,
            ~sign && isInf,
            ~sign && isNormal,
            ~sign && isSubnormal,
            ~sign && isZero,
            sign && isZero,
            sign && isSubnormal,
            sign && isNormal,
            sign && isInf
          )
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: FloatToIntExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))

    // result
    info.wb := ext.res.asUInt

    // fflags
    info.updateFFlags := true.B
    info.fflags := ext.fflags

    info
  }

  init()
}
