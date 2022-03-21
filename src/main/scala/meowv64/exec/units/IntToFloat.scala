package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.INToRecFN
import meowv64.core.CoreDef
import meowv64.core.FloatS
import meowv64.exec._
import meowv64.instr.Decoder

class IntToFloatExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.XLEN.W)

  val updateFFlags = Bool()
  val fflags = UInt(5.W)
}

/** Handles instructions: FMV.D.X, FMV.W.X, FCVT.S.W, FCVT.S.WU, FCVT.S.L,
  * FCVT.D.W, FCVT.D.WU, FCVT.D.L, FCVT.D.LU, FCVT.S.LU
  */
class IntToFloat(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new IntToFloatExt, coredef.REG_FLOAT) {

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[IntToFloatExt]
  ): (IntToFloatExt, Bool) = {
    val ext = Wire(new IntToFloatExt)
    ext.res := 0.U
    ext.updateFFlags := false.B
    ext.fflags := 0.U

    when(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FMV.D/W.X"
      ) && pipe.instr.instr.funct3 === 0.U
    ) {
      when(pipe.instr.instr.funct7(1, 0) === 0.U) {
        // fmv.w.x
        // nan boxing
        ext.res := FloatS.box(pipe.rs1val(31, 0), coredef.XLEN)
      }.otherwise {
        // fmv.d.x
        ext.res := pipe.rs1val
      }
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "INT2FLOAT"
      )
    ) {
      // convert int to float
      // int32 is a subset of int64
      // so we convert int32 to int64 first
      // and then convert to float32/float64
      for (float <- coredef.FLOAT_TYPES) {
        when(pipe.instr.instr.fmt === float.fmt) {
          val convI2F =
            Module(new INToRecFN(coredef.XLEN, float.exp, float.sig))
          convI2F.suggestName(s"convI2F_${float.name}")
          convI2F.io.signedIn := false.B
          convI2F.io.roundingMode := 0.U
          convI2F.io.detectTininess := false.B

          convI2F.io.in := pipe.rs1val
          when(pipe.instr.instr.rs2 === 0.U) {
            // FCVT.D.W/FCVT.S.W
            // convert 32-bit int to 32/64-bit float
            convI2F.io.signedIn := true.B
            convI2F.io.in := Fill(32, pipe.rs1val(31)) ## pipe.rs1val(31, 0)
          }.elsewhen(pipe.instr.instr.rs2 === 1.U) {
            // FCVT.D.WU/FCVT.S.WU
            // convert 32-bit uint to 32/64-bit float
            convI2F.io.in := pipe.rs1val(31, 0)
          }.elsewhen(pipe.instr.instr.rs2 === 2.U) {
            // FCVT.D.L/FCVT.S.L
            // convert 64-bit int to 32/64-bit float
            convI2F.io.signedIn := true.B
          }.elsewhen(pipe.instr.instr.rs2 === 3.U) {
            // FCVT.D.LU/FCVT.S.LU
            // convert 64-bit uint to 32/64-bit float
          }.otherwise {
            assert(false.B)
          }
          ext.res := float.box(
            float.fromHardfloat(convI2F.io.out),
            coredef.XLEN
          )
          ext.fflags := convI2F.io.exceptionFlags
          ext.updateFFlags := true.B
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: IntToFloatExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))

    // result
    info.wb := ext.res.asUInt

    // fflags
    info.updateFFlags := ext.updateFFlags
    info.fflags := ext.fflags

    info
  }

  init()
}
