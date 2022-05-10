package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.INToRecFN
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

class IntToFloatMultiCycleExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.XLEN.W)
  val out = MixedVec(
    coredef.FLOAT_TYPES.map(f => UInt(f.widthHardfloat().W))
  )
  val exceptionFlags = MixedVec(
    coredef.FLOAT_TYPES.map(_ => UInt(5.W))
  )

  val updateFFlags = Bool()
  val fflags = UInt(5.W)
}

/** Handles instructions: FCVT.S.W, FCVT.S.WU, FCVT.S.L, FCVT.D.W, FCVT.D.WU,
  * FCVT.D.L, FCVT.D.LU, FCVT.S.LU
  */
class IntToFloatMultiCycle(override implicit val coredef: CoreDef)
    extends ExecUnit(1, new IntToFloatMultiCycleExt, coredef.REG_FLOAT) {

  def map(
      stage: Int,
      pipe: PipeInstr,
      last_ext: Option[IntToFloatMultiCycleExt]
  ): (IntToFloatMultiCycleExt, Bool) = {
    val ext = Wire(new IntToFloatMultiCycleExt)
    ext := DontCare

    when(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "INT2FLOAT"
      )
    ) {
      // convert int to float
      // int32 is a subset of int64
      // so we convert int32 to int64 first
      // and then convert to float32/float64
      for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
        when(pipe.instr.instr.fmt === float.fmt) {
          if (stage == 0) {
            // stage 0: convert integer to hardfloat
            val convI2F =
              Module(new INToRecFN(coredef.XLEN, float.exp, float.sig))
            convI2F.suggestName(s"convI2F_${float.name}")
            convI2F.io.signedIn := false.B
            convI2F.io.roundingMode := 0.U
            convI2F.io.detectTininess := hardfloat.consts.tininess_afterRounding

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
            ext.out(idx) := convI2F.io.out
            ext.exceptionFlags(idx) := convI2F.io.exceptionFlags
          } else {
            // convert hardfloat to ieee & box
            ext.res := float.box(
              float.fromHardfloat(last_ext.get.out(idx)),
              coredef.XLEN
            )
            ext.fflags := last_ext.get.exceptionFlags(idx)
            ext.updateFFlags := true.B
          }
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: IntToFloatMultiCycleExt): RetireInfo = {
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
