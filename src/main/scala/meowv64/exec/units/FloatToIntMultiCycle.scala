package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.CompareRecFN
import hardfloat.RecFNToIN
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

class FloatToIntMultiCycleExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.XLEN.W)
  val rs1HFValues = MixedVec(
    coredef.FLOAT_TYPES.map(f => UInt(f.widthHardfloat().W))
  )
  val rs2HFValues = MixedVec(
    coredef.FLOAT_TYPES.map(f => UInt(f.widthHardfloat().W))
  )

  val updateFFlags = Bool()
  val fflags = UInt(5.W)
}

/** Handles instructions: FCVT.L.S, FCVT.LU.S, FEQ.D, FLT.D, FLE.D, FCVT.W.D,
  * FCVT.WU.D, FCVT.L.D, FCVT.LU.D
  */
class FloatToIntMultiCycle(override implicit val coredef: CoreDef)
    extends ExecUnit(1, new FloatToIntMultiCycleExt, coredef.REG_FLOAT)
    with WithFRM {

  val frm = IO(Input(UInt(3.W)))

  def map(
      stage: Int,
      pipe: PipeInstr,
      last_ext: Option[FloatToIntMultiCycleExt]
  ): (FloatToIntMultiCycleExt, Bool) = {
    val ext = Wire(new FloatToIntMultiCycleExt)
    ext.res := 0.U
    ext.updateFFlags := false.B
    ext.fflags := 0.U

    val rs1Values =
      coredef.FLOAT_TYPES.map(f => f.unbox(pipe.rs1val, coredef.XLEN))
    val rs2Values =
      coredef.FLOAT_TYPES.map(f => f.unbox(pipe.rs2val, coredef.XLEN))
    val rs1HFValues = coredef.FLOAT_TYPES
      .zip(rs1Values)
      .map({ case (f, v) => f.toHardfloat(v) })
    val rs2HFValues = coredef.FLOAT_TYPES
      .zip(rs2Values)
      .map({ case (f, v) => f.toHardfloat(v) })

    // stage 0: convert to hf
    ext.rs1HFValues := rs1HFValues
    ext.rs2HFValues := rs2HFValues

    // Floating-point operations use either a static rounding mode encoded in the instruction,
    // or a dynamic rounding mode held in frm.
    val roundingMode = Wire(UInt(3.W))
    when(pipe.instr.instr.funct3 === 7.U) {
      // dynamic
      roundingMode := frm
    }.otherwise {
      // static
      roundingMode := pipe.instr.instr.funct3
    }

    if (stage == 1) {
      when(
        pipe.instr.instr.funct5 === Decoder.FP_FUNC(
          "FCMP"
        )
      ) {
        ext.fflags := DontCare

        // loop over floating point types
        for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
          when(pipe.instr.instr.fmt === float.fmt) {
            val cmp = Module(new CompareRecFN(float.exp, float.sig))
            cmp.suggestName(s"cmp_${float.name}")
            cmp.io.a := last_ext.get.rs1HFValues(idx)
            cmp.io.b := last_ext.get.rs2HFValues(idx)
            cmp.io.signaling := true.B

            // FEQ, FLT, FLE
            when(pipe.instr.instr.funct3 === 2.U) {
              // FEQ
              ext.res := cmp.io.eq
              // do not signal qNaN in feq
              cmp.io.signaling := false.B
            }.elsewhen(pipe.instr.instr.funct3 === 1.U) {
              // FLT
              ext.res := cmp.io.lt
            }.elsewhen(pipe.instr.instr.funct3 === 0.U) {
              // FLE
              ext.res := cmp.io.lt || cmp.io.eq
            }
            ext.fflags := cmp.io.exceptionFlags
          }
        }

        ext.updateFFlags := true.B
      }.elsewhen(
        pipe.instr.instr.funct5 === Decoder.FP_FUNC(
          "FLOAT2INT"
        )
      ) {
        // convert float to int
        // we can't convert float32/float64 to int64 first and then clamp to int32
        // because when we convert float64 to int32, there may be precision lost
        for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
          when(pipe.instr.instr.fmt === float.fmt) {
            // check int format by rs2(1)
            for ((intFmt, intWidth) <- Seq((0, 32), (1, 64))) {
              when(pipe.instr.instr.rs2(1) === intFmt.U) {
                val convF2I =
                  Module(new RecFNToIN(float.exp, float.sig, intWidth))
                convF2I.io.in := last_ext.get.rs1HFValues(idx)
                convF2I.io.roundingMode := roundingMode
                convF2I.suggestName(s"convF2I_${float.name}_I${intWidth}")

                when(pipe.instr.instr.rs2(0) === 0.U) {
                  // FCVT.W.D/FCVT.L.D/FCVT.W.S/FCVT.L.S
                  // signed int
                  convF2I.io.signedOut := true.B
                }.otherwise {
                  convF2I.io.signedOut := false.B
                }

                ext.res := convF2I.io.out
                // see rocket chip
                ext.fflags := Cat(
                  convF2I.io.intExceptionFlags(2, 1).orR,
                  0.U(3.W),
                  convF2I.io.intExceptionFlags(0)
                )
              }
            }
          }
        }

        ext.updateFFlags := true.B
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: FloatToIntMultiCycleExt): RetireInfo = {
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
