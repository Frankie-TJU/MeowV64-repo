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
    extends ExecUnit(1, new FloatToIntMultiCycleExt, coredef.REG_FLOAT) {

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
        // convert float32/float64 to int64 first
        // then clamp to int32
        for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
          when(pipe.instr.instr.fmt === float.fmt) {
            val convF2I =
              Module(new RecFNToIN(float.exp, float.sig, coredef.XLEN))
            convF2I.io.in := last_ext.get.rs1HFValues(idx)
            convF2I.io.signedOut := false.B
            convF2I.io.roundingMode := pipe.instr.instr.funct3

            convF2I.suggestName(s"convF2I_${float.name}")

            // overflow in int64 -> int32
            val overflow = WireInit(false.B)
            when(pipe.instr.instr.rs2(1) === 0.U) {
              // FCVT.W.S/FCVT.WU.S/FCVT.W.D/FCVT.WU.D
              // convert 32/64-bit float to 32-bit int/uint
              // clamp to int32 range
              val clamped = WireInit(0.U(32.W))

              when(pipe.instr.instr.rs2(0) === 0.U) {
                // signed
                when(
                  convF2I.io.out(coredef.XLEN - 1) && ~convF2I.io
                    .out(coredef.XLEN - 1, 31)
                    .andR
                ) {
                  // negative underflow
                  clamped := 1.U ## Fill(31, 0.U)
                  overflow := true.B
                }.elsewhen(
                  ~convF2I.io.out(coredef.XLEN - 1) && convF2I.io
                    .out(coredef.XLEN - 1, 31)
                    .orR
                ) {
                  // positive overflow
                  clamped := 0.U ## Fill(31, 1.U)
                  overflow := true.B
                }.otherwise {
                  clamped := convF2I.io.out(31, 0)
                }
              }.otherwise {
                // unsigned
                when(convF2I.io.out(coredef.XLEN - 1, 32).orR) {
                  // positive overflow
                  clamped := Fill(32, 1.U)
                }.otherwise {
                  clamped := convF2I.io.out(31, 0)
                }
              }

              // sign extension
              ext.res := Fill(32, clamped(31)) ## clamped(31, 0)
            }.otherwise {
              // FCVT.L.S/FCVT.LU.S/FCVT.L.D/FCVT.LU.D
              // convert 32/64-bit float to 64-bit int/uint
              ext.res := convF2I.io.out
            }

            when(pipe.instr.instr.rs2(0) === 0.U) {
              // FCVT.W.D/FCVT.L.D/FCVT.W.S/FCVT.L.S
              // signed int
              convF2I.io.signedOut := true.B
            }

            // see rocket chip
            ext.fflags := Cat(
              convF2I.io.intExceptionFlags(2, 1).orR | overflow,
              0.U(3.W),
              convF2I.io.intExceptionFlags(0)
            )
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
