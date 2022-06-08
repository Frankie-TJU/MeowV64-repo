package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.CompareRecFN
import hardfloat.RecFNToRecFN
import hardfloat.fNFromRecFN
import meowv64.core.CoreDef
import meowv64.core.FloatD
import meowv64.core.FloatS
import meowv64.exec._
import meowv64.instr.Decoder
import meowv64.core.FloatH

class FloatMiscMultiCycleExt(implicit val coredef: CoreDef) extends Bundle {
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

/** Handles instructions: FMIN, FMAX, FCVT
  */
class FloatMiscMultiCycle(override implicit val coredef: CoreDef)
    extends ExecUnit(1, new FloatMiscMultiCycleExt, coredef.REG_FLOAT) {

  def map(
      stage: Int,
      pipe: PipeInstr,
      last_ext: Option[FloatMiscMultiCycleExt]
  ): (FloatMiscMultiCycleExt, Bool) = {
    val ext = Wire(new FloatMiscMultiCycleExt)
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
      // double
      val rs1valDoubleHF =
        last_ext.get.rs1HFValues(coredef.FLOAT_TYPES.indexOf(FloatD))
      // single
      val rs1valSingleHF =
        last_ext.get.rs1HFValues(coredef.FLOAT_TYPES.indexOf(FloatS))
      // half
      val rs1valHalfHF =
        last_ext.get.rs1HFValues(coredef.FLOAT_TYPES.indexOf(FloatH))

      when(
        pipe.instr.instr.funct5 ===
          Decoder.FP_FUNC("FMINMAX")
      ) {
        // loop over floating point types
        for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
          when(pipe.instr.instr.fmt === float.fmt) {
            val cmp = Module(new CompareRecFN(float.exp, float.sig))
            cmp.suggestName(s"cmp_${float.name}")
            cmp.io.a := last_ext.get.rs1HFValues(idx)
            cmp.io.b := last_ext.get.rs2HFValues(idx)
            cmp.io.signaling := true.B

            // FMIN, FMAX
            cmp.io.signaling := false.B
            val retRs1 = WireInit(true.B)
            val retNaN = WireInit(false.B)

            val rs1NaN = float.isNaN(pipe.rs1val)
            val rs2NaN = float.isNaN(pipe.rs2val)

            val lt = WireInit(cmp.io.lt)
            // special handling for -0.0 and +0.0
            when(
              pipe.rs1val(float.width - 1) && ~pipe.rs2val(float.width - 1)
            ) {
              // -0.0 < +0.0
              lt := true.B
            }.elsewhen(
              ~pipe.rs1val(float.width - 1) && pipe.rs2val(float.width - 1)
            ) {
              // -0.0 > +0.0
              lt := false.B
            }

            when(pipe.instr.instr.funct3 === 0.U) {
              // FMIN
              retRs1 := lt
            }.otherwise {
              // FMAX
              retRs1 := ~lt
            }

            when(rs1NaN && ~rs2NaN) {
              // rs2 is not nan
              retRs1 := false.B
            }.elsewhen(~rs1NaN && rs2NaN) {
              // rs1 is not nan
              retRs1 := true.B
            }.elsewhen(rs1NaN && rs2NaN) {
              // return nan
              retNaN := true.B
            }

            ext.res := float.box(
              Mux(
                retNaN,
                float.nan(),
                Mux(retRs1, pipe.rs1val, pipe.rs2val)
              ),
              coredef.XLEN
            )
            ext.fflags := cmp.io.exceptionFlags
          }
        }

        ext.updateFFlags := true.B
      }.elsewhen(
        pipe.instr.instr.funct5 ===
          Decoder.FP_FUNC("FLOAT2FLOAT")
      ) {
        // convert float to float
        when(pipe.instr.instr.funct7(1, 0) === FloatD.fmt) {
          when(pipe.instr.instr.rs2(1, 0) === FloatS.fmt) {
            // FCVT.D.S
            // single precision to double precision
            // widening
            val convS2D = Module(
              new RecFNToRecFN(
                FloatS.exp(),
                FloatS.sig(),
                FloatD.exp(),
                FloatD.sig()
              )
            )
            convS2D.io.in := rs1valSingleHF
            convS2D.io.detectTininess := hardfloat.consts.tininess_afterRounding
            convS2D.io.roundingMode := 0.U

            ext.res := fNFromRecFN(FloatD.exp(), FloatD.sig(), convS2D.io.out)
            ext.fflags := convS2D.io.exceptionFlags
          }.elsewhen(pipe.instr.instr.rs2(1, 0) === FloatH.fmt) {
            // FCVT.D.H
            // half precision to double precision
            // widening
            val convH2D = Module(
              new RecFNToRecFN(
                FloatH.exp(),
                FloatH.sig(),
                FloatD.exp(),
                FloatD.sig()
              )
            )
            convH2D.io.in := rs1valHalfHF
            convH2D.io.detectTininess := hardfloat.consts.tininess_afterRounding
            convH2D.io.roundingMode := 0.U

            ext.res := fNFromRecFN(FloatD.exp(), FloatD.sig(), convH2D.io.out)
            ext.fflags := convH2D.io.exceptionFlags
          }
        }.elsewhen(pipe.instr.instr.funct7(1, 0) === FloatS.fmt) {
          when(pipe.instr.instr.rs2(1, 0) === FloatD.fmt) {
            // FCVT.S.D
            // double precision to single precision
            val convD2S = Module(
              new RecFNToRecFN(
                FloatD.exp(),
                FloatD.sig(),
                FloatS.exp(),
                FloatS.sig()
              )
            )

            convD2S.io.in := rs1valDoubleHF
            convD2S.io.detectTininess := hardfloat.consts.tininess_afterRounding
            convD2S.io.roundingMode := 0.U

            // NaN boxing
            ext.res := FloatS.box(
              fNFromRecFN(FloatS.exp(), FloatS.sig(), convD2S.io.out),
              coredef.XLEN
            )
            ext.fflags := convD2S.io.exceptionFlags
          }.elsewhen(pipe.instr.instr.rs2(1, 0) === FloatH.fmt) {
            // FCVT.S.H
            // half precision to single precision
            val convH2S = Module(
              new RecFNToRecFN(
                FloatH.exp(),
                FloatH.sig(),
                FloatS.exp(),
                FloatS.sig()
              )
            )

            convH2S.io.in := rs1valHalfHF
            convH2S.io.detectTininess := hardfloat.consts.tininess_afterRounding
            convH2S.io.roundingMode := 0.U

            // NaN boxing
            ext.res := FloatS.box(
              fNFromRecFN(FloatS.exp(), FloatS.sig(), convH2S.io.out),
              coredef.XLEN
            )
            ext.fflags := convH2S.io.exceptionFlags
          }
        }.elsewhen(pipe.instr.instr.funct7(1, 0) === FloatH.fmt) {
          when(pipe.instr.instr.rs2(1, 0) === FloatD.fmt) {
            // FCVT.H.D
            // double precision to half precision
            val convD2H = Module(
              new RecFNToRecFN(
                FloatD.exp(),
                FloatD.sig(),
                FloatH.exp(),
                FloatH.sig()
              )
            )

            convD2H.io.in := rs1valDoubleHF
            convD2H.io.detectTininess := hardfloat.consts.tininess_afterRounding
            convD2H.io.roundingMode := 0.U

            // NaN boxing
            ext.res := FloatH.box(
              fNFromRecFN(FloatH.exp(), FloatH.sig(), convD2H.io.out),
              coredef.XLEN
            )
            ext.fflags := convD2H.io.exceptionFlags
          }.elsewhen(pipe.instr.instr.rs2(1, 0) === FloatS.fmt) {
            // FCVT.H.S
            // single precision to half precision
            val convS2H = Module(
              new RecFNToRecFN(
                FloatS.exp(),
                FloatS.sig(),
                FloatH.exp(),
                FloatH.sig()
              )
            )

            convS2H.io.in := rs1valSingleHF
            convS2H.io.detectTininess := hardfloat.consts.tininess_afterRounding
            convS2H.io.roundingMode := 0.U

            // NaN boxing
            ext.res := FloatH.box(
              fNFromRecFN(FloatH.exp(), FloatH.sig(), convS2H.io.out),
              coredef.XLEN
            )
            ext.fflags := convS2H.io.exceptionFlags
          }
        }.otherwise {
          assert(false.B)
        }
        ext.updateFFlags := true.B
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: FloatMiscMultiCycleExt): RetireInfo = {
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
