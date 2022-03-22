package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.CompareRecFN
import hardfloat.RecFNToRecFN
import hardfloat.fNFromRecFN
import hardfloat.recFNFromFN
import meowv64.core.CoreDef
import meowv64.core.FloatS
import meowv64.exec._
import meowv64.instr.Decoder

class FloatMiscExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.XLEN.W)

  val updateFFlags = Bool()
  val fflags = UInt(5.W)
}

/** Handles instructions: FSGNJ, FMIN, FMAX, FCVT
  */
class FloatMisc(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new FloatMiscExt, coredef.REG_FLOAT) {

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[FloatMiscExt]
  ): (FloatMiscExt, Bool) = {
    val ext = Wire(new FloatMiscExt)
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

    val expWidth = 11
    val sigWidth = 53
    val singleExpWidth = 8
    val singleSigWidth = 24

    // double
    val rs1valHF = WireInit(recFNFromFN(expWidth, sigWidth, pipe.rs1val))
    // single
    val rs1valSingleHF = WireInit(
      recFNFromFN(singleExpWidth, singleSigWidth, pipe.rs1val(31, 0))
    )

    when(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FMINMAX"
      )
    ) {
      ext.fflags := DontCare

      // loop over floating point types
      for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
        when(pipe.instr.instr.fmt === float.fmt) {
          val cmp = Module(new CompareRecFN(float.exp, float.sig))
          cmp.suggestName(s"cmp_${float.name}")
          cmp.io.a := rs1HFValues(idx)
          cmp.io.b := rs2HFValues(idx)
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
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FLOAT2FLOAT"
      )
    ) {
      // convert float to float
      when(pipe.instr.instr.rs2 === 0.U) {
        // FCVT.D.S
        // single precision to double precision
        // widening
        val convS2D = Module(
          new RecFNToRecFN(singleExpWidth, singleSigWidth, expWidth, sigWidth)
        )
        convS2D.io.in := rs1valSingleHF
        convS2D.io.detectTininess := false.B
        convS2D.io.roundingMode := 0.U

        ext.res := fNFromRecFN(expWidth, sigWidth, convS2D.io.out)
        ext.fflags := convS2D.io.exceptionFlags
      }.elsewhen(pipe.instr.instr.rs2 === 1.U) {
        // FCVT.S.D
        // double precision to single precision
        val convD2S = Module(
          new RecFNToRecFN(expWidth, sigWidth, singleExpWidth, singleSigWidth)
        )

        convD2S.io.in := rs1valHF
        convD2S.io.detectTininess := false.B
        convD2S.io.roundingMode := 0.U

        // NaN boxing
        ext.res := FloatS.box(
          fNFromRecFN(singleExpWidth, singleSigWidth, convD2S.io.out),
          coredef.XLEN
        )
        ext.fflags := convD2S.io.exceptionFlags
      }.otherwise {
        assert(false.B)
      }
      ext.updateFFlags := true.B
    }.elsewhen(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FSGNJ"
      )
    ) {
      // sign injection
      when(pipe.instr.instr.funct7(1, 0) === 0.U) {
        val rs1Value = rs1Values(0)
        val rs2Value = rs2Values(0)
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
    info.updateFFlags := ext.updateFFlags
    info.fflags := ext.fflags

    info
  }

  init()
}
