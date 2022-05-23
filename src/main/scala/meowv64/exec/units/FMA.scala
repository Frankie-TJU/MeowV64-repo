package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.MulAddRecFNToRaw_postMul
import hardfloat.MulAddRecFNToRaw_preMul
import hardfloat.MulAddRecFN_interIo
import hardfloat.RawFloat
import hardfloat.RoundRawFNToRecFN
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

class FMAExt(implicit val coredef: CoreDef) extends Bundle {
  // intermediate
  // stage 0 to stage 1
  val toPostMul = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    new MulAddRecFN_interIo(float.exp, float.sig)
  })
  val mulAddA = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    UInt(float.sig().W)
  })
  val mulAddB = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    UInt(float.sig().W)
  })
  val mulAddC = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    UInt((float.sig() * 2).W)
  })

  // stage 1 to stage 2
  val mulAddResult = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    UInt((float.sig() * 2 + 1).W)
  })

  // stage 2 to stage 3
  val rawOut = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    new RawFloat(float.exp, float.sig + 2)
  })
  val invalidExc = MixedVec(for (_ <- coredef.FLOAT_TYPES) yield {
    Bool()
  })

  // result
  val res = UInt(coredef.XLEN.W)
  val fflags = UInt(5.W)
}

/** 3 stage FMA.
  *
  * Cycle 0: convert to hardfloat, preMul
  *
  * Cycle 1: mulAdd
  *
  * Cycle 2: postMul
  *
  * Cycle 3: round, convert to ieee
  */
class FMA(override implicit val coredef: CoreDef)
    extends ExecUnit(
      3,
      new FMAExt,
      coredef.REG_FLOAT
    ) {

  def map(stage: Int, pipe: PipeInstr, ext: Option[FMAExt]): (FMAExt, Bool) = {
    val state = Wire(new FMAExt)
    state := DontCare

    // loop over floating point types
    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
      when(pipe.instr.instr.fmt === float.fmt) {
        if (stage == 0) {
          // stage 0
          // step 1: collect op and operands
          // a * b + c
          val a = WireInit(0.U(float.widthHardfloat.W))
          val b = WireInit(0.U(float.widthHardfloat.W))
          val c = WireInit(0.U(float.widthHardfloat.W))

          // convert to hardfloat
          val rs1val = float.unbox(pipe.rs1val, coredef.XLEN)
          val rs2val = float.unbox(pipe.rs2val, coredef.XLEN)
          val rs3val = float.unbox(pipe.rs3val, coredef.XLEN)
          val rs1valHF = float.toHardfloat(rs1val)
          val rs2valHF = float.toHardfloat(rs2val)
          val rs3valHF = float.toHardfloat(rs3val)
          val oneHF = float.oneHardfloat()

          val neg = WireInit(false.B)
          val sign = WireInit(false.B)
          val op = Cat(neg, sign)
          switch(pipe.instr.instr.op) {
            is(Decoder.Op("MADD").ident) {
              // rs1 * rs2 + rs3
              a := rs1valHF
              b := rs2valHF
              c := rs3valHF
            }
            is(Decoder.Op("MSUB").ident) {
              // rs1 * rs2 - rs3
              sign := true.B
              a := rs1valHF
              b := rs2valHF
              c := rs3valHF
            }
            is(Decoder.Op("NMSUB").ident) {
              // - (rs1 * rs2 - rs3)
              neg := true.B
              a := rs1valHF
              b := rs2valHF
              c := rs3valHF
            }
            is(Decoder.Op("NMADD").ident) {
              // - (rs1 * rs2 + rs3)
              neg := true.B
              sign := true.B
              a := rs1valHF
              b := rs2valHF
              c := rs3valHF
            }
            is(Decoder.Op("OP-FP").ident) {
              switch(pipe.instr.instr.funct5) {
                is(Decoder.FP_FUNC("FADD")) {
                  // 1 * rs1 + rs2
                  a := oneHF
                  b := rs1valHF
                  c := rs2valHF
                }
                is(Decoder.FP_FUNC("FSUB")) {
                  // 1 * rs1 - rs2
                  sign := true.B
                  a := oneHF
                  b := rs1valHF
                  c := rs2valHF
                }
                is(Decoder.FP_FUNC("FMUL")) {
                  // rs1 * rs2 + 0
                  a := rs1valHF
                  b := rs2valHF
                  // the signedness of 0 is rs1 ^ rs2
                  c := (rs1val(float.width() - 1) ^
                    rs2val(float.width() - 1)) << float.width()
                }
              }
            }
          }

          // step 2: preMul
          val preMul = Module(new MulAddRecFNToRaw_preMul(float.exp, float.sig))
          preMul.suggestName(s"preMul_${float.name}")
          preMul.io.op := op
          preMul.io.a := a
          preMul.io.b := b
          preMul.io.c := c

          state.toPostMul(idx) := preMul.io.toPostMul

          state.mulAddA(idx) := preMul.io.mulAddA
          state.mulAddB(idx) := preMul.io.mulAddB
          state.mulAddC(idx) := preMul.io.mulAddC
        } else if (stage == 1) {
          // stage 1
          val lastState = ext.get

          val mulAddResult =
            (lastState.mulAddA(idx) * lastState.mulAddB(idx)) +& lastState
              .mulAddC(idx)

          state.toPostMul(idx) := lastState.toPostMul(idx)
          state.mulAddResult(idx) := mulAddResult
        } else if (stage == 2) {
          // stage 2
          val lastState = ext.get

          val postMul =
            Module(new MulAddRecFNToRaw_postMul(float.exp, float.sig))
          postMul.suggestName(s"postMul_${float.name}")
          postMul.io.fromPreMul := lastState.toPostMul(idx)
          postMul.io.mulAddResult := lastState.mulAddResult(idx)
          // TODO
          postMul.io.roundingMode := 0.U

          state.rawOut(idx) := postMul.io.rawOut
          state.invalidExc(idx) := postMul.io.invalidExc
        } else if (stage == 3) {
          // stage 3
          val lastState = ext.get

          // rounding
          val round = Module(new RoundRawFNToRecFN(float.exp, float.sig, 0))
          round.suggestName(s"round_${float.name}")
          round.io.in := lastState.rawOut(idx)
          round.io.infiniteExc := false.B
          round.io.invalidExc := lastState.invalidExc(idx)
          round.io.detectTininess := hardfloat.consts.tininess_afterRounding
          round.io.roundingMode := 0.U

          // convert to ieee
          state.res := float.box(
            float.fromHardfloat(round.io.out),
            coredef.XLEN
          )
          state.fflags := round.io.exceptionFlags
        }
      }
    }

    // never stalls
    (state, false.B)
  }

  def finalize(pipe: PipeInstr, ext: FMAExt): RetireInfo = {
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
