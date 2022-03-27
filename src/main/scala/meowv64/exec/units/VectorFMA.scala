package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.MulAddRecFNToRaw_postMul
import hardfloat.MulAddRecFNToRaw_preMul
import hardfloat.MulAddRecFN_interIo
import hardfloat.RoundRawFNToRecFN
import meowv64.core.CoreDef
import meowv64.core.FloatD
import meowv64.core.FloatH
import meowv64.core.FloatS
import meowv64.core.VState
import meowv64.exec._

class VectorFMAExt(implicit val coredef: CoreDef) extends Bundle {
  // intermediate
  val toPostMul = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      new MulAddRecFN_interIo(float.exp, float.sig)
    )
  })
  val mulAddResult = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      UInt((2 * float.sig + 1).W)
    )
  })

  val res = UInt(coredef.VLEN.W)
  val fflags = UInt(5.W)
}

class VectorFMA(override implicit val coredef: CoreDef)
    extends ExecUnit(1, new VectorFMAExt, coredef.REG_VEC)
    with WithVState {

  val vState = IO(Input(new VState()))

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[VectorFMAExt]
  ): (VectorFMAExt, Bool) = {
    val state = Wire(new VectorFMAExt)
    state := DontCare

    val curFloat = MuxLookup(
      vState.vtype.vsew,
      0.U,
      Seq(
        1.U -> FloatH.fmt,
        2.U -> FloatS.fmt,
        3.U -> FloatD.fmt
      )
    )

    // loop over floating point types
    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
      val lanes = coredef.VLEN / float.width()
      val rs1Elements = Wire(
        Vec(lanes, UInt(float.width.W))
      )
      rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)
      val rs2Elements = Wire(
        Vec(lanes, UInt(float.width.W))
      )
      rs2Elements := pipe.rs2val.asTypeOf(rs2Elements)
      val rs3Elements = Wire(
        Vec(lanes, UInt(float.width.W))
      )
      rs3Elements := pipe.rs3val.asTypeOf(rs3Elements)

      val res = Wire(Vec(lanes, UInt(float.width.W)))
      val fflags = Wire(Vec(lanes, UInt(5.W)))
      res := DontCare
      fflags := DontCare

      when(curFloat === float.fmt) {
        for (lane <- 0 until lanes) {
          if (stage == 0) {
            // step 1: collect op and operands
            // a * b + c
            val a = WireInit(0.U(float.widthHardfloat.W))
            val b = WireInit(0.U(float.widthHardfloat.W))
            val c = WireInit(0.U(float.widthHardfloat.W))

            // convert to hardfloat
            val rs1val = rs1Elements(lane)
            val rs2val = rs2Elements(lane)
            val rs3val = rs3Elements(lane)
            val rs1valHF = float.toHardfloat(rs1Elements(lane))
            val rs2valHF = float.toHardfloat(rs2Elements(lane))
            val rs3valHF = float.toHardfloat(rs3Elements(lane))
            val oneHF =
              (BigInt(1) << (float.exp + float.sig - 1))
                .U(float.widthHardfloat.W)

            val neg = WireInit(false.B)
            val sign = WireInit(false.B)
            val op = Cat(neg, sign)
            switch(pipe.instr.instr.funct6) {
              is(0.U) {
                // 1 * rs1 + rs2
                a := oneHF
                b := rs1valHF
                c := rs2valHF
              }
            }

            // step 2: preMul
            val preMul =
              Module(new MulAddRecFNToRaw_preMul(float.exp, float.sig))
            preMul.suggestName(s"preMul_${float.name}")
            preMul.io.op := op
            preMul.io.a := a
            preMul.io.b := b
            preMul.io.c := c

            state.toPostMul(idx)(lane) := preMul.io.toPostMul

            // step 3: mul & add
            state.mulAddResult(idx)(lane) :=
              (preMul.io.mulAddA * preMul.io.mulAddB) +& preMul.io.mulAddC
          } else {
            // second stage
            state := ext.get

            // step 1: post mul
            val postMul =
              Module(new MulAddRecFNToRaw_postMul(float.exp, float.sig))
            postMul.suggestName(s"postMul_${float.name}")
            postMul.io.fromPreMul := state.toPostMul(idx)(lane)
            postMul.io.mulAddResult := state.mulAddResult(idx)(lane)
            // TODO
            postMul.io.roundingMode := 0.U

            // step 2: rounding
            val round = Module(new RoundRawFNToRecFN(float.exp, float.sig, 0))
            round.suggestName(s"round_${float.name}")
            round.io.in := postMul.io.rawOut
            round.io.infiniteExc := false.B
            round.io.invalidExc := postMul.io.invalidExc
            round.io.detectTininess := false.B
            round.io.roundingMode := 0.U

            // step 3: convert to ieee
            res(lane) := float.fromHardfloat(round.io.out)
            fflags(lane) := round.io.exceptionFlags
          }
        }

        state.res := Cat(res.reverse)
        state.fflags := fflags.reduce(_ | _)
      }
    }

    (state, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VectorFMAExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))
    info.wb := ext.res

    info
  }

  init()
}
