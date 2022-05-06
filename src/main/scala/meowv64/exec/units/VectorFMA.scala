package meowv64.exec.units

import chisel3._
import chisel3.util._
import hardfloat.MulAddRecFNToRaw_postMul
import hardfloat.MulAddRecFNToRaw_preMul
import hardfloat.MulAddRecFN_interIo
import hardfloat.RawFloat
import hardfloat.RoundRawFNToRecFN
import meowv64.core.CoreDef
import meowv64.core.VState
import meowv64.exec._

class VectorFMAExt(implicit val coredef: CoreDef) extends Bundle {
  // stage 0 to stage 1:
  val toPostMul = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      new MulAddRecFN_interIo(float.exp, float.sig)
    )
  })
  val mulAddA = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      UInt(float.sig().W)
    )
  })
  val mulAddB = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      UInt(float.sig().W)
    )
  })
  val mulAddC = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      UInt((float.sig() * 2).W)
    )
  })

  // stage 1 to stage 2:
  val rawOut = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      new RawFloat(float.exp, float.sig + 2)
    )
  })
  val invalidExc = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      Bool()
    )
  })

  val res = UInt(coredef.VLEN.W)
  val fflags = UInt(5.W)
}

class VectorFMA(override implicit val coredef: CoreDef)
    extends ExecUnit(2, new VectorFMAExt, coredef.REG_VEC)
    with WithVState {

  val vState = IO(Input(new VState()))

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[VectorFMAExt]
  ): (VectorFMAExt, Bool) = {
    val state = Wire(new VectorFMAExt)
    state := DontCare

    val curFloat = vState.vtype.floatFmt

    // loop over floating point types
    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
      val lanes = coredef.VLEN / float.width()

      def getLanes(index: Int, input: UInt) = {
        val res = Wire(
          Vec(lanes, UInt(float.width.W))
        )
        res := input.asTypeOf(res)
        when(pipe.instr.instr.funct3 === 5.U && (index == 1).B) {
          // vxxx.vf
          for (i <- 0 until lanes) {
            res(i) := input
          }
        }
        res
      }

      val rs1Elements = getLanes(1, pipe.rs1val)
      val rs2Elements = getLanes(2, pipe.rs2val)
      val rs3Elements = getLanes(3, pipe.rs3val)

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
            val rs1valHF = float.toHardfloat(rs1val)
            val rs2valHF = float.toHardfloat(rs2val)
            val rs3valHF = float.toHardfloat(rs3val)
            val oneHF =
              (BigInt(1) << (float.exp + float.sig - 1))
                .U(float.widthHardfloat.W)

            val neg = WireInit(false.B)
            val sign = WireInit(false.B)
            val op = Cat(neg, sign)
            switch(pipe.instr.instr.funct6) {
              is(0.U) {
                // vfadd: 1 * rs1 + rs2
                a := oneHF
                b := rs1valHF
                c := rs2valHF
              }
              is(0x2c.U) {
                // vfmacc: rs1 * rs2 + rs3
                a := rs1valHF
                b := rs2valHF
                c := rs3valHF
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
            state.mulAddA(idx)(lane) := preMul.io.mulAddA
            state.mulAddB(idx)(lane) := preMul.io.mulAddB
            state.mulAddC(idx)(lane) := preMul.io.mulAddC

          } else if (stage == 1) {
            // second stage
            val lastState = ext.get

            // step 1: mul & add
            val mulAddResult =
              (lastState.mulAddA(idx)(lane) * lastState.mulAddB(idx)(lane)) +&
                lastState.mulAddC(idx)(lane)

            // step 2: post mul
            val postMul =
              Module(new MulAddRecFNToRaw_postMul(float.exp, float.sig))
            postMul.suggestName(s"postMul_${float.name}")
            postMul.io.fromPreMul := lastState.toPostMul(idx)(lane)
            postMul.io.mulAddResult := mulAddResult
            // TODO
            postMul.io.roundingMode := 0.U

            state.rawOut(idx)(lane) := postMul.io.rawOut
            state.invalidExc(idx)(lane) := postMul.io.invalidExc
          } else {
            // third stage
            val lastState = ext.get

            // step 3: rounding
            val round = Module(new RoundRawFNToRecFN(float.exp, float.sig, 0))
            round.suggestName(s"round_${float.name}")
            round.io.in := lastState.rawOut(idx)(lane)
            round.io.infiniteExc := false.B
            round.io.invalidExc := lastState.invalidExc(idx)(lane)
            round.io.detectTininess := hardfloat.consts.tininess_afterRounding
            round.io.roundingMode := 0.U

            // step 2: convert to ieee
            when(
              (pipe.instr.instr.readVm() && ~pipe.vmval(
                lane
              )) || lane.U >= vState.vl
            ) {
              // masked off, retain old value
              res(lane) := rs3Elements(lane)
              fflags(lane) := 0.U
            }.otherwise {
              res(lane) := float.fromHardfloat(round.io.out)
              fflags(lane) := round.io.exceptionFlags
            }
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
    // result
    info.wb := ext.res

    // fflags
    info.updateFFlags := true.B
    info.fflags := ext.fflags

    info
  }

  init()
}
