package meowv64.exec.units

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import chisel3.util.experimental.decode.decoder
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
  val op = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      UInt(2.W)
    )
  })
  val a = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      UInt(float.widthHardfloat.W)
    )
  })
  val b = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      UInt(float.widthHardfloat.W)
    )
  })
  val c = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      UInt(float.widthHardfloat.W)
    )
  })

  // stage 1 to stage 2:
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

  // stage 2 to stage 3:
  val mulAddResult = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    Vec(
      coredef.VLEN / float.width,
      UInt((float.sig * 2 + 1).W)
    )
  })

  // stage 3 to stage 4:
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

  // final
  val res = UInt(coredef.VLEN.W)
  val fflags = UInt(5.W)
}

class VectorFMA(override implicit val coredef: CoreDef)
    extends ExecUnit(4, new VectorFMAExt, coredef.REG_VEC)
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

    // decode table
    // a = oneHF, rs1valHF
    // b = rs1valHF, rs2valHF, rs3valHF
    // c = rs1valHF, rs2valHF, rs3valHF, zeroHF
    val aSel = Wire(Bool())
    val bSel = Wire(UInt(2.W))
    val cSel = Wire(UInt(2.W))
    val neg = WireInit(false.B)
    val sign = WireInit(false.B)

    val Y = BitPat("b1")
    val N = BitPat("b0")

    val RS1 = BitPat("b00")
    val RS2 = BitPat("b01")
    val RS3 = BitPat("b10")
    val ZERO = BitPat("b11")

    val default: List[BitPat] =
      List(
        BitPat("b?"),
        BitPat("b??"),
        BitPat("b??"),
        BitPat("b?"),
        BitPat("b?")
      )
    val table: Array[(BitPat, List[BitPat])] =
      Array(
        // a, b, c, neg, sign
        // vfadd: 1 * rs1 + rs2
        BitPat("b000000") -> List(N, RS1, RS2, N, N),
        // vfsub: 1 * rs2 - rs1
        BitPat("b000010") -> List(N, RS2, RS1, N, Y),
        // vfmul: rs1 * rs2 + 0
        BitPat("b100100") -> List(Y, RS2, ZERO, N, N),
        // vfrsub: 1 * rs1 - rs2
        BitPat("b100111") -> List(N, RS1, RS2, N, Y),
        // vfmadd: rs1 * rs3 + rs2
        BitPat("b101000") -> List(Y, RS3, RS2, N, N),
        // vfnmadd: -(rs1 * rs3) - rs2
        BitPat("b101001") -> List(Y, RS3, RS2, Y, Y),
        // vfmsub: rs1 * rs3 - rs2
        BitPat("b101010") -> List(Y, RS3, RS2, N, Y),
        // vfnmsub: -(rs1 * rs3) + rs2
        BitPat("b101011") -> List(Y, RS3, RS2, Y, N),
        // vfmacc: rs1 * rs2 + rs3
        BitPat("b101100") -> List(Y, RS2, RS3, N, N),
        // vfnmacc: -(rs1 * rs2) - rs3
        BitPat("b101101") -> List(Y, RS2, RS3, Y, Y),
        // vfmsac: rs1 * rs2 - rs3
        BitPat("b101110") -> List(Y, RS2, RS3, N, Y),
        // vfnmsac: -(rs1 * rs2) + rs3
        BitPat("b101111") -> List(Y, RS2, RS3, Y, N)
      )
    val signals = Seq(aSel, bSel, cSel, neg, sign)

    for ((signal, i) <- signals.zipWithIndex) {
      val truthTable =
        TruthTable(
          table.map({ case (k, v) => (k, v(i)) }),
          default = default(i)
        )

      // println(truthTable)
      signal := decoder
        .qmc(pipe.instr.instr.funct6, truthTable)
        .asTypeOf(signal)
    }

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
            // stage 0
            // collect op and operands
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

            val op = Cat(neg, sign)
            a := Mux(aSel, rs1valHF, oneHF)
            b := MuxLookup(
              bSel,
              rs1valHF,
              Seq(1.U -> rs2valHF, 2.U -> rs3valHF)
            )
            c := MuxLookup(
              cSel,
              rs1valHF,
              Seq(1.U -> rs2valHF, 2.U -> rs3valHF, 3.U -> 0.U)
            )

            state.op(idx)(lane) := op
            state.a(idx)(lane) := a
            state.b(idx)(lane) := b
            state.c(idx)(lane) := c
          } else if (stage == 1) {
            // stage 1
            val lastState = ext.get
            val preMul =
              Module(new MulAddRecFNToRaw_preMul(float.exp, float.sig))
            preMul.suggestName(s"preMul_${float.name}")
            preMul.io.op := lastState.op(idx)(lane)
            preMul.io.a := lastState.a(idx)(lane)
            preMul.io.b := lastState.b(idx)(lane)
            preMul.io.c := lastState.c(idx)(lane)

            state.toPostMul(idx)(lane) := preMul.io.toPostMul
            state.mulAddA(idx)(lane) := preMul.io.mulAddA
            state.mulAddB(idx)(lane) := preMul.io.mulAddB
            state.mulAddC(idx)(lane) := preMul.io.mulAddC
          } else if (stage == 2) {
            // stage 2
            val lastState = ext.get

            val mulAddResult =
              (lastState.mulAddA(idx)(lane) * lastState.mulAddB(idx)(lane)) +&
                lastState.mulAddC(idx)(lane)

            state.toPostMul(idx)(lane) := lastState.toPostMul(idx)(lane)
            state.mulAddResult(idx)(lane) := mulAddResult
          } else if (stage == 3) {
            // stage 3: post mul
            val lastState = ext.get

            val postMul =
              Module(new MulAddRecFNToRaw_postMul(float.exp, float.sig))
            postMul.suggestName(s"postMul_${float.name}")
            postMul.io.fromPreMul := lastState.toPostMul(idx)(lane)
            postMul.io.mulAddResult := lastState.mulAddResult(idx)(lane)
            // TODO
            postMul.io.roundingMode := 0.U

            state.rawOut(idx)(lane) := postMul.io.rawOut
            state.invalidExc(idx)(lane) := postMul.io.invalidExc
          } else {
            // stage 4
            val lastState = ext.get

            // step 1: rounding
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
