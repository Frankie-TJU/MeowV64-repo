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
import meowv64.exec._

class FMAExt(implicit val coredef: CoreDef) extends Bundle {
  // intermediate
  // stage 0 to stage 1
  val op = MixedVec(for (_ <- coredef.FLOAT_TYPES) yield {
    UInt(2.W)
  })
  val a = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    UInt(float.widthHardfloat.W)
  })
  val b = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    UInt(float.widthHardfloat.W)
  })
  val c = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    UInt(float.widthHardfloat.W)
  })

  // stage 1 to stage 2
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

  // stage 2 to stage 3
  val mulAddResult = MixedVec(for (float <- coredef.FLOAT_TYPES) yield {
    UInt((float.sig() * 2 + 1).W)
  })

  // stage 3 to stage 4
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
  * Cycle 0: convert to hardfloat
  *
  * Cycle 1: preMul
  *
  * Cycle 2: mulAdd
  *
  * Cycle 3: postMul
  *
  * Cycle 4: round, convert to ieee
  */
class FMA(override implicit val coredef: CoreDef)
    extends ExecUnit(
      4,
      new FMAExt,
      coredef.REG_FLOAT
    ) {

  def map(stage: Int, pipe: PipeInstr, ext: Option[FMAExt]): (FMAExt, Bool) = {
    val state = Wire(new FMAExt)
    state := DontCare

    // decode table
    // a = rs1valHF, _, _, oneHF
    // b = rs1valHF, rs2valHF, _, _
    // c = _, rs2valHF, rs3valHF, zeroHF
    val aSel = Wire(UInt(2.W))
    val bSel = Wire(UInt(2.W))
    val cSel = Wire(UInt(2.W))
    val neg = WireInit(false.B)
    val sign = WireInit(false.B)

    val Y = BitPat("b1")
    val N = BitPat("b0")

    val RS1 = BitPat("b00")
    val RS2 = BitPat("b01")
    val RS3 = BitPat("b10")
    // special
    val SPL = BitPat("b11")

    val default: List[BitPat] =
      List(
        BitPat("b??"),
        BitPat("b??"),
        BitPat("b??"),
        BitPat("b?"),
        BitPat("b?")
      )
    val table: Array[(BitPat, List[BitPat])] =
      Array(
        // op ## funct5
        // a, b, c, neg, sign
        // fmadd: rs1 * rs2 + rs3
        BitPat("b10000?????") -> List(RS1, RS2, RS3, N, N),
        // fmsub: rs1 * rs2 - rs3
        BitPat("b10001?????") -> List(RS1, RS2, RS3, N, Y),
        // fnmsub: - (rs1 * rs2 - rs3)
        BitPat("b10010?????") -> List(RS1, RS2, RS3, Y, N),
        // fnmadd: - (rs1 * rs2 + rs3)
        BitPat("b10011?????") -> List(RS1, RS2, RS3, Y, Y),
        // fadd: 1 * rs1 + rs2
        BitPat("b1010000000") -> List(SPL, RS1, RS2, N, N),
        // fsub: 1 * rs1 - rs2
        BitPat("b1010000001") -> List(SPL, RS1, RS2, N, Y),
        // fmul: rs1 * rs2 + 0
        BitPat("b1010000010") -> List(RS1, RS2, SPL, N, N)
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
        .qmc(Cat(pipe.instr.instr.op, pipe.instr.instr.funct5), truthTable)
        .asTypeOf(signal)
    }

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
          // the signedness of 0 is rs1 ^ rs2
          val zeroHF = (rs1val(float.width() - 1) ^
            rs2val(float.width() - 1)) << float.width()

          val op = Cat(neg, sign)

          a := MuxLookup(
            aSel,
            rs1valHF,
            Seq(3.U -> oneHF)
          )
          b := MuxLookup(
            bSel,
            rs1valHF,
            Seq(1.U -> rs2valHF)
          )
          c := MuxLookup(
            cSel,
            rs2valHF,
            Seq(2.U -> rs3valHF, 3.U -> zeroHF)
          )

          state.op(idx) := op
          state.a(idx) := a
          state.b(idx) := b
          state.c(idx) := c
        } else if (stage == 1) {
          // stage 1
          val lastState = ext.get
          val preMul = Module(new MulAddRecFNToRaw_preMul(float.exp, float.sig))
          preMul.suggestName(s"preMul_${float.name}")
          preMul.io.op := lastState.op(idx)
          preMul.io.a := lastState.a(idx)
          preMul.io.b := lastState.b(idx)
          preMul.io.c := lastState.c(idx)

          state.toPostMul(idx) := preMul.io.toPostMul
          state.mulAddA(idx) := preMul.io.mulAddA
          state.mulAddB(idx) := preMul.io.mulAddB
          state.mulAddC(idx) := preMul.io.mulAddC
        } else if (stage == 2) {
          // stage 2
          val lastState = ext.get

          val mulAddResult =
            (lastState.mulAddA(idx) * lastState.mulAddB(idx)) +& lastState
              .mulAddC(idx)

          state.toPostMul(idx) := lastState.toPostMul(idx)
          state.mulAddResult(idx) := mulAddResult
        } else if (stage == 3) {
          // stage 3
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
        } else if (stage == 4) {
          // stage 4
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
