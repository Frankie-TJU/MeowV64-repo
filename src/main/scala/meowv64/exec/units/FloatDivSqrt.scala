package meowv64.exec.units

import chisel3._
import chisel3.util.Cat
import chisel3.util._
import chisel3.util.log2Ceil
import hardfloat.RawFloat
import hardfloat.RoundRawFNToRecFN
import hardfloat.consts.divSqrtOpt_twoBitsPerCycle
import hardfloat.isSigNaNRawFloat
import hardfloat.rawFloatFromRecFN
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

object FloatDivSqrtState extends ChiselEnum {
  var sIdle, sReq, sResp, sDone = Value
}

class FloatDivSqrt(implicit val coredef: CoreDef)
    extends Module
    with ExecUnitInt
    with WithFRM {

  val frm = IO(Input(UInt(3.W)))

  val regInfo = coredef.REG_FLOAT
  val DEPTH = 1
  val io = IO(new ExecUnitPort(regInfo))

  val state = RegInit(FloatDivSqrtState.sIdle)

  val div_sqrt =
    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) yield {
      val inner = Module(
        new DivSqrtRecFN_small(
          float.exp(),
          float.sig(),
          0
        )
      )

      // default wiring
      inner.io.inValid := false.B
      inner.io.a := 0.U
      inner.io.b := 0.U
      inner.io.detectTininess := hardfloat.consts.tininess_afterRounding
      inner.io.sqrtOp := false.B
      inner.io.roundingMode := frm
      inner.io.flush := io.flush

      inner.suggestName(s"DivSqrt_${float.name}")
      inner
    }

  val currentInstr = Reg(new PipeInstr(regInfo))
  val rs1valHF =
    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) yield {
      Reg(UInt(float.widthHardfloat.W))
    }
  val rs2valHF =
    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) yield {
      Reg(UInt(float.widthHardfloat.W))
    }
  val resHF =
    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) yield {
      Reg(UInt(float.widthHardfloat.W))
    }
  val fflags = Reg(UInt(5.W))
  val sqrtOp = Reg(Bool())

  io.stall := true.B
  io.retiredInstr := currentInstr
  io.retiredInstr.instr.valid := false.B
  io.retirement := RetireInfo.vacant(regInfo)
  switch(state) {
    is(FloatDivSqrtState.sIdle) {
      io.stall := false.B
    }

    is(FloatDivSqrtState.sReq) {
      for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
        when(float.fmt === currentInstr.instr.instr.fmt) {
          div_sqrt(idx).io.a := rs1valHF(idx)
          div_sqrt(idx).io.b := rs2valHF(idx)

          div_sqrt(idx).io.inValid := true.B
          div_sqrt(idx).io.roundingMode := frm
          div_sqrt(
            idx
          ).io.detectTininess := hardfloat.consts.tininess_afterRounding
          div_sqrt(idx).io.sqrtOp := sqrtOp

          when(div_sqrt(idx).io.inReady) {
            state := FloatDivSqrtState.sResp
          }
        }
      }
    }

    is(FloatDivSqrtState.sResp) {
      for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
        when(float.fmt === currentInstr.instr.instr.fmt) {
          when(
            div_sqrt(idx).io.outValid_div || div_sqrt(idx).io.outValid_sqrt
          ) {
            resHF(idx) := div_sqrt(idx).io.out
            fflags := div_sqrt(idx).io.exceptionFlags
            state := FloatDivSqrtState.sDone
          }
        }
      }
    }

    is(FloatDivSqrtState.sDone) {
      io.stall := false.B
      val res = WireInit(0.U(coredef.XLEN.W))

      for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
        when(float.fmt === currentInstr.instr.instr.fmt) {
          res := float.box(
            float.fromHardfloat(resHF(idx)),
            coredef.XLEN
          )
        }
      }

      io.retiredInstr.instr.valid := true.B

      io.retirement.wb := res
      io.retirement.markFSDirty := true.B
      io.retirement.updateFFlags := true.B
      io.retirement.fflags := fflags

      state := FloatDivSqrtState.sIdle
    }
  }

  when(!io.stall && io.next.valid) {
    // convert to hardfloat
    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
      when(float.fmt === io.next.instr.instr.fmt) {
        rs1valHF(idx) :=
          float.toHardfloat(float.unbox(io.next.rs1val, coredef.XLEN))
        rs2valHF(idx) :=
          float.toHardfloat(float.unbox(io.next.rs2val, coredef.XLEN))
      }
    }

    sqrtOp := io.next.instr.instr.funct5 === Decoder.FP_FUNC("FSQRT")

    currentInstr := io.next

    state := FloatDivSqrtState.sReq
  }

  when(io.flush) {
    state := FloatDivSqrtState.sIdle
  }
}

// the following code is copied from DivSqrtRecFN_small code
// an additional flush signal is added

/*----------------------------------------------------------------------------
| Computes a division or square root for floating-point in recoded form.
| Multiple clock cycles are needed for each division or square-root operation,
| except possibly in special cases.
 *----------------------------------------------------------------------------*/

class DivSqrtRawFN_small(expWidth: Int, sigWidth: Int, options: Int)
    extends Module {
  val io = IO(new Bundle {
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    val inReady = Output(Bool())
    val inValid = Input(Bool())
    val sqrtOp = Input(Bool())
    val a = Input(new RawFloat(expWidth, sigWidth))
    val b = Input(new RawFloat(expWidth, sigWidth))
    val roundingMode = Input(UInt(3.W))
    val flush = Input(Bool())
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    val rawOutValid_div = Output(Bool())
    val rawOutValid_sqrt = Output(Bool())
    val roundingModeOut = Output(UInt(3.W))
    val invalidExc = Output(Bool())
    val infiniteExc = Output(Bool())
    val rawOut = Output(new RawFloat(expWidth, sigWidth + 2))
  })

  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  val cycleNum = RegInit(0.U(log2Ceil(sigWidth + 3).W))
  val inReady = RegInit(true.B) // <-> (cycleNum <= 1)
  val rawOutValid = RegInit(false.B) // <-> (cycleNum === 1)

  val sqrtOp_Z = Reg(Bool())
  val majorExc_Z = Reg(Bool())
//*** REDUCE 3 BITS TO 2-BIT CODE:
  val isNaN_Z = Reg(Bool())
  val isInf_Z = Reg(Bool())
  val isZero_Z = Reg(Bool())
  val sign_Z = Reg(Bool())
  val sExp_Z = Reg(SInt((expWidth + 2).W))
  val fractB_Z = Reg(UInt(sigWidth.W))
  val roundingMode_Z = Reg(UInt(3.W))

  /*------------------------------------------------------------------------
    | (The most-significant and least-significant bits of 'rem_Z' are needed
    | only for square roots.)
   *------------------------------------------------------------------------*/
  val rem_Z = Reg(UInt((sigWidth + 2).W))
  val notZeroRem_Z = Reg(Bool())
  val sigX_Z = Reg(UInt((sigWidth + 2).W))

  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  val rawA_S = io.a
  val rawB_S = io.b

//*** IMPROVE THESE:
  val notSigNaNIn_invalidExc_S_div =
    (rawA_S.isZero && rawB_S.isZero) || (rawA_S.isInf && rawB_S.isInf)
  val notSigNaNIn_invalidExc_S_sqrt =
    !rawA_S.isNaN && !rawA_S.isZero && rawA_S.sign
  val majorExc_S =
    Mux(
      io.sqrtOp,
      isSigNaNRawFloat(rawA_S) || notSigNaNIn_invalidExc_S_sqrt,
      isSigNaNRawFloat(rawA_S) || isSigNaNRawFloat(rawB_S) ||
        notSigNaNIn_invalidExc_S_div ||
        (!rawA_S.isNaN && !rawA_S.isInf && rawB_S.isZero)
    )
  val isNaN_S =
    Mux(
      io.sqrtOp,
      rawA_S.isNaN || notSigNaNIn_invalidExc_S_sqrt,
      rawA_S.isNaN || rawB_S.isNaN || notSigNaNIn_invalidExc_S_div
    )
  val isInf_S = Mux(io.sqrtOp, rawA_S.isInf, rawA_S.isInf || rawB_S.isZero)
  val isZero_S = Mux(io.sqrtOp, rawA_S.isZero, rawA_S.isZero || rawB_S.isInf)
  val sign_S = rawA_S.sign ^ (!io.sqrtOp && rawB_S.sign)

  val specialCaseA_S = rawA_S.isNaN || rawA_S.isInf || rawA_S.isZero
  val specialCaseB_S = rawB_S.isNaN || rawB_S.isInf || rawB_S.isZero
  val normalCase_S_div = !specialCaseA_S && !specialCaseB_S
  val normalCase_S_sqrt = !specialCaseA_S && !rawA_S.sign
  val normalCase_S = Mux(io.sqrtOp, normalCase_S_sqrt, normalCase_S_div)

  val sExpQuot_S_div =
    rawA_S.sExp +&
      Cat(rawB_S.sExp(expWidth), ~rawB_S.sExp(expWidth - 1, 0)).asSInt
//*** IS THIS OPTIMAL?:
  val sSatExpQuot_S_div =
    Cat(
      Mux(
        ((BigInt(7) << (expWidth - 2)).S <= sExpQuot_S_div),
        6.U,
        sExpQuot_S_div(expWidth + 1, expWidth - 2)
      ),
      sExpQuot_S_div(expWidth - 3, 0)
    ).asSInt

  val evenSqrt_S = io.sqrtOp && !rawA_S.sExp(0)
  val oddSqrt_S = io.sqrtOp && rawA_S.sExp(0)

  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  val idle = cycleNum === 0.U
  val entering = inReady && io.inValid
  val entering_normalCase = entering && normalCase_S

  val processTwoBits =
    cycleNum >= 3.U && ((options & divSqrtOpt_twoBitsPerCycle) != 0).B
  val skipCycle2 = cycleNum === 3.U && sigX_Z(
    sigWidth + 1
  ) && ((options & divSqrtOpt_twoBitsPerCycle) == 0).B

  when(!idle || entering) {
    def computeCycleNum(f: UInt => UInt): UInt = {
      Mux(entering & !normalCase_S, f(1.U), 0.U) |
        Mux(
          entering_normalCase,
          Mux(
            io.sqrtOp,
            Mux(rawA_S.sExp(0), f(sigWidth.U), f((sigWidth + 1).U)),
            f((sigWidth + 2).U)
          ),
          0.U
        ) |
        Mux(
          !entering && !skipCycle2,
          f(cycleNum - Mux(processTwoBits, 2.U, 1.U)),
          0.U
        ) |
        Mux(skipCycle2, f(1.U), 0.U)
    }

    inReady := computeCycleNum(_ <= 1.U).asBool
    rawOutValid := computeCycleNum(_ === 1.U).asBool
    cycleNum := computeCycleNum(x => x)
  }

  io.inReady := inReady

  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  when(entering) {
    sqrtOp_Z := io.sqrtOp
    majorExc_Z := majorExc_S
    isNaN_Z := isNaN_S
    isInf_Z := isInf_S
    isZero_Z := isZero_S
    sign_Z := sign_S
    sExp_Z :=
      Mux(
        io.sqrtOp,
        (rawA_S.sExp >> 1) +& (BigInt(1) << (expWidth - 1)).S,
        sSatExpQuot_S_div
      )
    roundingMode_Z := io.roundingMode
  }
  when(entering || !inReady && sqrtOp_Z) {
    fractB_Z :=
      Mux(inReady && !io.sqrtOp, rawB_S.sig(sigWidth - 2, 0) << 1, 0.U) |
        Mux(
          inReady && io.sqrtOp && rawA_S.sExp(0),
          (BigInt(1) << (sigWidth - 2)).U,
          0.U
        ) |
        Mux(
          inReady && io.sqrtOp && !rawA_S.sExp(0),
          (BigInt(1) << (sigWidth - 1)).U,
          0.U
        ) |
        Mux(!inReady /* sqrtOp_Z */ && processTwoBits, fractB_Z >> 2, 0.U) |
        Mux(!inReady /* sqrtOp_Z */ && !processTwoBits, fractB_Z >> 1, 0.U)
  }

  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  val rem =
    Mux(inReady && !oddSqrt_S, rawA_S.sig << 1, 0.U) |
      Mux(
        inReady && oddSqrt_S,
        Cat(
          rawA_S.sig(sigWidth - 1, sigWidth - 2) - 1.U,
          rawA_S.sig(sigWidth - 3, 0) << 3
        ),
        0.U
      ) |
      Mux(!inReady, rem_Z << 1, 0.U)
  val bitMask = (1.U << cycleNum) >> 2
  val trialTerm =
    Mux(inReady && !io.sqrtOp, rawB_S.sig << 1, 0.U) |
      Mux(inReady && evenSqrt_S, (BigInt(1) << sigWidth).U, 0.U) |
      Mux(inReady && oddSqrt_S, (BigInt(5) << (sigWidth - 1)).U, 0.U) |
      Mux(!inReady, fractB_Z, 0.U) |
      Mux(!inReady && !sqrtOp_Z, 1.U << sigWidth, 0.U) |
      Mux(!inReady && sqrtOp_Z, sigX_Z << 1, 0.U)
  val trialRem = rem.zext -& trialTerm.zext
  val newBit = (0.S <= trialRem)

  val nextRem_Z = Mux(newBit, trialRem.asUInt, rem)(sigWidth + 1, 0)
  val rem2 = nextRem_Z << 1
  val trialTerm2_newBit0 =
    Mux(sqrtOp_Z, fractB_Z >> 1 | sigX_Z << 1, fractB_Z | (1.U << sigWidth))
  val trialTerm2_newBit1 =
    trialTerm2_newBit0 | Mux(sqrtOp_Z, fractB_Z << 1, 0.U)
  val trialRem2 =
    Mux(
      newBit,
      (trialRem << 1) - trialTerm2_newBit1.zext,
      (rem_Z << 2)(sigWidth + 2, 0).zext - trialTerm2_newBit0.zext
    )
  val newBit2 = (0.S <= trialRem2)
  val nextNotZeroRem_Z = Mux(inReady || newBit, trialRem =/= 0.S, notZeroRem_Z)
  val nextNotZeroRem_Z_2 = // <-> Mux(newBit2, trialRem2 =/= 0.S, nextNotZeroRem_Z)
    processTwoBits && newBit && (0.S < (trialRem << 1) - trialTerm2_newBit1.zext) ||
      processTwoBits && !newBit && (0.S < (rem_Z << 2)(
        sigWidth + 2,
        0
      ).zext - trialTerm2_newBit0.zext) ||
      !(processTwoBits && newBit2) && nextNotZeroRem_Z
  val nextRem_Z_2 =
    Mux(processTwoBits && newBit2, trialRem2.asUInt(sigWidth + 1, 0), 0.U) |
      Mux(processTwoBits && !newBit2, rem2(sigWidth + 1, 0), 0.U) |
      Mux(!processTwoBits, nextRem_Z, 0.U)

  when(entering || !inReady) {
    notZeroRem_Z := nextNotZeroRem_Z_2
    rem_Z := nextRem_Z_2
    sigX_Z :=
      Mux(inReady && !io.sqrtOp, newBit << (sigWidth + 1), 0.U) |
        Mux(inReady && io.sqrtOp, (BigInt(1) << sigWidth).U, 0.U) |
        Mux(inReady && oddSqrt_S, newBit << (sigWidth - 1), 0.U) |
        Mux(!inReady, sigX_Z, 0.U) |
        Mux(!inReady && newBit, bitMask, 0.U) |
        Mux(processTwoBits && newBit2, bitMask >> 1, 0.U)
  }

  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  io.rawOutValid_div := rawOutValid && !sqrtOp_Z
  io.rawOutValid_sqrt := rawOutValid && sqrtOp_Z
  io.roundingModeOut := roundingMode_Z
  io.invalidExc := majorExc_Z && isNaN_Z
  io.infiniteExc := majorExc_Z && !isNaN_Z
  io.rawOut.isNaN := isNaN_Z
  io.rawOut.isInf := isInf_Z
  io.rawOut.isZero := isZero_Z
  io.rawOut.sign := sign_Z
  io.rawOut.sExp := sExp_Z
  io.rawOut.sig := sigX_Z << 1 | notZeroRem_Z

  // add flush logic
  when(io.flush) {
    cycleNum := 0.U
    inReady := true.B
    rawOutValid := false.B
  }
}

/*----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/

class DivSqrtRecFNToRaw_small(expWidth: Int, sigWidth: Int, options: Int)
    extends Module {
  val io = IO(new Bundle {
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    val inReady = Output(Bool())
    val inValid = Input(Bool())
    val sqrtOp = Input(Bool())
    val a = Input(UInt((expWidth + sigWidth + 1).W))
    val b = Input(UInt((expWidth + sigWidth + 1).W))
    val roundingMode = Input(UInt(3.W))
    val flush = Input(Bool())
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    val rawOutValid_div = Output(Bool())
    val rawOutValid_sqrt = Output(Bool())
    val roundingModeOut = Output(UInt(3.W))
    val invalidExc = Output(Bool())
    val infiniteExc = Output(Bool())
    val rawOut = Output(new RawFloat(expWidth, sigWidth + 2))
  })

  val divSqrtRawFN =
    Module(new DivSqrtRawFN_small(expWidth, sigWidth, options))

  io.inReady := divSqrtRawFN.io.inReady
  divSqrtRawFN.io.inValid := io.inValid
  divSqrtRawFN.io.sqrtOp := io.sqrtOp
  divSqrtRawFN.io.a := rawFloatFromRecFN(expWidth, sigWidth, io.a)
  divSqrtRawFN.io.b := rawFloatFromRecFN(expWidth, sigWidth, io.b)
  divSqrtRawFN.io.roundingMode := io.roundingMode
  divSqrtRawFN.io.flush := io.flush

  io.rawOutValid_div := divSqrtRawFN.io.rawOutValid_div
  io.rawOutValid_sqrt := divSqrtRawFN.io.rawOutValid_sqrt
  io.roundingModeOut := divSqrtRawFN.io.roundingModeOut
  io.invalidExc := divSqrtRawFN.io.invalidExc
  io.infiniteExc := divSqrtRawFN.io.infiniteExc
  io.rawOut := divSqrtRawFN.io.rawOut

}

/*----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/

class DivSqrtRecFN_small(expWidth: Int, sigWidth: Int, options: Int)
    extends Module {
  val io = IO(new Bundle {
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    val inReady = Output(Bool())
    val inValid = Input(Bool())
    val sqrtOp = Input(Bool())
    val a = Input(UInt((expWidth + sigWidth + 1).W))
    val b = Input(UInt((expWidth + sigWidth + 1).W))
    val roundingMode = Input(UInt(3.W))
    val detectTininess = Input(UInt(1.W))
    val flush = Input(Bool())
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    val outValid_div = Output(Bool())
    val outValid_sqrt = Output(Bool())
    val out = Output(UInt((expWidth + sigWidth + 1).W))
    val exceptionFlags = Output(UInt(5.W))
  })

  //------------------------------------------------------------------------
  //------------------------------------------------------------------------
  val divSqrtRecFNToRaw =
    Module(new DivSqrtRecFNToRaw_small(expWidth, sigWidth, options))

  io.inReady := divSqrtRecFNToRaw.io.inReady
  divSqrtRecFNToRaw.io.inValid := io.inValid
  divSqrtRecFNToRaw.io.sqrtOp := io.sqrtOp
  divSqrtRecFNToRaw.io.a := io.a
  divSqrtRecFNToRaw.io.b := io.b
  divSqrtRecFNToRaw.io.roundingMode := io.roundingMode
  divSqrtRecFNToRaw.io.flush := io.flush

  //------------------------------------------------------------------------
  //------------------------------------------------------------------------
  io.outValid_div := divSqrtRecFNToRaw.io.rawOutValid_div
  io.outValid_sqrt := divSqrtRecFNToRaw.io.rawOutValid_sqrt

  val roundRawFNToRecFN =
    Module(new RoundRawFNToRecFN(expWidth, sigWidth, 0))
  roundRawFNToRecFN.io.invalidExc := divSqrtRecFNToRaw.io.invalidExc
  roundRawFNToRecFN.io.infiniteExc := divSqrtRecFNToRaw.io.infiniteExc
  roundRawFNToRecFN.io.in := divSqrtRecFNToRaw.io.rawOut
  roundRawFNToRecFN.io.roundingMode := divSqrtRecFNToRaw.io.roundingModeOut
  roundRawFNToRecFN.io.detectTininess := io.detectTininess
  io.out := roundRawFNToRecFN.io.out
  io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags

}
