package meowv64.exec.units

import chisel3._
import chisel3.util.Cat
import chisel3.util.log2Ceil
import hardfloat.AddRawFN
import hardfloat.RoundRawFNToRecFN
import hardfloat.rawFloatFromRecFN
import meowv64.core.CoreDef
import meowv64.core.VState
import meowv64.exec.ExecUnitInt
import meowv64.exec.ExecUnitPort
import meowv64.exec.PipeInstr
import meowv64.exec.RetireInfo
import meowv64.exec.WithVState

class VectorFloatRedSum(implicit val coredef: CoreDef)
    extends Module
    with ExecUnitInt
    with WithVState {
  val DEPTH = 1
  val regInfo = coredef.REG_VEC
  val io = IO(new ExecUnitPort(regInfo))
  val vState = IO(Input(new VState()))

  val currentInstr = Reg(new PipeInstr(regInfo))
  val busy = RegInit(false.B)
  val progress = Reg(UInt(log2Ceil(coredef.VLEN / 16 + 1).W))
  // add or round in this cycle
  val add = Reg(Bool())

  io.stall := busy
  io.retiredInstr := currentInstr
  io.retiredInstr.instr.valid := false.B
  io.retirement := RetireInfo.vacant(regInfo)

  val curFloat = vState.vtype.floatFmt

  // vfredosum:
  // vs1[0] + vs2[0] + vs2[1] + ... + vs2[n-1]
  // loop over floating point types
  for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
    val lanes = coredef.VLEN / float.width()

    val currentValueHF = Reg(UInt(float.widthHardfloat.W))
    val currentFFlags = Reg(UInt(5.W))
    val currentRs2ElementsHF = Reg(Vec(lanes, UInt(float.widthHardfloat.W)))
    val currentAdderB = Reg(UInt(float.widthHardfloat.W))
    when(busy) {
      val rs3Elements = Wire(
        Vec(lanes, UInt(float.width.W))
      )
      rs3Elements := currentInstr.rs3val.asTypeOf(rs3Elements)
      when(curFloat === float.fmt) {
        // compute a + b
        val a = WireInit(0.U(float.widthHardfloat.W))
        val b = WireInit(0.U(float.widthHardfloat.W))
        // currentValue + vs2[progress]
        a := currentValueHF
        b := currentAdderB

        // compute next b
        when(
          currentInstr.instr.instr.readVm() && ~currentInstr.vmval(
            progress + 1.U
          )
        ) {
          // masked off, do not update
          // TODO: skip this element instead
          currentAdderB := 0.U
        }.otherwise {
          currentAdderB := currentRs2ElementsHF(progress + 1.U)
        }

        val adder = Module(new AddRawFN(float.exp(), float.sig()))
        adder.suggestName(s"addRawFN_${float.name}")
        adder.io.subOp := false.B
        adder.io.a := rawFloatFromRecFN(float.exp(), float.sig(), a)
        adder.io.b := rawFloatFromRecFN(float.exp(), float.sig(), b)
        adder.io.roundingMode := 0.U

        val round = Module(new RoundRawFNToRecFN(float.exp(), float.sig(), 0))
        round.io.invalidExc := RegNext(adder.io.invalidExc)
        round.io.infiniteExc := false.B
        round.io.in := RegNext(adder.io.rawOut)
        round.io.roundingMode := 0.U
        round.io.detectTininess := hardfloat.consts.tininess_afterRounding

        // one cycle add, another cycle round
        add := ~add
        when(~add) {
          currentValueHF := round.io.out
          currentFFlags := currentFFlags | round.io.exceptionFlags
          progress := progress + 1.U
        }

        // one cycle delay for hardfloat conversion
        // only set lane 0 of vd
        rs3Elements(0) := float.fromHardfloat(currentValueHF)
        io.retirement.wb := Cat(rs3Elements.reverse)
        io.retirement.updateFFlags := true.B
        io.retirement.fflags := currentFFlags
        when(progress === vState.vl) {
          io.retiredInstr.instr.valid := true.B
          busy := false.B
          io.stall := false.B
          currentFFlags := 0.U
        }
      }
    }.elsewhen(io.next.valid && ~busy) {
      val rs1Elements = Wire(
        Vec(lanes, UInt(float.width.W))
      )
      rs1Elements := io.next.rs1val.asTypeOf(rs1Elements)
      val rs2Elements = Wire(
        Vec(lanes, UInt(float.width.W))
      )
      rs2Elements := io.next.rs2val.asTypeOf(rs2Elements)

      // convert ieee to hardfloat
      currentValueHF := float.toHardfloat(rs1Elements(0))
      for (i <- 0 until lanes) {
        currentRs2ElementsHF(i) := float.toHardfloat(rs2Elements(i))
      }
      currentFFlags := 0.U

      // progress = 0
      when(
        io.next.instr.instr.readVm() && ~io.next.vmval(0)
      ) {
        // masked off, do not update
        // TODO: skip this element instead
        currentAdderB := 0.U
      }.otherwise {
        currentAdderB := float.toHardfloat(rs2Elements(0))
      }
    }
  }

  when(io.next.valid && ~busy) {
    currentInstr := io.next
    busy := true.B
    progress := 0.U
    add := true.B
  }

  when(io.flush) {
    busy := false.B
  }
}
