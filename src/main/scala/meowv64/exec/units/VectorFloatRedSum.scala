package meowv64.exec.units

import chisel3._
import chisel3.util.Cat
import chisel3.util.log2Ceil
import hardfloat.AddRecFN
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

    val currentValue = Reg(UInt(float.widthHardfloat.W))
    val currentFFlags = Reg(UInt(5.W))
    val currentRs2ElementsHF = Reg(Vec(lanes, UInt(float.widthHardfloat.W)))
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
        a := currentValue

        when(
          currentInstr.instr.instr.readVm() && ~currentInstr.vmval(progress)
        ) {
          // masked off, do not update
          // TODO: skip this element instead
          b := 0.U
        }.otherwise {
          b := currentRs2ElementsHF(progress)
        }

        val adder = Module(new AddRecFN(float.exp(), float.sig()))
        adder.suggestName(s"addRecFN_${float.name}")
        adder.io.subOp := false.B
        adder.io.a := a
        adder.io.b := b
        adder.io.roundingMode := 0.U
        adder.io.detectTininess := hardfloat.consts.tininess_afterRounding

        currentValue := adder.io.out
        currentFFlags := currentFFlags | adder.io.exceptionFlags

        progress := progress + 1.U

        // only set lane 0 of vd
        rs3Elements(0) := float.fromHardfloat(adder.io.out)
        io.retirement.wb := Cat(rs3Elements.reverse)
        io.retirement.updateFFlags := true.B
        io.retirement.fflags := currentFFlags | adder.io.exceptionFlags
        when(progress + 1.U === vState.vl) {
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
      currentValue := float.toHardfloat(rs1Elements(0))
      for (i <- 0 until lanes) {
        currentRs2ElementsHF(i) := float.toHardfloat(rs2Elements(i))
      }
      currentFFlags := 0.U
    }
  }

  when(io.next.valid && ~busy) {
    currentInstr := io.next
    busy := true.B
    progress := 0.U
  }

  when(io.flush) {
    busy := false.B
  }
}
