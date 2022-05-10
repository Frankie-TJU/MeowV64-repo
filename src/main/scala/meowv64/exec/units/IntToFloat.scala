package meowv64.exec.units

import chisel3._
import meowv64.core.CoreDef
import meowv64.core.FloatS
import meowv64.exec._
import meowv64.instr.Decoder

class IntToFloatExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.XLEN.W)

  val updateFFlags = Bool()
  val fflags = UInt(5.W)
}

/** Handles instructions: FMV.D.X, FMV.W.X
  */
class IntToFloat(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new IntToFloatExt, coredef.REG_FLOAT) {

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[IntToFloatExt]
  ): (IntToFloatExt, Bool) = {
    val ext = Wire(new IntToFloatExt)
    ext.res := 0.U
    ext.updateFFlags := false.B
    ext.fflags := 0.U

    when(
      pipe.instr.instr.funct5 === Decoder.FP_FUNC(
        "FMV.D/W.X"
      ) && pipe.instr.instr.funct3 === 0.U
    ) {
      when(pipe.instr.instr.funct7(1, 0) === 0.U) {
        // fmv.w.x
        // nan boxing
        ext.res := FloatS.box(pipe.rs1val(31, 0), coredef.XLEN)
      }.otherwise {
        // fmv.d.x
        ext.res := pipe.rs1val
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: IntToFloatExt): RetireInfo = {
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
