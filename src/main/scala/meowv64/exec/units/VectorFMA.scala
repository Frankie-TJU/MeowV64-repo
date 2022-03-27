package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._

class VectorFMAExt(implicit val coredef: CoreDef) extends Bundle {
  val res = Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W))
}

class VectorFMA(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new VectorFMAExt, coredef.REG_VEC) {
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[VectorFMAExt]
  ): (VectorFMAExt, Bool) = {
    val ext = Wire(new VectorFMAExt)
    for (i <- 0 until coredef.vectorBankCount) {
      ext.res(i) := 0.U
    }

    val simm = Wire(SInt(coredef.XLEN.W))
    simm := pipe.instr.instr.simm5()

    val rs1Elements = Wire(Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W)))
    rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)
    val rs2Elements = Wire(Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W)))
    rs2Elements := pipe.rs2val.asTypeOf(rs2Elements)

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VectorFMAExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))
    info.wb := Cat(ext.res.reverse)

    info
  }

  init()
}
