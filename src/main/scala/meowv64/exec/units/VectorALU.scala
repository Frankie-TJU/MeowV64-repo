package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

class VectorALUExt(implicit val coredef: CoreDef) extends Bundle {
  val res = Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W))
}

class VectorALU(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new VectorALUExt) {
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[VectorALUExt]
  ): (VectorALUExt, Bool) = {
    val ext = Wire(new VectorALUExt)
    for (i <- 0 until coredef.vectorBankCount) {
      ext.res(i) := 0.U
    }

    val simm = Wire(SInt(coredef.XLEN.W))
    simm := pipe.instr.instr.simm5()

    val rs1Elements = Wire(Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W)))
    rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)
    val rs2Elements = Wire(Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W)))
    rs2Elements := pipe.rs2val.asTypeOf(rs2Elements)

    switch(pipe.instr.instr.funct6) {
      is(Decoder.VP_FUNC("VADD")) {
        switch(pipe.instr.instr.funct3) {
          is(0.U) {
            // VADD_VV
            for (i <- 0 until coredef.vectorBankCount) {
              ext.res(i) := rs1Elements(i) + rs2Elements(i)
            }
          }
          is(3.U) {
            // VADD_VI
            for (i <- 0 until coredef.vectorBankCount) {
              ext.res(i) := simm.asUInt + rs2Elements(i)
            }
          }
          is(4.U) {
            // VADD_VX
            for (i <- 0 until coredef.vectorBankCount) {
              ext.res(i) := rs1Elements(0) + rs2Elements(i)
            }
          }
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VectorALUExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))
    info.wb := Cat(ext.res.reverse)

    info
  }

  init()
}
