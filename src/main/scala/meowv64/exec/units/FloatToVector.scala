package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

class FloatToVectorExt(implicit val coredef: CoreDef) extends Bundle {
  val res = Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W))
}

class FloatToVector(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new FloatToVectorExt, coredef.REG_VEC) {
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[FloatToVectorExt]
  ): (FloatToVectorExt, Bool) = {
    val ext = Wire(new FloatToVectorExt)
    for (i <- 0 until coredef.vectorBankCount) {
      ext.res(i) := 0.U
    }

    val simm = Wire(SInt(coredef.XLEN.W))
    simm := pipe.instr.instr.simm5()

    val rs1Elements = Wire(Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W)))
    rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)
    val rs2Elements = Wire(Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W)))
    rs2Elements := pipe.rs2val.asTypeOf(rs1Elements)

    switch(pipe.instr.instr.funct6) {
      is(Decoder.VP_FUNC("VMV_S")) {
        switch(pipe.instr.instr.funct3) {
          is(5.U) {
            // VFMV_S_F
            ext.res(0) := rs1Elements(0)
          }
        }
      }
      is(Decoder.VP_FUNC("VMV_V")) {
        switch(pipe.instr.instr.funct3) {
          is(5.U) {
            // VFMV_V_F
            for (i <- 0 until coredef.vectorBankCount) {
              ext.res(i) := rs1Elements(0)
            }
          }
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: FloatToVectorExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))
    info.wb := Cat(ext.res.reverse)

    info
  }

  init()
}
