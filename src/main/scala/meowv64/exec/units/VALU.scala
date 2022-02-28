package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder

class VALUExt(implicit val coredef: CoreDef) extends Bundle {
  val res = Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W))
}

class VALU(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new VALUExt) {
  override def valueWidth = coredef.VLEN
  override def retireWidth = coredef.VLEN
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[VALUExt]
  ): (VALUExt, Bool) = {
    val ext = Wire(new VALUExt)
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
          is(1.U, 2.U) {
            // VFMV_F_S/VMV_X_S
            // TODO: retain other lanes
            ext.res(0) := pipe.rs2val
          }
          is(5.U, 6.U) {
            // VFMV_S_F/VMV_S_X
            ext.res(0) := rs1Elements(0)
          }
        }
      }
      is(Decoder.VP_FUNC("VMV_V")) {
        switch(pipe.instr.instr.funct3) {
          is(0.U) {
            // VMV_V_V
            ext.res := rs1Elements
          }
          is(3.U) {
            // VMV_V_I
            for (i <- 0 until coredef.vectorBankCount) {
              ext.res(i) := simm.asUInt
            }
          }
          is(4.U, 5.U) {
            // VMV_V_X/VFMV_V_F
            for (i <- 0 until coredef.vectorBankCount) {
              ext.res(i) := pipe.rs1val
            }
          }
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VALUExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(retireWidth))
    info.wb := Cat(ext.res.reverse)

    info
  }

  init()
}
