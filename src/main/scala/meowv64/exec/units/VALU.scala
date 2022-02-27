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

    val rs1Elements = Wire(Vec(coredef.vectorBankCount, UInt(coredef.XLEN.W)))
    rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)

    switch(pipe.instr.instr.funct6) {
      is(Decoder.VP_FUNC("VMV_S")) {
        switch(pipe.instr.instr.funct3) {
          is(2.U) {
            // VMV_X_S
            ext.res(0) := pipe.rs1val
          }
          is(6.U) {
            // VMV_S_X
            ext.res(0) := rs1Elements(0)
          }
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VALUExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(retireWidth))
    info.wb := Cat(ext.res)

    info
  }

  init()
}
