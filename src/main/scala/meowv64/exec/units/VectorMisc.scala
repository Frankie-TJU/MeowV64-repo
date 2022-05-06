package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.core.VState
import meowv64.exec._
import meowv64.instr.Decoder

class VectorMiscExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.VLEN.W)
}

class VectorMisc(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new VectorMiscExt, coredef.REG_VEC)
    with WithVState {

  val vState = IO(Input(new VState()))
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[VectorMiscExt]
  ): (VectorMiscExt, Bool) = {
    val ext = Wire(new VectorMiscExt)
    ext.res := 0.U

    val curFloat = vState.vtype.floatFmt

    for ((float, idx) <- coredef.FLOAT_TYPES.zipWithIndex) {
      val lanes = coredef.VLEN / float.width()
      val width = float.width()
      val simm = Wire(SInt(width.W))
      simm := pipe.instr.instr.simm5()

      val rs1Elements = Wire(Vec(lanes, UInt(width.W)))
      rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)
      val rs2Elements = Wire(Vec(lanes, UInt(width.W)))
      rs2Elements := pipe.rs2val.asTypeOf(rs2Elements)
      val rs3Elements = Wire(Vec(lanes, UInt(width.W)))
      rs3Elements := pipe.rs3val.asTypeOf(rs3Elements)
      val res = WireInit(VecInit.fill(lanes)(0.U(width.W)))

      when(curFloat === float.fmt) {
        ext.res := Cat(res.reverse)

        switch(pipe.instr.instr.funct6) {
          is(Decoder.VP_FUNC("VMV_S")) {
            switch(pipe.instr.instr.funct3) {
              is(1.U) {
                // VFMV_F_S
                // handle nan boxing
                ext.res := float.box(pipe.rs2val, coredef.XLEN)
              }
              is(2.U) {
                // VFMV_F_S/VMV_X_S
                res(0) := pipe.rs2val
              }
              is(5.U, 6.U) {
                // VFMV_S_F/VMV_S_X
                res(0) := rs1Elements(0)
              }
            }
          }
          is(Decoder.VP_FUNC("VMV_V")) {
            switch(pipe.instr.instr.funct3) {
              is(0.U) {
                // VMV_V_V
                res := rs1Elements
              }
              is(3.U) {
                // VMV_V_I
                for (i <- 0 until lanes) {
                  res(i) := simm.asUInt
                }
              }
              is(4.U, 5.U) {
                // VMV_V_X/VFMV_V_F
                for (i <- 0 until lanes) {
                  res(i) := rs1Elements(0)
                }
              }
            }
          }
          is(Decoder.VP_FUNC("VMVR_V")) {
            switch(pipe.instr.instr.rs1) {
              is(0.U) {
                // VMV1R_V
                res := rs2Elements
              }
            }
          }
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VectorMiscExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))
    info.wb := ext.res

    info
  }

  init()
}
