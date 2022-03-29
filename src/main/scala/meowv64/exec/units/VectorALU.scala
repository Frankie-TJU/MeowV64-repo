package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.core.VState
import meowv64.exec._
import meowv64.instr.Decoder

class VectorALUExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.VLEN.W)
}

class VectorALU(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new VectorALUExt, coredef.REG_VEC)
    with WithVState {

  val vState = IO(Input(new VState))
  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[VectorALUExt]
  ): (VectorALUExt, Bool) = {
    val ext = Wire(new VectorALUExt)
    ext.res := 0.U

    for ((sew, width) <- Seq((0, 8), (1, 16), (2, 32), (3, 64))) {
      val lanes = coredef.VLEN / width
      val simm = Wire(SInt(width.W))
      simm := pipe.instr.instr.simm5()
      val rs1Elements = Wire(Vec(lanes, UInt(width.W)))
      rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)
      val rs2Elements = Wire(Vec(lanes, UInt(width.W)))
      rs2Elements := pipe.rs2val.asTypeOf(rs2Elements)
      val rs3Elements = Wire(Vec(lanes, UInt(width.W)))
      rs3Elements := pipe.rs3val.asTypeOf(rs3Elements)
      val res = WireInit(VecInit.fill(lanes)(0.U(width.W)))

      when(vState.vtype.vsew === sew.U) {
        switch(pipe.instr.instr.funct6) {
          is(Decoder.VP_FUNC("VADD_V")) {
            switch(pipe.instr.instr.funct3) {
              is(0.U) {
                // VADD_VV
                for (i <- 0 until lanes) {
                  res(i) := rs1Elements(i) + rs2Elements(i)
                }
              }
              is(3.U) {
                // VADD_VI
                for (i <- 0 until lanes) {
                  res(i) := simm.asUInt + rs2Elements(i)
                }
              }
              is(4.U) {
                // VADD_VX
                for (i <- 0 until lanes) {
                  res(i) := rs1Elements(0) + rs2Elements(i)
                }
              }
            }
          }
          is(Decoder.VP_FUNC("VSLL_V")) {
            for (i <- 0 until lanes) {
              val shiftAmount = WireInit(0.U(log2Ceil(width).W))
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VSLL.VV
                  shiftAmount := rs1Elements(i)
                }
                is(3.U) {
                  // VSLL.VI
                  shiftAmount := simm.asUInt
                }
                is(4.U) {
                  // VSLL.VX
                  shiftAmount := pipe.rs1val
                }
              }

              // check vm
              when(pipe.instr.instr.readVm() && ~pipe.vmval(i)) {
                res(i) := rs3Elements(i)
              }.otherwise {
                res(i) := rs2Elements(i) << shiftAmount
              }
            }
          }
        }

        ext.res := Cat(res.reverse)
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VectorALUExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))
    info.wb := ext.res

    info
  }

  init()
}
