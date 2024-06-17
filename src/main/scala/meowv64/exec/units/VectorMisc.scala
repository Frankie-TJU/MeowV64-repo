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
      val float = coredef.FLOAT_TYPES.find(_.width() == width)

      when(vState.vtype.vsew === sew.U) {
        ext.res := Cat(res.reverse)

        switch(pipe.instr.instr.funct6) {
          is(Decoder.VP_FUNC("VMV_S")) {
            switch(pipe.instr.instr.funct3) {
              is(1.U) {
                // VFMV_F_S
                // handle nan boxing
                float match {
                  case Some(f) => {
                    ext.res := f.box(pipe.rs2val, coredef.XLEN)
                  }
                  case None => {
                    assert(false.B, "vfmv.f.s not implemented for the width")
                  }
                }
              }
              is(2.U) {
                // VMV_X_S
                // sign extension
                val signed = Wire(SInt(width.W))
                signed := rs2Elements(0).asSInt
                val extended = Wire(SInt(coredef.XLEN.W))
                extended := signed
                ext.res := extended.asUInt
              }
              is(5.U, 6.U) {
                // VFMV_S_F/VMV_S_X
                res(0) := rs1Elements(0)
              }
            }
          }
          is(Decoder.VP_FUNC("VSLIDEUP_V")) {
            switch(pipe.instr.instr.funct3) {
              is(3.U, 4.U) {
                // vslideup.vi/vx
                // FIXME: vx
                val shift = pipe.instr.instr.rs1
                for (i <- 0 until lanes) {
                  res(i) := 0.U
                  for (j <- 0 to i) {
                    when(shift === (i - j).U) {
                      res(i) := rs2Elements(j)
                    }
                  }
                }
              }
              is(5.U, 6.U) {
                // v[f]slide1up
                res(0) := rs1Elements(0)
                for (i <- 1 until lanes) {
                  res(i) := rs2Elements(i - 1)
                }
              }
            }
          }
          is(Decoder.VP_FUNC("VSLIDEDOWN_V")) {
            switch(pipe.instr.instr.funct3) {
              is(3.U, 4.U) {
                // vslidedown.vi/vx
                val shift = pipe.instr.instr.rs1
                for (i <- 0 until lanes) {
                  res(i) := 0.U
                  for (j <- i until lanes) {
                    when(shift === (j - i).U) {
                      res(i) := rs2Elements(j)
                    }
                  }
                }
              }
              is(5.U, 6.U) {
                // v[f]slide1down
                for (i <- 0 until lanes - 1) {
                  res(i) := rs2Elements(i + 1)
                }
                res(lanes - 1) := rs1Elements(0)
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
