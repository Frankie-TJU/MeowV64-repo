package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._
import meowv64.instr.Decoder
import meowv64.core.VState

class VectorMulExt(implicit val coredef: CoreDef) extends Bundle {
  // only consider 32bit/64bit mul
  val maxLanes = coredef.VLEN / 32
  val x1 = Vec(maxLanes, UInt(coredef.XLEN.W))
  val x2 = Vec(maxLanes, UInt(coredef.XLEN.W))

  val mid1 = Vec(maxLanes, UInt(coredef.XLEN.W))
  val mid2 = Vec(maxLanes, UInt(coredef.XLEN.W))

  val neg = Vec(maxLanes, Bool())
}

class VectorMul(override implicit val coredef: CoreDef)
    extends ExecUnit(2, new VectorMulExt, coredef.REG_VEC)
    with WithVState {

  val vState = IO(Input(new VState()))

  assert(coredef.XLEN == 64)

  override def map(
      stage: Int,
      pipe: PipeInstr,
      _ext: Option[VectorMulExt]
  ): (VectorMulExt, Bool) = {
    val ext = Wire(new VectorMulExt)
    ext := 0.U.asTypeOf(ext)

    for ((sew, width) <- Seq((2, 32), (3, 64))) {
      val lanes = coredef.VLEN / width

      val rs1Elements = Wire(Vec(lanes, UInt(width.W)))
      rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)
      val rs2Elements = Wire(Vec(lanes, UInt(width.W)))
      rs2Elements := pipe.rs2val.asTypeOf(rs2Elements)

      val isDWord = vState.vtype.vsew === 3.U
      when(vState.vtype.vsew === sew.U) {
        for (i <- 0 until lanes) {
          if (stage == 0) {
            // Pipelining requests

            val (op1, op2) =
              (Wire(SInt(coredef.XLEN.W)), Wire(SInt(coredef.XLEN.W)))

            when(pipe.instr.instr.funct3 === 0x6.U) {
              // vmul.vx
              op1 := rs1Elements(0).asSInt
            }.otherwise {
              // vmul.vv
              op1 := rs1Elements(i).asSInt
            }
            op2 := rs2Elements(i).asSInt

            when(isDWord) {
              ext.neg(i) := DontCare
              ext.x1(i) := DontCare
              ext.x2(i) := DontCare

              switch(pipe.instr.instr.funct6) {
                is(0x25.U) {
                  // vmul
                  ext.neg(i) := false.B
                  ext.x1(i) := op1.asUInt
                  ext.x2(i) := op2.asUInt
                }

                is(0x27.U) {
                  // vmulh
                  ext.neg(i) := op1(63) ^ op2(63)
                  ext.x1(i) := op1.abs().asUInt
                  ext.x2(i) := op2.abs().asUInt
                }

                is(0x24.U) {
                  // vmulhu
                  ext.neg(i) := false.B
                  ext.x1(i) := op1.asUInt
                  ext.x2(i) := op2.asUInt
                }

                is(0x26.U) {
                  // vmulhsu
                  ext.neg(i) := op1(63)
                  ext.x1(i) := op1.abs().asUInt
                  ext.x2(i) := op2.asUInt
                }
              }
            }.otherwise {
              ext.neg(i) := DontCare
              ext.x1(i) := op1.asUInt
              ext.x2(i) := op2.asUInt
            }

            ext.mid1(i) := DontCare
            ext.mid2(i) := DontCare

          } else if (stage == 1) {
            // printf(p"[MUL  0]: COMP ${Hexadecimal(pipe.rs1val)} * ${Hexadecimal(pipe.rs2val)}\n")
            val prev = _ext.get
            ext.neg(i) := prev.neg(i)

            ext.x1(i) := prev.x1(i)(31, 0) * prev.x2(i)(31, 0)
            ext.mid1(i) := prev.x1(i)(63, 32) * prev.x2(i)(31, 0)
            ext.mid2(i) := prev.x1(i)(31, 0) * prev.x2(i)(63, 32)
            ext.x2(i) := prev.x1(i)(63, 32) * prev.x2(i)(63, 32)

          } else if (stage == 2) {
            val prev = _ext.get

            when(!isDWord) {
              // Can only be MULW
              val extended = Wire(SInt(coredef.XLEN.W))
              extended := prev.x1(i)(31, 0).asSInt
              ext.x1(i) := extended.asUInt()
            }.otherwise {
              val added = Wire(UInt((coredef.XLEN * 2).W))
              added := prev.x1(i) +& ((prev.mid1(i) +& prev.mid2(i)) << 32) +& (prev.x2(i) << 64)
              when(pipe.instr.instr.funct3 === Decoder.MULDIV_FUNC("MUL")) {
                ext.x1(i) := added(63, 0)
              }.otherwise {
                val signed = added.asSInt

                when(prev.neg(i)) {
                  ext.x1(i) := (-signed).asUInt()(127, 64)
                }.otherwise {
                  ext.x1(i) := signed(127, 64)
                }
              }
            }
          } else {
            throw new Error(s"Unexpected stage $stage in Mul module")
          }
        }
      }

    }

    (ext, false.B)
  }

  override def finalize(pipe: PipeInstr, ext: VectorMulExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))

    info.wb := Cat(ext.x1.reverse)

    info
  }

  init()
}
