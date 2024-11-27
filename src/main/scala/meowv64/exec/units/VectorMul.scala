package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.core.VState
import meowv64.exec._

class VectorMulExt(implicit val coredef: CoreDef) extends Bundle {
  // only consider 32bit/64bit mul
  val widths = Seq(32, 64)
  val x1 = MixedVec(
    for (width <- widths) yield Vec(coredef.VLEN / width, UInt(width.W))
  )
  val x2 = MixedVec(
    for (width <- widths) yield Vec(coredef.VLEN / width, UInt(width.W))
  )

  val mid1 = MixedVec(
    for (width <- widths) yield Vec(coredef.VLEN / width, UInt(width.W))
  )
  val mid2 = MixedVec(
    for (width <- widths) yield Vec(coredef.VLEN / width, UInt(width.W))
  )

  val neg = MixedVec(
    for (width <- widths) yield Vec(coredef.VLEN / width, Bool())
  )

  val res = UInt(coredef.VLEN.W)
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

    for ((i, sew, width) <- Seq((0, 2, 32), (1, 3, 64))) {
      val lanes = coredef.VLEN / width

      val rs1Elements = Wire(Vec(lanes, UInt(width.W)))
      rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)
      val rs2Elements = Wire(Vec(lanes, UInt(width.W)))
      rs2Elements := pipe.rs2val.asTypeOf(rs2Elements)

      when(vState.vtype.vsew === sew.U) {
        for (lane <- 0 until lanes) {
          if (stage == 0) {
            // Pipelining requests

            val (op1, op2) =
              (Wire(SInt(width.W)), Wire(SInt(width.W)))

            when(pipe.instr.instr.funct3 === 0x6.U) {
              // vmul.vx
              op1 := rs1Elements(0).asSInt
            }.otherwise {
              // vmul.vv
              op1 := rs1Elements(lane).asSInt
            }
            op2 := rs2Elements(lane).asSInt

            ext.neg(i)(lane) := DontCare
            ext.x1(i)(lane) := DontCare
            ext.x2(i)(lane) := DontCare

            switch(pipe.instr.instr.funct6) {
              is(0x25.U) {
                // vmul
                ext.neg(i)(lane) := false.B
                ext.x1(i)(lane) := op1.asUInt
                ext.x2(i)(lane) := op2.asUInt
              }

              is(0x27.U) {
                // vmulh
                ext.neg(i)(lane) := op1(width - 1) ^ op2(width - 1)
                ext.x1(i)(lane) := op1.abs.asUInt
                ext.x2(i)(lane) := op2.abs.asUInt
              }

              is(0x24.U) {
                // vmulhu
                ext.neg(i)(lane) := false.B
                ext.x1(i)(lane) := op1.asUInt
                ext.x2(i)(lane) := op2.asUInt
              }

              is(0x26.U) {
                // vmulhsu
                // signed vs2, not rs1 in mulhsu
                ext.neg(i)(lane) := op2(width - 1)
                ext.x1(i)(lane) := op1.asUInt
                ext.x2(i)(lane) := op2.abs.asUInt
              }
            }

            ext.mid1(i)(lane) := DontCare
            ext.mid2(i)(lane) := DontCare

          } else if (stage == 1) {
            // printf(p"[MUL  0]: COMP ${Hexadecimal(pipe.rs1val)} * ${Hexadecimal(pipe.rs2val)}\n")
            val prev = _ext.get
            ext.neg(i)(lane) := prev.neg(i)(lane)

            ext.x1(i)(lane) := prev.x1(i)(lane)(width / 2 - 1, 0) *
              prev.x2(i)(lane)(width / 2 - 1, 0)
            ext.mid1(i)(lane) := prev.x1(i)(lane)(width - 1, width / 2) *
              prev.x2(i)(lane)(width / 2 - 1, 0)
            ext.mid2(i)(lane) := prev.x1(i)(lane)(width / 2 - 1, 0) *
              prev.x2(i)(lane)(width - 1, width / 2)
            ext.x2(i)(lane) := prev.x1(i)(lane)(width - 1, width / 2) *
              prev.x2(i)(lane)(width - 1, width / 2)

          } else if (stage == 2) {
            val prev = _ext.get

            val added = Wire(UInt((width * 2).W))
            added := prev.x1(i)(lane) +&
              ((prev.mid1(i)(lane) +& prev.mid2(i)(lane)) << (width / 2)) +&
              (prev.x2(i)(lane) << width)
            when(pipe.instr.instr.funct6 === 0x25.U) {
              // vmul
              ext.x1(i)(lane) := added(width - 1, 0)
            }.otherwise {
              val signed = added.asSInt

              when(prev.neg(i)(lane)) {
                ext.x1(i)(lane) := (-signed).asUInt(2 * width - 1, width)
              }.otherwise {
                ext.x1(i)(lane) := signed(2 * width - 1, width)
              }
            }
            ext.res := Cat(ext.x1(i).reverse)
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

    info.wb := ext.res

    info
  }

  init()
}
