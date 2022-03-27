package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._

class VectorToMemExt(implicit val coredef: CoreDef) extends Bundle {}

/** Collect data for vse.v
  */
class VectorToMem(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new VectorToMemExt, coredef.REG_VEC)
    with WithVectorToMem {

  val toMem = IO(Valid(new VectorToMemReq))
  toMem.valid := false.B
  toMem.bits := DontCare

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[VectorToMemExt]
  ): (VectorToMemExt, Bool) = {
    val ext = Wire(new VectorToMemExt)

    toMem.valid := true.B
    toMem.bits.data := pipe.rs2val
    toMem.bits.lsqIdx := pipe.lsqIndex

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VectorToMemExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))

    info
  }

  init()

  // never retires
  io.retiredInstr.valid := false.B
}
