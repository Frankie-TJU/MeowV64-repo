package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.exec._

class FloatToMemExt(implicit val coredef: CoreDef) extends Bundle {}

/** Collect data for fsw/fsd
  */
class FloatToMem(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new FloatToMemExt, coredef.REG_FLOAT)
    with WithFloatToMem {

  val toMem = IO(Valid(new FloatToMemReq))
  toMem.valid := false.B
  toMem.bits := DontCare

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[FloatToMemExt]
  ): (FloatToMemExt, Bool) = {
    val ext = Wire(new FloatToMemExt)

    toMem.valid := true.B
    toMem.bits.data := pipe.rs2val
    toMem.bits.lsqIdx := pipe.lsqIndex

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: FloatToMemExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))

    info
  }

  init()

  // never retires
  io.retiredInstr.valid := false.B
}
