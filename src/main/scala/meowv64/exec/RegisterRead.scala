package meowv64.exec

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import meowv64.core.CoreDef
import meowv64.core.PortInfo
import meowv64.core.RegInfo
import meowv64.reg.RegReader

/** Register Read -> Unit Selector
  */
class RegisterReadEgress(val regInfo: RegInfo)(implicit val coredef: CoreDef)
    extends Bundle {
  val instr = Decoupled(new PipeInstr(regInfo))
}

class RegisterRead(portInfo: PortInfo)(implicit
    coredef: CoreDef
) extends Module {
  val regInfo = portInfo.regInfo
  val io = IO(new Bundle {
    val toIssueQueue = new Bundle {
      val instr = Flipped(new IssueQueueEgress())
    }

    val toRegFile = new Bundle {
      val reader = Flipped(
        new RegReader(
          regInfo.width,
          log2Ceil(regInfo.physicalRegs)
        )
      )
    }

    val toUnits = new RegisterReadEgress(regInfo)
  })
}
