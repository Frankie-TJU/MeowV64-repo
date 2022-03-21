package meowv64.exec

import chisel3._
import chisel3.util.Decoupled
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
      val reader = Vec(
        portInfo.readPorts,
        new RegReader(
          regInfo.width,
          regInfo.physRegs
        )
      )
    }

    val toUnits = new RegisterReadEgress(regInfo)
  })

  // stage 0: read from register file
  val s0Instr = WireInit(io.toIssueQueue.instr.instr.bits)
  // stage 1: pass to execution unit
  val s1Instr = Reg(new PipeInstr(regInfo))
  val s1Valid = RegInit(false.B)

  val phys = WireInit(
    VecInit(s0Instr.rs1Phys, s0Instr.rs2Phys, s0Instr.rs3Phys)
  )
  for (i <- 0 until portInfo.readPorts) {
    io.toRegFile.reader(i).addr := phys(i)
  }

  val op = WireInit(VecInit.fill(portInfo.readPorts)(0.U(regInfo.width.W)))
  for (i <- 0 until portInfo.readPorts) {
    op(i) := io.toRegFile.reader(i).data
  }

  io.toUnits.instr.valid := s1Valid

  // pipe logic
  val s1ToEgress = WireInit(io.toUnits.instr.fire)
  val ingressToS1 = WireInit(io.toIssueQueue.instr.instr.fire)
  io.toIssueQueue.instr.instr.ready := s1ToEgress || !s1Valid

  when(ingressToS1) {
    s1Valid := true.B

    s1Instr.instr := s0Instr.instr
    s1Instr.robIndex := s0Instr.robIndex
    s1Instr.rdPhys := s0Instr.rdPhys

  }.elsewhen(s1ToEgress) {
    s1Valid := false.B
  }

  // save op in case of stall
  when(RegNext(ingressToS1)) {
    s1Instr.rs1val := op(0)
    s1Instr.rs2val := op(1)
    if (portInfo.readPorts > 2) {
      s1Instr.rs3val := op(2)
    } else {
      s1Instr.rs3val := 0.U
    }
  }

  io.toUnits.instr.bits := s1Instr
  // bypass register if it flows in one cycle
  when(RegNext(ingressToS1)) {
    io.toUnits.instr.bits.rs1val := op(0)
    io.toUnits.instr.bits.rs2val := op(1)
    if (portInfo.readPorts > 2) {
      io.toUnits.instr.bits.rs3val := op(2)
    } else {
      io.toUnits.instr.bits.rs3val := 0.U
    }
  }
}
