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
      val reader = Vec(
        portInfo.readPorts,
        new RegReader(
          regInfo.width,
          regInfo.physicalRegs
        )
      )
    }

    val toUnits = new RegisterReadEgress(regInfo)
  })

  // stage 1: read from register file
  val s1Instr = Reg(new IssueQueueInstr())
  val s1Valid = RegInit(false.B)

  val phys = WireInit(
    VecInit(s1Instr.rs1Phys, s1Instr.rs2Phys, s1Instr.rs3Phys)
  )
  for (i <- 0 until portInfo.readPorts) {
    io.toRegFile.reader(i).addr := phys(i)
  }

  val op = WireInit(VecInit.fill(portInfo.readPorts)(0.U(regInfo.width.W)))
  for (i <- 0 until portInfo.readPorts) {
    op(i) := io.toRegFile.reader(i).data
  }

  // stage 2: pass to execution unit
  val s2Instr = Reg(new PipeInstr(regInfo))
  val s2Valid = RegInit(false.B)
  io.toUnits.instr.bits := s2Instr
  io.toUnits.instr.valid := s2Valid

  // pipe logic
  val s2ToEgress = WireInit(io.toUnits.instr.fire)
  val s1ToS2 = WireInit(s1Valid && (s2ToEgress || !s2Valid))
  val ingressToS1 = io.toIssueQueue.instr.instr.valid
  io.toIssueQueue.instr.instr.ready := s1ToS2 || !s1Valid

  when(ingressToS1) {
    s1Valid := true.B
    s1Instr := io.toIssueQueue.instr.instr.bits
  }.elsewhen(s1ToS2) {
    s1Valid := false.B
  }

  when(s1ToS2) {
    s2Valid := true.B
    s2Instr.instr := s1Instr.instr
    s2Instr.robIndex := s1Instr.robIndex
    s2Instr.rdPhys := s1Instr.rdPhys

    s2Instr.rs1val := op(0)
    s2Instr.rs2val := op(1)
    if (portInfo.readPorts > 2) {
      s2Instr.rs3val := op(2)
    } else {
      s2Instr.rs3val := 0.U
    }
  }.elsewhen(s2ToEgress) {
    s2Valid := false.B
  }
}
