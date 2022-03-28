package meowv64.exec

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.MixedVec
import meowv64.core.CoreDef
import meowv64.core.PortInfo
import meowv64.core.RegInfo
import meowv64.reg.RegReader
import meowv64.reg.RegType

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

    val flush = Input(Bool())

    val toRegFile = new Bundle {
      val reader = MixedVec(
        for (regType <- portInfo.operandTypes.flatMap(x => x))
          yield new Bundle {
            val port = new RegReader(
              coredef.REG_MAPPING(regType).width,
              coredef.REG_MAPPING(regType).physRegs
            )
          }
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
    VecInit(s0Instr.rs1Phys, s0Instr.rs2Phys, s0Instr.rs3Phys, s0Instr.vmPhys)
  )
  val ty = WireInit(
    VecInit(
      s0Instr.instr.instr.getRs1Type(),
      s0Instr.instr.instr.getRs2Type(),
      s0Instr.instr.instr.getRs3Type(),
      RegType.vector
    )
  )

  // assign addr
  var j = 0
  for ((regTypes, i) <- portInfo.operandTypes.zipWithIndex) {
    for (_ <- regTypes) {
      io.toRegFile.reader(j).port.addr := phys(i)
      j += 1
    }
  }

  val numOperands = portInfo.operandTypes.length
  val op = WireInit(VecInit.fill(numOperands)(0.U(regInfo.width.W)))

  j = 0
  for ((regTypes, i) <- portInfo.operandTypes.zipWithIndex) {
    op(i) := 0.U
    for (regType <- regTypes) {
      // one cycle delay for ty
      when(regType === RegNext(ty(i))) {
        op(i) := io.toRegFile.reader(j).port.data
      }
      j += 1
    }
  }

  io.toUnits.instr.valid := s1Valid && ~io.flush

  // pipe logic
  val s1ToEgress = WireInit(io.toUnits.instr.fire)
  val ingressToS1 = WireInit(io.toIssueQueue.instr.instr.fire)
  io.toIssueQueue.instr.instr.ready := s1ToEgress || !s1Valid && ~io.flush
  io.toIssueQueue.instr.hasPending := s1Valid

  when(ingressToS1) {
    s1Valid := true.B

    s1Instr.instr := s0Instr.instr
    s1Instr.robIndex := s0Instr.robIndex
    s1Instr.rdPhys := s0Instr.rdPhys
    s1Instr.lsqIndex := s0Instr.lsqIndex

  }.elsewhen(s1ToEgress) {
    s1Valid := false.B
  }

  // save op in case of stall
  when(RegNext(ingressToS1)) {
    s1Instr.rs1val := op(0)
    s1Instr.rs2val := op(1)
    if (numOperands >= 3) {
      s1Instr.rs3val := op(2)
    } else {
      s1Instr.rs3val := 0.U
    }

    if (numOperands >= 4) {
      s1Instr.vmval := op(3)
    } else {
      s1Instr.vmval := 0.U
    }
  }

  io.toUnits.instr.bits := s1Instr
  // bypass register if it flows in one cycle
  when(RegNext(ingressToS1)) {
    io.toUnits.instr.bits.rs1val := op(0)
    io.toUnits.instr.bits.rs2val := op(1)
    if (numOperands >= 3) {
      io.toUnits.instr.bits.rs3val := op(2)
    } else {
      io.toUnits.instr.bits.rs3val := 0.U
    }

    if (numOperands >= 4) {
      io.toUnits.instr.bits.vmval := op(3)
    } else {
      io.toUnits.instr.bits.vmval := 0.U
    }
  }

  when(io.flush) {
    s1Valid := false.B
  }
}
