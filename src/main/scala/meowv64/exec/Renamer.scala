package meowv64.exec

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.instr.InstrExt
import meowv64.reg.RegType

/** Release stale physical register into free list
  */
class Release(implicit val coredef: CoreDef) extends Bundle {
  val stalePhys = Input(UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W))
  val regType = Input(RegType())
}

class Renamer(implicit coredef: CoreDef) extends Module {
  val REG_NUM = 32 // TODO: do we need to make this configurable?

  val cdb = IO(Input(new CDB))

  val toExec = IO(new Bundle {
    // instruction before rename
    val input = Input(Vec(coredef.ISSUE_NUM, new InstrExt))
    val commit = Input(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))

    // instruction after rename
    val output = Output(Vec(coredef.ISSUE_NUM, new IssueQueueInstr()))
    val allowBit = Output(Vec(coredef.ISSUE_NUM, Bool()))

    // instruction retired from rob
    val releases = Vec(coredef.RETIRE_NUM, new Release)
    val retire = Input(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))

    val flush = Input(Bool())
  })

  val NAME_LENGTH = log2Ceil(coredef.INFLIGHT_INSTR_LIMIT)
  val REG_ADDR_LENGTH = log2Ceil(REG_NUM)

  // each register type has: mapping table, busy table and free list
  val banks =
    for (regInfo <- coredef.REGISTER_TYPES) yield new {
      // Map logical -> physical
      val regType = regInfo.regType
      val mapping = RegInit(
        VecInit(Seq.fill(REG_NUM)(0.U(log2Ceil(regInfo.physicalRegs).W)))
      )
      val regBusy = RegInit(0.U(REG_NUM.W))

      // 0 means free
      // P0 is hardwired to zero for x0
      // it should never be allocated
      val init = if (regInfo.fixedZero) {
        1
      } else {
        0
      }
      val freelist = RegInit(init.U(regInfo.physicalRegs.W))

      // save committed state for recovery
      val committedMapping = RegInit(
        VecInit(Seq.fill(REG_NUM)(0.U(log2Ceil(regInfo.physicalRegs).W)))
      )
      val committedFreelist = RegInit(init.U(regInfo.physicalRegs.W))
    }

  def flush() = {
    for (bank <- banks) {
      bank.mapping := bank.committedMapping
      bank.freelist := bank.committedFreelist
      // all regs are ready
      bank.regBusy := 0.U
    }
  }

  // Update by CDB
  for (bank <- banks) {
    var currentBusy = WireInit(bank.regBusy)
    for (ent <- cdb.entries) {
      val mask = WireInit(-1.S(REG_NUM.W).asUInt)
      when(ent.valid && bank.regType === ent.regType) {
        mask := ~(1.U << ent.phys)
      }
      currentBusy = WireInit(currentBusy & mask)
    }
    bank.regBusy := currentBusy
  }

  // TODO: asserts that CDB never broadcasts names that are being allocated

  val canRename = (0 until coredef.ISSUE_NUM).map(idx => {
    if (idx == 0) {
      true.B
    } else {
      val ret = WireDefault(true.B)
      val rs1 = toExec.input(idx).instr.getRs1
      val rs2 = toExec.input(idx).instr.getRs2
      val rs3 = toExec.input(idx).instr.getRs3

      // check if this instruction relies on previous instructions
      for (i <- (0 until idx)) {
        when(toExec.input(i).instr.writeRdEffective) {
          val rd = toExec.input(i).instr.getRd
          when(
            rs1.ty === rd.ty && rs1.index === rd.index &&
              toExec.input(idx).instr.info.readRs1
          ) {
            ret := false.B
          }

          when(
            rs2.ty === rd.ty && rs2.index === rd.index &&
              toExec.input(idx).instr.info.readRs2
          ) {
            ret := false.B
          }

          when(
            rs3.ty === rd.ty && rs3.index === rd.index &&
              toExec.input(idx).instr.info.readRs3
          ) {
            ret := false.B
          }
        }
      }
      ret
    }
  })

  def readRegs(reg: UInt, bankIdx: Int) = {
    val mapping = banks(bankIdx).mapping
    val regBusy = banks(bankIdx).regBusy

    val phys = mapping(reg)
    val ready = WireDefault(~(regBusy(phys)))

    // Loop through CDB
    for (ent <- cdb.entries) {
      when(ent.valid && ent.phys === phys) {
        ready := true.B
      }
    }

    (phys, ready)
  }

  toExec.allowBit := VecInit(canRename)

  // Release before allocation
  for ((release, idx) <- toExec.releases.zipWithIndex) {
    when(idx.U < toExec.retire) {
      for ((bank, regInfo) <- banks.zip(coredef.REGISTER_TYPES)) {
        when(release.regType === bank.regType) {
          bank.freelist := bank.freelist | (1.U << release.stalePhys)
        }
      }
    }
  }

  for ((instr, idx) <- toExec.input.zipWithIndex) {
    toExec.output(idx).rs1Phys := 0.U
    toExec.output(idx).rs1Ready := true.B
    toExec.output(idx).rs2Phys := 0.U
    toExec.output(idx).rs2Ready := true.B
    toExec.output(idx).rs3Phys := 0.U
    toExec.output(idx).rs3Ready := true.B

    for ((regInfo, bankIdx) <- coredef.REGISTER_TYPES.zipWithIndex) {
      when(
        instr.instr.getRs1Type === regInfo.regType && instr.instr.info.readRs1
      ) {
        val (rs1Phys, rs1Ready) =
          readRegs(
            instr.instr.rs1,
            bankIdx
          )
        toExec.output(idx).rs1Phys := rs1Phys
        toExec.output(idx).rs1Ready := rs1Ready
      }

      when(
        instr.instr.getRs2Type === regInfo.regType && instr.instr.info.readRs2
      ) {
        val (rs2Phys, rs2Ready) =
          readRegs(
            instr.instr.rs2,
            bankIdx
          )
        toExec.output(idx).rs2Phys := rs2Phys
        toExec.output(idx).rs2Ready := rs2Ready
      }

      // third operand: rs3/vs3/vd
      // vector instructions that read from vd
      // should have rdAsRs3=1
      // and the value is stored in rs3val
      if (regInfo.maxOperandNum >= 3) {
        when(
          instr.instr.getRs3Type === regInfo.regType && instr.instr.info.readRs3
        ) {
          val (rs3Phys, rs3Ready) =
            readRegs(
              Mux(
                instr.instr.info.rdAsRs3,
                instr.instr.rd,
                instr.instr.rs3
              ),
              bankIdx
            )
          toExec.output(idx).rs3Phys := rs3Phys
          toExec.output(idx).rs3Ready := rs3Ready
        }
      }
    }

    toExec.output(idx).instr := instr

    when(idx.U < toExec.commit) {
      for ((regInfo, bank) <- coredef.REGISTER_TYPES.zip(banks)) {
        when(
          instr.instr
            .getRdType() === regInfo.regType && instr.instr.info.writeRd
        ) {
          // allocate register for rd
          val reg = bank.freelist
          bank.mapping(instr.instr.getRdIndex) := reg
          bank.freelist := bank.freelist & ~(1.U << reg)
          bank.regBusy := bank.regBusy | (1.U << reg)
        }
      }
    }
  }

  // Lastly, handles flush
  when(toExec.flush) {
    flush()
  }
}
