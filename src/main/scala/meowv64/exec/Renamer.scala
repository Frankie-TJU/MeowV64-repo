package meowv64.exec

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.instr.InstrExt
import meowv64.instr.RegIndex

/** Release stale physical register into free list and update committed mapping
  */
class Release(implicit val coredef: CoreDef) extends Bundle {
  val staleRdPhys = Input(UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W))
  val rdPhys = Input(UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W))
  val rdIndex = Input(new RegIndex)
  val valid = Input(Bool())
}

class Renamer(implicit coredef: CoreDef) extends Module {
  val REG_NUM = 32 // TODO: do we need to make this configurable?

  val cdb = IO(Input(new CDB))

  val toExec = IO(new Bundle {
    // instruction before rename
    val input = Input(Vec(coredef.ISSUE_NUM, new InstrExt))
    val commit = Input(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
    val nextRobIndex = Input(
      UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
    )

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
    for (regInfo <- coredef.REG_TYPES) yield new {
      // Map logical -> physical
      val regType = regInfo.regType
      val mapping = RegInit(
        VecInit(Seq.fill(REG_NUM)(0.U(log2Ceil(regInfo.physRegs).W)))
      )
      val regBusy = RegInit(0.U(regInfo.physRegs.W))

      // 1 means free
      // P0 is hardwired to zero for x0
      // it should never be allocated
      val freeMask = if (regInfo.fixedZero) {
        (BigInt(1) << regInfo.physRegs) - 2
      } else {
        (BigInt(1) << regInfo.physRegs) - 1
      }
      val freeList = RegInit(freeMask.U(regInfo.physRegs.W))

      // masks for updating freeList
      val setFreeMask = WireInit(0.U(regInfo.physRegs.W))
      val clearFreeMask = WireInit(0.U(regInfo.physRegs.W))
      freeList := freeList & ~clearFreeMask | (setFreeMask & freeMask.U)

      // masks for updating regBusy
      val setBusyMask = WireInit(0.U(regInfo.physRegs.W))
      val clearBusyMask = WireInit(0.U(regInfo.physRegs.W))
      regBusy := regBusy & ~clearBusyMask | setBusyMask

      // save committed state for recovery
      val committedMapping = RegInit(
        VecInit(Seq.fill(REG_NUM)(0.U(log2Ceil(regInfo.physRegs).W)))
      )
      val committedFreeList = RegInit(freeMask.U(regInfo.physRegs.W))
      // when flushing, use the newest mapping & free mask
      val nextCommittedMapping = WireInit(committedMapping)
      val nextCommittedFreeList = WireInit(committedFreeList)
      committedMapping := nextCommittedMapping
      committedFreeList := nextCommittedFreeList

      // masks for updating committedFreeList
      val setCommittedFreeMask = WireInit(0.U(regInfo.physRegs.W))
      val clearCommittedFreeMask = WireInit(0.U(regInfo.physRegs.W))
      nextCommittedFreeList := committedFreeList & ~clearCommittedFreeMask | (setCommittedFreeMask & freeMask.U)
    }

  def flush() = {
    for (bank <- banks) {
      bank.mapping := bank.nextCommittedMapping
      bank.freeList := bank.nextCommittedFreeList
      // all regs are ready
      bank.regBusy := 0.U
    }
  }

  // Update by CDB
  for ((bank, regInfo) <- banks.zip(coredef.REG_TYPES)) {
    var currentBusy = WireInit(0.U.asTypeOf(bank.clearBusyMask))
    for (ent <- cdb.entries) {
      val mask = WireInit(0.U(regInfo.physRegs.W))
      when(ent.valid && bank.regType === ent.regType) {
        mask := 1.U << ent.phys
      }
      currentBusy = WireInit(currentBusy | mask)
    }
    bank.clearBusyMask := currentBusy
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
        when(toExec.input(i).instr.writeRdEff) {
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
  for ((bank, regInfo) <- banks.zip(coredef.REG_TYPES)) {
    val setMasks = for ((release, idx) <- toExec.releases.zipWithIndex) yield {
      val mask = WireInit(0.U(regInfo.physRegs.W))
      when(release.rdIndex.ty === bank.regType && release.valid) {
        mask := 1.U << release.staleRdPhys
      }
      mask
    }

    bank.setFreeMask := setMasks.reduce(_ | _)
    bank.setCommittedFreeMask := setMasks.reduce(_ | _)

    // update committed state
    // and remove rd from committed free list
    val clearMasks =
      for ((release, idx) <- toExec.releases.zipWithIndex) yield {
        val mask = WireInit(0.U(regInfo.physRegs.W))
        when(release.rdIndex.ty === bank.regType && release.valid) {
          bank.nextCommittedMapping(release.rdIndex.index) := release.rdPhys
          mask := 1.U << release.rdPhys
        }
        mask
      }
    bank.clearCommittedFreeMask := clearMasks.reduce(_ | _)
  }

  for ((instr, idx) <- toExec.input.zipWithIndex) {
    toExec.output(idx).rs1Phys := 0.U
    toExec.output(idx).rs1Ready := true.B
    toExec.output(idx).rs2Phys := 0.U
    toExec.output(idx).rs2Ready := true.B
    toExec.output(idx).rs3Phys := 0.U
    toExec.output(idx).rs3Ready := true.B
    toExec.output(idx).staleRdPhys := 0.U
    toExec.output(idx).rdPhys := 0.U
    toExec.output(idx).robIndex := toExec.nextRobIndex +% idx.U

    for ((regInfo, bankIdx) <- coredef.REG_TYPES.zipWithIndex) {
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

    // handle illegal instruction
    when(instr.illegal) {
      toExec.output(idx).rs1Ready := true.B
      toExec.output(idx).rs2Ready := true.B
      toExec.output(idx).rs3Ready := true.B
    }
  }

  // assign new physical register
  for ((regInfo, bank) <- coredef.REG_TYPES.zip(banks)) {
    var clearFreeMask = WireInit(0.U(regInfo.physRegs.W))
    var setBusyMask = WireInit(0.U(regInfo.physRegs.W))

    for ((instr, idx) <- toExec.input.zipWithIndex) {
      val phys = PriorityEncoder(bank.freeList & ~clearFreeMask)
      val curMask = WireInit(0.U(regInfo.physRegs.W))
      when(
        instr.instr
          .getRdType() === regInfo.regType && instr.instr.writeRdEff()
      ) {
        when(idx.U < toExec.commit) {
          // allocate register for rd
          bank.mapping(instr.instr.getRdIndex) := phys
          toExec.output(idx).rdPhys := phys
          curMask := 1.U << phys

          // save stale physical register
          toExec.output(idx).staleRdPhys := bank.mapping(instr.instr.getRdIndex)
        }
      }

      // clear bit in free list and set busy bit
      clearFreeMask = clearFreeMask | curMask
      setBusyMask = setBusyMask | curMask
    }

    bank.clearFreeMask := clearFreeMask
    bank.setBusyMask := setBusyMask
  }

  // Lastly, handles flush
  when(toExec.flush) {
    flush()
  }
}
