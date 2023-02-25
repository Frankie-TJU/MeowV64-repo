package meowv64.exec

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.instr.InstrExt
import meowv64.instr.RegIndex
import meowv64.reg.RegType

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

  val difftest = IO(new Bundle {
    val intCommittedMap = Output(Vec(REG_NUM, UInt((log2Ceil(coredef.REG_INT.physRegs).W))))
    val fpCommittedMap = Output(Vec(REG_NUM, UInt((log2Ceil(coredef.REG_FLOAT.physRegs).W))))
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
      // and it can be used in staleRdPhys = 0 meaning no stale phys register
      // so keep it deallocated for float as well
      val freeMask = (BigInt(1) << regInfo.physRegs) - 2
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

      if (regInfo == coredef.REG_INT) {
        difftest.intCommittedMap := committedMapping
      } else if (regInfo == coredef.REG_FLOAT) {
        difftest.fpCommittedMap := committedMapping
      }

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
    if (true) {
      true.B
    } else {
      val ret = WireDefault(true.B)
      val rs1 = toExec.input(idx).instr.getRs1
      val rs2 = toExec.input(idx).instr.getRs2

      // NOTE: rdAsRs3 does not use rdType, use rs3Type instead
      val rs3 = RegIndex.create(
        toExec.input(idx).instr.getRs3Type(),
        Mux(
          toExec.input(idx).instr.info.rdAsRs3,
          // NOTE: do not use not effective rd here, because writeRd=0
          toExec.input(idx).instr.rd,
          toExec.input(idx).instr.rs3
        )
      )
      val vm = RegIndex.create(RegType.vector, 0.U)

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

          when(
            vm.ty === rd.ty && vm.index === rd.index &&
              toExec.input(idx).instr.readVm
          ) {
            ret := false.B
          }
        }
      }
      ret
    }
  })

  // Save assigned phys register for same cycle dependency
  val assignedPhys = WireInit(
    VecInit.fill(coredef.ISSUE_NUM)(
      0.U(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)
    )
  )

  def readRegs(reg: UInt, issueIdx: Int, bankIdx: Int) = {
    val mapping = banks(bankIdx).mapping
    val regBusy = banks(bankIdx).regBusy

    val phys = WireInit(mapping(reg))
    val ready = WireDefault(~(regBusy(phys)))

    // Handle same cycle dependency
    for (i <- (0 until issueIdx)) {
      when(toExec.input(i).instr.writeRdEff()) {
        val rd = toExec.input(i).instr.getRd
        when(banks(bankIdx).regType === rd.ty && reg === rd.index) {
          phys := assignedPhys(i)
          ready := false.B
        }
      }
    }

    // Loop through CDB
    for (ent <- cdb.entries) {
      when(
        ent.valid && ent.phys === phys && ent.regType === banks(bankIdx).regType
      ) {
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
    toExec.output(idx).vmPhys := 0.U
    toExec.output(idx).vmReady := true.B
    toExec.output(idx).staleRdPhys := 0.U
    toExec.output(idx).rdPhys := 0.U
    toExec.output(idx).robIndex := toExec.nextRobIndex +% idx.U
    toExec.output(idx).lsqIndex := 0.U // this will be filled in exec stage

    for ((regInfo, bankIdx) <- coredef.REG_TYPES.zipWithIndex) {
      when(
        instr.instr.getRs1Type === regInfo.regType && instr.instr.info.readRs1
      ) {
        val (rs1Phys, rs1Ready) =
          readRegs(
            instr.instr.rs1,
            idx,
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
            idx,
            bankIdx
          )
        toExec.output(idx).rs2Phys := rs2Phys
        toExec.output(idx).rs2Ready := rs2Ready
      }

      // third operand: rs3/vs3/vd
      // vector instructions that read from vd
      // should have rdAsRs3=1
      // and the value is stored in rs3val
      if (regInfo.maxOperandCount >= 3) {
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
              idx,
              bankIdx
            )
          toExec.output(idx).rs3Phys := rs3Phys
          toExec.output(idx).rs3Ready := rs3Ready
        }
      }

      // fourth operand: vm
      when(
        RegType.vector === regInfo.regType && instr.instr.readVm
      ) {
        val (vmPhys, vmReady) =
          readRegs(0.U, idx, bankIdx)
        toExec.output(idx).vmPhys := vmPhys
        toExec.output(idx).vmReady := vmReady
      }
    }

    toExec.output(idx).instr := instr

    // handle illegal instruction
    when(instr.illegal) {
      toExec.output(idx).rs1Ready := true.B
      toExec.output(idx).rs2Ready := true.B
      toExec.output(idx).rs3Ready := true.B
      toExec.output(idx).vmReady := true.B
    }
  }

  // assign new physical register
  for ((regInfo, bank) <- coredef.REG_TYPES.zip(banks)) {
    val clearFreeMask = WireInit(
      VecInit.fill(coredef.ISSUE_NUM + 1)(0.U(regInfo.physRegs.W))
    )
    val setBusyMask = WireInit(
      VecInit.fill(coredef.ISSUE_NUM + 1)(0.U(regInfo.physRegs.W))
    )

    for ((instr, idx) <- toExec.input.zipWithIndex) {
      val freeMask = bank.freeList & ~clearFreeMask(idx)
      val phys = PriorityEncoder(freeMask)
      val curMask = WireInit(0.U(regInfo.physRegs.W))
      when(
        instr.instr
          .getRdType() === regInfo.regType && instr.instr.writeRdEff()
      ) {
        when(~freeMask.orR) {
          // no registers are free!
          toExec.allowBit(idx) := false.B
        }

        curMask := 1.U << phys
        assignedPhys(idx) := phys
        when(idx.U < toExec.commit) {
          // allocate register for rd
          bank.mapping(instr.instr.getRdIndex) := phys
          toExec.output(idx).rdPhys := phys

          // save stale physical register
          // handle same cycle dependency
          val staleRdPhys = WireInit(bank.mapping(instr.instr.getRdIndex))
          for (i <- 0 until idx) {
            when(
              toExec.input(i).instr.writeRdEff() &&
                toExec.input(i).instr.getRdIndex() === instr.instr
                  .getRdIndex() &&
                toExec.input(i).instr.getRdType() === instr.instr.getRdType()
            ) {
              staleRdPhys := assignedPhys(i)
            }
          }

          toExec.output(idx).staleRdPhys := staleRdPhys
        }
      }

      // clear bit in free list and set busy bit
      clearFreeMask(idx + 1) := clearFreeMask(idx) | curMask
      setBusyMask(idx + 1) := setBusyMask(idx) | curMask
    }

    bank.clearFreeMask := clearFreeMask(toExec.commit)
    bank.setBusyMask := setBusyMask(toExec.commit)
  }

  // Lastly, handles flush
  when(toExec.flush) {
    flush()
  }
}
