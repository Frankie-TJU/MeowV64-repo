package meowv64.exec

import chisel3._
import chisel3.util._
import meowv64.cache.CoreDCReader
import meowv64.cache.CoreDCWriter
import meowv64.cache.DCFenceStatus
import meowv64.cache.L1UCPort
import meowv64.core.CSRWriter
import meowv64.core.CoreDef
import meowv64.core.ExReq
import meowv64.core.PrivLevel
import meowv64.core.Satp
import meowv64.core.StageCtrl
import meowv64.core.Status
import meowv64.core.VState
import meowv64.exec.units._
import meowv64.instr._
import meowv64.paging.TLBExt
import meowv64.reg._
import meowv64.util._

import scala.collection.mutable.ArrayBuffer

/** Out-of-order execution (Tomasulo's algorithm)
  *
  * First we check if instructions are eligible to be issues. Criteria include:
  *   - Target reservation station has free slots
  *   - Number of in-flight instructions haven't exceeded the limit. This limit
  *     affects our rob buffer length, as well as renamed reg tags' length
  *   - Issue FIFO is not depleted
  */
class Exec(implicit val coredef: CoreDef) extends Module {
  val toCtrl = IO(new Bundle {
    val ctrl = StageCtrl.stage()
    val retCnt = Output(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))
    val nepc = Output(UInt(coredef.XLEN.W))

    val branch = Output(new ExceptionResult)
    val tval = Output(UInt(coredef.XLEN.W))

    val int = Input(Bool())
    val intAck = Output(Bool())

    val priv = Input(PrivLevel())
    val status = Input(new Status)
    val vState = Input(new VState)
    val debugMode = Input(Bool())

    val step = Input(Bool()) // single stepping in dcsr
    val stepAck = Output(Bool())

    val tlbRst = Input(Bool())

    /** Update fflags
      */
    val updateFFlags = Valid(UInt(5.W))

    /** Update vState
      */
    val updateVState = Valid(new VState())
  })

  val toBPU = IO(new Bundle {

    /** Update BPU based on execution result
      */
    val valid = Output(Bool())
    val lpc = Output(UInt(coredef.XLEN.W))
    val taken = Output(Bool())
    val hist = Output(new BPUResult)
  })

  val toCore = IO(new Bundle {
    val satp = Input(new Satp)
    val ptw = new TLBExt

    // debug
    val iqEmptyMask = Output(UInt(coredef.ISSUE_QUEUES.length.W))
    val iqFullMask = Output(UInt(coredef.ISSUE_QUEUES.length.W))
    val issueNum = Output(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
    val issueNumBoundedByROBSize = Output(Bool())
    val issueNumBoundedByLSQSize = Output(Bool())
    val retireNum = Output(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))
  })

  val csrWriter = IO(new CSRWriter())

  // We don't stall now
  toCtrl.ctrl.stall := false.B

  // Register file read/write port
  // for each port
  val toRF = IO(new Bundle {
    // readPorts(i)(j): the j operand of port i
    val readPorts =
      MixedVec(
        for (port <- coredef.PORTS)
          yield new Bundle {
            // `readPorts` read ports
            val reader = MixedVec(
              for (regType <- port.operandTypes.flatMap(x => x))
                yield new Bundle {
                  val port = new RegReader(
                    coredef.REG_MAPPING(regType).width,
                    coredef.REG_MAPPING(regType).physRegs
                  )
                }
            )
          }
      )

    // writePorts(i): the i-th write port
    val writePorts =
      MixedVec(
        for (port <- coredef.REG_WRITE_PORTS)
          yield new Bundle {
            val writer =
              new RegWriter(
                port.regInfo.width,
                port.regInfo.physRegs
              )
          }
      )
  })

  val toIF = IO(new MultiQueueIO(new InstrExt, coredef.ISSUE_NUM))

  val toDC = IO(new Bundle {
    val r = new CoreDCReader
    val w = new CoreDCWriter(coredef.L1D)
    val fs = new DCFenceStatus(coredef.L1D)
    val u = new L1UCPort(coredef.L1D)
  })

  val cdb = Wire(new CDB)

  val renamer = Module(new Renamer)
  renamer.cdb := cdb
  renamer.toExec.flush := toCtrl.ctrl.flush

  // Inflight instr info
  val inflights = Module(
    new MultiQueue(
      new InflightInstr,
      coredef.INFLIGHT_INSTR_LIMIT,
      coredef.ISSUE_NUM,
      coredef.RETIRE_NUM
    )
  )
  inflights.flush := toCtrl.ctrl.flush

  // Delayed memory ops
  val releaseMem = Wire(DeqIO(new DelayedMemResult))
  releaseMem.nodeq()

  // Units
  val lsu = Module(new LSU).suggestName("LSU")

  // collect execution units dynamically
  // Issue Queue -> Port -> Execution Unit
  val units = ArrayBuffer[UnitSelIO]()
  val issueQueues = ArrayBuffer[IssueQueue]()
  var portIdx = 0
  for ((issueQueueInfo, i) <- coredef.ISSUE_QUEUES.zipWithIndex) {
    val isLSU = issueQueueInfo.issueQueueType == IssueQueueType.mem
    val issueQueue = Module(new OoOIssueQueue(issueQueueInfo))
    issueQueue.suggestName(s"IssueQueue_${issueQueueInfo.issueQueueType}")
    issueQueue.cdb <> cdb
    issueQueues.append(issueQueue)

    for ((port, j) <- issueQueueInfo.ports.zipWithIndex) {
      val bypassIdx =
        port.units.map(_.execUnitType).indexOf(ExecUnitType.bypass)
      val regRead = Module(new RegisterRead(port))
      regRead.suggestName(s"RegisterRead_Port${portIdx}")
      regRead.io.flush := toCtrl.ctrl.flush

      val unitSel = if (isLSU) {
        lsu
      } else {
        val module = Module(
          new UnitSel(
            port,
            for (unit <- port.units) yield {
              unit.execUnitType match {
                case ExecUnitType.alu => Module(new ALU).suggestName("ALU")
                case ExecUnitType.branch =>
                  Module(new Branch).suggestName("Branch")
                case ExecUnitType.csr => Module(new CSR).suggestName("CSR")
                case ExecUnitType.bypass =>
                  Module(new Bypass).suggestName("Bypass")
                case ExecUnitType.mul => Module(new Mul).suggestName("Mul")
                case ExecUnitType.div => Module(new Div(16)).suggestName("Div")
                case ExecUnitType.intToFloat =>
                  Module(new IntToFloat).suggestName("IntToFloat")
                case ExecUnitType.intToFloatMultiCycle =>
                  Module(new IntToFloatMultiCycle)
                    .suggestName("IntToFloatMultiCycle")
                case ExecUnitType.fma => Module(new FMA).suggestName("FMA")
                case ExecUnitType.floatMisc =>
                  Module(new FloatMisc).suggestName("FloatMisc")
                case ExecUnitType.floatMiscMultiCycle =>
                  Module(new FloatMiscMultiCycle)
                    .suggestName("FloatMiscMultiCycle")
                case ExecUnitType.floatDivSqrt =>
                  Module(new FloatDivSqrt).suggestName("FloatDivSqrt")
                case ExecUnitType.floatToInt =>
                  Module(new FloatToInt).suggestName("FloatToInt")
                case ExecUnitType.lsu => {
                  // this is actually float to mem
                  if (unit.regType == RegType.float) {
                    Module(new FloatToMem).suggestName("FloatToMem")
                  } else {
                    Module(new VectorToMem).suggestName("VectorToMem")
                  }
                }
                case ExecUnitType.vectorAlu =>
                  Module(new VectorALU).suggestName("VectorALU")
                case ExecUnitType.vectorMul =>
                  Module(new VectorMul).suggestName("VectorMul")
                case ExecUnitType.vectorFma =>
                  Module(new VectorFMA).suggestName("VectorFMA")
                case ExecUnitType.vectorFloatRedSum =>
                  Module(new VectorFloatRedSum).suggestName("VectorFloatRedSum")
                case ExecUnitType.vectorMisc =>
                  Module(new VectorMisc).suggestName("VectorMisc")
              }
            },
            instr => {
              port.units.map({ unit =>
                instr.info.execUnit === unit.execUnitType
              })
            },
            bypassIdx = if (bypassIdx == -1) { None }
            else { Option(bypassIdx) },
            hasPipe = false
          )
        )
        module.suggestName(s"UnitSel_Port${portIdx}")
        module
      }

      unitSel.issue <> regRead.io.toUnits
      regRead.io.toIssueQueue.instr <> issueQueue.egress(j)
      regRead.io.toRegFile.reader <> toRF.readPorts(portIdx).reader

      units.append(unitSel)

      portIdx += 1
    }
  }

  // wire register write ports
  for ((port, i) <- coredef.REG_WRITE_PORTS.zipWithIndex) {
    val regType = port.regType
    if (port.ports.length == 1) {
      // case 1: only one port
      // no arbiter required
      val unitIdx = port.ports(0)
      val unitSel = units(unitIdx)
      val retire = unitSel.retire
      toRF.writePorts(i).writer.addr := retire.bits.rdPhys
      toRF.writePorts(i).writer.data := retire.bits.info.wb
      // do not write if exception occurred
      toRF.writePorts(i).writer.valid := retire.valid &&
        retire.bits.writeRdEff &&
        retire.bits.info.exception.ex === ExReq.none &&
        retire.bits.rdType === port.regType
      val unitConfig = coredef.PORTS(unitIdx)
      retire.ready(unitConfig.writeRegTypes.indexOf(regType)) := true.B
    } else {
      // case 2: more than one port
      val arbiter = Module(
        new Arbiter(
          new Bundle {
            val addr = UInt(log2Ceil(port.regInfo.physRegs).W)
            val data = UInt(port.regInfo.width.W)
          },
          port.ports.length
        )
      )
      arbiter.suggestName(s"WritebackArbiter_${i}")

      toRF.writePorts(i).writer.addr := arbiter.io.out.bits.addr
      toRF.writePorts(i).writer.data := arbiter.io.out.bits.data
      toRF.writePorts(i).writer.valid := arbiter.io.out.valid
      arbiter.io.out.ready := true.B

      for ((unitIdx, j) <- port.ports.zipWithIndex) {
        val unitSel = units(unitIdx)
        val retire = unitSel.retire
        arbiter.io.in(j).bits.addr := retire.bits.rdPhys
        arbiter.io.in(j).bits.data := retire.bits.info.wb
        // do not write if exception occurred
        arbiter.io.in(j).valid := retire.valid &&
          retire.bits.writeRdEff &&
          retire.bits.info.exception.ex === ExReq.none &&
          retire.bits.rdType === port.regType
        val unitConfig = coredef.PORTS(unitIdx)
        retire.ready(unitConfig.writeRegTypes.indexOf(regType)) := arbiter.io
          .in(j)
          .ready
      }
    }
  }

  lsu.toMem.reader <> toDC.r
  lsu.toMem.writer <> toDC.w
  lsu.toMem.uncached <> toDC.u
  lsu.release <> releaseMem
  lsu.ptw <> toCore.ptw
  lsu.satp := toCore.satp
  lsu.priv := toCtrl.priv
  lsu.vState := toCtrl.vState
  lsu.tlbRst := toCtrl.tlbRst
  lsu.status := toCtrl.status

  // Connect extra ports
  units(0).extras("CSR") <> csrWriter
  for (unit <- units) {
    if (unit.extras.contains("priv")) {
      unit.extras("priv") := toCtrl.priv
    }
    if (unit.extras.contains("status")) {
      unit.extras("status") := toCtrl.status
    }
    if (unit.extras.contains("floatToMem")) {
      unit.extras("floatToMem") <> lsu.toFloat
    }
    if (unit.extras.contains("vectorToMem")) {
      unit.extras("vectorToMem") <> lsu.toVector
    }
    if (unit.extras.contains("vState")) {
      unit.extras("vState") := toCtrl.vState
    }
  }

  assume(units.length == coredef.PORTS.length)
  // TODO: asserts Bypass is in unit 0

  // collect issue queue free mask to find bottleneck
  toCore.iqEmptyMask := Cat(issueQueues.map(_.ingress.empty).reverse)
  toCore.iqFullMask := Cat(issueQueues.map(_.ingress.full).reverse)

  for (iq <- issueQueues) {
    iq.cdb := cdb
    iq.ctrl.flush := toCtrl.ctrl.flush

    // By default: nothing pushes
    iq.ingress := DontCare
    for (i <- 0 until coredef.ISSUE_NUM) {
      iq.ingress.instr(i).valid := false.B
    }
  }

  for (u <- units) {
    u.flush := toCtrl.ctrl.flush
  }

  // ROB & ptrs
  val rob = RegInit(
    VecInit(Seq.fill(coredef.INFLIGHT_INSTR_LIMIT)(ROBEntry.empty))
  )
  val retirePtr = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
  val issuePtr = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))

  val retireNum = Wire(UInt(log2Ceil(coredef.RETIRE_NUM + 1).W))
  val issueNum = Wire(UInt(log2Ceil(coredef.ISSUE_NUM + 1).W))

  when(retirePtr === issuePtr) {
    assert(inflights.reader.cnt === 0.U)
  }.otherwise {
    assert(inflights.reader.cnt =/= 0.U)
  }

  toIF.accept := issueNum
  renamer.toExec.commit := issueNum
  renamer.toExec.input := toIF.view
  renamer.toExec.nextRobIndex := issuePtr

  issuePtr := issuePtr + issueNum
  retirePtr := retirePtr + retireNum

  toCtrl.retCnt := retireNum

  // Issue
  val maxIssueNum =
    retirePtr -% issuePtr -% 1.U // issuePtr cannot reach retirePtr
  assert(issueNum <= maxIssueNum)

  // debug
  toCore.issueNum := issueNum
  toCore.issueNumBoundedByROBSize := issueNum === maxIssueNum
  toCore.issueNumBoundedByLSQSize := issueNum === lsu.toExec.lsqEmptyEntries
  toCore.retireNum := retireNum

  val wasGFence = RegInit(false.B)
  val canIssue = Wire(Vec(coredef.ISSUE_NUM, Bool()))
  issueNum := 0.U

  // allocate lsq entry
  var lsqAllocMask = WireInit(VecInit.fill(coredef.ISSUE_NUM)(false.B))
  lsu.toExec.lsqAllocCount := lsqAllocMask
    .zip(canIssue)
    .map({ case (alloc, issue) => (alloc & issue).asUInt })
    .reduce(_ +& _)

  for (idx <- (0 until coredef.ISSUE_NUM)) {
    // whether this instruction can issue without considering previous instructions
    val selfCanIssue = Wire(Bool()).suggestName(s"selfCanIssue_$idx")
    // sending to which issue queue
    val sending =
      Wire(UInt(coredef.ISSUE_QUEUES.length.W)).suggestName(s"sending_$idx")
    val instr = Wire(new IssueQueueInstr)

    // Is global fence? (FENCE.I, CSR)
    val isGFence = (
      (instr.instr.instr.info.execUnit === ExecUnitType.csr &&
        instr.instr.instr.op =/= Decoder.Op("OP-V").ident) ||
        (instr.instr.instr.op === Decoder.Op("SYSTEM").ident &&
          instr.instr.instr.funct3 =/= Decoder.SYSTEM_FUNC("PRIV"))
    )

    // At most only one sending except floatMem/vectorMem
    when(
      instr.instr.instr.info.issueQueue =/= IssueQueueType.floatMem &&
        instr.instr.instr.info.issueQueue =/= IssueQueueType.vectorMem
    ) {
      assert(!(sending & (sending -% 1.U)).orR)
    }
    assert(!selfCanIssue || sending.orR)

    instr := renamer.toExec.output(idx)

    when(
      idx.U >= toIF.cnt || !renamer.toExec.allowBit(idx) || idx.U >= maxIssueNum
    ) {
      selfCanIssue := false.B
      sending := 0.U
    }.otherwise {
      // Route to applicable issue queues
      val applicable = Exec.route(toIF.view(idx).instr)
      applicable.suggestName(s"applicable_$idx")
      // Find available issue queue
      val avails = VecInit(issueQueues.map(_.ingress.instr(idx).ready)).asUInt()
      avails.suggestName(s"avails_$idx")

      sending := 0.U

      when(
        toIF.view(idx).illegal
          || !applicable.orR()
      ) {
        // Is an illegal instruction
        // Forward to bypass unit in integer issue queue
        selfCanIssue := issueQueues(0).ingress.instr(idx).ready
        sending := 1.U
        // Run immediately
        instr.rs1Ready := true.B
        instr.rs2Ready := true.B
        instr.rs3Ready := true.B
        instr.vmReady := true.B
      }.elsewhen(wasGFence && issuePtr =/= retirePtr) {
        // GFence in-flight
        sending := DontCare
        selfCanIssue := false.B
        // TODO: only apply to first instr to optimize timing?
      }.otherwise {
        val mask = applicable & avails
        mask.suggestName(s"mask_$idx")

        sending := mask

        selfCanIssue := applicable === mask

        if (idx != 0) {
          // Cannot issue GFence that is not on the first slot
          when(isGFence) {
            selfCanIssue := false.B
          }
        } else {
          // Block GFence if there is still in-flight instrs
          when(isGFence && retirePtr =/= issuePtr) {
            selfCanIssue := false.B
          }
        }

        // check if sending to mem issue queue
        // allocate lsq index
        when(
          (toIF
            .view(idx)
            .instr
            .info
            .issueQueue
            .asUInt & IssueQueueType.mem.value.U) === IssueQueueType.mem.value.U
        ) {
          lsqAllocMask(idx) := true.B

          // lsq full
          when(idx.U >= lsu.toExec.lsqAllocAccept) {
            selfCanIssue := false.B
          }

          // compute lsq index
          if (idx == 0) {
            instr.lsqIndex := lsu.toExec.lsqIdxBase
          } else {
            instr.lsqIndex := lsu.toExec.lsqIdxBase + lsqAllocMask
              .slice(0, idx)
              .map(_.asUInt)
              .reduce(_ +& _)
          }
        }
      }
    }

    if (idx == 0) canIssue(idx) := selfCanIssue
    else canIssue(idx) := selfCanIssue && canIssue(idx - 1)

    when(canIssue(idx)) {
      issueNum := (idx + 1).U

      for ((s, en) <- issueQueues.zip(sending.asBools)) {
        when(en) {
          s.ingress.instr(idx).valid := true.B
          s.ingress.instr(idx).bits := instr
        }
      }

      if (idx == 0) {
        wasGFence := isGFence
      }
    }
  }

  inflights.writer.cnt := issueNum
  assert(inflights.writer.accept >= issueNum)
  // save necessary info in fifo
  inflights.writer.view := toIF.view
    .zip(renamer.toExec.output)
    .map({ case (i, r) => InflightInstr.from(i, r) })

  val pendingBr = RegInit(false.B)
  val pendingBrTag = RegInit(0.U(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W))
  val pendingBrResult = RegInit(ExceptionResult.empty)
  val pendingBrTval = RegInit(0.U(coredef.XLEN.W))
  assert(!pendingBr || pendingBrResult.fire())

  // find the first branch instruction
  // branch here means control flow interruption (trap, missed prediction)
  val brMask = Wire(Vec(units.size, UInt(coredef.INFLIGHT_INSTR_LIMIT.W)))
  val brMux = Wire(UInt(coredef.INFLIGHT_INSTR_LIMIT.W))
  brMux := brMask.reduceTree(_ | _) | Mux(
    pendingBr,
    UIntToOH(pendingBrTag -% retirePtr),
    0.U
  )
  // index of first branch instruction
  val brSel = VecInit(PriorityEncoderOH(brMux.asBools())).asUInt()
  val brSeled = Wire(Vec(units.size, Bool()))
  // branch result and branch trap target
  val brResults = Wire(Vec(units.size, new ExceptionResult))
  val brTvals = Wire(Vec(units.size, UInt(coredef.XLEN.W)))

  when(brSeled.asUInt.orR()) {
    // a branch instruction is found
    pendingBr := true.B
    pendingBrTag := OHToUInt(brSel) +% retirePtr // Actually this is always true
    pendingBrResult := Mux1H(brSeled, brResults)
    pendingBrTval := Mux1H(brSeled, brTvals)
  }

  // set hasMem when delayed memory op is pushed into queue
  when(lsu.toExec.setHasMem.valid) {
    rob(lsu.toExec.setHasMem.bits.robIndex).valid := true.B
    rob(lsu.toExec.setHasMem.bits.robIndex).hasMem := true.B
  }

  // Filling ROB & CDB broadcast
  for (((u, ent), idx) <- units.zip(cdb.entries).zipWithIndex) {
    ent.valid := false.B
    // better timing
    ent.phys := u.retire.bits.rdPhys
    ent.regType := u.retire.bits.rdType

    // TODO: maybe pipeline here?
    val dist = u.retire.bits.robIndex -% retirePtr
    val oh = UIntToOH(dist)
    val branchResult = u.retire.bits.info.exception
    val canBr = u.retire.valid && branchResult.fire()
    brMask(idx) := Mux(canBr, oh, 0.U)
    brSeled(idx) := brSel === oh && canBr
    brResults(idx) := branchResult
    brTvals(idx) := u.retire.bits.info.wb

    when(u.retire.fire) {
      ent.valid := true.B

      rob(u.retire.bits.robIndex).valid := true.B
      val info = u.retire.bits.info
      // for BRANCH instructions, this means taken before normalization
      rob(u.retire.bits.robIndex).branchTaken := info.branchTaken

      rob(u.retire.bits.robIndex).updateFFlags := info.updateFFlags
      rob(u.retire.bits.robIndex).fflags := info.fflags
      rob(u.retire.bits.robIndex).updateVState := info.updateVState
      rob(u.retire.bits.robIndex).vState := info.vState

      rob(
        u.retire.bits.robIndex
      ).exceptionOccurred := info.exception.ex =/= ExReq.none
    }
  }

  // Commit

  toBPU.valid := false.B
  toBPU.lpc := DontCare
  toBPU.taken := DontCare
  toBPU.hist := DontCare

  retireNum := 0.U

  // Default: no branch if nothing is retired
  // nepc is set to the next retiring instruction for interrupts
  toCtrl.branch.nofire
  toCtrl.tval := DontCare
  toCtrl.nepc := inflights.reader.view(0).addr

  //cdb.entries(coredef.UNIT_COUNT) := DontCare
  //cdb.entries(coredef.UNIT_COUNT).valid := false.B

  // do not update fflags by default
  toCtrl.updateFFlags.valid := false.B
  toCtrl.updateFFlags.bits := 0.U

  // do not update vState by default
  toCtrl.updateVState.valid := false.B
  toCtrl.updateVState.bits := 0.U.asTypeOf(new VState())

  // single stepping counter
  val singleStepCounter = RegInit(0.U(1.W))
  val checkStep = WireInit(toCtrl.step && ~toCtrl.debugMode)
  when(checkStep) {
    assert(singleStepCounter + retireNum <= 1.U)
    singleStepCounter := singleStepCounter + retireNum
  }.elsewhen(toCtrl.debugMode) {
    singleStepCounter := 0.U
  }
  toCtrl.stepAck := false.B

  val retireNext = rob(retirePtr)

  when(!retireNext.valid) {
    // First one invalid, cannot retire anything
    retireNum := 0.U
    toCtrl.branch := ExceptionResult.empty
  }.elsewhen(checkStep && singleStepCounter === 1.U) {
    // Not in debug mode, step=1
    // One instruction has been retired
    // Retires nothing
    toCtrl.stepAck := true.B
    retireNum := 0.U

    toCtrl.branch := ExceptionResult.empty
  }.elsewhen(retireNext.hasMem) {
    // Is memory operation, wait for memAccSucc

    // NOTE: previously, for some reason,
    // a one tick delay is added here
    releaseMem.deq()

    // For BPU mis-predict on previous instructions
    toCtrl.branch := ExceptionResult.empty

    when(releaseMem.fire) {
      retireNum := 1.U
      rob(retirePtr).clear()
      retirePtr := retirePtr +% 1.U

      // if hasMem=true, it should never signals exception
      // because in lsu, we check for exceptions early
      when(pendingBr && pendingBrTag === retirePtr) {
        assert(false.B)
        // toCtrl.branch := pendingBrResult
      }
    }.otherwise {
      retireNum := 0.U
    }
  }.elsewhen(toCtrl.int && toCtrl.intAck) {
    // Interrupts inbound, retires nothing
    retireNum := 0.U

    toCtrl.branch := ExceptionResult.empty
  }.otherwise {
    val blocked = Wire(Vec(coredef.RETIRE_NUM, Bool()))
    val isBranch = Wire(Vec(coredef.RETIRE_NUM, Bool()))

    val mask = blocked.asUInt
    val retireNumFast = Mux(
      mask === 0.U,
      coredef.RETIRE_NUM.U,
      OHToUInt(
        PriorityEncoderOH(
          blocked
        )
      )
    )
    retireNum := retireNumFast

    // First only not memory operation, possible to do multiple retirement
    // Compute if we can retire a certain instruction
    for (idx <- (0 until coredef.RETIRE_NUM)) {
      val inflight = inflights.reader.view(idx)
      val tag = retirePtr +% idx.U
      val info = rob(tag)

      // NOTE: it's okay to fire two beq at the same time.
      // If the first beq mis-predicted, it will be used to train BPU.
      // Otherwise, the BPU will get the result of the second beq.
      isBranch(idx) := (
        pendingBr && pendingBrTag === tag
      )

      if (idx == 0) {
        blocked(idx) := !info.valid
      } else {
        // Only allow mem ops in the first retire slot
        // Stall second retire slot when step=1
        blocked(idx) := !info.valid || isBranch(
          idx - 1
        ) || info.hasMem || checkStep
      }

      when(idx.U < retireNum) {
        // TODO

        // update fflags
        when(info.updateFFlags) {
          toCtrl.updateFFlags.valid := true.B
          toCtrl.updateFFlags.bits := info.fflags
        }

        // update vState
        when(info.updateVState) {
          toCtrl.updateVState.valid := true.B
          toCtrl.updateVState.bits := info.vState
        }

        // Update BPU accordingly
        when(
          inflight.op === Decoder.Op("BRANCH").ident ||
            inflight.op === Decoder.Op("JAL").ident
        ) {
          toBPU.valid := true.B
          toBPU.lpc := inflight.npc - 1.U
          toBPU.taken := info.branchTaken
          toBPU.hist := inflight.pred
        }

        when(
          pendingBr && pendingBrTag === tag && pendingBrResult.ex === ExReq.ex
        ) {
          toCtrl.tval := pendingBrTval
          toCtrl.nepc := inflight.addr
        }

        info.clear()
      }.otherwise {}
    }

    toCtrl.branch := ExceptionResult.empty
    when(pendingBr && pendingBrTag -% retirePtr < retireNumFast) {
      toCtrl.branch := pendingBrResult
    }
  }

  // Renamer release
  for (i <- (0 until coredef.RETIRE_NUM)) {
    // TODO
    // renamer.toExec.releases(i).name := rob(retirePtr +% i.U).retirement.instr.rdname
    renamer.toExec.releases(i).staleRdPhys := inflights.reader
      .view(i)
      .staleRdPhys
    renamer.toExec.releases(i).rdPhys := inflights.reader
      .view(i)
      .rdPhys
    renamer.toExec.releases(i).rdIndex := inflights.reader.view(i).rdIndex
    // only release stale register if no exception occurred
    renamer.toExec.releases(i).valid := i.U < retireNum &&
      !rob(retirePtr +% i.U).exceptionOccurred
  }

  renamer.toExec.retire := retireNum
  inflights.reader.accept := retireNum
  assert(inflights.reader.cnt >= retireNum)

  // intAck
  when(retireNext.valid && retireNext.hasMem) {
    toCtrl.intAck := false.B
  }.otherwise {
    // We need to ensure that the address we are giving ctrl is valid
    // which is equivalent to the inflight queue being not empty
    toCtrl.intAck := retirePtr =/= issuePtr
  }

  // Flushing control
  when(toCtrl.ctrl.flush) {
    issuePtr := 0.U
    retirePtr := 0.U
    pendingBr := false.B

    for (row <- rob) {
      row.clear()
    }
  }
}

class ROBEntry(implicit val coredef: CoreDef) extends Bundle {

  /** This entry is valid
    */
  val valid = Bool()

  /** Has delayed memory access
    */
  val hasMem = Bool()

  /** Branch has taken
    */
  val branchTaken = Bool()

  /** Update fflags
    */
  val updateFFlags = Bool()
  val fflags = UInt(5.W)

  /** Update vState
    */
  val updateVState = Bool()
  val vState = new VState()

  /** Exception has occurred
    */
  val exceptionOccurred = Bool()

  def clear() = {
    this.valid := false.B
    this.hasMem := false.B
    this.branchTaken := false.B
    this.updateFFlags := false.B
    this.updateVState := false.B
    this.exceptionOccurred := false.B
  }
}

object ROBEntry {
  def empty(implicit coredef: CoreDef) = {
    val ret = Wire(new ROBEntry)
    ret.hasMem := false.B
    ret.valid := false.B
    ret.branchTaken := false.B
    ret.updateFFlags := false.B
    ret.updateVState := false.B
    ret.fflags := 0.U
    ret.vState := 0.U.asTypeOf(new VState)
    ret.exceptionOccurred := false.B

    ret
  }
}
object Exec {

  def route(instr: Instr)(implicit coredef: CoreDef): UInt = {
    val ret = Wire(Vec(coredef.ISSUE_QUEUES.length, Bool()))

    // compute bitset from unit configuration
    for ((info, idx) <- coredef.ISSUE_QUEUES.zipWithIndex) {
      // compute bit mask
      ret(
        idx
      ) := (instr.info.issueQueue.asUInt & info.issueQueueType.value.U) === info.issueQueueType.value.U
    }

    Cat(ret.reverse)
  }
}
