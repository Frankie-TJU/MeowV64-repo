package meowv64.exec
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.DecoupledIO
import chisel3.util.log2Ceil
import meowv64.cache.DCFenceStatus
import meowv64.core.CoreDef
import meowv64.core.IssueQueueInfo
import meowv64.instr.Decoder
import meowv64.reg.RegType

/** Issue Queue -> Register Read
  */
class IssueQueueEgress()(implicit val coredef: CoreDef) extends Bundle {
  val instr = Decoupled(new IssueQueueInstr())
  // for LSBuf
  val hasPending = Input(Bool())
}

trait IssueQueue {
  // Sorry, I was unable to come up with a good naming for the ports
  val ingress: Bundle {
    val instr: Vec[DecoupledIO[IssueQueueInstr]]

    // For debugging
    val empty: Bool
    val full: Bool
  }

  val egress: Vec[IssueQueueEgress]

  val cdb: CDB
  val ctrl: Bundle {
    val flush: Bool
  }
}

/** Out-of-Order Issue Queue
  *
  * For every cycle, ISSUE_NUM instrs maybe issued into this station, and
  * ISSUE_NUM ready instrs may start executing
  */
class OoOIssueQueue(info: IssueQueueInfo)(implicit
    coredef: CoreDef
) extends Module
    with IssueQueue {

  val DEPTH = info.depth
  val ISSUE_NUM = coredef.ISSUE_NUM
  val PORT_COUNT = info.ports.length

  val ingress = IO(new Bundle {
    val instr = Flipped(Vec(coredef.ISSUE_NUM, Decoupled(new IssueQueueInstr)))
    val empty = Output(Bool())
    val full = Output(Bool())
  })

  val egress = IO(Vec(PORT_COUNT, new IssueQueueEgress()))

  val cdb = IO(Input(new CDB))
  val ctrl = IO(new Bundle {
    val flush = Input(Bool())
  })

  // lower entries are older, and should be issued first
  val emptyEntries = RegInit(DEPTH.U(log2Ceil(DEPTH + 1).W))
  val store = RegInit(VecInit(Seq.fill(DEPTH)(IssueQueueInstr.empty())))
  val occupied = RegInit(VecInit(Seq.fill(DEPTH)(false.B)))

  // Egress part
  val nextStore = WireDefault(store)
  val egMask = WireDefault(
    VecInit(
      nextStore
        .zip(occupied)
        .map({ case (instr, valid) => valid && instr.ready })
    )
  )
  val nextOccupied = WireDefault(occupied)

  // CDB data fetch
  for ((instr, idx) <- store.zipWithIndex) {
    // Later entries takes priority
    for (ent <- cdb.entries ++ Seq(cdb.wakeup)) {
      when(
        ent.phys === instr.rs1Phys && ent.regType === instr.instr.instr
          .getRs1Type() && ent.valid
      ) {
        nextStore(idx).rs1Ready := true.B
      }

      when(
        ent.phys === instr.rs2Phys && ent.regType === instr.instr.instr
          .getRs2Type() && ent.valid
      ) {
        nextStore(idx).rs2Ready := true.B
      }

      when(
        ent.phys === instr.rs3Phys && ent.regType === instr.instr.instr
          .getRs3Type() && ent.valid
      ) {
        nextStore(idx).rs3Ready := true.B
      }

      when(
        ent.phys === instr.vmPhys && ent.regType === RegType.vector && ent.valid
      ) {
        nextStore(idx).vmReady := true.B
      }
    }
  }

  // default wiring
  for (i <- 0 until PORT_COUNT) {
    egress(i).instr.valid := false.B
    egress(i).instr.bits := DontCare
  }

  // mutable array of port issued
  val portIssued = Array.fill(PORT_COUNT)(Bool())
  for (i <- 0 until PORT_COUNT) {
    portIssued(i) = false.B
  }

  for (i <- 0 until DEPTH) {
    var issued = false.B
    for (j <- 0 until PORT_COUNT) {
      val portUnits = info.ports(j).units.map(_.execUnitType)
      val allow = WireInit(
        portUnits
          .map(_ === nextStore(i).instr.instr.info.execUnit)
          .reduce(_ | _)
      )
      // send illegal instructions to first port
      when(nextStore(i).instr.illegal) {
        allow := (j == 0).B
      }

      val fire = WireInit(false.B)
      when(egMask(i) && !issued && !portIssued(j) && allow) {
        egress(j).instr.valid := true.B
        egress(j).instr.bits := nextStore(i)
        fire := egress(j).instr.ready

        when(fire) {
          nextOccupied(i) := false.B
        }
      }
      issued = issued || fire
      portIssued(j) = portIssued(j) || fire
    }
  }

  // Ingress part
  //
  // Placed after CDB fetch to avoid being overwritten after a flush,
  // when a pending instruction may lies in the store
  ingress.empty := emptyEntries === DEPTH.U
  ingress.full := emptyEntries === 0.U

  // ready if there are empty slots
  for (i <- 0 until ISSUE_NUM) {
    ingress.instr(i).ready := emptyEntries > i.U
  }

  // follow the implementation of boom
  // concat occupied + ingress instructions
  // and compute down shift amount for each element
  val maxShift = ISSUE_NUM
  val shift = WireInit(
    VecInit(Seq.fill(DEPTH + ISSUE_NUM)(0.U(log2Ceil(maxShift + 1).W)))
  )
  val curAllOccupied = WireInit(VecInit(occupied ++ ingress.instr.map(_.valid)))

  shift(0) := 0.U
  for (i <- 1 until DEPTH + ISSUE_NUM) {
    when(curAllOccupied(i - 1)) {
      // if the previous entry is occupied
      // follow suit
      shift(i) := shift(i - 1)
    }.otherwise {
      // otherwise, we are able to shift one more slot
      shift(i) := Mux(
        shift(i - 1) === ISSUE_NUM.U,
        ISSUE_NUM.U,
        shift(i - 1) + 1.U
      )
    }
  }

  // update store & occupied by shifting
  val allInsts = nextStore ++ ingress.instr.map(_.bits)
  val allOccupied = nextOccupied ++ ingress.instr.map(_.valid)
  for (i <- 0 until DEPTH) {
    // if shift != 0, clear by default
    when(shift(i) =/= 0.U) {
      occupied(i) := false.B
    }.otherwise {
      occupied(i) := allOccupied(i)
    }
    // update ready if not shifted
    store(i) := allInsts(i)
    // check if new entry shifted to here
    for (j <- 1 to maxShift) {
      when(shift(i + j) === j.U) {
        occupied(i) := allOccupied(i + j)
        store(i) := allInsts(i + j)
      }
    }
  }

  // maintain emptyEntries count
  val push = ingress.instr.map(_.fire.asUInt).reduce(_ +& _)
  val pop = portIssued.map(_.asUInt).reduce(_ +& _)
  emptyEntries := emptyEntries - push + pop
  val occupiedEntries = occupied.map(_.asUInt).reduce(_ +& _)
  // emptyEntries + occupiedEntries = depth
  assert(occupiedEntries + emptyEntries === DEPTH.U)

  when(ctrl.flush) {
    // We don't need to reset store
    // store := VecInit(Seq.fill(DEPTH)(IssueQueueInstr.empty))
    // clear occupied array
    emptyEntries := DEPTH.U
    for (i <- 0 until DEPTH) {
      occupied(i) := false.B
    }
  }
}

/** Load-Store Buffer
  *
  * Instructions are executed in-order, so effects of all memory operations
  * become visible to the core itself in program order
  *
  * L1 may do RAW and WAW reordering, so the effect may not be in program order
  * for other cores
  */
class LSBuf(info: IssueQueueInfo)(implicit val coredef: CoreDef)
    extends Module
    with IssueQueue {

  val DEPTH = info.depth
  val ISSUE_NUM = coredef.ISSUE_NUM
  val PORT_COUNT = info.ports.length
  assert(PORT_COUNT == 1)

  val ingress = IO(new Bundle {
    val instr = Flipped(Vec(coredef.ISSUE_NUM, Decoupled(new IssueQueueInstr)))
    val empty = Output(Bool())
    val full = Output(Bool())
  })

  val egress = IO(Vec(PORT_COUNT, new IssueQueueEgress()))

  val fs = IO(new DCFenceStatus(coredef.L1D))

  val cdb = IO(Input(new CDB))
  val ctrl = IO(new Bundle {
    val flush = Input(Bool())
  })

  /** Are there any unfinished pending memory operations? */
  val hasPending = IO(
    Input(Bool())
  )

  val store = RegInit(
    VecInit(Seq.fill(DEPTH)(IssueQueueInstr.empty()))
  )

  val head = RegInit(0.U(log2Ceil(DEPTH).W))
  val tail = RegInit(0.U(log2Ceil(DEPTH).W))

  // power of 2
  assume((DEPTH & (DEPTH - 1)) == 0)

  // Egress part
  // Extra restrictions: no pending writes

  val headIsLoad = (
    store(head).instr.instr.op === Decoder.Op("LOAD").ident
      || store(head).instr.instr.op === Decoder.Op("LOAD-FP").ident
      || (store(head).instr.instr.op === Decoder.Op("AMO").ident
        && store(head).instr.instr.funct7(6, 2) === Decoder.AMO_FUNC("LR"))
  )
  val headIsFence = (
    store(head).instr.instr.op === Decoder.Op("MISC-MEM").ident
    // Release ops cannot be reordered before any previous ops
      || (store(head).instr.instr.op === Decoder.Op("AMO").ident
        && store(head).instr.instr.funct7(1))
  )

  // FIXME: are there acquire ops dispatched?

  // TODO: optimize: allow stores with different address to slip over?
  // FIXME: there is one stage for register read, so hasPending is not new,
  // but this solution is bad
  val hasPendingFast = hasPending || egress(0).hasPending
  val loadBlocked = hasPendingFast
  val fenceBlocked = hasPendingFast || !fs.wbufClear
  val instrReady = head =/= tail && store(head).ready
  when(headIsFence) {
    egress(0).instr.valid := instrReady && !fenceBlocked
  }.elsewhen(headIsLoad) {
    egress(0).instr.valid := instrReady && !loadBlocked
  }.otherwise {
    egress(0).instr.valid := instrReady
  }

  egress(0).instr.bits := store(head)
  when(egress(0).instr.fire) { // FIXME: check egress.valid on regular ResStation
    head := head +% 1.U
  }

  // CDB data fetch
  for (instr <- store) {
    // Later entries takes priority
    for (ent <- cdb.entries) {
      when(
        ent.phys === instr.rs1Phys && ent.regType === instr.instr.instr
          .getRs1Type() && ent.valid
      ) {
        instr.rs1Ready := true.B
      }

      when(
        ent.phys === instr.rs2Phys && ent.regType === instr.instr.instr
          .getRs2Type() && ent.valid
      ) {
        instr.rs2Ready := true.B
      }

      when(
        ent.phys === instr.rs3Phys && ent.regType === instr.instr.instr
          .getRs3Type() && ent.valid
      ) {
        instr.rs3Ready := true.B
      }

      when(
        ent.phys === instr.vmPhys && ent.regType === RegType.vector
          && ent.valid
      ) {
        instr.vmReady := true.B
      }
    }
  }

  // Ingress part
  ingress.empty := tail === head
  ingress.full := tail +% 1.U === head
  // simplify: only one instruction ingress at a time
  ingress.instr(0).ready := tail +% 1.U =/= head
  for (i <- 1 until ISSUE_NUM) {
    ingress.instr(i).ready := false.B
  }
  when(ingress.instr(0).fire) {
    store(tail) := ingress.instr(0).bits
    tail := tail +% 1.U
  }

  // Flush
  when(ctrl.flush) {
    head := 0.U
    tail := 0.U
  }
}
