package meowv64.exec

import chisel3.Module
import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil
import meowv64.core.CSRWriter
import meowv64.core.CoreDef
import meowv64.core.ExReq
import meowv64.core.ExType
import meowv64.core.PrivLevel
import meowv64.core.Status
import meowv64.instr.BPUResult
import meowv64.instr.BranchPrediction
import meowv64.instr.Decoder.InstrType
import meowv64.instr.InstrExt
import meowv64.instr.RegIndex

/** Exception result
  *
  * Exceptions are used to state an interrupt in pipelined execution, which
  * comes from the execution of an instruction. Following conditions are
  * considered as a branch:
  *   - Jumps and branches that don't match branch prediction results
  *   - Instructions caused an exceptions
  *   - Instructions have side-effects on previous stages, so that we need to
  *     flush the pipeline. This includes:
  *     - CSR writes (may alter instr fetch)
  *     - FENCE.I
  *
  * @param coredef:
  *   Core definition
  */
class ExceptionResult(implicit val coredef: CoreDef) extends Bundle {

  /** Whether a pipeline exception occurs
    */
  val valid = Bool()

  /** ICache Flush for fence.i
    */
  val iRst = Bool()

  /** TLB Flush for sfence.vma
    */
  val tlbRst = Bool()

  /** Target PC
    */
  val target = UInt(coredef.XLEN.W)

  val ex = ExReq()
  val exType = ExType()

  def nofire = {
    valid := false.B
    target := 0.U
    iRst := false.B
    tlbRst := false.B

    ex := ExReq.none
    exType := DontCare
  }

  /** Flush pipeline and jump to addr
    */
  def fire(addr: UInt) = {
    valid := true.B
    target := addr
    iRst := false.B
    tlbRst := false.B

    ex := ExReq.none
    exType := DontCare
  }

  def ifence(addr: UInt) = {
    valid := true.B
    target := addr
    iRst := true.B
    tlbRst := false.B

    ex := ExReq.none
    exType := DontCare
  }

  def sfence(addr: UInt) = {
    valid := true.B
    target := addr
    iRst := false.B
    tlbRst := true.B

    ex := ExReq.none
    exType := DontCare
  }

  /** RISC-V Exception
    */
  def ex(et: ExType.Type) {
    valid := false.B
    target := DontCare
    iRst := false.B
    tlbRst := false.B

    ex := ExReq.ex
    exType := et
  }

  /** MRET/SRET
    */
  def ret(req: ExReq.Type) {
    valid := false.B
    target := DontCare
    iRst := false.B
    tlbRst := false.B

    ex := req
    exType := DontCare
  }

  def fire(): Bool = valid || ex =/= ExReq.none
}

object ExceptionResult {
  def empty(implicit coredef: CoreDef): ExceptionResult = {
    val ret = Wire(new ExceptionResult)
    ret.nofire
    ret
  }
}

/** Execution result of an retiring instruction
  *
  * wb: writeback data branch: branch info mem: sequential memory access info
  *
  * @param coredef
  */
class RetireInfo(val retireWidth: Int)(implicit val coredef: CoreDef)
    extends Bundle {

  /** Writeback data or trap value(mtval, stval)
    */
  val wb = UInt(retireWidth.W)

  /** Update fflags for floating point operations
    */
  val updateFFlags = Bool()
  val fflags = UInt(5.W)

  /** Exception result
    */
  val exception = new ExceptionResult

  /** Has delayed memory ops?
    */
  val hasMem = Bool()

  /** Whether this branch has taken. Used in updating BPU.
    */
  val branchTaken = Bool()
}

object RetireInfo {
  def vacant(dataWidth: Int)(implicit coredef: CoreDef): RetireInfo = {
    val info = Wire(new RetireInfo(dataWidth))

    info.exception.nofire
    info.wb := 0.U
    info.hasMem := false.B
    info.branchTaken := false.B
    info.updateFFlags := false.B
    info.fflags := 0.U

    info
  }
}

/** Instruction in execution pipeline, after both operands are ready
  *
  * Besides the instruction from decoding, we have the following additional
  * fields
  *   - rs1val: value of the rs1 operand
  *   - rs2val: value of the rs2 operand
  *   - rs3val: value of the rs3 operand
  *   - rdname: Name of the rd register. This comes from renaming
  *   - tag: tag of this instruction. Tags are self-incrementing based on issue
  *     order, and wraps around at length(rob) = 2^length(name) =
  *     MAX_INFLIGHT_INSTR. When its execution finishes, this instruction is
  *     always put into rob[tag]
  *
  * @param coredef
  */
class PipeInstr(val valueWidth: Int)(implicit val coredef: CoreDef)
    extends Bundle {
  val instr = new InstrExt

  val rs1val = UInt(valueWidth.W)
  val rs2val = UInt(valueWidth.W)
  val rs3val = UInt(valueWidth.W)

  val rdname = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val tag = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
}

/** Instruction in reservation station
  *
  * Besides the fields in PipeStr, we have the following additional fields
  *   - rs1name: name of the rs1 operand
  *   - rs2name: name of the rs2 operand
  *   - rs3name: name of the rs3 operand
  *   - rs1ready: is rs1 ready?
  *   - rs2ready: is rs2 ready?
  *   - rs3ready: is rs3 ready?
  *
  * @param coredef
  */
class ReservedInstr(override val valueWidth: Int)(
    override implicit val coredef: CoreDef
) extends PipeInstr(valueWidth) {
  val rs1name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs2name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs3name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val rs1ready = Bool()
  val rs2ready = Bool()
  val rs3ready = Bool()

  /** Illegal instruction
    */
  def illegal = instr.illegal

  def ready = (illegal || (rs1ready && rs2ready && rs3ready))
}

/** Instruction pushed by issuer, and reused by rob
  */
class InflightInstr(implicit val coredef: CoreDef) extends Bundle {
  // val rdname = UInt(coredef.XLEN.W)
  // rdname === tag, so we don't need this wire anymore
  val op = UInt(5.W)
  val isC = Bool()
  val addr = UInt(coredef.XLEN.W)
  val erd = new RegIndex() // Effective rd
  /** Prediction result from BPU
    */
  val pred = new BPUResult

  /** Override prediction result to be taken e.g. JAL
    */
  val overridePred = Bool() // FIXME: change to default pred

  def taken = overridePred || pred.prediction === BranchPrediction.taken
  def npc = Mux(isC, 2.U, 4.U) +% addr
}

object InflightInstr {
  def from(instr: InstrExt)(implicit coredef: CoreDef) = {
    val ret = Wire(new InflightInstr)
    ret.op := instr.instr.op
    ret.addr := instr.addr
    ret.isC := instr.instr.base === InstrType.C
    ret.erd := instr.instr.getRd()
    ret.pred := instr.pred
    ret.overridePred := instr.overridePred

    ret
  }
}

object PipeInstr {
  def empty(valueWidth: Int)(implicit coredef: CoreDef): PipeInstr = {
    val ret = Wire(new PipeInstr(valueWidth))
    ret.instr := InstrExt.empty
    ret.rs1val := DontCare
    ret.rs2val := DontCare
    ret.rs3val := DontCare
    ret.rdname := DontCare
    ret.tag := DontCare

    ret
  }
}

object ReservedInstr {
  def empty(valueWidth: Int)(implicit coredef: CoreDef): ReservedInstr = {
    val ret = Wire(new ReservedInstr(valueWidth))
    ret := DontCare
    ret.instr := InstrExt.empty

    ret
  }
}

/** IO port of an execution unit
  *
  *   - next: next instruction
  *   - stall: unit -> pipeline stall signal
  *   - flush: Flush signal
  *   - retired: The instruction that just finished executing
  *   - retirement: Result of execution
  *
  * @param coredef
  */
class ExecUnitPort(val valueWidth: Int, val retireWidth: Int)(implicit
    val coredef: CoreDef
) extends Bundle {
  val next = Input(new PipeInstr(valueWidth))

  val stall = Output(Bool())
  val flush = Input(Bool())

  /** The instruction that just finished execution
    */
  val retirement = Output(new RetireInfo(retireWidth))

  /** Result of execution
    */
  val retired = Output(new PipeInstr(valueWidth))
}

/** Trait representing an execution unit
  *
  * DEPTH is the pipeline delay of this unit. For example, ALU, which works in
  * and asynchronous manner, have DEPTH = 0. LSU have a DEPTH of 1, and Div's
  * DEPTH depends on its configuration
  */
trait ExecUnitInt {
  val DEPTH: Int
  val io: ExecUnitPort
}

/** Base class of an execution unit
  *
  * This class automatically generates stage registers, which contains the
  * instruction and an custom bundle specified by the implementation.
  *
  * Implementations are required to implement two methods:
  *   - def map(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool) Mapping
  *     of one stage
  *   - def finalize(pipe: PipeInstr, ext: T): RetireInfo Mapping from the last
  *     stage's output into RetireInfo
  *
  * @param DEPTH:
  *   pipeline delay
  * @param ExtData:
  *   extra data's type
  * @param coredef:
  *   core defination
  */
abstract class ExecUnit[T <: Data](
    val DEPTH: Int,
    val ExtData: T
)(implicit
    val coredef: CoreDef
) extends Module
    with ExecUnitInt {
  def valueWidth: Int = ???
  def retireWidth: Int = ???
  val io = IO(new ExecUnitPort(valueWidth, retireWidth))

  var current = if (DEPTH != 0) {
    val storeInit = Wire(
      Vec(
        DEPTH,
        new Bundle {
          val pipe = new PipeInstr(valueWidth)
          val ext = ExtData.cloneType
        }
      )
    )

    // default wiring
    for (i <- (0 until DEPTH)) {
      storeInit(i) := DontCare
      // storeInit(i).pipe.instr.instr.imm := 0.S // Treadle bug?
      storeInit(i).pipe.instr.valid := false.B
      storeInit(i).ext := DontCare
    }

    RegInit(storeInit)
  } else {
    null
  }

  def init(): Unit = {
    if (DEPTH != 0) {
      // if any stage stall, the whole pipeline stalls
      val (fExt, fStall) = connectStage(0, io.next, None)
      var stall = fStall

      when(!io.stall) {
        current(0).pipe := io.next
        current(0).ext := fExt
      }

      for (i <- (1 until DEPTH)) {
        val (nExt, sStall) =
          connectStage(i, current(i - 1).pipe, Some(current(i - 1).ext))
        when(!io.stall) {
          current(i).pipe := current(i - 1).pipe
          current(i).ext := nExt
        }

        when(sStall) {
          current(i - 1).ext := nExt
        }

        stall = stall || sStall
      }

      val (nExt, lStall) = connectStage(
        DEPTH,
        current(DEPTH - 1).pipe,
        Some(current(DEPTH - 1).ext)
      )

      when(lStall) {
        current(DEPTH - 1).ext := nExt
      }

      when(io.flush) { // Override current
        for (c <- current) {
          c.pipe := PipeInstr.empty(valueWidth)
          c.ext := DontCare
        }
      }

      io.retired := current(DEPTH - 1).pipe
      when(!io.retired.instr.valid) {
        io.retirement := RetireInfo.vacant(retireWidth)
      }.otherwise {
        io.retirement := finalize(current(DEPTH - 1).pipe, nExt)
      }
      io.stall := stall || lStall
    } else {
      val (nExt, sStall) = connectStage(0, io.next, None)
      // Use chisel's unconnected wire check to enforce that no ext is exported from this exec unit
      io.retired := io.next
      when(!io.retired.instr.valid) {
        io.retirement := RetireInfo.vacant(retireWidth)
      }.otherwise {
        io.retirement := finalize(io.next, nExt)
      }
      io.stall := sStall
    }
  }

  /** Override this function and define combinatorial logic
    */
  def map(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool)

  def finalize(pipe: PipeInstr, ext: T): RetireInfo

  def connectStage(stage: Int, pipe: PipeInstr, ext: Option[T]): (T, Bool) = {
    val nExt = Wire(ExtData.cloneType)
    val sStall = Wire(Bool())

    when(!pipe.instr.valid) {
      nExt := DontCare
      sStall := false.B
    }.otherwise {
      val (ce, stall) = map(stage, pipe, ext)
      nExt := ce
      sStall := stall
    }

    (nExt, sStall)
  }
}

/** Entry of common data bus
  *
  *   - valid: does this entry have any data in it?
  *   - name: name of the broadcasted virtual register
  *   - data: value to be broadcasted
  *
  * @param coredef
  *   core definition
  */
class CDBEntry(implicit val coredef: CoreDef) extends Bundle {
  val valid = Bool()
  val name = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)
  val data = UInt(coredef.VLEN.W)
}

/** Common data bus
  *
  * @param coredef
  *   core definition
  */
class CDB(implicit val coredef: CoreDef) extends Bundle {
  val entries = Vec(coredef.UNIT_COUNT + 1, new CDBEntry)
}

/** Additional ports
  */

class DelayedMemResult(implicit val coredef: CoreDef) extends Bundle {
  val hasWB = Bool()
  val data = UInt(coredef.XLEN.W)
}

trait WithCSRWriter {
  val writer: CSRWriter
}

trait WithPrivPort {
  val priv: PrivLevel.Type
}

trait WithStatus {
  val status: Status
}
