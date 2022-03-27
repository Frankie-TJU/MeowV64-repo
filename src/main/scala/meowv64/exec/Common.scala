package meowv64.exec

import chisel3.Module
import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil
import meowv64.core.CSRWriter
import meowv64.core.CoreDef
import meowv64.core.ExReq
import meowv64.core.ExType
import meowv64.core.PortInfo
import meowv64.core.PrivLevel
import meowv64.core.RegInfo
import meowv64.core.Status
import meowv64.instr.BPUResult
import meowv64.instr.BranchPrediction
import meowv64.instr.Decoder.InstrType
import meowv64.instr.InstrExt
import meowv64.instr.RegIndex
import meowv64.reg.RegType

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

  /** Exception request
    */
  val ex = ExReq()

  /** Exception type
    */
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

  /** Standard RISC-V Exception
    */
  def ex(et: ExType.Type) {
    valid := false.B
    target := DontCare
    iRst := false.B
    tlbRst := false.B

    ex := ExReq.ex
    exType := et
  }

  /** MRET/SRET/DRET
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
class RetireInfo(val regInfo: RegInfo)(implicit val coredef: CoreDef)
    extends Bundle {

  /** Writeback data or trap value(mtval, stval)
    */
  val wb = UInt(regInfo.width.W)

  /** Update fflags for floating point operations
    */
  val updateFFlags = Bool()
  val fflags = UInt(5.W)

  /** Exception result
    */
  val exception = new ExceptionResult

  /** Whether this branch has taken. Used in updating BPU.
    */
  val branchTaken = Bool()
}

object RetireInfo {
  def vacant(regInfo: RegInfo)(implicit coredef: CoreDef): RetireInfo = {
    val info = Wire(new RetireInfo(regInfo))

    info.exception.nofire
    info.wb := 0.U
    info.branchTaken := false.B
    info.updateFFlags := false.B
    info.fflags := 0.U

    info
  }
}

/** Instruction in execution pipeline, after all operands are ready
  *
  * Besides the instruction from decoding, we have the following additional
  * fields
  *   - rs1val: value of the rs1 operand
  *   - rs2val: value of the rs2 operand
  *   - rs3val: value of the rs3 operand
  *
  * @param coredef
  */
class PipeInstr(val regInfo: RegInfo)(implicit val coredef: CoreDef)
    extends Bundle() {
  val instr = new InstrExt

  val rs1val = UInt(regInfo.width.W)
  val rs2val = UInt(regInfo.width.W)
  val rs3val = UInt(regInfo.width.W)

  /** Physical register of rd
    */
  val rdPhys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)

  /** ROB index
    */
  val robIndex = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)

  /** LSQ index
    */
  val lsqIndex = UInt(log2Ceil(coredef.LSQ_DEPTH).W)

  /** Illegal instruction
    */
  def illegal = instr.illegal
  def valid = instr.valid
}

/** Instruction in issue queue
  *
  * Besides the fields in InstrExt, we have the following additional fields
  *   - rs1Phys: physical register of the rs1 operand
  *   - rs2Phys: physical register of the rs2 operand
  *   - rs3Phys: physical register of the rs3 operand
  *   - rs1Ready: is rs1 ready?
  *   - rs2Ready: is rs2 ready?
  *   - rs3Ready: is rs3 ready?
  *   - rdPhys: physical register of rd
  *   - robIndex: index of this instruction in rob
  *   - lsqIndex: index of this instruction in load store queue
  *
  * @param coredef
  */
class IssueQueueInstr()(implicit
    val coredef: CoreDef
) extends Bundle {
  val instr = new InstrExt

  val rs1Phys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)
  val rs2Phys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)
  val rs3Phys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)
  val rs1Ready = Bool()
  val rs2Ready = Bool()
  val rs3Ready = Bool()

  /** Physical register of rd
    */
  val rdPhys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)

  /** Physical register of rd previously
    */
  val staleRdPhys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)

  /** ROB index
    */
  val robIndex = UInt(log2Ceil(coredef.INFLIGHT_INSTR_LIMIT).W)

  /** LSQ index
    */
  val lsqIndex = UInt(log2Ceil(coredef.LSQ_DEPTH).W)

  /** Illegal instruction
    */
  def illegal = instr.illegal

  def ready = (illegal || (rs1Ready && rs2Ready && rs3Ready))
}

/** Instruction pushed by issuer, and reused by rob
  */
class InflightInstr(implicit val coredef: CoreDef) extends Bundle {
  // val rdname = UInt(coredef.XLEN.W)
  // rdname === tag, so we don't need this wire anymore
  val op = UInt(5.W)
  val isC = Bool()
  val addr = UInt(coredef.XLEN.W)

  /** Release physical register
    */
  val writeRdEff = Bool()
  val staleRdPhys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)
  val rdPhys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)
  val rdIndex = new RegIndex()

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
  def from(instr: InstrExt, renamed: IssueQueueInstr)(implicit
      coredef: CoreDef
  ) = {
    val ret = Wire(new InflightInstr)
    ret.op := instr.instr.op
    ret.addr := instr.addr
    ret.isC := instr.instr.base === InstrType.C
    ret.rdIndex := instr.instr.getRd()
    ret.writeRdEff := instr.instr.writeRdEff()
    ret.rdPhys := renamed.rdPhys
    ret.staleRdPhys := renamed.staleRdPhys
    ret.pred := instr.pred
    ret.overridePred := instr.overridePred

    ret
  }
}

object PipeInstr {
  def empty(regInfo: RegInfo)(implicit coredef: CoreDef): PipeInstr = {
    val ret = Wire(new PipeInstr(regInfo))
    ret.instr := InstrExt.empty
    ret.rs1val := DontCare
    ret.rs2val := DontCare
    ret.rs3val := DontCare
    ret.rdPhys := 0.U
    ret.robIndex := 0.U
    ret.lsqIndex := 0.U

    ret
  }
}

object IssueQueueInstr {
  def empty()(implicit coredef: CoreDef): IssueQueueInstr = {
    val ret = Wire(new IssueQueueInstr())
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
  *   - retiredInstr: The instruction that just finished executing
  *   - retirement: Result of execution
  *
  * @param coredef
  */
class ExecUnitPort(val regInfo: RegInfo)(implicit
    val coredef: CoreDef
) extends Bundle {
  val next = Input(new PipeInstr(regInfo))

  val stall = Output(Bool())
  val flush = Input(Bool())

  /** The instruction that just finished execution
    */
  val retirement = Output(new RetireInfo(regInfo))

  /** Result of execution
    */
  val retiredInstr = Output(new PipeInstr(regInfo))
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
  *   core definition
  */
abstract class ExecUnit[T <: Data](
    val DEPTH: Int,
    val ExtData: T,
    val regInfo: RegInfo
)(implicit
    val coredef: CoreDef
) extends Module
    with ExecUnitInt {
  val io = IO(new ExecUnitPort(regInfo))

  var current = if (DEPTH != 0) {
    val storeInit = Wire(
      Vec(
        DEPTH,
        new Bundle {
          val pipe = new PipeInstr(regInfo)
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
          c.pipe := PipeInstr.empty(regInfo)
          c.ext := DontCare
        }
      }

      io.retiredInstr := current(DEPTH - 1).pipe
      when(!io.retiredInstr.instr.valid) {
        io.retirement := RetireInfo.vacant(regInfo)
      }.otherwise {
        io.retirement := finalize(current(DEPTH - 1).pipe, nExt)
      }
      io.stall := stall || lStall
    } else {
      val (nExt, sStall) = connectStage(0, io.next, None)
      // Use chisel's unconnected wire check to enforce that no ext is exported from this exec unit
      io.retiredInstr := io.next
      when(!io.retiredInstr.instr.valid) {
        io.retirement := RetireInfo.vacant(regInfo)
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
  *   - phys: index of the broadcasted physical register
  *   - data: value to be broadcasted
  *
  * @param coredef
  *   core definition
  */
class CDBEntry(val portInfo: PortInfo)(implicit val coredef: CoreDef)
    extends Bundle {
  val valid = Bool()
  val phys = UInt(log2Ceil(coredef.MAX_PHYSICAL_REGISTERS).W)
  val regType = RegType()
  val data = UInt(
    portInfo.writeRegTypes.map(coredef.REG_MAPPING(_).width).max.W
  )
}

/** Common data bus
  *
  * @param coredef
  *   core definition
  */
class CDB(implicit val coredef: CoreDef) extends Bundle {
  // each port has a corresponding cdb entry
  val entries = MixedVec(
    coredef.PORTS.map(portInfo => new CDBEntry(portInfo))
  )
}

/** Additional ports
  */

class DelayedMemResult(implicit val coredef: CoreDef) extends Bundle {}

trait WithCSRWriter {
  val writer: CSRWriter
}

trait WithPrivPort {
  val priv: PrivLevel.Type
}

trait WithStatus {
  val status: Status
}

class FloatToMemReq(implicit val coredef: CoreDef) extends Bundle {
  val data = UInt(coredef.XLEN.W)
  val lsqIdx = UInt(log2Ceil(coredef.LSQ_DEPTH).W)
}

trait WithFloatToMem {
  val toMem: ValidIO[FloatToMemReq]
}

class VectorToMemReq(implicit val coredef: CoreDef) extends Bundle {
  val data = UInt(coredef.VLEN.W)
  val lsqIdx = UInt(log2Ceil(coredef.LSQ_DEPTH).W)
}

trait WithVectorToMem {
  val toMem: ValidIO[VectorToMemReq]
}