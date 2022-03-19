package meowv64.core

import chisel3.util.log2Ceil
import meowv64.cache._
import meowv64.instr.ExecUnitType
import meowv64.instr.IssueQueueType
import meowv64.reg.RegType

/** Info about register
  *
  * @param regType
  *   Register type
  * @param width
  *   Register width
  * @param physicalRegs
  *   Number of physical registers
  * @param maxOperandNum
  *   Maximum number of operands
  * @param fixedZero
  *   Zero register is fixed to zero
  */
case class RegInfo(
    regType: RegType.Type,
    width: Int,
    physicalRegs: Int,
    maxOperandNum: Int,
    fixedZero: Boolean
)

case class IssueQueueInfo(
    issueQueueType: IssueQueueType.Type,
    depth: Int,
    ports: Seq[PortInfo]
)

/** Info about execution unit
  *
  * @param execUnitType
  *   Execution unit type enum
  * @param maxOperandNum
  *   Maximum operand count
  * @param regType
  *   Operand register type
  */
case class ExecutionUnitInfo(
    execUnitType: ExecUnitType.Type,
    maxOperandNum: Int,
    regType: RegType.Type
)

class ExecutionUnitALU
    extends ExecutionUnitInfo(ExecUnitType.alu, 2, RegType.integer)
class ExecutionUnitBranch
    extends ExecutionUnitInfo(ExecUnitType.branch, 2, RegType.integer)
class ExecutionUnitCSR
    extends ExecutionUnitInfo(ExecUnitType.csr, 2, RegType.integer)
class ExecutionUnitBypass
    extends ExecutionUnitInfo(ExecUnitType.bypass, 0, RegType.integer)
class ExecutionUnitMul
    extends ExecutionUnitInfo(ExecUnitType.mul, 2, RegType.integer)
class ExecutionUnitDiv
    extends ExecutionUnitInfo(ExecUnitType.div, 2, RegType.integer)
class ExecutionUnitFMA
    extends ExecutionUnitInfo(ExecUnitType.fma, 3, RegType.float)
class ExecutionUnitFloatMisc
    extends ExecutionUnitInfo(ExecUnitType.floatMisc, 2, RegType.float)
class ExecutionUnitFloatDivSqrt
    extends ExecutionUnitInfo(ExecUnitType.floatDivSqrt, 2, RegType.float)
class ExecutionUnitLSU
    extends ExecutionUnitInfo(ExecUnitType.lsu, 2, RegType.integer)

case class PortInfo(
    regType: RegType.Type,
    units: Seq[ExecutionUnitInfo],
    readPorts: Int
)(implicit coredef: CoreDef) {
  def regInfo = coredef.REGISTER_MAPPING(regType)
}

abstract class CoreDef {
  outer =>
  val XLEN: Int = 64
  val VLEN: Int = 256
  val VADDR_WIDTH: Int = 48
  val PADDR_WIDTH: Int = 56
  val FETCH_NUM: Int = 2
  val ISSUE_NUM: Int = 2
  val RETIRE_NUM: Int = 2
  val ISSUE_FIFO_DEPTH: Int = FETCH_NUM * 4
  val INIT_VEC: BigInt = BigInt("FFFF20000000", 16)

  // branch history table
  val BHT_SIZE: Int = 256
  val BHT_WIDTH: Int = 2
  // write bypass entries in BPU
  val BPU_WRITE_BYPASS_COUNT: Int = 2

  val TLB_SIZE: Int = 32

  val HART_ID: Int

  /** Inflight instruction limit, also rob size
    */
  val INFLIGHT_INSTR_LIMIT = 32

  val ISSUE_QUEUES: Seq[IssueQueueInfo] = Seq(
    // Integer issue queue
    IssueQueueInfo(
      IssueQueueType.int,
      16,
      ports = Seq(
        // port 1: ALU + Branch + CSR + Bypass
        PortInfo(
          RegType.integer,
          Seq(
            new ExecutionUnitALU(),
            new ExecutionUnitBranch(),
            new ExecutionUnitCSR(),
            new ExecutionUnitBypass()
          ),
          2
        )(this),
        // port 2: ALU + Mul + Div
        PortInfo(
          RegType.integer,
          Seq(
            new ExecutionUnitALU(),
            new ExecutionUnitMul(),
            new ExecutionUnitDiv()
          ),
          2
        )(this)
      )
    ),
    // Float issue queue
    IssueQueueInfo(
      IssueQueueType.fp,
      16,
      ports = Seq(
        // port 3: FMA + FloatMisc + FloatDivSqrt
        PortInfo(
          RegType.float,
          Seq(
            new ExecutionUnitFMA(),
            new ExecutionUnitFloatMisc(),
            new ExecutionUnitFloatDivSqrt()
          ),
          3
        )(this)
      )
    ),
    // Memory issue queue
    IssueQueueInfo(
      IssueQueueType.mem,
      16,
      ports = Seq(
        // port 5: LSU
        PortInfo(
          RegType.integer,
          Seq(
            new ExecutionUnitLSU()
          ),
          2
        )(this)
      )
    )
  )

  /** Ports
    */
  val PORTS: Seq[PortInfo] = ISSUE_QUEUES.flatMap(_.ports)

  /** L1 line width in bytes
    */
  val L1_LINE_BYTES: Int = 16

  /** Return address stack size
    */
  val RAS_SIZE: Int = 8

  def REGISTER_INTEGER =
    RegInfo(RegType.integer, XLEN, 64, 2, true)
  def REGISTER_FLOAT =
    RegInfo(RegType.float, XLEN, 32, 3, false)
  def REGISTER_VECTOR =
    RegInfo(RegType.vector, VLEN, 32, 4, false)

  /** List of register configurations
    */
  def REGISTER_TYPES: Seq[RegInfo] =
    Seq(
      REGISTER_INTEGER,
      REGISTER_FLOAT
    )

  def REGISTER_TYPE_COUNT: Int = REGISTER_TYPES.length

  def REGISTER_MAPPING: Map[RegType.Type, RegInfo] =
    Map((RegType.integer, REGISTER_INTEGER), (RegType.float, REGISTER_FLOAT))

  /** Maximum physical register count across different types
    */
  def MAX_PHYSICAL_REGISTERS: Int = REGISTER_TYPES.map(_.physicalRegs).max

  /** Compute register read ports
    */
  def REGISTER_READ_PORTS: Map[RegType.Type, Int] = {
    for (regInfo <- REGISTER_TYPES) yield {
      // collect all read ports
      (
        regInfo.regType,
        PORTS
          .filter(_.regType == regInfo.regType)
          .map(_.readPorts)
          .sum
      )
    }
  }.toMap

  /** Compute register write ports
    */
  def REGISTER_WRITE_PORTS: Map[RegType.Type, Int] = {
    for (regInfo <- REGISTER_TYPES) yield {
      // collect all write ports
      (
        regInfo.regType,
        PORTS
          .count(_.regType == regInfo.regType)
      )
    }
  }.toMap

  /** List of supported float types
    */
  def FLOAT_TYPES: Seq[FloatType] = Seq(FloatS, FloatD)

  object L1I
      extends {
        val ADDR_WIDTH: Int = outer.PADDR_WIDTH
        val ASSOC: Int = 2
        val LINE_BYTES: Int = outer.L1_LINE_BYTES
        val SIZE_BYTES: Int = 2048 // 2KB L1 I
        val TO_CORE_TRANSFER_WIDTH: Int = 64 // 64 bits
        val XLEN: Int = outer.XLEN
      }
      with L1Opts

  object L1D
      extends {
        val ADDR_WIDTH: Int = outer.PADDR_WIDTH
        val ASSOC: Int = 2
        val LINE_BYTES: Int = outer.L1_LINE_BYTES
        val SIZE_BYTES: Int = 2048 // 2KB L1 D
        val TO_CORE_TRANSFER_WIDTH: Int = outer.XLEN
        val XLEN: Int = outer.XLEN

        val WRITE_BUF_DEPTH: Int = 4
      }
      with L1DOpts

  def tlbIdxWidth = log2Ceil(TLB_SIZE)
  def vpnWidth = VADDR_WIDTH - 12
  def ppnWidth = PADDR_WIDTH - 12
  def vectorBankCount = VLEN / XLEN
}

// TODO: moves into MulticoreDef
object CoreDef {
  def default(id: Int, initVec: BigInt, cacheLineBytes: Int) = {
    new CoreDef {
      override val HART_ID = id
      override val INIT_VEC = initVec
      override val L1_LINE_BYTES: Int = cacheLineBytes
    }
  }
}
