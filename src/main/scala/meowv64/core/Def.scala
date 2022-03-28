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
  * @param physRegs
  *   Number of physical registers
  * @param maxOperandCount
  *   Maximum number of operands
  * @param fixedZero
  *   Zero register is fixed to zero
  */
case class RegInfo(
    regType: RegType.Type,
    width: Int,
    physRegs: Int,
    maxOperandCount: Int,
    fixedZero: Boolean
) {
  // it may deadlock if physical register is no greater than architectural registers
  assert(physRegs > 32)
}

case class IssueQueueInfo(
    issueQueueType: IssueQueueType.Type,
    depth: Int,
    ports: Seq[PortInfo]
)

/** Info about execution unit
  *
  * @param execUnitType
  *   Execution unit type enum
  * @param maxOperandCount
  *   Maximum operand count
  * @param regType
  *   Operand register type
  * @param writeRegTypes
  *   Write destination register types
  */
case class ExecutionUnitInfo(
    execUnitType: ExecUnitType.Type,
    maxOperandCount: Int,
    regType: RegType.Type,
    writeRegTypes: Seq[RegType.Type]
)

class ExecutionUnitALU
    extends ExecutionUnitInfo(
      ExecUnitType.alu,
      2,
      RegType.integer,
      Seq(RegType.integer)
    )
class ExecutionUnitBranch
    extends ExecutionUnitInfo(
      ExecUnitType.branch,
      2,
      RegType.integer,
      Seq(RegType.integer)
    )
class ExecutionUnitCSR
    extends ExecutionUnitInfo(
      ExecUnitType.csr,
      2,
      RegType.integer,
      Seq(RegType.integer)
    )
class ExecutionUnitBypass
    extends ExecutionUnitInfo(
      ExecUnitType.bypass,
      0,
      RegType.integer,
      Seq(RegType.integer)
    )
class ExecutionUnitInt2Float
    extends ExecutionUnitInfo(
      ExecUnitType.intToFloat,
      1,
      RegType.integer,
      Seq(RegType.float)
    )
class ExecutionUnitMul
    extends ExecutionUnitInfo(
      ExecUnitType.mul,
      2,
      RegType.integer,
      Seq(RegType.integer)
    )
class ExecutionUnitDiv
    extends ExecutionUnitInfo(
      ExecUnitType.div,
      2,
      RegType.integer,
      Seq(RegType.integer)
    )
class ExecutionUnitFMA
    extends ExecutionUnitInfo(
      ExecUnitType.fma,
      3,
      RegType.float,
      Seq(RegType.float)
    )
class ExecutionUnitFloat2Int
    extends ExecutionUnitInfo(
      ExecUnitType.floatToInt,
      2,
      RegType.float,
      Seq(RegType.integer)
    )

// fsd/fsw instructions send to fp & mem issue queues
class ExecutionUnitFloat2Mem
    extends ExecutionUnitInfo(
      ExecUnitType.lsu,
      2,
      RegType.float,
      Seq(RegType.float)
    )

class ExecutionUnitFloatMisc
    extends ExecutionUnitInfo(
      ExecUnitType.floatMisc,
      2,
      RegType.float,
      Seq(RegType.float)
    )
class ExecutionUnitFloatDivSqrt
    extends ExecutionUnitInfo(
      ExecUnitType.floatDivSqrt,
      2,
      RegType.float,
      Seq(RegType.float)
    )
class ExecutionUnitLSU
    extends ExecutionUnitInfo(
      ExecUnitType.lsu,
      2,
      RegType.integer,
      Seq(RegType.integer, RegType.float, RegType.vector)
    )

class ExecutionUnitVectorALU
    extends ExecutionUnitInfo(
      ExecUnitType.vectorAlu,
      2,
      RegType.vector,
      Seq(RegType.vector)
    )

class ExecutionUnitVectorFMA
    extends ExecutionUnitInfo(
      ExecUnitType.vectorFma,
      4, // vs1 vs2 vd vm
      RegType.vector,
      Seq(RegType.vector)
    )

class ExecutionUnitVectorToMem
    extends ExecutionUnitInfo(
      ExecUnitType.lsu,
      1,
      RegType.vector,
      Seq(RegType.vector)
    )

class ExecutionUnitVectorMisc
    extends ExecutionUnitInfo(
      ExecUnitType.vectorMisc,
      1,
      RegType.vector,
      Seq(RegType.integer, RegType.float, RegType.vector)
    )

/** Each port can read from one register file, potentially write to one or more
  * register files
  */
case class PortInfo(
    regType: RegType.Type,
    units: Seq[ExecutionUnitInfo],
    readPorts: Int
)(implicit coredef: CoreDef) {
  def regInfo = coredef.REG_MAPPING(regType)

  // all units should have the same input register type with the port
  for (unit <- units) {
    assert(unit.regType == regType)
  }

  // writeRegTypes should be the union of writeRegTypes of units
  def writeRegTypes = units.flatMap(_.writeRegTypes).toSet.toSeq

  // find the reg type with largest width
  def widestWriteRegType = writeRegTypes.maxBy(coredef.REG_MAPPING(_).width)
}

/** Write ports to register file.
  *
  * Use arbitration to reduce register write port count.
  */
case class RegWritePortInfo(
    regType: RegType.Type,
    ports: Seq[Int]
)(implicit coredef: CoreDef) {
  def regInfo = coredef.REG_MAPPING(regType)
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
  val BHT_SIZE: Int = 32
  val BHT_WIDTH: Int = 2
  // write bypass entries in BPU
  val BPU_WRITE_BYPASS_COUNT: Int = 2

  val TLB_SIZE: Int = 32

  val HART_ID: Int

  /** Inflight instruction limit, also rob size
    */
  val INFLIGHT_INSTR_LIMIT = 32

  val LSU_PORT_INFO: PortInfo =
    // port 3: LSU
    PortInfo(
      RegType.integer,
      Seq(
        new ExecutionUnitLSU()
      ),
      2
    )(this)

  val LSQ_DEPTH: Int = 16

  val ISSUE_QUEUES: Seq[IssueQueueInfo] = Seq(
    // Integer issue queue
    IssueQueueInfo(
      IssueQueueType.int,
      16,
      ports = Seq(
        // port 0: ALU + Branch + CSR + Bypass
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
        // port 1: ALU + Mul + Div + Int2Float + Branch
        PortInfo(
          RegType.integer,
          Seq(
            new ExecutionUnitALU(),
            new ExecutionUnitMul(),
            new ExecutionUnitDiv(),
            new ExecutionUnitInt2Float(),
            new ExecutionUnitBranch()
          ),
          2
        )(this)
      )
    ),
    // Float issue queue
    IssueQueueInfo(
      IssueQueueType.float,
      16,
      ports = Seq(
        // port 2: FMA + FloatMisc + FloatDivSqrt + FloatToInt + FloatToMem
        PortInfo(
          RegType.float,
          Seq(
            new ExecutionUnitFMA(),
            new ExecutionUnitFloatMisc(),
            new ExecutionUnitFloatDivSqrt(),
            new ExecutionUnitFloat2Int(),
            new ExecutionUnitFloat2Mem()
          ),
          3 // rs1 rs2 rs3
        )(this)
      )
    ),
    // Memory issue queue
    IssueQueueInfo(
      IssueQueueType.mem,
      16,
      ports = Seq(
        // port 3: LSU
        LSU_PORT_INFO
      )
    ),
    // Vector issue queue
    IssueQueueInfo(
      IssueQueueType.vec,
      16,
      ports = Seq(
        // port 4: VectorALU
        PortInfo(
          RegType.vector,
          Seq(
            new ExecutionUnitVectorALU(),
            new ExecutionUnitVectorFMA(),
            new ExecutionUnitVectorToMem(),
            new ExecutionUnitVectorMisc()
          ),
          4 // vs1 vs2 vs3/vd vm
        )(this)
      )
    )
  )

  /** Ports
    */
  val PORTS: Seq[PortInfo] = ISSUE_QUEUES.flatMap(_.ports)

  /** Register write ports
    */
  val REG_WRITE_PORTS: Seq[RegWritePortInfo] = Seq(
    // integer
    RegWritePortInfo(RegType.integer, Seq(0))(this), // port 0 alu
    RegWritePortInfo(RegType.integer, Seq(1))(this), // port 1 alu
    RegWritePortInfo(RegType.integer, Seq(3, 2, 4))(
      this
    ), // port 2 float2int & 3 lsu & 4 vector
    // float
    RegWritePortInfo(RegType.float, Seq(2))(this), // port 2 float
    RegWritePortInfo(RegType.float, Seq(3, 1, 4))(
      this
    ), // port 1 int2float & 3 lsu & 4 vector
    // vector
    RegWritePortInfo(RegType.vector, Seq(3))(
      this
    ), // port & 3 lsu
    RegWritePortInfo(RegType.vector, Seq(4))(this) // port 4 vector
  )

  /** L1 line width in bytes
    */
  val L1_LINE_BYTES: Int = 16

  /** Return address stack size
    */
  val RAS_SIZE: Int = 8

  def REG_INT =
    RegInfo(RegType.integer, XLEN, 64, 2, true)
  def REG_FLOAT =
    RegInfo(RegType.float, XLEN, 48, 3, false)
  def REG_VEC =
    RegInfo(RegType.vector, VLEN, 48, 4, false)

  /** List of register configurations
    */
  def REG_TYPES: Seq[RegInfo] =
    Seq(
      REG_INT,
      REG_FLOAT,
      REG_VEC
    )

  def REG_MAPPING: Map[RegType.Type, RegInfo] =
    Map(
      (RegType.integer, REG_INT),
      (RegType.float, REG_FLOAT),
      (RegType.vector, REG_VEC)
    )

  /** Maximum physical register count across different types
    */
  def MAX_PHYSICAL_REGISTERS: Int = REG_TYPES.map(_.physRegs).max

  /** Compute register read ports
    */
  def REG_READ_PORT_COUNT: Map[RegType.Type, Int] = {
    for (regInfo <- REG_TYPES) yield {
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
  def REG_WRITE_PORT_COUNT: Map[RegType.Type, Int] = {
    for (regInfo <- REG_TYPES) yield {
      // collect all write ports
      (
        regInfo.regType,
        REG_WRITE_PORTS.count(_.regType == regInfo.regType)
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
