package meowv64.instr

import chisel3._
import chisel3.experimental._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import chisel3.util.experimental.decode.decoder
import meowv64.reg.RegType

// scalafmt: { align.preset = most }
object ExecUnitType extends ChiselEnum {
  val alu, branch, bypass, csr, intToFloat     = Value
  val div, mul                                 = Value
  val fma, floatMisc, floatToInt, floatDivSqrt = Value
  val vectorAlu, vectorMisc                    = Value
  val lsu                                      = Value

  implicit def bitpat(op: ExecUnitType.Type): BitPat =
    BitPat(op.litValue.U(getWidth.W))
}

object IssueQueueType extends ChiselEnum {
  val int, fp, vec, mem = Value

  implicit def bitpat(op: IssueQueueType.Type): BitPat =
    BitPat(op.litValue.U(getWidth.W))
}

class DecodeInfo extends Bundle {
  // legal instruction
  val legal = Bool()

  // register related
  // rd is read as rs3
  val rdAsRs3 = Bool()
  // writes to rd
  val writeRd = Bool()
  // rd register type
  val rdType = RegType()
  // reads rs1
  val readRs1 = Bool()
  // rs1 register type
  val rs1Type = RegType()
  // reads rs2
  val readRs2 = Bool()
  // rs2 register type
  val rs2Type = RegType()
  // reads rs3
  val readRs3 = Bool()
  // rs3 register type
  val rs3Type = RegType()

  // execution unit
  val execUnit = ExecUnitType()
  // issue queue
  val issueQueue = IssueQueueType()

  def signals = Seq(
    legal,
    rdAsRs3,
    writeRd,
    rdType,
    readRs1,
    rs1Type,
    readRs2,
    rs2Type,
    readRs3,
    rs3Type,
    execUnit,
    issueQueue
  )
}

object DecodeInfo {
  val X  = BitPat("b?")
  val XX = BitPat("b??")
  val Y  = BitPat("b1")
  val N  = BitPat("b0")

  import Instructions._
  import RegType._
  import ExecUnitType._
  import IssueQueueType._
  val default: List[BitPat] =
    List(N, N, N, XX, N, XX, N, XX, N, XX, bypass, int)
  val table: Array[(BitPat, List[BitPat])] =
    Array(
      // RV32I Base Instruction Set
      LUI     -> List(Y, N, Y, integer, N, XX, N, XX, N, XX, bypass, int),
      AUIPC   -> List(Y, N, Y, integer, N, XX, N, XX, N, XX, bypass, int),
      JAL     -> List(Y, N, Y, integer, N, XX, N, XX, N, XX, bypass, int),
      JALR    -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, branch, int),
      BEQ     -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, branch, int),
      BNE     -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, branch, int),
      BLT     -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, branch, int),
      BGE     -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, branch, int),
      BLTU    -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, branch, int),
      BGEU    -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, branch, int),
      LB      -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, lsu, mem),
      LH      -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, lsu, mem),
      LW      -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, lsu, mem),
      LBU     -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, lsu, mem),
      LHU     -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, lsu, mem),
      SB      -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, lsu, mem),
      SH      -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, lsu, mem),
      SW      -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, lsu, mem),
      ADDI    -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      SLTI    -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      SLTIU   -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      XORI    -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      ORI     -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      ANDI    -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      SLLI    -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      SRLI    -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      SRAI    -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      ADD     -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SUB     -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SLL     -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SLT     -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SLTU    -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      XOR     -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SRL     -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SRA     -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      OR      -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      AND     -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      FENCE   -> List(Y, N, N, XX, N, XX, N, XX, N, XX, lsu, mem),
      FENCE_I -> List(Y, N, N, XX, N, XX, N, XX, N, XX, lsu, mem),
      ECALL   -> List(Y, N, N, XX, N, XX, N, XX, N, XX, branch, int),
      EBREAK  -> List(Y, N, N, XX, N, XX, N, XX, N, XX, branch, int),
      CSRRW   -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, csr, int),
      CSRRS   -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, csr, int),
      CSRRC   -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, csr, int),
      CSRRWI  -> List(Y, N, Y, integer, N, XX, N, XX, N, XX, csr, int),
      CSRRSI  -> List(Y, N, Y, integer, N, XX, N, XX, N, XX, csr, int),
      CSRRCI  -> List(Y, N, Y, integer, N, XX, N, XX, N, XX, csr, int),

      // RV64I Base Instruction Set (in addition to RV32I)
      LWU   -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, lsu, mem),
      LD    -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, lsu, mem),
      SD    -> List(Y, N, N, XX, Y, integer, Y, integer, N, XX, lsu, mem),
      ADDIW -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SLLIW -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      SRLIW -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      SRAIW -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, alu, int),
      ADDW  -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SUBW  -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SLLW  -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SRLW  -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),
      SRAW  -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, alu, int),

      // RV32M Standard Extension
      MUL    -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, mul, int),
      MULH   -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, mul, int),
      MULHSU -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, mul, int),
      MULHU  -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, mul, int),
      DIV    -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, div, int),
      DIVU   -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, div, int),
      REM    -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, div, int),
      REMU   -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, div, int),

      // RV64M Standard Extension (in addition to RV32M)
      MULW  -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, mul, int),
      DIVW  -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, div, int),
      DIVUW -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, div, int),
      REMW  -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, div, int),
      REMUW -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, div, int),

      // RV32A Standard Extension
      LR_W      -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, lsu, mem),
      SC_W      -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, lsu, mem),
      AMOSWAP_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOADD_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOXOR_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOAND_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOOR_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOMIN_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOMAX_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOMINU_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOMAXU_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),

      // RV64A Standard Extension (in addition to RV32A)
      LR_D      -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, lsu, mem),
      SC_D      -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, lsu, mem),
      AMOSWAP_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOADD_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOXOR_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOAND_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOOR_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOMIN_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOMAX_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOMINU_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),
      AMOMAXU_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        lsu,
        mem
      ),

      // RV32F Standard Extension
      FLW      -> List(Y, N, Y, float, Y, integer, N, XX, N, XX, lsu, mem),
      FSW      -> List(Y, N, N, XX, Y, integer, Y, float, N, XX, lsu, mem),
      FMADD_S  -> List(Y, N, Y, float, Y, float, Y, float, Y, float, fma, float),
      FMSUB_S  -> List(Y, N, Y, float, Y, float, Y, float, Y, float, fma, float),
      FNMADD_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        fma,
        float
      ),
      FNMSUB_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        fma,
        float
      ),
      FADD_S -> List(Y, N, Y, float, Y, float, Y, float, N, XX, fma, float),
      FSUB_S -> List(Y, N, Y, float, Y, float, Y, float, N, XX, fma, float),
      FMUL_S -> List(Y, N, Y, float, Y, float, Y, float, N, XX, fma, float),
      FDIV_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatDivSqrt,
        float
      ),
      FSQRT_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatDivSqrt,
        float
      ),
      FSGNJ_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FSGNJN_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FSGNJX_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FMIN_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FMAX_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_W_S -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_WU_S -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FMV_X_W -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FEQ_S -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FLT_S -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FLE_S -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FCLASS_S -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_S_W -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_S_WU -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FMV_W_X -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),

      // RV64F Standard Extension (in addition to RV32F)
      FCVT_L_S -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_LU_S -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_S_L -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_S_LU -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),

      // RV32D Standard Extension
      FLD      -> List(Y, N, Y, float, Y, integer, N, XX, N, XX, lsu, mem),
      FSD      -> List(Y, N, N, XX, Y, integer, Y, float, N, XX, lsu, mem),
      FMADD_D  -> List(Y, N, Y, float, Y, float, Y, float, Y, float, fma, float),
      FMSUB_D  -> List(Y, N, Y, float, Y, float, Y, float, Y, float, fma, float),
      FNMADD_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        fma,
        float
      ),
      FNMSUB_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        fma,
        float
      ),
      FADD_D -> List(Y, N, Y, float, Y, float, Y, float, N, XX, fma, float),
      FSUB_D -> List(Y, N, Y, float, Y, float, Y, float, N, XX, fma, float),
      FMUL_D -> List(Y, N, Y, float, Y, float, Y, float, N, XX, fma, float),
      FDIV_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatDivSqrt,
        float
      ),
      FSQRT_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatDivSqrt,
        float
      ),
      FSGNJ_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FSGNJN_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FSGNJX_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FMIN_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FMAX_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_S_D -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_D_S -> List(
        Y,
        N,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FEQ_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FLT_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FLE_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        Y,
        float,
        N,
        XX,
        floatMisc,
        float
      ),
      FCLASS_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_W_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_WU_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_D_W -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_D_WU -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),

      // RV64D Standard Extension (in addition to RV32D)
      FCVT_L_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_LU_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FMV_X_D -> List(
        Y,
        N,
        Y,
        integer,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_D_L -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FCVT_D_LU -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),
      FMV_D_X -> List(
        Y,
        N,
        Y,
        float,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        floatMisc,
        float
      ),

      // Trap-Return Instructions
      URET -> List(Y, N, N, XX, N, XX, N, XX, N, XX, branch, int),
      SRET -> List(Y, N, N, XX, N, XX, N, XX, N, XX, branch, int),
      MRET -> List(Y, N, N, XX, N, XX, N, XX, N, XX, branch, int),
      DRET -> List(Y, N, N, XX, N, XX, N, XX, N, XX, branch, int),

      // Interrupt-Management Instructions
      WFI -> List(Y, N, N, XX, N, XX, N, XX, N, XX, branch, int),

      // Memory-Management Instructions
      SFENCE_VMA -> List(
        Y,
        N,
        N,
        XX,
        Y,
        integer,
        Y,
        integer,
        N,
        XX,
        branch,
        int
      ),

      // Vector
      VSETVLI  -> List(Y, N, Y, integer, Y, integer, N, XX, N, XX, csr, int),
      VSETIVLI -> List(Y, N, Y, integer, N, XX, N, XX, N, XX, csr, int),
      VSETVL   -> List(Y, N, Y, integer, Y, integer, Y, integer, N, XX, csr, int),

      // Vector Load/Store
      VLE64_V    -> List(Y, N, Y, vector, Y, integer, N, XX, N, XX, lsu, mem),
      VLUXEI64_V -> List(
        Y,
        N,
        Y,
        vector,
        Y,
        integer,
        Y,
        vector,
        N,
        XX,
        lsu,
        mem
      ),
      VSE64_V -> List(Y, Y, N, XX, Y, integer, N, XX, Y, vector, lsu, mem),

      // Vector Integer
      VADD_VV -> List(
        Y,
        N,
        Y,
        vector,
        Y,
        vector,
        Y,
        vector,
        N,
        XX,
        vectorAlu,
        vec
      ),
      VADD_VI -> List(Y, N, Y, vector, Y, vector, N, XX, N, XX, vectorAlu, vec),
      VADD_VX -> List(
        Y,
        N,
        Y,
        vector,
        Y,
        vector,
        Y,
        integer,
        N,
        XX,
        vectorAlu,
        vec
      ),
      VFMV_F_S -> List(
        Y,
        N,
        Y,
        float,
        N,
        XX,
        Y,
        vector,
        N,
        XX,
        vectorMisc,
        vec
      ),
      VMV_X_S -> List(
        Y,
        N,
        Y,
        integer,
        N,
        XX,
        Y,
        vector,
        N,
        XX,
        vectorMisc,
        vec
      ),
      VFMV_S_F -> List(
        Y,
        N,
        Y,
        vector,
        Y,
        float,
        N,
        XX,
        N,
        XX,
        vectorMisc,
        vec
      ),
      VMV_S_X -> List(
        Y,
        N,
        Y,
        vector,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        vectorMisc,
        vec
      ),
      VMV_V_V -> List(
        Y,
        N,
        Y,
        vector,
        Y,
        vector,
        N,
        XX,
        N,
        XX,
        vectorMisc,
        vec
      ),
      VMV_V_I -> List(Y, N, Y, vector, N, XX, N, XX, N, XX, vectorMisc, vec),
      VMV_V_X -> List(
        Y,
        N,
        Y,
        vector,
        Y,
        integer,
        N,
        XX,
        N,
        XX,
        vectorMisc,
        vec
      ),
      VFMV_V_F -> List(Y, N, Y, vector, Y, float, N, XX, N, XX, vectorMisc, vec)
    )

  for (execUnitType <- ExecUnitType.all) {
    val tableFilter = table.filter(_._2(10).value == execUnitType.value)
    if (!tableFilter.isEmpty) {
      val rdAsRs3 = tableFilter.map(_._2(1).value).reduce(_ | _)
      val writeRd = tableFilter.map(_._2(2).value).reduce(_ | _)
      val rdType  =
        tableFilter.filter(_._2(3).mask != 0).map(_._2(3).value).distinct
      val readRs1 = tableFilter.map(_._2(4).value).reduce(_ | _)
      val rs1Type =
        tableFilter.filter(_._2(5).mask != 0).map(_._2(5).value).distinct
      val readRs2 = tableFilter.map(_._2(6).value).reduce(_ | _)
      val rs2Type =
        tableFilter.filter(_._2(7).mask != 0).map(_._2(7).value).distinct
      val readRs3 = tableFilter.map(_._2(8).value).reduce(_ | _)
      val rs3Type =
        tableFilter.filter(_._2(9).mask != 0).map(_._2(9).value).distinct

      def printType(n: Array[BigInt]) {
        if (n.contains(RegType.float.value)) {
          print("F")
        }
        if (n.contains(RegType.integer.value)) {
          print("I")
        }
        if (n.contains(RegType.vector.value)) {
          print("V")
        }
      }

      def printReg(
          name: String,
          read: BigInt,
          write: BigInt,
          types: Array[BigInt]
      ) {
        if (read == 0 && write == 0) {
          return
        }

        print(s" ${name}(")
        if (read != 0) {
          print("R")
        }
        if (write != 0) {
          print("W")
        }
        print(")[")
        printType(types)
        print("]")
      }

      print(s"Type ${execUnitType}:")
      printReg("rd", 0, writeRd, rdType)
      printReg("rs1", readRs1, 0, rs1Type)
      printReg("rs2", readRs2, 0, rs2Type)
      printReg("rs3", readRs3, 0, rs3Type)
      if (rdAsRs3 > 0) {
        print(" RdAsRs3")
      }
      println()
    }
  }

  def assign(inst: BitPat) = {
    val res   = Wire(new DecodeInfo)
    val entry =
      table.find(e => e._1.mask == inst.mask && e._1.value == inst.value).get
    for ((signal, value) <- res.signals.zip(entry._2)) {
      signal := value.value.U.asTypeOf(signal)
    }

    res
  }

  /** Illegal Instruction
    */
  def illegal = {
    val res = Wire(new DecodeInfo)
    for ((signal, value) <- res.signals.zip(default)) {
      signal := value.value.U.asTypeOf(signal)
    }

    res
  }

  def decode(inst: UInt) = {
    val res = Wire(new DecodeInfo)

    for ((signal, i) <- res.signals.zipWithIndex) {
      val truthTable =
        TruthTable(
          table.map({ case (k, v) => (k, v(i)) }),
          default = default(i)
        )

      //println(truthTable)
      signal := decoder.qmc(inst, truthTable).asTypeOf(signal)
    }

    res
  }
}

object Instructions {
  // RV32I Base Instruction Set
  val LUI   = BitPat("b?????????????????????????0110111")
  val AUIPC = BitPat("b?????????????????????????0010111")

  val JAL  = BitPat("b?????????????????????????1101111")
  val JALR = BitPat("b?????????????????000?????1100111")

  val BEQ  = BitPat("b?????????????????000?????1100011")
  val BNE  = BitPat("b?????????????????001?????1100011")
  val BLT  = BitPat("b?????????????????100?????1100011")
  val BGE  = BitPat("b?????????????????101?????1100011")
  val BLTU = BitPat("b?????????????????110?????1100011")
  val BGEU = BitPat("b?????????????????111?????1100011")

  val LB  = BitPat("b?????????????????000?????0000011")
  val LH  = BitPat("b?????????????????001?????0000011")
  val LW  = BitPat("b?????????????????010?????0000011")
  val LBU = BitPat("b?????????????????100?????0000011")
  val LHU = BitPat("b?????????????????101?????0000011")

  val SB = BitPat("b?????????????????000?????0100011")
  val SH = BitPat("b?????????????????001?????0100011")
  val SW = BitPat("b?????????????????010?????0100011")

  val ADDI  = BitPat("b?????????????????000?????0010011")
  val SLTI  = BitPat("b?????????????????010?????0010011")
  val SLTIU = BitPat("b?????????????????011?????0010011")
  val XORI  = BitPat("b?????????????????100?????0010011")
  val ORI   = BitPat("b?????????????????110?????0010011")
  val ANDI  = BitPat("b?????????????????111?????0010011")

  // one more shamt bit in rv64
  val SLLI = BitPat("b000000???????????001?????0010011")
  val SRLI = BitPat("b000000???????????101?????0010011")
  val SRAI = BitPat("b010000???????????101?????0010011")

  val ADD  = BitPat("b0000000??????????000?????0110011")
  val SUB  = BitPat("b0100000??????????000?????0110011")
  val SLL  = BitPat("b0000000??????????001?????0110011")
  val SLT  = BitPat("b0000000??????????010?????0110011")
  val SLTU = BitPat("b0000000??????????011?????0110011")
  val XOR  = BitPat("b0000000??????????100?????0110011")
  val SRL  = BitPat("b0000000??????????101?????0110011")
  val SRA  = BitPat("b0100000??????????101?????0110011")
  val OR   = BitPat("b0000000??????????110?????0110011")
  val AND  = BitPat("b0000000??????????111?????0110011")

  val FENCE   = BitPat("b0000????????00000000000000001111")
  val FENCE_I = BitPat("b00000000000000000001000000001111")

  val ECALL  = BitPat("b00000000000000000000000001110011")
  val EBREAK = BitPat("b00000000000100000000000001110011")

  val CSRRW  = BitPat("b?????????????????001?????1110011")
  val CSRRS  = BitPat("b?????????????????010?????1110011")
  val CSRRC  = BitPat("b?????????????????011?????1110011")
  val CSRRWI = BitPat("b?????????????????101?????1110011")
  val CSRRSI = BitPat("b?????????????????110?????1110011")
  val CSRRCI = BitPat("b?????????????????111?????1110011")

  // RV64I Base Instruction Set (in addition to RV32I)
  val LWU = BitPat("b?????????????????110?????0000011")
  val LD  = BitPat("b?????????????????011?????0000011")
  val SD  = BitPat("b?????????????????011?????0100011")

  val ADDIW = BitPat("b?????????????????000?????0011011")
  val SLLIW = BitPat("b0000000??????????001?????0011011")
  val SRLIW = BitPat("b0000000??????????101?????0011011")
  val SRAIW = BitPat("b0100000??????????101?????0011011")

  val ADDW = BitPat("b0000000??????????000?????0111011")
  val SUBW = BitPat("b0100000??????????000?????0111011")
  val SLLW = BitPat("b0000000??????????001?????0111011")
  val SRLW = BitPat("b0000000??????????101?????0111011")
  val SRAW = BitPat("b0100000??????????101?????0111011")

  // RV32M Standard Extension
  val MUL    = BitPat("b0000001??????????000?????0110011")
  val MULH   = BitPat("b0000001??????????001?????0110011")
  val MULHSU = BitPat("b0000001??????????010?????0110011")
  val MULHU  = BitPat("b0000001??????????011?????0110011")
  val DIV    = BitPat("b0000001??????????100?????0110011")
  val DIVU   = BitPat("b0000001??????????101?????0110011")
  val REM    = BitPat("b0000001??????????110?????0110011")
  val REMU   = BitPat("b0000001??????????111?????0110011")

  // RV64M Standard Extension (in addition to RV32M)
  val MULW  = BitPat("b0000001??????????000?????0111011")
  val DIVW  = BitPat("b0000001??????????100?????0111011")
  val DIVUW = BitPat("b0000001??????????101?????0111011")
  val REMW  = BitPat("b0000001??????????110?????0111011")
  val REMUW = BitPat("b0000001??????????111?????0111011")

  // RV32A Standard Extension
  val LR_W      = BitPat("b00010??00000?????010?????0101111")
  val SC_W      = BitPat("b00011????????????010?????0101111")
  val AMOSWAP_W = BitPat("b00001????????????010?????0101111")
  val AMOADD_W  = BitPat("b00000????????????010?????0101111")
  val AMOXOR_W  = BitPat("b00100????????????010?????0101111")
  val AMOAND_W  = BitPat("b01100????????????010?????0101111")
  val AMOOR_W   = BitPat("b01000????????????010?????0101111")
  val AMOMIN_W  = BitPat("b10000????????????010?????0101111")
  val AMOMAX_W  = BitPat("b10100????????????010?????0101111")
  val AMOMINU_W = BitPat("b11000????????????010?????0101111")
  val AMOMAXU_W = BitPat("b11100????????????010?????0101111")

  // RV64A Standard Extension (in addition to RV32A)
  val LR_D      = BitPat("b00010??00000?????011?????0101111")
  val SC_D      = BitPat("b00011????????????011?????0101111")
  val AMOSWAP_D = BitPat("b00001????????????011?????0101111")
  val AMOADD_D  = BitPat("b00000????????????011?????0101111")
  val AMOXOR_D  = BitPat("b00100????????????011?????0101111")
  val AMOAND_D  = BitPat("b01100????????????011?????0101111")
  val AMOOR_D   = BitPat("b01000????????????011?????0101111")
  val AMOMIN_D  = BitPat("b10000????????????011?????0101111")
  val AMOMAX_D  = BitPat("b10100????????????011?????0101111")
  val AMOMINU_D = BitPat("b11000????????????011?????0101111")
  val AMOMAXU_D = BitPat("b11100????????????011?????0101111")

  // RV32F Standard Extension
  val FLW = BitPat("b?????????????????010?????0000111")
  val FSW = BitPat("b?????????????????010?????0100111")

  val FMADD_S  = BitPat("b?????00??????????????????1000011")
  val FMSUB_S  = BitPat("b?????00??????????????????1000111")
  val FNMSUB_S = BitPat("b?????00??????????????????1001011")
  val FNMADD_S = BitPat("b?????00??????????????????1001111")

  val FADD_S    = BitPat("b0000000??????????????????1010011")
  val FSUB_S    = BitPat("b0000100??????????????????1010011")
  val FMUL_S    = BitPat("b0001000??????????????????1010011")
  val FDIV_S    = BitPat("b0001100??????????????????1010011")
  val FSQRT_S   = BitPat("b010110000000?????????????1010011")
  val FSGNJ_S   = BitPat("b0010000??????????000?????1010011")
  val FSGNJN_S  = BitPat("b0010000??????????001?????1010011")
  val FSGNJX_S  = BitPat("b0010000??????????010?????1010011")
  val FMIN_S    = BitPat("b0010100??????????000?????1010011")
  val FMAX_S    = BitPat("b0010100??????????001?????1010011")
  val FCVT_W_S  = BitPat("b110000000000?????????????1010011")
  val FCVT_WU_S = BitPat("b110000000001?????????????1010011")
  val FMV_X_W   = BitPat("b111000000000?????000?????1010011")
  val FEQ_S     = BitPat("b1010000??????????010?????1010011")
  val FLT_S     = BitPat("b1010000??????????001?????1010011")
  val FLE_S     = BitPat("b1010000??????????000?????1010011")
  val FCLASS_S  = BitPat("b111000000000?????001?????1010011")
  val FCVT_S_W  = BitPat("b110100000000?????????????1010011")
  val FCVT_S_WU = BitPat("b110100000001?????????????1010011")
  val FMV_W_X   = BitPat("b111100000000?????000?????1010011")

  // RV64F Standard Extension (in addition to RV32F)
  val FCVT_L_S  = BitPat("b110000000010?????????????1010011")
  val FCVT_LU_S = BitPat("b110000000011?????????????1010011")
  val FCVT_S_L  = BitPat("b110100000010?????????????1010011")
  val FCVT_S_LU = BitPat("b110100000011?????????????1010011")

  // RV32D Standard Extension
  val FLD = BitPat("b?????????????????011?????0000111")
  val FSD = BitPat("b?????????????????011?????0100111")

  val FMADD_D  = BitPat("b?????01??????????????????1000011")
  val FMSUB_D  = BitPat("b?????01??????????????????1000111")
  val FNMSUB_D = BitPat("b?????01??????????????????1001011")
  val FNMADD_D = BitPat("b?????01??????????????????1001111")

  val FADD_D    = BitPat("b0000001??????????????????1010011")
  val FSUB_D    = BitPat("b0000101??????????????????1010011")
  val FMUL_D    = BitPat("b0001001??????????????????1010011")
  val FDIV_D    = BitPat("b0001101??????????????????1010011")
  val FSQRT_D   = BitPat("b010110100000?????????????1010011")
  val FSGNJ_D   = BitPat("b0010001??????????000?????1010011")
  val FSGNJN_D  = BitPat("b0010001??????????001?????1010011")
  val FSGNJX_D  = BitPat("b0010001??????????010?????1010011")
  val FMIN_D    = BitPat("b0010101??????????000?????1010011")
  val FMAX_D    = BitPat("b0010101??????????001?????1010011")
  val FCVT_S_D  = BitPat("b010000000001?????????????1010011")
  val FCVT_D_S  = BitPat("b010000100000?????????????1010011")
  val FEQ_D     = BitPat("b1010001??????????010?????1010011")
  val FLT_D     = BitPat("b1010001??????????001?????1010011")
  val FLE_D     = BitPat("b1010001??????????000?????1010011")
  val FCLASS_D  = BitPat("b111000100000?????001?????1010011")
  val FCVT_W_D  = BitPat("b110000100000?????????????1010011")
  val FCVT_WU_D = BitPat("b110000100001?????????????1010011")
  val FCVT_D_W  = BitPat("b110100100000?????????????1010011")
  val FCVT_D_WU = BitPat("b110100100001?????????????1010011")

  // RV64D Standard Extension (in addition to RV32D)
  val FCVT_L_D  = BitPat("b110000100010?????????????1010011")
  val FCVT_LU_D = BitPat("b110000100011?????????????1010011")
  val FMV_X_D   = BitPat("b111000100000?????000?????1010011")
  val FCVT_D_L  = BitPat("b110100100010?????????????1010011")
  val FCVT_D_LU = BitPat("b110100100011?????????????1010011")
  val FMV_D_X   = BitPat("b111100100000?????000?????1010011")

  // Privileged Spec
  // Trap-Return Instructions
  val URET = BitPat("b00000000001000000000000001110011")
  val SRET = BitPat("b00010000001000000000000001110011")
  val MRET = BitPat("b00110000001000000000000001110011")
  val DRET = BitPat("b01111011001000000000000001110011")

  // Interrupt-Management Instructions
  val WFI = BitPat("b00010000010100000000000001110011")

  // Memory-Management Instructions
  val SFENCE_VMA = BitPat("b0001001??????????000000001110011")

  // CMO(Cache Memory Operation) extension
  val CBO_CLEAN = BitPat("b000000000001?????010000000001111")
  val CBO_FLUSH = BitPat("b000000000010?????010000000001111")
  val CBO_INVAL = BitPat("b000000000000?????010000000001111")
  val CBO_ZERO  = BitPat("b000000000100?????010000000001111")

  // Vector extension
  val VSETVLI  = BitPat("b0????????????????111?????1010111")
  val VSETIVLI = BitPat("b11???????????????111?????1010111")
  val VSETVL   = BitPat("b1000000??????????111?????1010111")

  // Vector Load
  val VLE16_V    = BitPat("b000000?00000?????101?????0000111")
  val VLE32_V    = BitPat("b000000?00000?????110?????0000111")
  val VLE64_V    = BitPat("b000000?00000?????111?????0000111")
  val VLUXEI64_V = BitPat("b000001???????????111?????0000111")

  // Vector Store
  val VSE16_V = BitPat("b000000?00000?????101?????0100111")
  val VSE32_V = BitPat("b000000?00000?????110?????0100111")
  val VSE64_V = BitPat("b000000?00000?????111?????0100111")

  // Vector Integer
  val VADD_VV  = BitPat("b000000???????????000?????1010111")
  val VADD_VI  = BitPat("b000000???????????011?????1010111")
  val VADD_VX  = BitPat("b000000???????????100?????1010111")
  val VFMV_F_S = BitPat("b0100001?????00000001?????1010111")
  val VMV_X_S  = BitPat("b0100001?????00000010?????1010111")
  val VFMV_S_F = BitPat("b010000100000?????101?????1010111")
  val VMV_S_X  = BitPat("b010000100000?????110?????1010111")
  val VMV_V_V  = BitPat("b010111100000?????000?????1010111")
  val VMV_V_I  = BitPat("b010111100000?????011?????1010111")
  val VMV_V_X  = BitPat("b010111100000?????100?????1010111")
  val VFMV_V_F = BitPat("b010111100000?????101?????1010111")
}
