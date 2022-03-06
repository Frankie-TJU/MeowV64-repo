package meowv64.instr

import chisel3._
import chisel3.experimental._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import chisel3.util.experimental.decode.decoder
import meowv64.reg.RegType

// scalafmt: { align.preset = most }
object ExecUnitType extends ChiselEnum {
  val alu, branch, bypass, csr = Value
  val div, mul                 = Value
  val fma, floatMisc, fDivSqrt = Value
  val vAlu                     = Value
  val lsu                      = Value

  implicit def bitpat(op: ExecUnitType.Type): BitPat =
    BitPat(op.litValue.U(getWidth.W))
}

class DecodeInfo extends Bundle {
  // legal instruction
  val legal = Bool()

  // register related
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

  // execution unit related
  val execUnit = ExecUnitType()

  def signals = Seq(
    legal,
    writeRd,
    rdType,
    readRs1,
    rs1Type,
    readRs2,
    rs2Type,
    readRs3,
    rs3Type,
    execUnit
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
  val default: List[BitPat] =
    List(N, N, XX, N, XX, N, XX, N, XX, bypass)
  val table: Array[(BitPat, List[BitPat])] =
    Array(
      // RV32I Base Instruction Set
      LUI     -> List(Y, Y, integer, N, XX, N, XX, N, XX, bypass),
      AUIPC   -> List(Y, Y, integer, N, XX, N, XX, N, XX, bypass),
      JAL     -> List(Y, Y, integer, N, XX, N, XX, N, XX, bypass),
      JALR    -> List(Y, Y, integer, Y, integer, N, XX, N, XX, branch),
      BEQ     -> List(Y, N, XX, Y, integer, Y, integer, N, XX, branch),
      BNE     -> List(Y, N, XX, Y, integer, Y, integer, N, XX, branch),
      BLT     -> List(Y, N, XX, Y, integer, Y, integer, N, XX, branch),
      BGE     -> List(Y, N, XX, Y, integer, Y, integer, N, XX, branch),
      BLTU    -> List(Y, N, XX, Y, integer, Y, integer, N, XX, branch),
      BGEU    -> List(Y, N, XX, Y, integer, Y, integer, N, XX, branch),
      LB      -> List(Y, Y, integer, Y, integer, N, XX, N, XX, lsu),
      LH      -> List(Y, Y, integer, Y, integer, N, XX, N, XX, lsu),
      LW      -> List(Y, Y, integer, Y, integer, N, XX, N, XX, lsu),
      LBU     -> List(Y, Y, integer, Y, integer, N, XX, N, XX, lsu),
      LHU     -> List(Y, Y, integer, Y, integer, N, XX, N, XX, lsu),
      SB      -> List(Y, N, XX, Y, integer, Y, integer, N, XX, lsu),
      SH      -> List(Y, N, XX, Y, integer, Y, integer, N, XX, lsu),
      SW      -> List(Y, N, XX, Y, integer, Y, integer, N, XX, lsu),
      ADDI    -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      SLTI    -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      SLTIU   -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      XORI    -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      ORI     -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      ANDI    -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      SLLI    -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      SRLI    -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      SRAI    -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      ADD     -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SUB     -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SLL     -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SLT     -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SLTU    -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      XOR     -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SRL     -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SRA     -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      OR      -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      AND     -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      FENCE   -> List(Y, N, XX, N, XX, N, XX, N, XX, lsu),
      FENCE_I -> List(Y, N, XX, N, XX, N, XX, N, XX, lsu),
      ECALL   -> List(Y, N, XX, N, XX, N, XX, N, XX, branch),
      EBREAK  -> List(Y, N, XX, N, XX, N, XX, N, XX, branch),
      CSRRW   -> List(Y, Y, integer, Y, integer, N, XX, N, XX, csr),
      CSRRS   -> List(Y, Y, integer, Y, integer, N, XX, N, XX, csr),
      CSRRC   -> List(Y, Y, integer, Y, integer, N, XX, N, XX, csr),
      CSRRWI  -> List(Y, Y, integer, N, XX, N, XX, N, XX, csr),
      CSRRSI  -> List(Y, Y, integer, N, XX, N, XX, N, XX, csr),
      CSRRCI  -> List(Y, Y, integer, N, XX, N, XX, N, XX, csr),

      // RV64I Base Instruction Set (in addition to RV32I)
      LWU   -> List(Y, Y, integer, Y, integer, N, XX, N, XX, lsu),
      LD    -> List(Y, Y, integer, Y, integer, N, XX, N, XX, lsu),
      SD    -> List(Y, N, XX, Y, integer, Y, integer, N, XX, lsu),
      ADDIW -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SLLIW -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      SRLIW -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      SRAIW -> List(Y, Y, integer, Y, integer, N, XX, N, XX, alu),
      ADDW  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SUBW  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SLLW  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SRLW  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),
      SRAW  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, alu),

      // RV32M Standard Extension
      MUL    -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, mul),
      MULH   -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, mul),
      MULHSU -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, mul),
      MULHU  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, mul),
      DIV    -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, div),
      DIVU   -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, div),
      REM    -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, div),
      REMU   -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, div),

      // RV64M Standard Extension (in addition to RV32M)
      MULW  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, mul),
      DIVW  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, div),
      DIVUW -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, div),
      REMW  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, div),
      REMUW -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, div),

      // RV32A Standard Extension
      LR_W      -> List(Y, Y, integer, Y, integer, N, XX, N, XX, lsu),
      SC_W      -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOSWAP_W -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOADD_W  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOXOR_W  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOAND_W  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOOR_W   -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOMIN_W  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOMAX_W  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOMINU_W -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOMAXU_W -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),

      // RV64A Standard Extension (in addition to RV32A)
      LR_D      -> List(Y, Y, integer, Y, integer, N, XX, N, XX, lsu),
      SC_D      -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOSWAP_D -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOADD_D  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOXOR_D  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOAND_D  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOOR_D   -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOMIN_D  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOMAX_D  -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOMINU_D -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),
      AMOMAXU_D -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, lsu),

      // RV32F Standard Extension
      FLW       -> List(Y, Y, float, Y, integer, N, XX, N, XX, lsu),
      FSW       -> List(Y, N, XX, Y, integer, Y, float, N, XX, lsu),
      FMADD_S   -> List(Y, Y, float, Y, float, Y, float, Y, float, fma),
      FMSUB_S   -> List(Y, Y, float, Y, float, Y, float, Y, float, fma),
      FNMADD_S  -> List(Y, Y, float, Y, float, Y, float, Y, float, fma),
      FNMSUB_S  -> List(Y, Y, float, Y, float, Y, float, Y, float, fma),
      FADD_S    -> List(Y, Y, float, Y, float, Y, float, N, XX, fma),
      FSUB_S    -> List(Y, Y, float, Y, float, Y, float, N, XX, fma),
      FMUL_S    -> List(Y, Y, float, Y, float, Y, float, N, XX, fma),
      FDIV_S    -> List(Y, Y, float, Y, float, Y, float, N, XX, fDivSqrt),
      FSQRT_S   -> List(Y, Y, float, Y, float, N, XX, N, XX, fDivSqrt),
      FSGNJ_S   -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FSGNJN_S  -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FSGNJX_S  -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FMIN_S    -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FMAX_S    -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FCVT_W_S  -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FCVT_WU_S -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FMV_X_W   -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FEQ_S     -> List(Y, Y, integer, Y, float, Y, float, N, XX, floatMisc),
      FLT_S     -> List(Y, Y, integer, Y, float, Y, float, N, XX, floatMisc),
      FLE_S     -> List(Y, Y, integer, Y, float, Y, float, N, XX, floatMisc),
      FCLASS_S  -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FCVT_S_W  -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),
      FCVT_S_WU -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),
      FMV_W_X   -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),

      // RV64F Standard Extension (in addition to RV32F)
      FCVT_L_S  -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FCVT_LU_S -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FCVT_S_L  -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),
      FCVT_S_LU -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),

      // RV32D Standard Extension
      FLD       -> List(Y, Y, float, Y, integer, N, XX, N, XX, lsu),
      FSD       -> List(Y, N, XX, Y, integer, Y, float, N, XX, lsu),
      FMADD_D   -> List(Y, Y, float, Y, float, Y, float, Y, float, fma),
      FMSUB_D   -> List(Y, Y, float, Y, float, Y, float, Y, float, fma),
      FNMADD_D  -> List(Y, Y, float, Y, float, Y, float, Y, float, fma),
      FNMSUB_D  -> List(Y, Y, float, Y, float, Y, float, Y, float, fma),
      FADD_D    -> List(Y, Y, float, Y, float, Y, float, N, XX, fma),
      FSUB_D    -> List(Y, Y, float, Y, float, Y, float, N, XX, fma),
      FMUL_D    -> List(Y, Y, float, Y, float, Y, float, N, XX, fma),
      FDIV_D    -> List(Y, Y, float, Y, float, Y, float, N, XX, fDivSqrt),
      FSQRT_D   -> List(Y, Y, float, Y, float, N, XX, N, XX, fDivSqrt),
      FSGNJ_D   -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FSGNJN_D  -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FSGNJX_D  -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FMIN_D    -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FMAX_D    -> List(Y, Y, float, Y, float, Y, float, N, XX, floatMisc),
      FCVT_S_D  -> List(Y, Y, float, Y, float, N, XX, N, XX, floatMisc),
      FCVT_D_S  -> List(Y, Y, float, Y, float, N, XX, N, XX, floatMisc),
      FEQ_D     -> List(Y, Y, integer, Y, float, Y, float, N, XX, floatMisc),
      FLT_D     -> List(Y, Y, integer, Y, float, Y, float, N, XX, floatMisc),
      FLE_D     -> List(Y, Y, integer, Y, float, Y, float, N, XX, floatMisc),
      FCLASS_D  -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FCVT_W_D  -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FCVT_WU_D -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FCVT_D_W  -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),
      FCVT_D_WU -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),

      // RV64D Standard Extension (in addition to RV32D)
      FCVT_L_D  -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FCVT_LU_D -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FMV_X_D   -> List(Y, Y, integer, Y, float, N, XX, N, XX, floatMisc),
      FCVT_D_L  -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),
      FCVT_D_LU -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),
      FMV_D_X   -> List(Y, Y, float, Y, integer, N, XX, N, XX, floatMisc),

      // Trap-Return Instructions
      URET -> List(Y, N, XX, N, XX, N, XX, N, XX, branch),
      SRET -> List(Y, N, XX, N, XX, N, XX, N, XX, branch),
      MRET -> List(Y, N, XX, N, XX, N, XX, N, XX, branch),

      // Interrupt-Management Instructions
      WFI -> List(Y, N, XX, N, XX, N, XX, N, XX, branch),

      // Memory-Management Instructions
      SFENCE_VMA -> List(Y, N, XX, Y, integer, Y, integer, N, XX, branch),

      // Vector
      VSETVLI  -> List(Y, Y, integer, Y, integer, N, XX, N, XX, csr),
      VSETIVLI -> List(Y, Y, integer, N, XX, N, XX, N, XX, csr),
      VSETVL   -> List(Y, Y, integer, Y, integer, Y, integer, N, XX, csr),

      // Vector Load/Store
      VLE64_V  -> List(Y, Y, vector, Y, integer, N, XX, N, XX, lsu),

      // Vector Integer
      VADD_VV  -> List(Y, Y, vector, Y, vector, N, XX, N, XX, vAlu),
      VADD_VI  -> List(Y, Y, vector, N, XX, N, XX, N, XX, vAlu),
      VADD_VX  -> List(Y, Y, vector, Y, integer, N, XX, N, XX, vAlu),
      VFMV_F_S -> List(Y, Y, float, N, XX, Y, vector, N, XX, vAlu),
      VMV_X_S  -> List(Y, Y, integer, N, XX, Y, vector, N, XX, vAlu),
      VFMV_S_F -> List(Y, Y, vector, Y, float, N, XX, N, XX, vAlu),
      VMV_S_X  -> List(Y, Y, vector, Y, integer, N, XX, N, XX, vAlu),
      VMV_V_V  -> List(Y, Y, vector, Y, vector, N, XX, N, XX, vAlu),
      VMV_V_I  -> List(Y, Y, vector, N, XX, N, XX, N, XX, vAlu),
      VMV_V_X  -> List(Y, Y, vector, Y, integer, N, XX, N, XX, vAlu),
      VFMV_V_F -> List(Y, Y, vector, Y, float, N, XX, N, XX, vAlu)
    )

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
  val VLE16_V = BitPat("b000000?00000?????101?????0000111")
  val VLE32_V = BitPat("b000000?00000?????110?????0000111")
  val VLE64_V = BitPat("b000000?00000?????111?????0000111")

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
