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
    execUnit
  )
}

object DecodeInfo {
  val X = BitPat("b?")
  val Y = BitPat("b1")
  val N = BitPat("b0")

  import Instructions._
  import RegType._
  import ExecUnitType._
  val default: List[BitPat] =
    List(N, N, X, N, X, N, X, bypass)
  val table: Array[(BitPat, List[BitPat])] =
    Array(
      // RV32I Base Instruction Set
      LUI     -> List(Y, Y, integer, N, X, N, X, bypass),
      AUIPC   -> List(Y, Y, integer, N, X, N, X, bypass),
      JAL     -> List(Y, Y, integer, N, X, N, X, bypass),
      JALR    -> List(Y, Y, integer, Y, integer, N, X, branch),
      BEQ     -> List(Y, N, X, Y, integer, Y, integer, branch),
      BNE     -> List(Y, N, X, Y, integer, Y, integer, branch),
      BLT     -> List(Y, N, X, Y, integer, Y, integer, branch),
      BGE     -> List(Y, N, X, Y, integer, Y, integer, branch),
      BLTU    -> List(Y, N, X, Y, integer, Y, integer, branch),
      BGEU    -> List(Y, N, X, Y, integer, Y, integer, branch),
      LB      -> List(Y, Y, integer, Y, integer, N, X, lsu),
      LH      -> List(Y, Y, integer, Y, integer, N, X, lsu),
      LW      -> List(Y, Y, integer, Y, integer, N, X, lsu),
      LBU     -> List(Y, Y, integer, Y, integer, N, X, lsu),
      LHU     -> List(Y, Y, integer, Y, integer, N, X, lsu),
      SB      -> List(Y, N, X, Y, integer, Y, integer, lsu),
      SH      -> List(Y, N, X, Y, integer, Y, integer, lsu),
      SW      -> List(Y, N, X, Y, integer, Y, integer, lsu),
      ADDI    -> List(Y, Y, integer, Y, integer, N, X, alu),
      SLTI    -> List(Y, Y, integer, Y, integer, N, X, alu),
      SLTIU   -> List(Y, Y, integer, Y, integer, N, X, alu),
      XORI    -> List(Y, Y, integer, Y, integer, N, X, alu),
      ORI     -> List(Y, Y, integer, Y, integer, N, X, alu),
      ANDI    -> List(Y, Y, integer, Y, integer, N, X, alu),
      SLLI    -> List(Y, Y, integer, Y, integer, N, X, alu),
      SRLI    -> List(Y, Y, integer, Y, integer, N, X, alu),
      SRAI    -> List(Y, Y, integer, Y, integer, N, X, alu),
      ADD     -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SUB     -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SLL     -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SLT     -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SLTU    -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      XOR     -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SRL     -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SRA     -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      OR      -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      AND     -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      FENCE   -> List(Y, N, X, N, X, N, X, lsu),
      FENCE_I -> List(Y, N, X, N, X, N, X, lsu),
      ECALL   -> List(Y, N, X, N, X, N, X, branch),
      EBREAK  -> List(Y, N, X, N, X, N, X, branch),
      CSRRW   -> List(Y, Y, integer, Y, integer, N, X, csr),
      CSRRS   -> List(Y, Y, integer, Y, integer, N, X, csr),
      CSRRC   -> List(Y, Y, integer, Y, integer, N, X, csr),
      CSRRWI  -> List(Y, Y, integer, N, X, N, X, csr),
      CSRRSI  -> List(Y, Y, integer, N, X, N, X, csr),
      CSRRCI  -> List(Y, Y, integer, N, X, N, X, csr),

      // RV64I Base Instruction Set (in addition to RV32I)
      LWU   -> List(Y, Y, integer, Y, integer, N, X, lsu),
      LD    -> List(Y, Y, integer, Y, integer, N, X, lsu),
      SD    -> List(Y, N, X, Y, integer, Y, integer, lsu),
      ADDIW -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SLLIW -> List(Y, Y, integer, Y, integer, N, X, alu),
      SRLIW -> List(Y, Y, integer, Y, integer, N, X, alu),
      SRAIW -> List(Y, Y, integer, Y, integer, N, X, alu),
      ADDW  -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SUBW  -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SLLW  -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SRLW  -> List(Y, Y, integer, Y, integer, Y, integer, alu),
      SRAW  -> List(Y, Y, integer, Y, integer, Y, integer, alu),

      // RV32M Standard Extension
      MUL    -> List(Y, Y, integer, Y, integer, Y, integer, mul),
      MULH   -> List(Y, Y, integer, Y, integer, Y, integer, mul),
      MULHSU -> List(Y, Y, integer, Y, integer, Y, integer, mul),
      MULHU  -> List(Y, Y, integer, Y, integer, Y, integer, mul),
      DIV    -> List(Y, Y, integer, Y, integer, Y, integer, div),
      DIVU   -> List(Y, Y, integer, Y, integer, Y, integer, div),
      REM    -> List(Y, Y, integer, Y, integer, Y, integer, div),
      REMU   -> List(Y, Y, integer, Y, integer, Y, integer, div),

      // RV64M Standard Extension (in addition to RV32M)
      MULW  -> List(Y, Y, integer, Y, integer, Y, integer, mul),
      DIVW  -> List(Y, Y, integer, Y, integer, Y, integer, div),
      DIVUW -> List(Y, Y, integer, Y, integer, Y, integer, div),
      REMW  -> List(Y, Y, integer, Y, integer, Y, integer, div),
      REMUW -> List(Y, Y, integer, Y, integer, Y, integer, div),

      // RV32A Standard Extension
      LR_W      -> List(Y, Y, integer, Y, integer, N, X, lsu),
      SC_W      -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOSWAP_W -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOADD_W  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOXOR_W  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOAND_W  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOOR_W   -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOMIN_W  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOMAX_W  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOMINU_W -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOMAXU_W -> List(Y, Y, integer, Y, integer, Y, integer, lsu),

      // RV64A Standard Extension (in addition to RV32A)
      LR_D      -> List(Y, Y, integer, Y, integer, N, X, lsu),
      SC_D      -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOSWAP_D -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOADD_D  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOXOR_D  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOAND_D  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOOR_D   -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOMIN_D  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOMAX_D  -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOMINU_D -> List(Y, Y, integer, Y, integer, Y, integer, lsu),
      AMOMAXU_D -> List(Y, Y, integer, Y, integer, Y, integer, lsu),

      // RV32F Standard Extension
      FLW      -> List(Y, Y, float, Y, integer, N, X, lsu),
      FSW      -> List(Y, N, X, Y, integer, Y, float, lsu),
      FADD_S   -> List(Y, Y, float, Y, float, Y, float, fma),
      FSUB_S   -> List(Y, Y, float, Y, float, Y, float, fma),
      FMUL_S   -> List(Y, Y, float, Y, float, Y, float, fma),
      FSGNJ_S  -> List(Y, Y, float, Y, float, Y, float, floatMisc),
      FSGNJN_S -> List(Y, Y, float, Y, float, Y, float, floatMisc),
      FSGNJX_S -> List(Y, Y, float, Y, float, Y, float, floatMisc),
      FEQ_S    -> List(Y, Y, integer, Y, float, Y, float, floatMisc),
      FMV_X_W  -> List(Y, Y, integer, Y, float, N, X, floatMisc),
      FMV_W_X  -> List(Y, Y, float, Y, integer, N, X, floatMisc),

      // RV32D Standard Extension
      FLD       -> List(Y, Y, float, Y, integer, N, X, lsu),
      FSD       -> List(Y, N, X, Y, integer, Y, float, lsu),
      FADD_D    -> List(Y, Y, float, Y, float, Y, float, fma),
      FSUB_D    -> List(Y, Y, float, Y, float, Y, float, fma),
      FMUL_D    -> List(Y, Y, float, Y, float, Y, float, fma),
      FDIV_D    -> List(Y, Y, float, Y, float, Y, float, fDivSqrt),
      FSQRT_D   -> List(Y, Y, float, Y, float, N, X, fDivSqrt),
      FSGNJ_D   -> List(Y, Y, float, Y, float, Y, float, floatMisc),
      FSGNJN_D  -> List(Y, Y, float, Y, float, Y, float, floatMisc),
      FSGNJX_D  -> List(Y, Y, float, Y, float, Y, float, floatMisc),
      FMIN_D    -> List(Y, Y, float, Y, float, Y, float, floatMisc),
      FMAX_D    -> List(Y, Y, float, Y, float, Y, float, floatMisc),
      FCVT_S_D  -> List(Y, Y, float, Y, float, N, X, floatMisc),
      FCVT_D_S  -> List(Y, Y, float, Y, float, N, X, floatMisc),
      FEQ_D     -> List(Y, Y, integer, Y, float, Y, float, floatMisc),
      FLT_D     -> List(Y, Y, integer, Y, float, Y, float, floatMisc),
      FLE_D     -> List(Y, Y, integer, Y, float, Y, float, floatMisc),
      FCLASS_D  -> List(Y, Y, integer, Y, float, Y, float, floatMisc),
      FCVT_W_D  -> List(Y, Y, integer, Y, float, N, X, floatMisc),
      FCVT_WU_D -> List(Y, Y, integer, Y, float, N, X, floatMisc),
      FCVT_D_W  -> List(Y, Y, float, Y, integer, N, X, floatMisc),
      FCVT_D_WU -> List(Y, Y, float, Y, integer, N, X, floatMisc),

      // RV64D Standard Extension (in addition to RV32D)
      FCVT_L_D  -> List(Y, Y, integer, Y, float, N, X, floatMisc),
      FCVT_LU_D -> List(Y, Y, integer, Y, float, N, X, floatMisc),
      FMV_X_D   -> List(Y, Y, integer, Y, float, N, X, floatMisc),
      FCVT_D_L  -> List(Y, Y, float, Y, integer, N, X, floatMisc),
      FCVT_D_LU -> List(Y, Y, float, Y, integer, N, X, floatMisc),
      FMV_D_X   -> List(Y, Y, float, Y, integer, N, X, floatMisc),

      // Trap-Return Instructions
      URET -> List(Y, N, X, N, X, N, X, branch),
      SRET -> List(Y, N, X, N, X, N, X, branch),
      MRET -> List(Y, N, X, N, X, N, X, branch),

      // Interrupt-Management Instructions
      WFI -> List(Y, N, X, N, X, N, X, branch),

      // Memory-Management Instructions
      SFENCE_VMA -> List(Y, N, X, Y, integer, Y, integer, branch)
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

  def invalid = {
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
  val FMV_X_W   = BitPat("b111000000000?????????????1010011")
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
}
