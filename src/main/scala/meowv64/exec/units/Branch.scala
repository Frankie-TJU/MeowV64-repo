package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.core.ExReq
import meowv64.core.ExType
import meowv64.core.PrivLevel
import meowv64.core.Status
import meowv64.exec._
import meowv64.instr.Decoder

class BranchExt(implicit val coredef: CoreDef) extends Bundle {

  /** whether the branch is taken */
  val branchTaken = Bool()

  val ex = ExReq()
  val exType = ExType()
  val tval = UInt(coredef.XLEN.W)
}

class Branch(override implicit val coredef: CoreDef)
    extends ExecUnit(0, new BranchExt, coredef.REG_INT)
    with WithPrivPort
    with WithStatus {
  val priv = IO(Input(PrivLevel()))
  val status = IO(Input(new Status))

  def map(
      stage: Int,
      pipe: PipeInstr,
      ext: Option[BranchExt]
  ): (BranchExt, Bool) = {
    val ext = Wire(new BranchExt)
    ext.branchTaken := false.B
    ext.ex := ExReq.none
    ext.exType := DontCare
    ext.tval := 0.U

    val op1 = pipe.rs1val
    val op2 = pipe.rs2val

    when(pipe.instr.instr.op === Decoder.Op("BRANCH").ident) {
      switch(pipe.instr.instr.funct3) {
        is(Decoder.BRANCH_FUNC("BEQ")) {
          ext.branchTaken := op1 === op2
        }

        is(Decoder.BRANCH_FUNC("BNE")) {
          ext.branchTaken := op1 =/= op2
        }

        is(Decoder.BRANCH_FUNC("BLT")) {
          ext.branchTaken := op1.asSInt < op2.asSInt
        }

        is(Decoder.BRANCH_FUNC("BGE")) {
          ext.branchTaken := op1.asSInt >= op2.asSInt
        }

        is(Decoder.BRANCH_FUNC("BLTU")) {
          ext.branchTaken := op1 < op2
        }

        is(Decoder.BRANCH_FUNC("BGEU")) {
          ext.branchTaken := op1 >= op2
        }
      }
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("SYSTEM").ident) { // ECALL...
      when(pipe.instr.instr.funct7 === Decoder.PRIV_FUNCT7("SFENCE.VMA")) {
        when(priv =/= PrivLevel.M && status.tvm) {
          ext.ex := ExReq.ex
          ext.exType := ExType.ILLEGAL_INSTR
          ext.tval := pipe.instr.instr.raw
        }

        // Currently does nothing
      }.otherwise {
        switch(pipe.instr.instr.rs2) {
          is(Decoder.PRIV_RS2("WFI")) {
            when(priv =/= PrivLevel.M && status.tw) {
              ext.ex := ExReq.ex
              ext.exType := ExType.ILLEGAL_INSTR
              ext.tval := pipe.instr.instr.raw
            }

            // No-op
          }

          is(Decoder.PRIV_RS2("ECALL")) {
            ext.ex := ExReq.ex
            ext.exType := Mux1H(
              Seq(
                (priv === PrivLevel.M) -> ExType.M_CALL,
                (priv === PrivLevel.S) -> ExType.S_CALL,
                (priv === PrivLevel.U) -> ExType.U_CALL
              )
            )
            ext.tval := 0.U
          }

          is(Decoder.PRIV_RS2("EBREAK")) {
            ext.ex := ExReq.ex
            ext.exType := ExType.BREAKPOINT
            // If mtval is written with a nonzero value when a breakpoint,
            // address-misaligned, access-fault, or page-fault exception occurs
            // on an instruction fetch, load, or store, then mtval will contain
            // the faulting virtual address.
            ext.tval := pipe.instr.addr
          }

          is(Decoder.PRIV_RS2("RET"), Decoder.PRIV_RS2("DRET")) {
            val t = MuxLookup(pipe.instr.instr.funct7, ExReq.ex)(
              Seq(
                Integer.parseInt("0011000", 2).U -> ExReq.mret,
                Integer.parseInt("0001000", 2).U -> ExReq.sret,
                Integer.parseInt("0111101", 2).U -> ExReq.dret
              )
            )

            when(priv === PrivLevel.U) {
              ext.ex := ExReq.ex
              ext.exType := ExType.ILLEGAL_INSTR
              ext.tval := pipe.instr.instr.raw
            }.elsewhen(priv === PrivLevel.S && t === ExReq.mret) {
              ext.ex := ExReq.ex
              ext.exType := ExType.ILLEGAL_INSTR
              ext.tval := pipe.instr.instr.raw
            }.elsewhen(priv === PrivLevel.S && status.tsr) {
              ext.ex := ExReq.ex
              ext.exType := ExType.ILLEGAL_INSTR
              ext.tval := pipe.instr.instr.raw
            }.otherwise {
              ext.ex := t
              ext.tval := 0.U
            }
          }
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: BranchExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))

    when(ext.ex === ExReq.ex) {
      // info.regWaddr := 0.U
      info.exception.ex(ext.exType)
      info.wb := ext.tval
    }.elsewhen(ext.ex =/= ExReq.none) {
      // info.regWaddr := 0.U
      info.exception.ret(ext.ex)
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("SYSTEM").ident) {
      when(pipe.instr.instr.funct7 === Decoder.PRIV_FUNCT7("SFENCE.VMA")) {
        info.exception.sfence(pipe.instr.addr +% 4.U)
      }.otherwise {
        info.exception.nofire
      }
    }.elsewhen(pipe.instr.instr.op === Decoder.Op("BRANCH").ident) {
      // info.regWaddr := 0.U
      info.branchTaken := ext.branchTaken
      // expected branch target
      val target = Wire(UInt(coredef.XLEN.W))
      when(ext.branchTaken) {
        // addr + imm
        target := (pipe.instr.instr.imm + pipe.instr.addr.asSInt).asUInt
      } otherwise {
        // next pc
        target := pipe.instr.npc
      }

      when(ext.branchTaken =/= pipe.instr.taken) {
        // mis-predict
        // branch to actual address
        info.exception.fire(target)
      }.otherwise {
        info.exception.nofire
      }
    }.otherwise { // JAL/JALR, JAL is now in Bypass, so this must be JALR
      // info.regWaddr := pipe.instr.instr.rd
      info.wb := pipe.instr.npc

      // actual branch target
      val dest =
        (((pipe.rs1val.asSInt + pipe.instr.instr.imm) >> 1) << 1).asUInt

      when(pipe.instr.taken && dest === pipe.instr.pred.targetAddress) {
        // predicted to be taken and destination is correctly predicted
        info.exception.nofire
      }.otherwise {
        // mis-predict
        info.exception.fire(dest)
      }
    }

    info
  }

  init()
}
