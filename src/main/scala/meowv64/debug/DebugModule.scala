package meowv64.debug

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import meowv64.system.SystemDef
import meowv64.core.CoreToDebugModule
import meowv64.interrupt.MMIODef
import meowv64.interrupt.MMIOMapping
import meowv64.interrupt.MMIOAccess
import meowv64.interrupt.MMIOReqOp
import meowv64.cache.L1ICPort
import meowv64.core.CoreDef
import meowv64.cache.DCWriteLen

class DebugModuleReq extends Bundle {
  val address = UInt(7.W)
  val data = UInt(32.W)
  val isRead = Bool()
}

class DebugModuleResp extends Bundle {
  val data = UInt(32.W)
  val fail = Bool()
}

class DebugModuleInterface extends Bundle {
  val req = Flipped(Decoupled(new DebugModuleReq))
  val resp = Decoupled(new DebugModuleResp)
}

object DebugModuleState extends ChiselEnum {
  val idle, req, resp = Value
}

class DMControl extends Bundle {
  val haltreq = Bool()
  val resumereq = Bool()
  val hartreset = Bool()
  val ackhavereset = Bool()
  val ackunavail = Bool()
  val hasel = Bool()
  val hartsello = UInt(10.W)
  val hartselhi = UInt(10.W)
  val setkeepalive = Bool()
  val clrkeepalive = Bool()
  val setresethaltreq = Bool()
  val clrresethaltreq = Bool()
  val ndmreset = Bool()
  val dmactive = Bool()
}

class DMStatus extends Bundle {
  val ndmresetpending = Bool()
  val stickyunavail = Bool()
  val impebreak = Bool()
  val zero = UInt(2.W)
  val allhavereset = Bool()
  val anyhavereset = Bool()
  val allresumeack = Bool()
  val anyresumeack = Bool()
  val allnonexistent = Bool()
  val anynonexistent = Bool()
  val allunavail = Bool()
  val anyunavail = Bool()
  val allrunning = Bool()
  val anyrunning = Bool()
  val allhalted = Bool()
  val anyhalted = Bool()
  val authenticated = Bool()
  val authbusy = Bool()
  val hasresethaltreq = Bool()
  val confstrptrvalid = Bool()
  val version = UInt(4.W)
}

class HartInfo extends Bundle {
  val nscratch = UInt(4.W)
  val zero = UInt(3.W)
  val dataaccess = Bool()
  val datasize = UInt(4.W)
  val dataaddr = UInt(4.W)
}

class AbstractCS extends Bundle {
  val progbufsize = UInt(5.W)
  val zero1 = UInt(11.W)
  val busy = Bool()
  val relaxedpriv = Bool()
  val cmderr = UInt(3.W)
  val zero2 = UInt(4.W)
  val datacount = UInt(4.W)
}

class AbstractCmd extends Bundle {
  val cmdtype = UInt(8.W)
  val control = UInt(24.W)
}

/** Access Register Command (cmdtype = 0)
  */
class AccessRegisterCmd extends Bundle {
  val aarsize = UInt(3.W)
  val aarpostincrement = Bool()
  val postexec = Bool()
  val transfer = Bool()
  val write = Bool()
  val regno = UInt(16.W)
}

class SystemBusCS extends Bundle {
  val sbversion = UInt(3.W)
  val zero = UInt(6.W)
  val sbbusyerror = Bool()
  val sbbusy = Bool()
  val sbreadonaddr = Bool()
  val sbaccess = UInt(3.W)
  val sbautoincrement = Bool()
  val sbreadondata = Bool()
  val sberror = UInt(3.W)
  val sbasize = UInt(7.W)
  val sbaccess128 = Bool()
  val sbaccess64 = Bool()
  val sbaccess32 = Bool()
  val sbaccess16 = Bool()
  val sbaccess8 = Bool()
}

object DebugModule {
  val DM_DATA_REGION_START = BigInt("00000000", 16)
  val DM_DATA_REGION_SIZE = 0x10000
  val DM_DATA_ADDR_WIDTH = log2Ceil(DM_DATA_REGION_SIZE)

  val DM_CODE_REGION_START = BigInt("00010000", 16)
  val DM_CODE_REGION_SIZE = 0x20000
  val DM_CODE_ADDR_WIDTH = log2Ceil(DM_CODE_REGION_SIZE)

  val DM_CODE_ROM_REGION_START = BigInt("00010000", 16)
  val DM_CODE_ROM_REGION_SIZE = 0x10000
  val DM_CODE_RAM_REGION_START = BigInt("00020000", 16)
  val DM_CODE_RAM_REGION_SIZE = 0x10000
}

object DebugModuleMMIODef
    extends {
      override val ADDR_WIDTH: Int = DebugModule.DM_DATA_ADDR_WIDTH
      override val XLEN: Int = 64
    }
    with MMIODef

object DebugModuleMapping
    extends {
      override val MAPPED_START = DebugModule.DM_DATA_REGION_START
      override val MAPPED_SIZE = BigInt(DebugModule.DM_DATA_REGION_SIZE)
    }
    with MMIOMapping

object AbstractState extends ChiselEnum {
  val idle, action, resume = Value
}

/** Debug module
  *
  * Maps to memory region:
  *
  * [0x0, 0xFFFF]: maps data0-12 & some internal registers.
  *
  * [0x10000, 0x1FFFF]: maps rom internal program buffer
  *
  * [0x20000, 0x2FFFF]: maps ram internal program buffer
  */
class DebugModule(implicit sDef: SystemDef) extends Module {
  val io = IO(new Bundle {
    val dmi = new DebugModuleInterface()
    val core = Vec(sDef.CORE_COUNT, Flipped(new CoreToDebugModule))

    // cached code access
    val toL1I =
      Flipped(new L1ICPort(CoreDef.default(0, 0, sDef.L2_LINE_BYTES).L1I))
    // uncached data access
    val toL2 = new MMIOAccess(DebugModuleMMIODef)
  })

  io.dmi.req.ready := false.B
  io.dmi.resp.valid := false.B

  val state = RegInit(DebugModuleState.idle)
  val curReq = Reg(new DebugModuleReq())
  val curResp = Reg(new DebugModuleResp())
  io.dmi.resp.bits := curResp

  // abstract data
  val absData = RegInit(VecInit.fill(12)(0.U(32.W)))

  // abstractcs registers
  val absState = RegInit(AbstractState.idle)
  val cmderr = RegInit(0.U(3.W))

  // dmcontrol registers
  val hartreset = RegInit(false.B)
  val hartsel = RegInit(
    0.U(log2Ceil(sDef.CORE_COUNT))
  ) // only one hart at a time
  val ndmreset = RegInit(false.B)
  val dmactive = RegInit(false.B)
  val haltreq = RegInit(VecInit.fill(sDef.CORE_COUNT)(false.B))
  for (i <- 0 until sDef.CORE_COUNT) {
    io.core(i).haltreq := haltreq(i)
  }

  // handle resumeack
  val resumeack = RegInit(VecInit.fill(sDef.CORE_COUNT)(false.B))
  when(
    ~resumeack(hartsel) && ~io.core(hartsel).halted && RegNext(
      io.core(hartsel).halted
    )
  ) {
    // resumed
    resumeack(hartsel) := true.B
  }

  // rom region
  val TO_L2_TRANSFER_WIDTH = io.toL1I.data.getWidth
  val debugCodeBytes = getClass.getResourceAsStream("/debug.bin").readAllBytes()
  val byteChunk = TO_L2_TRANSFER_WIDTH / 8
  val len = debugCodeBytes.length
  val chunks = (len + byteChunk - 1) / byteChunk
  val paddedLen = chunks * byteChunk
  val padded =
    debugCodeBytes ++ Array.fill(paddedLen - len)(0.asInstanceOf[Byte])
  val initValues = for (i <- 0 until chunks) yield {
    var value = BigInt(0)
    for (j <- 0 until byteChunk) {
      var unsigned = BigInt(padded(i * byteChunk + j))
      if (unsigned < 0) {
        unsigned += 256
      }
      value |= unsigned << (j * 8)
    }

    value.U(TO_L2_TRANSFER_WIDTH.W)
  }
  val codeMem = RegInit(VecInit(initValues))

  // ram region
  val ramInstCount = 6
  val ramInsts = RegInit(VecInit.fill(ramInstCount)(0.U(32.W)))
  val instChunkSize = byteChunk / 4
  val instChunks = (ramInstCount + instChunkSize - 1) / instChunkSize
  val ramView = Wire(Vec(instChunks, UInt(TO_L2_TRANSFER_WIDTH.W)))
  for (i <- 0 until instChunks) {
    val signal = Wire(UInt(TO_L2_TRANSFER_WIDTH.W))
    signal := Cat((for (j <- 0 until instChunkSize) yield {
      val idx = i * instChunkSize + j
      if (idx < ramInstCount) {
        ramInsts(idx)
      } else {
        0.U(32.W)
      }
    }).reverse)
    ramView(i) := signal
  }

  val done = WireInit(false.B)
  switch(state) {
    is(DebugModuleState.idle) {
      io.dmi.req.ready := true.B
      when(io.dmi.req.fire) {
        curReq := io.dmi.req.bits
        state := DebugModuleState.req
      }
    }
    is(DebugModuleState.req) {
      // default: fail
      done := true.B
      curResp.fail := true.B

      switch(curReq.address) {
        is(
          0x04.U,
          0x05.U,
          0x06.U,
          0x07.U,
          0x08.U,
          0x09.U,
          0x0a.U,
          0x0b.U,
          0x0c.U,
          0x0d.U,
          0x0e.U,
          0x0f.U
        ) {
          // data0~data11
          val idx = curReq.address - 0x04.U
          when(curReq.isRead) {
            curResp.data := absData(idx)
          }.otherwise {
            absData(idx) := curReq.data
          }

          curResp.fail := false.B
          done := true.B
        }
        is(0x10.U) {
          // dmcontrol
          when(curReq.isRead) {
            val resp = WireInit(0.U.asTypeOf(new DMControl))
            resp.hartreset := hartreset
            val hartselExtended = Wire(UInt(20.W))
            hartselExtended := hartsel
            resp.hartsello := hartselExtended(9, 0)
            resp.hartselhi := hartselExtended(19, 10)
            resp.ndmreset := ndmreset
            resp.dmactive := dmactive

            curResp.data := resp.asUInt
          }.otherwise {
            val req = Wire(new DMControl)
            req := curReq.data.asTypeOf(req)

            hartreset := req.hartreset
            val newHartsel = Wire(UInt(log2Ceil(sDef.CORE_COUNT).W))
            newHartsel := Cat(req.hartselhi, req.hartsello)
            hartsel := newHartsel

            haltreq(newHartsel) := req.haltreq
            when(req.resumereq) {
              resumeack(newHartsel) := false.B

              assert(absState === AbstractState.idle)
              absState := AbstractState.resume
            }
            ndmreset := req.ndmreset
            dmactive := req.dmactive
          }
          curResp.fail := false.B
          done := true.B
        }
        is(0x11.U) {
          // dmstatus
          when(curReq.isRead) {
            val resp = WireInit(0.U.asTypeOf(new DMStatus))
            // halted == in debug mode
            val halted = WireInit(io.core(hartsel).halted)
            resp.allhalted := halted
            resp.anyhalted := halted
            resp.allrunning := ~halted
            resp.anyrunning := ~halted
            resp.allresumeack := resumeack(hartsel)
            resp.anyresumeack := resumeack(hartsel)
            val nonexistent = WireInit(hartsel >= sDef.CORE_COUNT.U)
            resp.allnonexistent := nonexistent
            resp.anynonexistent := nonexistent
            resp.authenticated := true.B // always authenticated
            resp.version := 3.U // debug 1.0

            curResp.data := resp.asUInt
          }
          curResp.fail := false.B
          done := true.B
        }
        is(0x12.U) {
          // hartinfo
          when(curReq.isRead) {
            val resp = WireInit(0.U.asTypeOf(new HartInfo))

            curResp.data := resp.asUInt
          }
          curResp.fail := false.B
          done := true.B
        }
        is(0x16.U) {
          // abstractcs
          when(curReq.isRead) {
            val resp = WireInit(0.U.asTypeOf(new AbstractCS))
            resp.busy := absState =/= AbstractState.idle
            resp.cmderr := cmderr
            resp.datacount := 12.U

            curResp.data := resp.asUInt
          }.otherwise {
            val req = Wire(new AbstractCS)
            req := curReq.data.asTypeOf(req)

            when(req.cmderr =/= 0.U) {
              cmderr := 0.U
            }
          }
          curResp.fail := false.B
          done := true.B
        }
        is(0x17.U) {
          // command
          when(curReq.isRead) {
            curResp.data := 0.U
          }.otherwise {
            val req = Wire(new AbstractCmd)
            req := curReq.data.asTypeOf(req)
            when(cmderr === 0.U) {
              when(absState =/= AbstractState.idle) {
                cmderr := 1.U
              }.otherwise {
                // execute
                val supported = WireInit(false.B)
                when(~supported) {
                  cmderr := 2.U // not supported by default
                }.otherwise {
                  absState := AbstractState.action
                }

                when(req.cmdtype === 0.U) {
                  // access register
                  val cmd = Wire(new AccessRegisterCmd)
                  cmd := req.control.asTypeOf(cmd)

                  val csr = cmd.regno < 0x1000.U
                  val csrIdx = cmd.regno & 0xfff.U
                  val gpr = 0x1000.U <= cmd.regno && cmd.regno <= 0x101f.U
                  val gprIdx = cmd.regno & 0x1f.U
                  val fpr = 0x1020.U <= cmd.regno && cmd.regno <= 0x103f.U
                  val fprIdx = cmd.regno & 0x1f.U

                  // a0 = 0x1000
                  val a0 = 10.U
                  // a1 is temporary
                  val a1 = 11.U

                  val ebreak = WireInit(((1 << 20) | (0x73)).U)
                  // sw zero, 4(a0)
                  // rs2=zero rs1=a0 imm=4
                  val finish =
                    WireInit((a0 << 15) | (2.U << 12) | (4.U << 7) | (0x23.U))

                  // set ram inst
                  // instructions:
                  // load to register
                  // read/write data
                  // ebreak
                  when(cmd.transfer) {
                    when(cmd.write) {
                      // write
                      when(csr) {
                        // write csr
                        when(cmd.aarsize === 3.U) {
                          // 64 bits
                          // ld a1, 0(zero)
                          // rd=a1 rs1=zero imm=0
                          ramInsts(0) := (2.U << 12) |
                            (a1 << 7) | (0x03.U)
                          // csrrw zero, csrIdx, a1
                          // csr=csrIdx rs1=a1 001 rd=zero 1110011
                          ramInsts(1) := (csrIdx << 20) | (a1 << 15) |
                            (1.U << 12) | (0x73.U)
                          // finish
                          ramInsts(2) := finish
                          // ebreak
                          ramInsts(3) := ebreak
                          supported := true.B
                        }
                      }
                    }.otherwise {
                      // read
                      when(csr) {
                        // read csr
                        // csrrs a1, csrIdx, zero
                        // csr=csrIdx rs1=zero 010 rd=a1 1110011
                        ramInsts(0) := (csrIdx << 20) | (2.U << 12) |
                          (a1 << 7) | (0x73.U)
                        when(cmd.aarsize === 3.U) {
                          // 64 bits
                          // sd a1, 0(zero)
                          // rs2=a1 rs1=zero imm=0
                          ramInsts(1) := (a1 << 20) | (3.U << 12) |
                            (0.U << 7) | (0x23.U)
                        }.elsewhen(cmd.aarsize === 2.U) {
                          // 32 bits
                          // sw a1, 0(zero)
                          // rs2=a1 rs1=zero imm=0
                          ramInsts(1) := (a1 << 20) | (2.U << 12) |
                            (0.U << 7) | (0x23.U)
                        }
                        // finish
                        ramInsts(2) := finish
                        // ebreak
                        ramInsts(3) := ebreak
                        supported := true.B
                      }.elsewhen(gpr) {
                        // read gpr
                        when(cmd.aarsize === 3.U) {
                          // 64 bits
                          // step 1:
                          // mv a1, reg
                          // special case: reg == a0 or a1
                          // read from dscratch0/1 register to a1 first
                          when(gprIdx === a0 || gprIdx === a1) {
                            val scratchIdx =
                              Mux(gprIdx === a0, 0x7b2.U, 0x7b3.U)
                            // csrrs a1, scratchIdx, zero
                            // csr=scratcIdx rs1=zero 010 rd=a1 1110011
                            ramInsts(0) := (scratchIdx << 20) | (2.U << 12) |
                              (a1 << 7) | (0x73.U)
                          }.otherwise {
                            // addi a1, reg, 0
                            // rs1=reg rd=a1 imm=0
                            ramInsts(0) := (gprIdx << 15) | (0.U << 12) |
                              (a1 << 7) | (0x13.U)
                          }

                          // sd a1, 0(zero)
                          // rs2=a1 rs1=zero imm=0
                          ramInsts(1) := (a1 << 20) | (3.U << 12) |
                            (0.U << 7) | (0x23.U)
                          // finish
                          ramInsts(2) := finish
                          // ebreak
                          ramInsts(3) := ebreak
                          supported := true.B
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          curResp.fail := false.B
          done := true.B
        }
        is(0x38.U) {
          // sbcs
          when(curReq.isRead) {
            val resp = WireInit(0.U.asTypeOf(new SystemBusCS))
            resp.sbversion := 1.U
            resp.sbaccess := 2.U

            curResp.data := resp.asUInt
          }.otherwise {
            val req = Wire(new SystemBusCS)
            req := curReq.data.asTypeOf(req)
          }
          curResp.fail := false.B
          done := true.B
        }
      }
      when(done) {
        state := DebugModuleState.resp
      }
    }
    is(DebugModuleState.resp) {
      io.dmi.resp.valid := true.B
      when(io.dmi.resp.fire) {
        state := DebugModuleState.idle
      }
    }
  }

  // code access
  // to l1 icache
  val offset = io.toL1I.read.bits >> log2Ceil(byteChunk)
  when(
    DebugModule.DM_CODE_ROM_REGION_START.U <= io.toL1I.read.bits && io.toL1I.read.bits < (DebugModule.DM_CODE_ROM_REGION_START + DebugModule.DM_CODE_ROM_REGION_SIZE).U
  ) {
    // rom region
    io.toL1I.data := codeMem(offset)
  }.otherwise {
    // ram region
    io.toL1I.data := ramView(offset)
  }
  io.toL1I.stall := false.B

  // MMIO access
  io.toL2.req.ready := true.B
  io.toL2.resp.valid := io.toL2.req.fire
  io.toL2.resp.bits := 0.U

  when(io.toL2.req.fire) {
    when(0.U <= io.toL2.req.bits.addr && io.toL2.req.bits.addr < 48.U) {
      // data0-12
      val idx = io.toL2.req.bits.addr >> 2
      when(io.toL2.req.bits.op === MMIOReqOp.read) {
        // read two 32-bits at most
        io.toL2.resp.bits := absData(idx + 1.U) ## absData(idx)
      }.otherwise {
        when(io.toL2.req.bits.len === DCWriteLen.D) {
          // write two 32-bits
          absData(idx) := io.toL2.req.bits.wdata
          absData(idx + 1.U) := io.toL2.req.bits.wdata >> 32
        }.otherwise {
          absData(idx) := io.toL2.req.bits.wdata
        }
      }
    }.elsewhen(io.toL2.req.bits.addr === 0x1000.U) {
      // read current action
      when(io.toL2.req.bits.op === MMIOReqOp.read) {
        io.toL2.resp.bits := absState.asUInt
      }
    }.elsewhen(io.toL2.req.bits.addr === 0x1004.U) {
      // current abstract command is done
      when(io.toL2.req.bits.op === MMIOReqOp.write) {
        absState := AbstractState.idle
      }
    }
  }
}
