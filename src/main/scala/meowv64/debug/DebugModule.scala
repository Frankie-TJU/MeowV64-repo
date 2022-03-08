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
  val DM_CODE_REGION_SIZE = 0x10000
  val DM_CODE_ADDR_WIDTH = log2Ceil(DM_CODE_REGION_SIZE)
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

/**
  * Debug module
  *
  * Maps to memory region:
  * [0x0, 0xFFFF]: maps data0-12 & some internal registers
  * [0x10000, 0x1FFFF]: maps debug module internal program buffer
  */
class DebugModule(implicit sDef: SystemDef) extends Module {
  val io = IO(new Bundle {
    val dmi = new DebugModuleInterface()
    val core = Vec(sDef.CORE_COUNT, Flipped(new CoreToDebugModule))

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
  val absBusy = RegInit(false.B)
  val relaxedpriv = RegInit(false.B)
  val cmderr = RegInit(0.U(3.W))

  // dmcontrol registers
  val hartreset = RegInit(false.B)
  val hartsel = RegInit(0.U(log2Ceil(sDef.CORE_COUNT))) // only one hart at a time
  val ndmreset = RegInit(false.B)
  val dmactive = RegInit(false.B)
  val haltreq = RegInit(VecInit.fill(sDef.CORE_COUNT)(false.B))
  for (i <- 0 until sDef.CORE_COUNT) {
    io.core(i).haltreq := haltreq(i)
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
            resp.busy := absBusy
            resp.relaxedpriv := relaxedpriv
            resp.cmderr := cmderr
            resp.datacount := 12.U

            curResp.data := resp.asUInt
          }.otherwise {
            val req = Wire(new AbstractCS)
            req := curReq.data.asTypeOf(req)

            relaxedpriv := req.relaxedpriv
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

  // MMIO access
  io.toL2.req.ready := true.B
  io.toL2.resp.valid := io.toL2.req.fire
  io.toL2.resp.bits := 0.U

  when(io.toL2.req.fire) {
    when(0.U <= io.toL2.req.bits.addr && io.toL2.req.bits.addr < 48.U) {
      // data0-12
      val idx = io.toL2.req.bits.addr >> 2
      when(io.toL2.req.bits.op === MMIOReqOp.read) {
        io.toL2.resp.bits := absData(idx)
      }.otherwise {
        absData(idx) := io.toL2.req.bits.wdata
      }
    }
  }
}
