package meowv64.interrupt

import chisel3._
import chisel3.experimental._
import chisel3.util._
import meowv64.system.SystemDef

object CLINT {
  val CLINT_REGION_START = BigInt("02000000", 16)
  val CLINT_REGION_SIZE = 0x10000
  val CLINT_ADDR_WIDTH = log2Ceil(CLINT_REGION_SIZE)
}

object CLINTMMIODef
    extends {
      override val ADDR_WIDTH: Int = CLINT.CLINT_ADDR_WIDTH
      override val XLEN: Int = 64
    }
    with MMIODef

object CLINTMapping
    extends {
      override val MAPPED_START = CLINT.CLINT_REGION_START
      override val MAPPED_SIZE = BigInt(CLINT.CLINT_REGION_SIZE)
    }
    with MMIOMapping

class LocalInt extends Bundle {

  /** Software interrupt
    */
  val msip = Bool()

  /** Timer interrupt
    */
  val mtip = Bool()
}

/** CLINT, core local interrupt controller.
  *
  * Implements msip, mtimecmp and mtime
  */
class CLINT(implicit sDef: SystemDef) extends Module {
  val toL2 = IO(new MMIOAccess(CLINTMMIODef))
  val ints = IO(Output(Vec(sDef.CORE_COUNT, new LocalInt)))

  toL2.req.nodeq()
  toL2.resp.bits := 0.U
  toL2.resp.valid := false.B

  // Timer
  val mtime = RegInit(0.U(64.W))
  mtime := mtime +% 1.U

  val mtimecmp = RegInit(VecInit(Seq.fill(sDef.CORE_COUNT)(0.U(64.W))))
  for ((c, m) <- ints.zip(mtimecmp)) {
    c.mtip := m < mtime
  }

  val msip = RegInit(VecInit(Seq.fill(sDef.CORE_COUNT)(false.B)))
  for ((c, s) <- ints.zip(msip)) {
    c.msip := s
  }

  object State extends ChiselEnum {
    val idle, commit = Value
  }

  object Seg extends ChiselEnum {
    val msip, mtimecmp, mtime = Value
  }

  val state = RegInit(State.idle)
  val seg = RegInit(Seg.msip)
  val idx = Reg(UInt(log2Ceil(sDef.CORE_COUNT).W))
  val wdata = RegInit(0.U(64.W))
  val write = RegInit(false.B)
  // check out of bounds
  val invalid = RegInit(false.B)

  switch(state) {
    is(State.idle) {
      val cur = toL2.req.deq()
      wdata := cur.wdata
      write := cur.op === MMIOReqOp.write

      when(cur.addr < 0x4000.U) {
        seg := Seg.msip
        val index = cur.addr(11, 0) >> 2
        idx := index
        invalid := index >= sDef.CORE_COUNT.U
      }.elsewhen(cur.addr =/= 0xbff8.U) {
        seg := Seg.mtimecmp
        val index = cur.addr(11, 0) >> 3
        idx := index
        invalid := index >= sDef.CORE_COUNT.U
      }.otherwise {
        seg := Seg.mtime
        invalid := false.B
      }

      when(toL2.req.fire) {
        state := State.commit
      }
    }

    is(State.commit) {
      state := State.idle

      toL2.resp.valid := true.B

      switch(seg) {
        is(Seg.msip) {
          toL2.resp.bits := msip(idx)
        }

        is(Seg.mtimecmp) {
          toL2.resp.bits := mtimecmp(idx)
        }

        is(Seg.mtime) {
          toL2.resp.bits := mtime
        }
      }

      // out of bounds
      when(invalid) {
        toL2.resp.bits := 0.U
      }

      when(write && ~invalid) {
        switch(seg) {
          is(Seg.msip) {
            msip(idx) := wdata(0)
          }

          is(Seg.mtimecmp) {
            mtimecmp(idx) := wdata
          }

          is(Seg.mtime) {
            mtime := wdata
          }
        }
      }
    }
  }
}
