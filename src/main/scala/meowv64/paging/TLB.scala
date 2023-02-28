package meowv64.paging

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.util.random.LFSR
import meowv64.core.CoreDef
import meowv64.core.Satp

/** Lookup privilege mode. send both if its in supervisor and SUM = true
  */
object TLBLookupMode extends ChiselEnum {
  val U, S, both = Value
}

object TLBAccessMode extends ChiselEnum {
  val R, W, X = Value
}

class TLBReq(implicit val coredef: CoreDef) extends Bundle {
  val mode = TLBLookupMode()

  /** This access is read/write/execute, check dirty and rwx bit
    */
  val access = TLBAccessMode()
  /* mstatus.MXR (Make eXecutable Readable) */ 
  val mxr = Bool()
  val vpn = UInt(coredef.vpnWidth.W)
}

class TLBResp(implicit val coredef: CoreDef) extends Bundle {
  val ppn = UInt(coredef.ppnWidth.W)
  val fault = Bool()
}

class TLB(implicit val coredef: CoreDef) extends Module {
  val ptw = IO(new TLBExt)

  val satp = IO(Input(new Satp))

  val query = IO(new Bundle {
    val req = Flipped(Decoupled(new TLBReq))
    val resp = Output(new TLBResp)
  })

  val flush = IO(Input(Bool()))

  object TLBState extends ChiselEnum {
    val idle, req, resp = Value
  }

  val state = RegInit(TLBState.idle)

  val storage = RegInit(VecInit(Seq.fill(coredef.TLB_SIZE)(TLBEntry.empty)))
  val hitMap = storage.map(_.hit(query.req.bits.vpn))
  val hit = Mux1H(hitMap, storage)

  // at most one hit
  when(query.req.valid) {
    assert(PopCount(hitMap) <= 1.U)
  }

  // save last ptw faulted information
  val refilling = Reg(UInt())
  val ptwFaulted = RegInit(false.B)

  val inStore = VecInit(hitMap).asUInt().orR()
  // handle SUM and U bit
  val modeMismatch = MuxLookup(
    query.req.bits.mode.asUInt(),
    false.B,
    Seq(
      TLBLookupMode.S.asUInt -> hit.u,
      TLBLookupMode.U.asUInt -> !hit.u
    )
  )

  // Determine if the requested memory access is allowed by the pte.r, pte.w,
  // pte.x, and pte.u bits, given the current privilege mode and the value of
  // the SUM and MXR fields of the mstatus register.
  // If pte.a = 0, or if the original memory access is a store and pte.d = 0,
  // raise a page-fault exception corresponding to the original access type
  val accessFault =
    modeMismatch ||
      (query.req.bits.access === TLBAccessMode.R && !hit.r && !query.req.bits.mxr) ||
      (query.req.bits.access === TLBAccessMode.R && !hit.r && !hit.x && query.req.bits.mxr) ||
      (query.req.bits.access === TLBAccessMode.W && !hit.w) ||
      (query.req.bits.access === TLBAccessMode.X && !hit.x) ||
      (query.req.bits.access === TLBAccessMode.W && !hit.d) || !hit.a
  val fault =
    (ptwFaulted && refilling === query.req.bits.vpn) || (inStore && accessFault)

  query.resp.ppn := 0.U
  query.resp.fault := false.B

  // response when a match is found or faulted
  when(query.req.fire) {
    query.resp.ppn := hit.fromVPN(query.req.bits.vpn)
    query.resp.fault := fault
  }

  query.req.ready := (inStore || fault) && !flush

  ptw.req.noenq()

  // IF can be flushed during a blocked TLB refill, so we need to ensure that we properly handles it
  // This is done by using a state machine
  switch(state) {
    is(TLBState.idle) {
      query.req.ready := false.B
      when(!flush && query.req.valid) {
        query.req.ready := inStore || fault

        when(!query.req.ready) {
          refilling := query.req.bits.vpn
          state := TLBState.req
          ptwFaulted := false.B
        }
      }
    }

    is(TLBState.req) {
      query.req.ready := false.B

      ptw.req.enq(refilling)

      when(ptw.req.fire) {
        state := TLBState.resp
      }
    }

    is(TLBState.resp) {
      query.req.ready := false.B

      val random = LFSR(log2Ceil(coredef.TLB_SIZE))
      val invalids = storage.map(!_.v)
      val hasInvalid = VecInit(invalids).asUInt().orR
      val victim = Mux(
        hasInvalid,
        PriorityEncoder(invalids),
        random
      )

      val written =
        TLBEntry.fromPTE(refilling, ptw.resp.bits.level, ptw.resp.bits.pte)

      when(ptw.resp.valid) {
        when(!ptw.resp.bits.fault) {
          // save ptw entry to tlb
          storage(victim) := written
        }
        ptwFaulted := ptw.resp.bits.fault
        state := TLBState.idle
      }
    }
  }

  when(flush) {
    for (slot <- storage) {
      slot.v := false.B
    }
  }
}
