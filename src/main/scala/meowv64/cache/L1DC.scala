package meowv64.cache

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.util.log2Ceil
import meowv64.cache.L1DCPort.L1Req
import meowv64.cache.L1DCPort.L2Req
import meowv64.core.CoreDef
import meowv64.data._
import meowv64.exec.MuxBE

class CoreDCReadReq(val opts: L1DOpts) extends Bundle {

  /** Address can be unaligned with regard to cache line boundary
    */
  val addr = UInt(opts.ADDR_WIDTH.W)

  /** for lr instruction */
  val reserve = Bool()
}

object CoreDCReadReq {
  def load(addr: UInt)(implicit coredef: CoreDef): CoreDCReadReq = {
    val ret = Wire(new CoreDCReadReq(coredef.L1D))
    ret.reserve := false.B
    ret.addr := addr
    ret
  }

  def lr(addr: UInt)(implicit coredef: CoreDef) = {
    val ret = Wire(new CoreDCReadReq(coredef.L1D))
    ret.reserve := true.B
    ret.addr := addr
  }
}

/** DCache reader from core to cache
  */
class CoreDCReader(implicit val coredef: CoreDef) extends Bundle {
  val req = Decoupled(new CoreDCReadReq(coredef.L1D))

  /** Data will be shifted for in-line offset
    */
  val resp = Input(Valid(UInt(coredef.L1D.TO_CORE_TRANSFER_WIDTH.W)))
}

class DCInnerReader(val opts: L1DOpts) extends Bundle {
  val req = Decoupled(new CoreDCReadReq(opts))

  val data = Input(UInt(opts.TO_CORE_TRANSFER_WIDTH.W))
}

object DCWriteOp extends ChiselEnum {
  val write = Value
  // store conditional
  // Write cond
  val cond = Value
  // atomic
  // TODO: optimize LUT
  val swap, add, and, or, xor, max, maxu, min, minu =
    Value
  val commitLR = Value // Commit pending LR
}

object DCWriteLen extends ChiselEnum {
  val B, H, W, D, Q, O = Value

  /** Compute byte enable
    */
  def toByteEnable(len: DCWriteLen.Type) = MuxLookup(
    len.asUInt,
    0.U,
    Seq(
      DCWriteLen.B.asUInt -> 0x1.U,
      DCWriteLen.H.asUInt -> 0x3.U,
      DCWriteLen.W.asUInt -> 0xf.U,
      DCWriteLen.D.asUInt -> 0xff.U
    )
  )

  def toAXISize(len: DCWriteLen.Type) = Mux1H(
    Seq(
      (len === DCWriteLen.B) -> AXI.Constants.Size.S1.U,
      (len === DCWriteLen.H) -> AXI.Constants.Size.S2.U,
      (len === DCWriteLen.W) -> AXI.Constants.Size.S4.U,
      (len === DCWriteLen.D) -> AXI.Constants.Size.S8.U,
      (len === DCWriteLen.Q) -> AXI.Constants.Size.S16.U,
      (len === DCWriteLen.O) -> AXI.Constants.Size.S32.U
    )
  )
}

/** Write request from core
  */
class CoreDCWriteReq(val opts: L1DOpts) extends Bundle {
  // Offset is now embedded inside addr
  val addr = UInt(opts.ADDR_WIDTH.W)
  val len = DCWriteLen()
  // Byte enable
  val be = UInt(opts.TO_CORE_TRANSFER_BYTES.W)
  val op = DCWriteOp()
  // WData should be sign extended
  val wdata = UInt(opts.TO_CORE_TRANSFER_WIDTH.W)
}

/** DCache write port (from LSU to DCache)
  */
class CoreDCWriter(val opts: L1DOpts) extends Bundle {
  val req = Decoupled(new CoreDCWriteReq(opts))

  val rdata = Input(UInt(opts.TO_CORE_TRANSFER_WIDTH.W)) // AMOSWAP and friends

  val IGNORED_WIDTH = log2Ceil(opts.TO_CORE_TRANSFER_BYTES)

  /** Byte enable */
  def be = req.bits.be

  /** Shifted wdata */
  def sdata = {
    val offset = req.bits.addr(IGNORED_WIDTH - 1, 0)
    val sliced = Wire(UInt(opts.TO_CORE_TRANSFER_WIDTH.W))
    sliced := req.bits.wdata << (offset << 3)
    sliced
  }

  /** Aligned address
    */
  def aligned =
    req.bits.addr(opts.ADDR_WIDTH - 1, IGNORED_WIDTH) ## 0.U(IGNORED_WIDTH.W)
}

class DCFenceStatus(val opts: L1DOpts) extends Bundle {

  /** Write buffer is empty
    */
  val wbufClear = Input(Bool())
}

class DLine(val opts: L1Opts) extends Bundle {
  val tag = UInt(opts.TAG_WIDTH.W)
  val valid = Bool()
  val dirty = Bool()
  val data = Vec(opts.TRANSFER_COUNT, UInt(opts.TO_CORE_TRANSFER_WIDTH.W))
}

object DLine {
  def empty(opts: L1DOpts): DLine = {
    val ret = Wire(new DLine(opts))

    ret := DontCare
    ret.valid := false.B

    ret
  }
}

// Write FIFO
class WriteEv(val opts: L1DOpts) extends Bundle {
  val aligned = UInt(opts.ADDR_WIDTH.W)
  val be = UInt(opts.TO_CORE_TRANSFER_BYTES.W)
  val sdata = UInt(opts.TO_CORE_TRANSFER_WIDTH.W)
  val isAMO = Bool()

  /** Store conditional
    */
  val isCond = Bool()
  val valid = Bool()
}

object WriteEv {
  def default(opts: L1DOpts): WriteEv = {
    val ret = Wire(new WriteEv(opts))

    ret.aligned := 0.U
    ret.be := 0.U
    ret.sdata := 0.U
    ret.valid := false.B
    ret.isAMO := false.B
    ret.isCond := false.B

    ret
  }
}

class L1DC(val opts: L1DOpts)(implicit coredef: CoreDef) extends Module {
  // Constants and helpers
  val IGNORED_WIDTH = log2Ceil(opts.TO_CORE_TRANSFER_BYTES)
  assert(opts.TO_CORE_TRANSFER_WIDTH == opts.TO_L2_TRANSFER_WIDTH)

  def getLineAddr(addr: UInt) =
    addr(opts.ADDR_WIDTH - 1, IGNORED_WIDTH) ## 0.U(IGNORED_WIDTH.W)
  def getTag(addr: UInt) = opts.getTag(addr)
  def getIndex(addr: UInt) = opts.getIndex(addr)
  def getSublineIdx(addr: UInt) = if (opts.TRANSFER_COUNT == 1) {
    0.U
  } else {
    addr(opts.OFFSET_WIDTH - 1, IGNORED_WIDTH)
  }

  // memory blackbox does not support nested aggregate data type
  val dcDataArray =
    SyncReadMem(
      opts.LINE_PER_ASSOC,
      Vec(opts.ASSOC, UInt((new DLine(opts)).getWidth.W))
    )

  // Ports, mr = Memory read, ptw = Page table walker
  val mr = IO(Flipped(new CoreDCReader))
  val ptw = IO(Flipped(new CoreDCReader))
  // w = Memory write
  val w = IO(Flipped(new CoreDCWriter(opts)))
  // memory fence
  val fs = IO(Flipped(new DCFenceStatus(opts)))
  val toL2 = IO(new L1DCPort(opts))

  // Convert mr + ptw to r
  val rArbiter = Module(new RRArbiter(new CoreDCReadReq(opts), 2))
  class RReq extends Bundle {
    val addr = UInt(coredef.PADDR_WIDTH.W)
    val chosen = UInt()
  }

  val r = Wire(new DCInnerReader(opts))
  r.req.valid := rArbiter.io.out.valid
  rArbiter.io.out.ready := r.req.ready
  r.req.bits.addr := rArbiter.io.out.bits.addr
  r.req.bits.reserve := rArbiter.io.out.bits.reserve

  rArbiter.io.in(0) <> ptw.req
  rArbiter.io.in(1) <> mr.req

  val current = Reg(UInt())
  when(rArbiter.io.out.fire) {
    current := rArbiter.io.chosen
  }

  // Asserting that in-line offset is 0
  // assert((!r.req.valid) || r.req.bits.addr(IGNORED_WIDTH - 1, 0) === 0.U)
  // The check for read is no longer true, because we shift the data here
  // assert((!w.write) || w.addr(IGNORED_WIDTH-1, 0) === 0.U)
  // The check for write is no longer true, because of AMO

  toL2.l1data := 0.U
  toL2.l1addr := 0.U
  toL2.l1req := L1Req.idle

  // Read pipes
  val pipeRead = RegInit(false.B)
  val pipeReadReserve = RegInit(false.B)
  val pipeReadAddr = RegInit(0.U(opts.ADDR_WIDTH.W))

  ptw.resp.bits := r.data
  ptw.resp.valid := pipeRead && r.req.ready && current === 0.U
  mr.resp.bits := r.data
  mr.resp.valid := pipeRead && r.req.ready && current === 1.U

  val queryAddr = Wire(UInt(opts.ADDR_WIDTH.W))
  val lookups = dcDataArray
    .read(getIndex(queryAddr))
    .asTypeOf(Vec(opts.ASSOC, new DLine(opts)))

  // AMO/SC stuff
  val amoalu = Module(new AMOALU(opts))
  amoalu.io.rdata := 0.U
  val resValid = RegInit(false.B)
  val resCommitted = RegInit(false.B)
  val reserved = Reg(UInt(opts.ADDR_WIDTH.W))
  def reserveMatch(addr: UInt) = (
    resValid && resCommitted
      && getTag(reserved) === getTag(addr)
      && getIndex(reserved) === getIndex(addr)
  )

  val pendingWriteOp = RegNext(w.req.bits.op)
  val pendingWriteData = RegNext(w.req.bits.wdata)
  val pendingWriteLen = RegNext(w.req.bits.len)
  val pendingWriteAddr = RegNext(w.req.bits.addr)
  amoalu.io.offset := pendingWriteAddr(
    log2Ceil(opts.TO_CORE_TRANSFER_BYTES) - 1,
    0
  )
  amoalu.io.wdata := pendingWriteData
  amoalu.io.length := pendingWriteLen
  amoalu.io.op := pendingWriteOp

  /** Write result for amo and sc instructions
    */
  val pendingWriteRet = RegInit(0.U(opts.XLEN.W))

  val wbuf = RegInit(
    VecInit(Seq.fill(opts.WRITE_BUF_DEPTH)(WriteEv.default(opts)))
  )
  val WBUF_IDX_WIDTH = log2Ceil(opts.WRITE_BUF_DEPTH)
  val wbufHead = RegInit(0.U(WBUF_IDX_WIDTH.W))
  val wbufTail = RegInit(0.U(WBUF_IDX_WIDTH.W))

  fs.wbufClear := wbufHead === wbufTail

  val pendingRead = Wire(Bool())

  // Write handler

  object MainState extends ChiselEnum {
    val writing, reading, walloc, readingRefill, wallocRefill, readingSpin,
        idle, rst = Value
    // readingSpin is for making sure that reading has finished.
    // Maybe can be optimized (handshake w <-> r)
  }

  val state = RegInit(MainState.rst)
  val nstate = Wire(MainState())

  nstate := state
  state := nstate

  val l2data = RegInit(0.U(opts.LINE_BYTES))

  val waddr = wbuf(wbufHead).aligned
  val nwaddr = wbuf(wbufHead +% 1.U).aligned
  assert(wbufHead === wbufTail || waddr(IGNORED_WIDTH - 1, 0) === 0.U)

  val wlookupAddr = Wire(waddr.cloneType)
  when(state === MainState.idle) {
    wlookupAddr := MuxLookup(
      nstate.asUInt,
      waddr,
      Seq(
        (MainState.reading.asUInt(), pipeReadAddr),
        (MainState.readingRefill.asUInt(), pipeReadAddr),
        (MainState.readingSpin.asUInt(), pipeReadAddr)
      )
    )
  }.otherwise {
    wlookupAddr := MuxLookup(
      state.asUInt(),
      waddr,
      Seq(
        (MainState.reading.asUInt(), pipeReadAddr),
        (MainState.readingRefill.asUInt(), pipeReadAddr),
        (MainState.readingSpin.asUInt(), pipeReadAddr)
      )
    )
  }

  // FIXME: merge this with lookups
  val wlookups = dcDataArray
    .read(getIndex(wlookupAddr))
    .asTypeOf(Vec(opts.ASSOC, new DLine(opts)))
  val whits = wlookups.map(line => line.valid && line.tag === getTag(waddr))
  val whit = VecInit(whits).asUInt.orR
  val wdirtyHits =
    wlookups.map(line => line.valid && line.tag === getTag(waddr) && line.dirty)
  val wdirtyHit = VecInit(wdirtyHits).asUInt.orR

  val rand = chisel3.util.random.LFSR(8)
  val victim = RegInit(0.U(opts.ASSOC_IDX_WIDTH.W))

  // writing / reading is never directly gone to
  assert(
    (nstate =/= MainState.writing && nstate =/= MainState.reading) || state === MainState.idle || nstate === state
  )

  // Write port
  val writing = Wire(Vec(opts.ASSOC, Bool()))
  val l1writing = Wire(Vec(opts.ASSOC, Bool()))
  val writingData = Wire(new DLine(opts))
  val writingAddr = Wire(UInt(opts.INDEX_WIDTH.W))
  l1writing := VecInit(Seq.fill(opts.ASSOC)(false.B))
  writing := l1writing
  writingData := DontCare
  writingAddr := DontCare

  dontTouch(writingAddr)
  dontTouch(writingData)
  dontTouch(writing)
  dcDataArray.write(
    writingAddr,
    VecInit(Seq.fill(opts.ASSOC)(writingData))
      .asTypeOf(Vec(opts.ASSOC, UInt((new DLine(opts)).getWidth.W))),
    writing
  )

  // Rst
  val rstCnt = RegInit(0.U(log2Ceil(opts.LINE_PER_ASSOC).W))

  val pipeReadAddrIdx = Wire(UInt())
  val waddrIdx = Wire(UInt())
  val wlookupIdx = Wire(UInt())
  pipeReadAddrIdx := getIndex(pipeReadAddr)
  waddrIdx := getIndex(waddr)
  wlookupIdx := getIndex(wlookupAddr)

  switch(state) {
    is(MainState.rst) {
      l1writing := VecInit(Seq.fill(opts.ASSOC)(true.B))
      writingAddr := rstCnt
      writingData := DLine.empty(opts)

      rstCnt := rstCnt +% 1.U

      when(rstCnt === (opts.LINE_PER_ASSOC - 1).U) {
        nstate := MainState.idle
      }
    }

    is(MainState.idle) {
      // Wait one extra cycle after walloc
      // for the allocated data to propagate to the read port
      when(pendingRead) {
        nstate := MainState.reading
        victim := rand(opts.ASSOC_IDX_WIDTH - 1, 0)
      }.elsewhen(wbufHead =/= wbufTail) {
        nstate := MainState.writing
      }
    }

    is(MainState.writing) {
      toL2.l1addr := getTag(waddr) ## getIndex(waddr) ## 0.U(
        opts.OFFSET_WIDTH.W
      )

      assert(wbufHead =/= wbufTail)

      def commit() {
        val hitMask = VecInit(
          wlookups.map(lookup => lookup.valid && lookup.tag === getTag(waddr))
        )
        val lookup = MuxCase(
          DLine.empty(opts),
          wlookups.zipWithIndex.map({ case (lookup, idx) =>
            (
              hitMask(idx),
              lookup
            )
          })
        )

        val written = Wire(lookup.cloneType)

        written := lookup
        written.data(getSublineIdx(waddr)) := MuxBE(
          wbuf(wbufHead).be,
          wbuf(wbufHead).sdata,
          lookup.data(getSublineIdx(waddr))
        )

        when(wbuf(wbufHead).isAMO) {
          amoalu.io.rdata := lookup.data(getSublineIdx(waddr))
          written.data(getSublineIdx(waddr)) := amoalu.io.muxed
          pendingWriteRet := amoalu.io.rsliced
        }.otherwise {
          pendingWriteRet := 0.U // Successful SC
        }

        when(wbuf(wbufHead).isCond) {
          resValid := false.B
        }

        written.dirty := true.B

        l1writing := hitMask
        writingAddr := getIndex(waddr)
        writingData := written

        wbuf(wbufHead).valid := false.B
        wbufHead := wbufHead +% 1.U

        nstate := MainState.idle
      }

      when(
        wbuf(wbufHead).isCond && !reserveMatch(waddr)
      ) {
        // SC failed
        pendingWriteRet := 1.U
        wbuf(wbufHead).valid := false.B
        wbufHead := wbufHead +% 1.U
        nstate := MainState.idle
        resValid := false.B
      }.elsewhen(
        RegNext(toL2.l2req) =/= L2Req.idle && RegNext(
          getIndex(toL2.l2addr)
        ) === getIndex(waddr)
      ) {
        // Wait for one extra cycle
        nstate := MainState.writing
      }.elsewhen(wdirtyHit) {
        // Commit directly
        commit()
      }.elsewhen(whit) {
        // Is an write hit, send modify directly
        toL2.l1req := L1DCPort.L1Req.modify

        when(!toL2.l1stall) {
          // Commit
          commit()
        }
      }.otherwise {
        // Not hit, goto walloc
        nstate := MainState.walloc
        victim := rand(opts.ASSOC_IDX_WIDTH - 1, 0)
      }
    }

    is(MainState.reading) {
      val reservedCollision = (wlookups(victim).valid
        && getIndex(pipeReadAddr) === getIndex(reserved)
        && wlookups(victim).tag === getTag(reserved)
        && resValid)
      when(if (opts.ASSOC_IDX_WIDTH == 0) { false.B }
      else { reservedCollision }) {
        // Reselect victim
        victim := victim +% 1.U
      }.elsewhen(!wlookups(victim).valid || !wlookups(victim).dirty) {
        nstate := MainState.readingRefill
      }.otherwise {
        toL2.l1addr := wlookups(victim).tag ## getIndex(pipeReadAddr) ## 0.U(
          opts.OFFSET_WIDTH.W
        )
        toL2.l1req := L1DCPort.L1Req.writeback
        toL2.l1data := wlookups(victim).data.asUInt()

        val invalid = Wire(new DLine(opts))
        invalid := DontCare
        invalid.valid := false.B

        when(!toL2.l1stall) {
          // Must be an read-miss
          l1writing(victim) := true.B
          writingAddr := getIndex(pipeReadAddr)
          writingData := invalid

          nstate := MainState.readingRefill
        }
      }
    }

    is(MainState.walloc) {
      when(!wlookups(victim).valid || !wlookups(victim).dirty) {
        nstate := MainState.wallocRefill
      }.otherwise {
        toL2.l1addr := wlookups(victim).tag ## getIndex(waddr) ## 0.U(
          opts.OFFSET_WIDTH.W
        )
        toL2.l1req := L1DCPort.L1Req.writeback
        toL2.l1data := wlookups(victim).data.asUInt()

        val invalid = Wire(new DLine(opts))
        invalid := DontCare
        invalid.valid := false.B

        when(!toL2.l1stall) {
          // Must be an read-miss
          l1writing(victim) := true.B
          writingAddr := getIndex(waddr)
          writingData := invalid

          nstate := MainState.wallocRefill
        }
      }
    }

    is(MainState.wallocRefill, MainState.readingRefill) {
      toL2.l1req := Mux(
        state === MainState.readingRefill,
        L1DCPort.L1Req.read,
        L1DCPort.L1Req.modify
      )
      val l1addr = Mux(state === MainState.readingRefill, pipeReadAddr, waddr)
      toL2.l1addr := getTag(l1addr) ## getIndex(l1addr) ## 0.U(
        opts.OFFSET_WIDTH.W
      )

      val writtenAddr =
        Mux(state === MainState.readingRefill, pipeReadAddr, waddr)

      val written = Wire(new DLine(opts))
      written.valid := true.B
      written.dirty := state === MainState.wallocRefill
      written.tag := getTag(writtenAddr)
      written.data := toL2.l2data.asTypeOf(written.data)

      when(state === MainState.wallocRefill) {
        written.data(getSublineIdx(waddr)) := MuxBE(
          wbuf(wbufHead).be,
          wbuf(wbufHead).sdata,
          toL2.l2data.asTypeOf(written.data)(getSublineIdx(waddr))
        )

        when(wbuf(wbufHead).isAMO) {
          amoalu.io.rdata := toL2.l2data.asTypeOf(written.data)(
            getSublineIdx(waddr)
          )
          written.data(getSublineIdx(waddr)) := amoalu.io.muxed
          // pendingWriteRet := amoalu.io.rsliced
          // pendingWriteRet is set in the branch below
        }
      }

      when(!toL2.l1stall) {
        writingAddr := getIndex(writtenAddr)
        writingData := written

        when(state === MainState.readingRefill) {
          l1writing(victim) := true.B
          nstate := MainState.readingSpin
        }.otherwise {
          l1writing(victim) := true.B

          when(wbuf(wbufHead).isCond) {
            resValid := false.B
            when(!reserveMatch(waddr)) {
              // SC failed
              pendingWriteRet := 1.U
              l1writing(victim) := false.B
            }
          }.elsewhen(wbuf(wbufHead).isAMO) {
            pendingWriteRet := amoalu.io.rsliced
          }.otherwise {
            pendingWriteRet := 0.U
          }

          wbuf(wbufHead).valid := false.B
          wbufHead := wbufHead +% 1.U

          nstate := MainState.idle
        }
      }
    }

    // TODO: drop spin stage
    is(MainState.readingSpin) {
      when(!pendingRead) {
        nstate := MainState.idle
      }
    }
  }

  // Handle write interface
  // This operates synchronously
  val wmHits = wbuf.map(ev => ev.valid && ev.aligned === w.aligned)
  val wmHit = VecInit(wmHits).asUInt().orR
  val wmHitHead = wbuf(wbufHead).valid && wbuf(wbufHead).aligned === w.aligned

  w.rdata := pendingWriteRet
  val pushed = RegInit(false.B) // Pushed state for

  when(~w.req.valid) {
    // No request
    w.req.ready := true.B
  }.elsewhen(w.req.bits.op === DCWriteOp.commitLR) {
    w.req.ready := true.B
    when(
      getTag(w.req.bits.addr) === getTag(reserved) && getIndex(
        w.req.bits.addr
      ) === getIndex(
        reserved
      )
    ) {
      resCommitted := true.B
    }
  }.elsewhen(w.req.bits.op =/= DCWriteOp.write) {
    // Wait for write queue to clear up
    when(!pushed) {
      w.req.ready := false.B
      when(wbufTail +% 1.U =/= wbufHead) {
        wbuf(wbufTail).aligned := w.aligned
        wbuf(wbufTail).be := w.be
        wbuf(wbufTail).sdata := w.sdata
        wbuf(wbufTail).valid := true.B
        wbuf(wbufTail).isAMO := w.req.bits.op =/= DCWriteOp.cond
        wbuf(wbufTail).isCond := w.req.bits.op === DCWriteOp.cond

        wbufTail := wbufTail +% 1.U

        pushed := true.B
      }
    }.elsewhen(wbufHead === wbufTail) {
      pushed := false.B
      w.req.ready := true.B
    }.otherwise {
      w.req.ready := false.B
    }
  }.elsewhen(wmHitHead) {
    // Head may be being processed right now, wait for it to finish and do a push
    w.req.ready := false.B
  }.otherwise {
    for (buf <- wbuf) {
      when(buf.aligned === w.aligned) {
        buf.sdata := MuxBE(w.be, w.sdata, buf.sdata)
        buf.be := buf.be | w.be
      }
    }

    when(wmHit) {
      // Write merge completing
      w.req.ready := true.B
    }.elsewhen(wbufTail +% 1.U =/= wbufHead) {
      // Write merge miss, waiting to push
      assert(w.aligned(IGNORED_WIDTH - 1, 0) === 0.U)
      wbuf(wbufTail).aligned := w.aligned
      wbuf(wbufTail).be := w.be
      wbuf(wbufTail).sdata := w.sdata
      wbuf(wbufTail).valid := true.B
      wbuf(wbufTail).isAMO := false.B
      wbuf(wbufTail).isCond := false.B

      wbufTail := wbufTail +% 1.U

      w.req.ready := true.B
    }.otherwise {
      // merge miss, fifo full, wait for fifo
      w.req.ready := false.B
    }
  }

  // Handle read interface
  // For debug use only
  val hitCount = PopCount(
    lookups.map(line => line.valid && line.tag === getTag(pipeReadAddr))
  )
  // when reset, hitMap contains X
  when(state =/= MainState.rst) {
    // at most one hit
    assert(hitCount <= 1.U)
  }

  val hits =
    lookups.map(line => line.valid && line.tag === getTag(pipeReadAddr))
  val hit = VecInit(hits).asUInt().orR
  val storeJustWrittenData = RegNext(writingData)
  val storeJustWritten = (
    RegNext(writing).asUInt().orR && RegNext(writingAddr) === getIndex(
      pipeReadAddr
    ) && (
      storeJustWrittenData.valid && storeJustWrittenData.tag === getTag(
        pipeReadAddr
      )
    )
  )
  val lookupRdata = Mux(
    storeJustWritten,
    storeJustWrittenData.data(getSublineIdx(pipeReadAddr)),
    Mux1H(
      lookups.map(line =>
        (
          line.valid && line.tag === getTag(pipeReadAddr),
          line.data(getSublineIdx(pipeReadAddr))
        )
      )
    )
  )

  val pendingWriteBufHits =
    wbuf.map(buf => buf.valid && buf.aligned === getLineAddr(pipeReadAddr))
  val pendingWriteBufData = Mux1H(pendingWriteBufHits, wbuf.map(_.sdata))
  val pendingWriteBufBe = Mux1H(pendingWriteBufHits, wbuf.map(_.be))

  val rdata = Mux(
    VecInit(pendingWriteBufHits).asUInt.orR,
    MuxBE(pendingWriteBufBe, pendingWriteBufData, lookupRdata),
    lookupRdata
  )

  when(r.req.ready) {
    pipeRead := r.req.valid
    pipeReadReserve := r.req.bits.reserve
    pipeReadAddr := r.req.bits.addr

    queryAddr := r.req.bits.addr

    assert((!pipeRead) || RegNext(queryAddr) === pipeReadAddr)
  }.otherwise {
    queryAddr := pipeReadAddr
  }

  when(pipeRead && pipeReadReserve && r.req.ready) {
    resValid := true.B
    resCommitted := false.B
    reserved := getTag(pipeReadAddr) ## getIndex(pipeReadAddr) ## 0.U(
      opts.OFFSET_WIDTH.W
    )
  }

  pendingRead := false.B

  // FIXME: change this one back?
  when(RegNext(toL2.l2req) =/= L2Req.idle) {
    // read port occupied
    r.req.ready := false.B
  }.elsewhen((!pipeRead) || hit || storeJustWritten) {
    r.req.ready := true.B
  }.otherwise {
    r.req.ready := false.B
    pendingRead := true.B
  }

  // handle offset in read addr
  r.data := rdata >> (pipeReadAddr(IGNORED_WIDTH - 1, 0) << 3)

  // L2 Handler
  toL2.l2stall := false.B
  val mainWriting = l1writing.asUInt().orR
  val lookupReady = !mainWriting && RegNext(toL2.l2stall && !mainWriting)
  // FIXME: impl this
  val resCancelDelay = RegInit(0.U(5.W)) // Up to 32

  val l2Rdata = MuxCase(
    0.U,
    lookups.map(line =>
      (
        line.valid && line.tag === getTag(toL2.l2addr),
        line.data.asUInt()
      )
    )
  )

  when(toL2.l2req =/= L2Req.idle) {
    // assert(toL2.l1stall) // L2req can only happen when l2 is processing other ports' requests
    // Nope that may happen. Suppose we are to victimize a line held by this L1

    assert(!toL2.l2addr(IGNORED_WIDTH - 1, 0).orR())

    queryAddr := toL2.l2addr
    toL2.l2stall := true.B

    when(lookupReady) { // To generate only two rw ports
      // For flushes, hit is asserted
      // For invals, wdata is ignored
      // So we should be safe to just use l2Rdata here without checking
      toL2.l1data := l2Rdata

      val written = Wire(new DLine(opts))
      written.data := l2Rdata.asTypeOf(written.data)
      written.tag := getTag(toL2.l2addr)
      written.dirty := false.B
      written.valid := toL2.l2req =/= L2Req.invalidate

      // TODO: assert hit for flush,
      // Assert !dirty for inval

      /** Although the next read/write may not fetch the updated value, this
        * doesn't violate the memory model, because the memory operations on the
        * same core are still compatible with the program order. Only
        * load/stores from another core is affected, and writes from this core
        * cannot propagate into other cores within one cycle.
        */
      toL2.l2stall := false.B
      val hitmask = VecInit(
        lookups.map(lookup =>
          lookup.valid && lookup.tag === getTag(toL2.l2addr)
        )
      )
      writing := hitmask
      writingData := written
      writingAddr := getIndex(toL2.l2addr)

      when(toL2.l2req === L2Req.invalidate && toL2.l2addr === reserved) {
        resValid := false.B
      }
    }
  }
}
