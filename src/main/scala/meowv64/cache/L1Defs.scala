package meowv64.cache

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.Valid
import chisel3.util.log2Ceil

/** Cache definitions and interfaces
  *
  * Unless explicitly stated, size/width numbers are in bits
  */

/** Trait of L1 cache's external interface
  *
  * This is used in L2 to generate polymorphic interfaces L1 can never actively
  * stalls L2
  */
trait L1Port extends Bundle {
  // Address
  def getAddr: UInt
  // L1 -> L2 request
  def getReq: L1DCPort.L1Req.Type

  // L2 -> L1 stall
  def getStall: Bool
  // L2 -> L1 read data
  def getRdata: UInt
  // L1 -> L2 write data
  def getWdata: UInt
}

trait CacheOpts {

  /** Address width in bits */
  val ADDR_WIDTH: Int

  /** Line width in bytes
    */
  val LINE_BYTES: Int

  /** Cache size in bytes
    */
  val SIZE_BYTES: Int
  val ASSOC: Int

  // TODO: check is log2
  assume(SIZE_BYTES % LINE_BYTES == 0)

  // Compute widths for cache usage
  def LINE_PER_ASSOC = SIZE_BYTES / ASSOC / LINE_BYTES
  def INDEX_OFFSET_WIDTH = log2Ceil(SIZE_BYTES / ASSOC)
  def ASSOC_IDX_WIDTH = log2Ceil(ASSOC)

  // three parts: tag, index, offset
  def TAG_WIDTH = ADDR_WIDTH - INDEX_OFFSET_WIDTH
  def INDEX_WIDTH = log2Ceil(LINE_PER_ASSOC)
  def OFFSET_WIDTH = log2Ceil(LINE_BYTES)

  def getTag(addr: UInt) = addr(ADDR_WIDTH - 1, INDEX_OFFSET_WIDTH)
  def getIndex(addr: UInt) =
    addr(INDEX_OFFSET_WIDTH - 1, OFFSET_WIDTH)

  // sanity check
  assert(INDEX_WIDTH == log2Ceil(LINE_PER_ASSOC))
  assert(INDEX_OFFSET_WIDTH == INDEX_WIDTH + OFFSET_WIDTH)
}

trait L1Opts extends CacheOpts {
  // Word width
  val XLEN: Int

  /** Core <-> L1 transfer size in bits.
    */
  val TO_CORE_TRANSFER_WIDTH: Int

  def TO_CORE_TRANSFER_BYTES: Int = TO_CORE_TRANSFER_WIDTH / 8

  /** L1 <-> L2 transfer size in bits.
    */
  def TO_L2_TRANSFER_WIDTH: Int = LINE_BYTES * 8

  /** Line width in bits.
    */
  def LINE_WIDTH: Int = LINE_BYTES * 8

  /** LINE_WIDTH / CORE_DATA_WIDTH
    */
  def TRANSFER_COUNT: Int = LINE_WIDTH / TO_CORE_TRANSFER_WIDTH

  // check
  if (TO_CORE_TRANSFER_WIDTH != 0) {
    assert(LINE_WIDTH % TO_CORE_TRANSFER_WIDTH == 0 && TRANSFER_COUNT >= 1)
  }
}

trait L1DOpts extends L1Opts {
  // Write buffer depth in L1DC
  val WRITE_BUF_DEPTH: Int
}

/** I$ -> L2
  *
  * I$ doesn't enforce cache coherence restrictions, so we don't have coherence
  * protocol-related wires. Also, I$ doesn't have write channel, so we don't
  * have uplink data wires
  */
class L1ICPort(val opts: L1Opts) extends Bundle with L1Port {
  val read = Valid(UInt(opts.ADDR_WIDTH.W))
  val stall = Input(Bool())
  val data = Input(UInt((opts.TO_L2_TRANSFER_WIDTH).W))

  override def getAddr: UInt = read.bits
  override def getReq = {
    val result = Wire(L1DCPort.L1Req())
    when(read.valid) {
      result := L1DCPort.L1Req.read
    }.otherwise {
      result := L1DCPort.L1Req.idle
    }

    result
  }

  override def getStall: Bool = stall
  override def getRdata = data
  override def getWdata = {
    val ret = UInt()
    ret <> DontCare
    ret
  }
}

object L1ICPort {
  def empty(opts: L1Opts): L1ICPort = {
    val port = Wire(Flipped(new L1ICPort(opts)))
    port := DontCare
    port.read.valid := false.B

    port
  }
}

/** Uncached access
  */
class L1UCPort(val opts: L1Opts) extends Bundle {
  val read = Output(Bool())
  val write = Output(Bool())
  val addr = Output(UInt(opts.ADDR_WIDTH.W))
  val len = Output(DCWriteLen())
  val wdata = Output(UInt(opts.XLEN.W))
  val stall = Input(Bool())

  val rdata = Input(UInt(opts.XLEN.W))
}

object L1UCPort {
  def empty(opts: L1Opts): L1UCPort = {
    val port = Wire(Flipped(new L1UCPort(opts)))
    port := DontCare
    port.read := false.B
    port.write := false.B

    port
  }
}

/** D$ -> L2
  *
  * We define L2 as the master device, so L1 -> L2 is uplink, and vice-versa
  *
  * Downlink requests always have higher precedence than uplink requests.
  * However, if a response of an uplink request is going on, it's guaranteed to
  * not be interrupted
  *
  * In reality, L2 cache operates in an serial manner. No concurrent request may
  * be processed at the same time, hence the guarantee kept
  *
  * The only exceptions is a read with L2 miss. If that's the case, then no
  * other cache should have the same line, so no additional requests sent to
  * other caches.
  *
  * Write with L2 miss is an no-op: L1 should enforce the write-allocate policy.
  * A read must be issued if the written line is missed L2 should enforce that
  * all valid lines in L1 is also valid in L2
  */
class L1DCPort(val opts: L1Opts) extends Bundle with L1Port {
  // L1 -> L2 request
  val l1req = Output(L1DCPort.L1Req())
  val l1addr = Output(UInt(opts.ADDR_WIDTH.W))
  val l1stall = Input(Bool())
  val l1data = Output(UInt((opts.TO_L2_TRANSFER_WIDTH).W))

  // L1 <- L2 request
  val l2req = Input(L1DCPort.L2Req())
  val l2addr = Input(UInt(opts.ADDR_WIDTH.W))
  val l2stall = Output(Bool())
  val l2data = Input(UInt((opts.TO_L2_TRANSFER_WIDTH).W))
  // TODO: add a debug signal to show if L1 really has the entry

  override def getAddr: UInt = l1addr
  override def getReq = l1req
  override def getStall: Bool = l1stall
  override def getRdata: UInt = l2data
  override def getWdata: UInt = l1data
}

object L1DCPort {

  /** Uplink (L1 -> L2) requests
    *
    *   - read: request to read one cache line
    *   - modify: request to invalidate all other out-standing cache duplicates,
    *     and write one cache line
    *   - writeback: request to writeback a line (dirty -> non-dirty)
    */
  object L1Req extends ChiselEnum {
    // TODO: do we include inval here? is it worth it?
    val idle, read, modify, writeback = Value
  }

  /** Downlink (L2 -> L1) requests
    *
    *   - flush: request to write-back one cache line. This should generate a
    *     writeback event, overriding the pending event on the port
    *   - invalidate: request to invalidate one cache line. If the invalidated
    *     cache line is also a target of a pending write in write queue,
    *     especially the head of the write queue, L1 should fetch
    *     (write-allocate) the line again before sending an modify request
    */
  object L2Req extends ChiselEnum {
    val idle, flush, invalidate = Value
  }

  def empty(opts: L1Opts): L1DCPort = {
    val port = Wire(Flipped(new L1DCPort(opts)))
    port := DontCare
    port.l1addr := 0.U
    port.l1req := L1Req.idle
    port.l2stall := false.B

    port
  }
}
