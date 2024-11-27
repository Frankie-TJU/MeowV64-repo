package meowv64.cache

import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil
import meowv64.exec.MuxBE

/** Computes atomic operations
  */
class AMOALU(val opts: L1DOpts) extends Module {
  val io = IO(new Bundle {
    val op = Input(DCWriteOp()) // write is treated as idle

    /** Data read from memory
      */
    val rdata = Input(UInt(opts.TO_CORE_TRANSFER_WIDTH.W))

    /** Data from register
      */
    val wdata = Input(UInt(opts.XLEN.W))

    val offset = Input(
      UInt(log2Ceil(opts.TO_CORE_TRANSFER_BYTES).W)
    ) // If op =/= write or idle, then length must be W or D
    val length = Input(DCWriteLen())

    // output register
    val rsliced = Output(
      UInt(opts.XLEN.W)
    ) // Only valid if length is W or D. This is for AMO

    /** Data to be written to memory
      */
    val muxed = Output(UInt(opts.TO_CORE_TRANSFER_WIDTH.W))
  })

  val shiftedRData = Wire(UInt(opts.XLEN.W))
  shiftedRData := io.rdata >> (io.offset << 3)

  /** Memory, sign extended to 64 bits
    */
  val rextended = Wire(SInt(opts.XLEN.W))
  rextended := shiftedRData.asSInt

  val rconverted = rextended.asUInt

  /** Return register: memory sign extended to 64 bits
    */
  io.rsliced := rconverted

  /** Memory, zero extended to 64 bits
    */
  val rraw = Wire(UInt(opts.XLEN.W))
  rraw := shiftedRData

  // clip wdata to 32 bits if necessary
  val wdataSigned = Wire(SInt(opts.XLEN.W))
  val wdataUnsigned = Wire(UInt(opts.XLEN.W))
  wdataUnsigned := io.wdata
  wdataSigned := io.wdata.asSInt

  when(io.length === DCWriteLen.W) {
    wdataUnsigned := io.wdata(31, 0)
    wdataSigned := io.wdata(31, 0).asSInt
    rextended := shiftedRData(31, 0).asSInt
    rraw := shiftedRData(31, 0)
  }

  /** Compute result
    */
  val filtered = Wire(UInt(opts.XLEN.W))
  filtered := 0.U
  switch(io.op) {
    is(DCWriteOp.write, DCWriteOp.cond, DCWriteOp.swap) {
      filtered := io.wdata
    }

    is(DCWriteOp.add) {
      filtered := rconverted + io.wdata
    }

    is(DCWriteOp.and) {
      filtered := rconverted & io.wdata
    }

    is(DCWriteOp.or) {
      filtered := rconverted | io.wdata
    }

    is(DCWriteOp.xor) {
      filtered := rconverted ^ io.wdata
    }

    is(DCWriteOp.max) {
      filtered := rextended.max(wdataSigned).asUInt
    }

    is(DCWriteOp.maxu) {
      filtered := rraw.max(wdataUnsigned)
    }

    is(DCWriteOp.min) {
      filtered := rextended.min(wdataSigned).asUInt
    }

    is(DCWriteOp.minu) {
      filtered := rraw.min(wdataUnsigned)
    }
  }

  val mask = DCWriteLen.toByteEnable(io.length)
  val shiftedMask = Wire(UInt((opts.TO_CORE_TRANSFER_BYTES).W))
  shiftedMask := mask << io.offset
  val shiftedFiltered = Wire(UInt(opts.TO_CORE_TRANSFER_WIDTH.W))
  shiftedFiltered := filtered << (io.offset << 3)

  io.muxed := MuxBE(shiftedMask, shiftedFiltered, io.rdata)
}
