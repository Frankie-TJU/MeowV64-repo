package meowv64.rocket

import chisel3._
import chisel3.experimental.dataview.DataView
import chisel3.experimental.dataview._
import freechips.rocketchip.amba.axi4.AXI4Bundle
import freechips.rocketchip.amba.axi4.AXI4BundleParameters

// https://www.chisel-lang.org/chisel3/docs/explanations/dataview.html
// use standard names
class StandardAXI4Bundle(
    val addrBits: Int,
    val dataBits: Int,
    val idBits: Int
) extends Bundle {
  val AWREADY = Input(Bool())
  val AWVALID = Output(Bool())
  val AWID = Output(UInt(idBits.W))
  val AWADDR = Output(UInt(addrBits.W))
  val AWLEN = Output(UInt(8.W))
  val AWSIZE = Output(UInt(3.W))
  val AWBURST = Output(UInt(2.W))
  val AWLOCK = Output(UInt(1.W))
  val AWCACHE = Output(UInt(4.W))
  val AWPROT = Output(UInt(3.W))
  val AWQOS = Output(UInt(4.W))

  val WREADY = Input(Bool())
  val WVALID = Output(Bool())
  val WDATA = Output(UInt(dataBits.W))
  val WSTRB = Output(UInt((dataBits / 8).W))
  val WLAST = Output(Bool())

  val BREADY = Output(Bool())
  val BVALID = Input(Bool())
  val BID = Input(UInt(idBits.W))
  val BRESP = Input(UInt(2.W))

  val ARREADY = Input(Bool())
  val ARVALID = Output(Bool())
  val ARID = Output(UInt(idBits.W))
  val ARADDR = Output(UInt(addrBits.W))
  val ARLEN = Output(UInt(8.W))
  val ARSIZE = Output(UInt(3.W))
  val ARBURST = Output(UInt(2.W))
  val ARLOCK = Output(UInt(1.W))
  val ARCACHE = Output(UInt(4.W))
  val ARPROT = Output(UInt(3.W))
  val ARQOS = Output(UInt(4.W))

  val RREADY = Output(Bool())
  val RVALID = Input(Bool())
  val RID = Input(UInt(idBits.W))
  val RDATA = Input(UInt(dataBits.W))
  val RRESP = Input(UInt(2.W))
  val RLAST = Input(Bool())
}

object StandardAXI4Bundle {
  implicit val axiView = DataView[StandardAXI4Bundle, AXI4Bundle](
    vab =>
      new AXI4Bundle(
        AXI4BundleParameters(vab.addrBits, vab.dataBits, vab.idBits)
      ),
    // AW
    _.AWREADY -> _.aw.ready,
    _.AWVALID -> _.aw.valid,
    _.AWID -> _.aw.bits.id,
    _.AWADDR -> _.aw.bits.addr,
    _.AWLEN -> _.aw.bits.len,
    _.AWSIZE -> _.aw.bits.size,
    _.AWBURST -> _.aw.bits.burst,
    _.AWLOCK -> _.aw.bits.lock,
    _.AWCACHE -> _.aw.bits.cache,
    _.AWPROT -> _.aw.bits.prot,
    _.AWQOS -> _.aw.bits.qos,
    // W
    _.WREADY -> _.w.ready,
    _.WVALID -> _.w.valid,
    _.WDATA -> _.w.bits.data,
    _.WSTRB -> _.w.bits.strb,
    _.WLAST -> _.w.bits.last,
    // B
    _.BREADY -> _.b.ready,
    _.BVALID -> _.b.valid,
    _.BID -> _.b.bits.id,
    _.BRESP -> _.b.bits.resp,
    // AR
    _.ARREADY -> _.ar.ready,
    _.ARVALID -> _.ar.valid,
    _.ARID -> _.ar.bits.id,
    _.ARADDR -> _.ar.bits.addr,
    _.ARLEN -> _.ar.bits.len,
    _.ARSIZE -> _.ar.bits.size,
    _.ARBURST -> _.ar.bits.burst,
    _.ARLOCK -> _.ar.bits.lock,
    _.ARCACHE -> _.ar.bits.cache,
    _.ARPROT -> _.ar.bits.prot,
    _.ARQOS -> _.ar.bits.qos,
    // R
    _.RREADY -> _.r.ready,
    _.RVALID -> _.r.valid,
    _.RID -> _.r.bits.id,
    _.RDATA -> _.r.bits.data,
    _.RRESP -> _.r.bits.resp,
    _.RLAST -> _.r.bits.last
  )
  implicit val axiView2 = StandardAXI4Bundle.axiView.invert(ab =>
    new StandardAXI4Bundle(
      ab.params.addrBits,
      ab.params.dataBits,
      ab.params.idBits
    )
  )
}
