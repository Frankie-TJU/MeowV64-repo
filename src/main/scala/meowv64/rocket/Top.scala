package meowv64.rocket

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.dataview.DataView
import chisel3.experimental.dataview._
import freechips.rocketchip.amba.axi4.AXI4Bundle
import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.devices.debug.JtagDTMKey
import freechips.rocketchip.devices.tilelink.BootROM
import freechips.rocketchip.devices.tilelink.BootROMLocated
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

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

class RiscVSystem(implicit val p: Parameters) extends Module {
  val target = Module(LazyModule(new RocketTop).module)

  require(target.mem_axi4.size == 1)
  require(target.mmio_axi4.size == 1)

  val interrupts = IO(Input(UInt(p(NExtTopInterrupts).W)))
  val mem_axi4 = IO(
    new StandardAXI4Bundle(
      target.mem_axi4.head.params.addrBits,
      target.mem_axi4.head.params.dataBits,
      target.mem_axi4.head.params.idBits
    )
  )
  val mmio_axi4 = IO(
    new StandardAXI4Bundle(
      target.mem_axi4.head.params.addrBits,
      target.mem_axi4.head.params.dataBits,
      target.mem_axi4.head.params.idBits
    )
  )
  val jtag = IO(Flipped(new JTAGIO))

  // ndreset can reset all harts
  val childReset = reset.asBool | target.debug.map(_.ndreset).getOrElse(false.B)
  target.reset := childReset

  // setup jtag
  val systemJtag = target.debug.get.systemjtag.get
  systemJtag.jtag.TCK := jtag.TCK
  systemJtag.jtag.TMS := jtag.TMS
  systemJtag.jtag.TDI := jtag.TDI
  jtag.TDO := systemJtag.jtag.TDO
  systemJtag.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
  systemJtag.part_number := p(JtagDTMKey).idcodePartNum.U(16.W)
  systemJtag.version := p(JtagDTMKey).idcodeVersion.U(4.W)
  // MUST use async reset here
  // otherwise the internal logic(e.g. TLXbar) might not function
  // if reset deasserted before TCK rises
  systemJtag.reset := reset.asAsyncReset
  target.resetctrl.foreach { rc =>
    rc.hartIsInReset.foreach { _ := childReset }
  }

  Debug.connectDebugClockAndReset(target.debug, clock)

  mem_axi4 <> target.mem_axi4.head.viewAs[StandardAXI4Bundle]
  mmio_axi4 <> target.mmio_axi4.head.viewAs[StandardAXI4Bundle]

  target.interrupts := interrupts

  target.dontTouchPorts()
}

class RocketTop(implicit p: Parameters)
    extends RocketSubsystem
    with HasAsyncExtInterrupts
    with CanHaveMasterAXI4MemPort
    with CanHaveMasterAXI4MMIOPort {
  override lazy val module = new RocketTopModule(this)

  // from freechips.rocketchip.system.ExampleRocketSystem
  val bootROM = p(BootROMLocated(location)).map {
    BootROM.attach(_, this, CBUS)
  }
}

class RocketTopModule(outer: RocketTop)
    extends RocketSubsystemModuleImp(outer)
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with DontTouch {
  lazy val mem_axi4 = outer.mem_axi4
  lazy val mmio_axi4 = outer.mmio_axi4
}
