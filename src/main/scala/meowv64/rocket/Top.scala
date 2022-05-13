package meowv64.rocket

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.dataview._
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.devices.debug.JtagDTMKey
import freechips.rocketchip.devices.tilelink.BootROM
import freechips.rocketchip.devices.tilelink.BootROMLocated
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import meowv64.core.CoreDebug
import freechips.rocketchip.diplomacy.BundleBridgeNexusNode

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
  val debug = IO(target.debugIO.cloneType)
  debug := target.debugIO

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

  // expose debug
  val traceNexus = BundleBridgeNexusNode[CoreDebug]()
  val tileTraceNodes = tiles
    .flatMap { case tile: MeowV64Tile =>
      Some(tile)
    }
    .map { _.customDebugNode }

  tileTraceNodes.foreach { traceNexus := _ }
}

class RocketTopModule(outer: RocketTop)
    extends RocketSubsystemModuleImp(outer)
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with DontTouch {
  lazy val mem_axi4 = outer.mem_axi4
  lazy val mmio_axi4 = outer.mmio_axi4

  // wire custom debug signals
  val customDebug = outer.traceNexus.in.map(_._1)
  val debugIO = IO(
    Output(
      Vec(customDebug.length, customDebug(0).cloneType)
    )
  )
  for (i <- 0 until customDebug.length) {
    debugIO(i) := customDebug(i)
  }
}
