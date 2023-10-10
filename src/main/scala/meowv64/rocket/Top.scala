package meowv64.rocket

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.dataview._
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.devices.tilelink.BootROM
import freechips.rocketchip.devices.tilelink.BootROMLocated
import freechips.rocketchip.diplomacy.BundleBridgeNexusNode
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import meowv64.core.CoreDebug
import freechips.rocketchip.amba.axi4.AXI4SlaveNode
import freechips.rocketchip.diplomacy.SimpleBus
import freechips.rocketchip.amba.axi4.AXI4SlavePortParameters
import freechips.rocketchip.amba.axi4.AXI4SlaveParameters
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.TransferSizes
import freechips.rocketchip.amba.axi4.AXI4Buffer
import freechips.rocketchip.amba.axi4.AXI4UserYanker
import freechips.rocketchip.amba.axi4.AXI4Deinterleaver
import freechips.rocketchip.amba.axi4.AXI4IdIndexer
import freechips.rocketchip.tilelink.TLToAXI4
import freechips.rocketchip.tilelink.TLWidthWidget
import freechips.rocketchip.diplomacy.InModuleBody
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy.MemoryDevice
import freechips.rocketchip.diplomacy.RegionType

class RiscVSystem(implicit val p: Parameters) extends Module {
  val target = Module(LazyModule(new RocketTop).module)

  require(target.mem_axi4.size == 1)
  require(target.mmio_axi4.size == 1)

  val interrupts = IO(Input(UInt(p(NExtTopInterrupts).W)))
  assert(target.mem_axi4.head.params.addrBits == 33)
  val mem_axi4 = IO(
    new StandardAXI4Bundle(
      32, // force 32 bits addr
      target.mem_axi4.head.params.dataBits,
      target.mem_axi4.head.params.idBits
    )
  )
  assert(target.mmio_axi4.head.params.addrBits == 34)
  val mmio_axi4 = IO(
    new StandardAXI4Bundle(
      32, // force 32 bits addr
      target.mmio_axi4.head.params.dataBits,
      target.mmio_axi4.head.params.idBits
    )
  )
  val jtag = IO(Flipped(new JTAGIO))

  // expose custom debug interface if any
  target.customDebug match {
    case Some(customDebug) => {
      val debug = IO(customDebug.cloneType)
      debug := customDebug
    }
    case None =>
  }

  // ndreset can reset all harts
  val childReset = reset.asBool | target.debug.map(_.ndreset).getOrElse(false.B)
  target.reset := childReset

  // setup jtag
  val systemJtag = target.debug.get.systemjtag.get
  systemJtag.jtag.TCK := jtag.TCK
  systemJtag.jtag.TMS := jtag.TMS
  systemJtag.jtag.TDI := jtag.TDI
  jtag.TDO := systemJtag.jtag.TDO
  //systemJtag.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
  //systemJtag.part_number := p(JtagDTMKey).idcodePartNum.U(16.W)
  //systemJtag.version := p(JtagDTMKey).idcodeVersion.U(4.W)
  // custom idcode
  systemJtag.mfr_id := 0.U
  systemJtag.part_number := 0x2222.U
  systemJtag.version := 1.U
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

  if (p(FlipMSBInAXI)) {
    // if requested,
    // toggle msb and truncate to 32 bits for both mem and mmio
    // [0x80000000, 0x100000000] -> [0x00000000, 0x800000000]
    // [0x100000000, 0x180000000] -> [0x80000000, 0x100000000]
    val mask = BigInt("80000000", 16).U
    mem_axi4.ARADDR := target.mem_axi4.head.ar.bits.addr ^ mask
    mem_axi4.AWADDR := target.mem_axi4.head.aw.bits.addr ^ mask
    mmio_axi4.ARADDR := target.mmio_axi4.head.ar.bits.addr ^ mask
    mmio_axi4.AWADDR := target.mmio_axi4.head.aw.bits.addr ^ mask
  }

  if (target.slave_axi4.size > 0) {
    require(target.slave_axi4.size == 1)
    val slave_axi4 = IO(
      Flipped(
        new StandardAXI4Bundle(
          32, // force 32 bits addr
          target.slave_axi4.head.params.dataBits,
          target.slave_axi4.head.params.idBits
        )
      )
    )
    slave_axi4 <> target.slave_axi4.head.viewAs[StandardAXI4Bundle]
  }

  target.interrupts := interrupts

  target.dontTouchPorts()
}

class RocketTop(implicit p: Parameters)
    extends RocketSubsystem
    with HasAsyncExtInterrupts
    with CanHaveCustomMasterAXI4MemPort
    with CanHaveCustomMasterAXI4MMIOPort
    with CanHaveSlaveAXI4Port {
  override lazy val module = new RocketTopModule(this)

  // from freechips.rocketchip.system.ExampleRocketSystem
  val bootROM = p(BootROMLocated(location)).map {
    BootROM.attach(_, this, CBUS)
  }

  // expose debug
  val customDebugNexus = BundleBridgeNexusNode[CoreDebug]()
  val tileCustomDebugNodes = tiles
    .flatMap {
      case tile: MeowV64Tile =>
        Some(tile)
      case _ => None
    }
    .map { _.customDebugNode }

  tileCustomDebugNodes.foreach { customDebugNexus := _ }
}

class RocketTopModule(outer: RocketTop)
    extends RocketSubsystemModuleImp(outer)
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with DontTouch {
  lazy val mem_axi4 = outer.mem_axi4
  lazy val mmio_axi4 = outer.mmio_axi4
  lazy val slave_axi4 = outer.l2_frontend_bus_axi4

  // wire custom debug signals
  val customDebugIO = outer.customDebugNexus.in.map(_._1)
  val customDebug = if (customDebugIO.length > 0) {
    val customDebug = IO(
      Output(
        Vec(customDebugIO.length, customDebugIO(0).cloneType)
      )
    )

    for (i <- 0 until customDebug.length) {
      customDebug(i) := customDebugIO(i)
    }

    Some(customDebug)
  } else {
    None
  }

}

// Customize CanHaveMasterAXI4MMIOPort to allow multiple address ranges
case object CustomExtBus extends config.Field[Seq[MasterPortParams]](Seq())

/** Adds a AXI4 port to the system intended to master an MMIO device bus */
trait CanHaveCustomMasterAXI4MMIOPort { this: BaseSubsystem =>
  private val mmioPortParams = p(CustomExtBus)
  private val mmioPortParam = mmioPortParams(0)
  private val portName = "mmio_port_axi4"
  private val device = new SimpleBus(portName.kebab, Nil)

  // validate
  for (param <- mmioPortParams) {
    assert(param.beatBytes == mmioPortParam.beatBytes)
    assert(param.idBits == mmioPortParam.idBits)
    assert(param.maxXferBytes == mmioPortParam.maxXferBytes)
    assert(param.executable == mmioPortParam.executable)
  }

  val mmioAXI4Node = AXI4SlaveNode(
    Seq(
      AXI4SlavePortParameters(
        slaves = mmioPortParams
          .map(params =>
            AXI4SlaveParameters(
              address = AddressSet.misaligned(params.base, params.size),
              resources = device.ranges,
              executable = params.executable,
              supportsWrite = TransferSizes(1, params.maxXferBytes),
              supportsRead = TransferSizes(1, params.maxXferBytes)
            )
          )
          .toSeq,
        beatBytes = mmioPortParam.beatBytes
      )
    )
  )

  sbus.coupleTo(s"port_named_$portName") {
    (mmioAXI4Node
      := AXI4Buffer()
      := AXI4UserYanker()
      := AXI4Deinterleaver(sbus.blockBytes)
      := AXI4IdIndexer(mmioPortParam.idBits)
      := TLToAXI4()
      := TLWidthWidget(sbus.beatBytes)
      := _)
  }

  val mmio_axi4 = InModuleBody { mmioAXI4Node.makeIOs() }
}

// Flip MSB of MEM/MMIO axi4 ports
case object FlipMSBInAXI extends config.Field[Boolean](false)

// Size of Buffets per Core
case object BuffetsSizePerCore extends config.Field[BigInt](0x400L)

// Customize CanHaveMasterAXI4MemPort to allow multiple address ranges
case object CustomExtMem extends config.Field[Seq[MasterPortParams]](Seq())

/** Adds a port to the system intended to master an AXI4 DRAM controller. */
trait CanHaveCustomMasterAXI4MemPort { this: BaseSubsystem =>
  private val memPortParams = p(CustomExtMem)
  private val memPortParam = memPortParams(0)
  private val portName = "axi4"
  private val device = new MemoryDevice
  private val idBits = memPortParam.idBits

  // validate
  for (param <- memPortParams) {
    assert(param.beatBytes == memPortParam.beatBytes)
    assert(param.idBits == memPortParam.idBits)
    assert(param.maxXferBytes == memPortParam.maxXferBytes)
    assert(param.executable == memPortParam.executable)
  }

  val memAXI4Node = AXI4SlaveNode(
    Seq(
      AXI4SlavePortParameters(
        slaves = memPortParams
          .map(params =>
            AXI4SlaveParameters(
              address = AddressSet.misaligned(params.base, params.size),
              resources = device.reg,
              regionType = RegionType.UNCACHED, // cacheable
              executable = true,
              supportsWrite = TransferSizes(1, mbus.blockBytes),
              supportsRead = TransferSizes(1, mbus.blockBytes),
              interleavedId = Some(0)
            )
          )
          .toSeq, // slave does not interleave read responses
        beatBytes = memPortParam.beatBytes
      )
    )
  )

  mbus.coupleTo(s"memory_controller_port_named_$portName") {
    (memAXI4Node
      :*= AXI4UserYanker()
      :*= AXI4IdIndexer(idBits)
      :*= TLToAXI4()
      :*= TLWidthWidget(mbus.beatBytes)
      :*= _)
  }

  val mem_axi4 = InModuleBody { memAXI4Node.makeIOs() }
}
