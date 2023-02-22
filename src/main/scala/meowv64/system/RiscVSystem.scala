package meowv64.system

import chisel3._
import chisel3.util.RRArbiter
import meowv64.cache.L2Cache
import meowv64.core.Core
import meowv64.core.CoreDebug
import meowv64.core.CoreDef
import meowv64.data._
import meowv64.debug.DebugModule
import meowv64.debug.Jtag
import meowv64.debug.JtagRiscvTap
import meowv64.interrupt.CLINT
import meowv64.interrupt.PLIC

class RiscVSystem(implicit val sDef: SystemDef = new DefaultSystemDef)
    extends Module {
  // L1 & L2 has same LINE_BYTES
  val coreConfigs = (0 until sDef.CORE_COUNT).map(id =>
    CoreDef.default(id, sDef.INIT_VEC, sDef.L2_LINE_BYTES)
  )

  val io = IO(new Bundle {
    val axi = new AXI(sDef.L2.AXI_DATA_WIDTH, sDef.PADDR_WIDTH)
    val eints = Input(Vec(sDef.INTERRUPT_CNT + 1, Bool()))

    val jtag = new Jtag()

    // Debug infos
    val debug = Output(
      Vec(sDef.CORE_COUNT, new CoreDebug()(coreConfigs(0)))
    ) // HART_ID doesn't matter here
  })

  val cores =
    (0 until sDef.CORE_COUNT).map(id => Module(new Core()(coreConfigs(id))))

  val l2 = Module(new L2Cache(sDef.L2))
  val clint = Module(new CLINT)
  val plic = Module(new PLIC(sDef.PLIC))

  l2.axi <> io.axi
  l2.mmio(0) <> clint.toL2
  l2.mmio(1) <> plic.toL2
  for (idx <- (0 until sDef.CORE_COUNT)) {
    l2.ic(idx) <> cores(idx).io.frontend.ic
    l2.dc(idx) <> cores(idx).io.frontend.dc
    l2.directs(idx) <> cores(idx).io.frontend.uc
  }

  for (idx <- (0 until sDef.CORE_COUNT)) {
    cores(idx).io.int.msip := clint.ints(idx).msip
    cores(idx).io.int.mtip := clint.ints(idx).mtip
  }

  plic.source := io.eints.asUInt()
  for (idx <- (0 until sDef.CORE_COUNT)) {
    cores(idx).io.int.meip := plic.eints(idx * 2)
    cores(idx).io.int.seip := plic.eints(idx * 2 + 1)
  }

  // TAPs used as a DTM must have an IR of at least 5 bits.
  val jtagRiscvTap = Module(new JtagRiscvTap(5))
  jtagRiscvTap.io.jtag.tck := io.jtag.tck
  jtagRiscvTap.io.jtag.tms := io.jtag.tms
  jtagRiscvTap.io.jtag.trstn := io.jtag.trstn
  jtagRiscvTap.io.jtag.tdi := io.jtag.tdi
  io.jtag.tdo := jtagRiscvTap.io.jtag.tdo

  // Debug Module
  val dm = Module(new DebugModule)
  dm.io.dmi <> jtagRiscvTap.dmi
  for (idx <- (0 until sDef.CORE_COUNT)) {
    cores(idx).io.dm <> dm.io.core(idx)
  }
  l2.mmio(2) <> dm.io.toL2

  // access uncached code from debug module
  val arbiter = Module(new RRArbiter(UInt(sDef.PADDR_WIDTH.W), sDef.CORE_COUNT))
  for (idx <- (0 until sDef.CORE_COUNT)) {
    cores(idx).io.frontend.ui.stall := ~arbiter.io.in(idx).ready
    arbiter.io.in(idx).valid := cores(idx).io.frontend.ui.read.valid
    arbiter.io.in(idx).bits := cores(idx).io.frontend.ui.read.bits

    cores(idx).io.frontend.ui.data := dm.io.toL1I.data
  }
  dm.io.toL1I.read.valid := arbiter.io.out.valid
  dm.io.toL1I.read.bits := arbiter.io.out.bits
  arbiter.io.out.ready := ~dm.io.toL1I.stall

  io.debug := cores.map(_.io.debug)
}
