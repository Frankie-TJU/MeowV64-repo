package meowv64.rocket

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

class RocketTop(implicit p: Parameters)
    extends RocketSubsystem
    with HasAsyncExtInterrupts
    with CanHaveMasterAXI4MemPort
    with CanHaveMasterAXI4MMIOPort {
  override lazy val module = new RocketTopModule(this)
}

class RocketTopModule(outer: RocketTop)
    extends RocketSubsystemModuleImp(outer)
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with DontTouch {
  lazy val mem_axi4 = outer.mem_axi4
  lazy val mmio_axi4 = outer.mmio_axi4
}
