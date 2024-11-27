package meowv64.system

import meowv64.debug.DebugModuleMapping
import meowv64.interrupt.CLINTMapping
import meowv64.interrupt.PLICDef
import meowv64.interrupt.PLICMapping

abstract class SystemDef(val coreCount: Int) {
  outer =>
  val CORE_COUNT = coreCount

  val INIT_VEC = BigInt(0x80000000L)

  val PADDR_WIDTH: Int = 56
  val XLEN: Int = 64
  val VLEN: Int = 256

  val CYCLE_PER_TIMEUNIT: Int = 50 // We're running on 50M

  val L2_LINE_BYTES: Int = 32 // In bytes

  val INTERRUPT_CNT: Int = 15

  object PLIC
      extends {
        override val CONTEXT_COUNT: Int = outer.CORE_COUNT * 2
        override val MAX_PRIORITY: Int = 7
        override val MAX_SOURCE: Int = outer.INTERRUPT_CNT
      }
      with PLICDef
}

class DefaultSystemDef extends SystemDef(coreCount = 1)

class SingleCoreSystemDef extends SystemDef(coreCount = 1)
class DualCoreSystemDef extends SystemDef(coreCount = 2)
class HexaCoreSystemDef extends SystemDef(coreCount = 6)
class DecaCoreSystemDef extends SystemDef(coreCount = 10)
