package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import meowv64.system.SingleCoreSystemDef
import meowv64.system.SystemDef
import meowv64.core.CoreDef

/** Create multiple copies of a MeowV64 tile (and thus a core). Override with
  * the default mixins to control all params of the tiles.
  */
class WithMeowV64Cores(
    systemDef: SystemDef = new SingleCoreSystemDef,
    overrideIdOffset: Option[Int] = None
) extends Config((_, _, up) => {
      case CacheBlockBytes => systemDef.L2_LINE_BYTES
      case MemoryBusKey => up(MemoryBusKey).copy(beatBytes = systemDef.L2_LINE_BYTES)
      case TilesLocated(InSubsystem) => {
        val prev = up(TilesLocated(InSubsystem))
        val idOffset = overrideIdOffset.getOrElse(prev.size)
        (0 until systemDef.CORE_COUNT).map { i =>
          MeowV64TileAttachParams(
            tileParams = MeowV64TileParams(
              coredef = CoreDef
                .default(
                  id = i + idOffset,
                  systemDef.INIT_VEC,
                  systemDef.L2_LINE_BYTES
                )
            ),
            crossingParams = RocketCrossingParams()
          )
        } ++ prev
      }
      case SystemBusKey =>
        up(SystemBusKey).copy(beatBytes = systemDef.L2_LINE_BYTES)
      case XLen => 64
    })
