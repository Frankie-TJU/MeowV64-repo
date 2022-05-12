package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import meowv64.system.SingleCoreSystemDef
import meowv64.system.SystemDef

/** Create multiple copies of a MeowV64 tile (and thus a core). Override with
  * the default mixins to control all params of the tiles.
  */
class WithNMeowV64Cores(
    systemDef: SystemDef = new SingleCoreSystemDef,
    overrideIdOffset: Option[Int] = None
) extends Config((_, _, up) => {
      case TilesLocated(InSubsystem) => {
        val prev = up(TilesLocated(InSubsystem))
        val idOffset = overrideIdOffset.getOrElse(prev.size)
        (0 until systemDef.CORE_COUNT).map { i =>
          MeowV64TileAttachParams(
            tileParams = MeowV64TileParams(hartId = i + idOffset),
            crossingParams = RocketCrossingParams()
          )
        } ++ prev
      }
      case SystemBusKey =>
        up(SystemBusKey).copy(beatBytes = systemDef.L2_LINE_BYTES)
      case XLen => 64
    })
