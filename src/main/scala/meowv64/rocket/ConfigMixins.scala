package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import meowv64.core.CoreDef
import meowv64.system.SingleCoreSystemDef
import meowv64.system.SystemDef

/** Create multiple copies of a MeowV64 tile (and thus a core). Override with
  * the default mixins to control all params of the tiles.
  */
class WithMeowV64Cores(
    systemDef: SystemDef = new SingleCoreSystemDef,
    overrideIdOffset: Option[Int] = None,
    initVec: Option[BigInt] = None,
    enableDifftest: Boolean = false
) extends Config((_, _, up) => {
      case XLen => 64
      // Set to line bytes
      case CacheBlockBytes => systemDef.L2_LINE_BYTES
      case MemoryBusKey =>
        up(MemoryBusKey).copy(beatBytes = 16)
      case SystemBusKey =>
        up(SystemBusKey).copy(beatBytes = 16)
      // Tiles
      case TilesLocated(InSubsystem) => {
        val prev = up(TilesLocated(InSubsystem))
        val idOffset = overrideIdOffset.getOrElse(prev.size)
        (0 until systemDef.CORE_COUNT).map { i =>
          MeowV64TileAttachParams(
            tileParams = MeowV64TileParams(
              coredef = CoreDef
                .default(
                  initVec = initVec.getOrElse(systemDef.INIT_VEC),
                  cacheLineBytes = systemDef.L2_LINE_BYTES,
                  inRocketSystem = true,
                  enableDifftest = enableDifftest
                ),
              hartId = i + idOffset,
            ),
            crossingParams = RocketCrossingParams()
          )
        } ++ prev
      }
    })

class FlipMSB(
) extends Config((_, _, _) => { case FlipMSBInAXI =>
      true
    })
