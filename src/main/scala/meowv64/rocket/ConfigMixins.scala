package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.diplomacy.DTSTimebase
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
    frequency: BigInt = 1000000000
) extends Config((site, _, up) => {
      case XLen => 64
      // 100MHz
      case PeripheryBusKey =>
        up(PeripheryBusKey).copy(dtsFrequency = Some(frequency))
      case DTSTimebase => frequency
      // Set to line bytes
      case CacheBlockBytes => systemDef.L2_LINE_BYTES
      case MemoryBusKey =>
        up(MemoryBusKey).copy(beatBytes = 16)
      case SystemBusKey =>
        up(SystemBusKey).copy(beatBytes = 16)
      // MMIO 64 bits
      case ExtBus =>
        Some(
          MasterPortParams(
            base = BigInt("60000000", 16),
            size = BigInt("20000000", 16),
            beatBytes = 8,
            idBits = 4
          )
        )
      // Mem
      case ExtMem =>
        Some(
          MemoryPortParams(
            MasterPortParams(
              base = BigInt("80000000", 16),
              size = BigInt("80000000", 16),
              beatBytes = site(MemoryBusKey).beatBytes,
              idBits = 4
            ),
            1
          )
        )
      // Tiles
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
    })
