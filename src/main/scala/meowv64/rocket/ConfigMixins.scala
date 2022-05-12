package meowv64.rocket

import chisel3._
import chisel3.util.{log2Up}

import freechips.rocketchip.config.{Parameters, Config, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._

/** Create multiple copies of a MeowV64 tile (and thus a core). Override with
  * the default mixins to control all params of the tiles.
  *
  * @param n
  *   amount of tiles to duplicate
  */
class WithNMeowV64Cores(n: Int = 1, overrideIdOffset: Option[Int] = None)
    extends Config((_, _, up) => {
      case TilesLocated(InSubsystem) => {
        val prev = up(TilesLocated(InSubsystem))
        val idOffset = overrideIdOffset.getOrElse(prev.size)
        (0 until n).map { i =>
          MeowV64TileAttachParams(
            tileParams = MeowV64TileParams(hartId = i + idOffset),
            crossingParams = RocketCrossingParams()
          )
        } ++ prev
      }
      case SystemBusKey => up(SystemBusKey).copy(beatBytes = 32)
      case XLen         => 64
    })
