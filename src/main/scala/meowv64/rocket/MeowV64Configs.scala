package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem.RocketTilesKey
import freechips.rocketchip.subsystem.WithCacheBlockBytes
import freechips.rocketchip.subsystem.WithCoherentBusTopology
import freechips.rocketchip.subsystem.WithInclusiveCache
import freechips.rocketchip.subsystem.WithIncoherentBusTopology
import freechips.rocketchip.subsystem.WithIncoherentTiles
import freechips.rocketchip.subsystem.WithJtagDTM
import freechips.rocketchip.subsystem.WithNBigCores
import freechips.rocketchip.subsystem.WithNoSlavePort
import freechips.rocketchip.system.BaseConfig
import meowv64.system.DualCoreSystemDef
import meowv64.system.HexaCoreSystemDef
import meowv64.system.SingleCoreSystemDef

class MeowV64BaseConfig
    extends Config(
      new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithInclusiveCache ++
        new WithCoherentBusTopology ++
        new BaseConfig ++
        new WithCacheBlockBytes(32)
    )

class MeowV64SingleCoreConfig
    extends Config(
      new WithMeowV64Cores(new SingleCoreSystemDef) ++
        new MeowV64BaseConfig
    )

class MeowV64DualCoreConfig
    extends Config(
      new WithMeowV64Cores(new DualCoreSystemDef) ++
        new MeowV64BaseConfig
    )

class MeowV64HexaCoreConfig
    extends Config(
      new WithMeowV64Cores(new HexaCoreSystemDef) ++
        new MeowV64BaseConfig
    )

// for testing
class RocketDualCoreWithInclusiveCacheConfig
    extends Config(
      new WithNBigCores(2) ++
        new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithInclusiveCache ++
        new WithCoherentBusTopology ++
        new BaseConfig
    )

class RocketDualCoreWithBroadcastHubConfig
    extends Config(
      new WithNBigCores(2) ++
        new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithCoherentBusTopology ++
        new BaseConfig
    )

class RocketDualCoreWithIncoherentBusConfig
    extends Config(
      new WithNBigCores(2) ++
        new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithIncoherentBusTopology ++
        new WithIncoherentTiles ++
        new BaseConfig
    )

// adapted from WithScratchpadsOnly from rocket-chip
class WithScratchpads(addr: Long)
    extends Config((_, _, up) => { case RocketTilesKey =>
      up(RocketTilesKey) map { r =>
        r.copy(
          core = r.core.copy(useVM = false),
          dcache = r.dcache.map(
            _.copy(
              nSets = 256, // 16Kb scratchpad
              nWays = 1,
              scratch = Some(addr)
            )
          )
        )
      }
    })

class RocketSingleCoreWithScratchpad
    extends Config(
      new WithScratchpads(0x50000000L) ++
        new WithNBigCores(1) ++
        new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithIncoherentBusTopology ++
        new WithIncoherentTiles ++
        new BaseConfig
    )
