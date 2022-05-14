package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.subsystem.WithCoherentBusTopology
import freechips.rocketchip.subsystem.WithInclusiveCache
import freechips.rocketchip.subsystem.WithJtagDTM
import freechips.rocketchip.subsystem.WithNoSlavePort
import freechips.rocketchip.system.BaseConfig
import meowv64.system.SingleCoreSystemDef
import freechips.rocketchip.subsystem.WithCacheBlockBytes
import meowv64.system.DualCoreSystemDef

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
