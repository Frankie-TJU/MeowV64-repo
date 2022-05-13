package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.subsystem.WithCoherentBusTopology
import freechips.rocketchip.subsystem.WithInclusiveCache
import freechips.rocketchip.subsystem.WithJtagDTM
import freechips.rocketchip.subsystem.WithNoSlavePort
import freechips.rocketchip.system.BaseConfig
import meowv64.system.SingleCoreSystemDef

class MeowV64BaseConfig
    extends Config((_, _, _) => { case CacheBlockBytes =>
      32
    })

class MeowV64Config
    extends Config(
      new WithMeowV64Cores(new SingleCoreSystemDef) ++
        new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithInclusiveCache ++
        new WithCoherentBusTopology ++
        new BaseConfig ++
        new MeowV64BaseConfig
    )
