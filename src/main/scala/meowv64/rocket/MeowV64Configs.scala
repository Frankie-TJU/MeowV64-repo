package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem.WithJtagDTM
import freechips.rocketchip.subsystem.WithNoSlavePort
import freechips.rocketchip.system.BaseConfig
import meowv64.system.SingleCoreSystemDef
import freechips.rocketchip.subsystem.WithCoherentBusTopology
import freechips.rocketchip.subsystem.WithInclusiveCache

class MeowV64Config
    extends Config(
      new WithMeowV64Cores(new SingleCoreSystemDef) ++
        new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithInclusiveCache ++
        new WithCoherentBusTopology ++
        new BaseConfig
    )
