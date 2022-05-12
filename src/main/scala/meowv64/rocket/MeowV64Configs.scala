package meowv64.rocket

import freechips.rocketchip.config.{Config}
import freechips.rocketchip.system.DefaultConfig
import freechips.rocketchip.subsystem.WithNSmallCores
import freechips.rocketchip.system.DefaultSmallConfig
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.subsystem.WithJtagDTM
import freechips.rocketchip.subsystem.WithNoMMIOPort
import freechips.rocketchip.subsystem.WithNoSlavePort
import freechips.rocketchip.subsystem.WithIncoherentBusTopology

class MeowV64Config
    extends Config(
      new WithNMeowV64Cores(1) ++
        new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithIncoherentBusTopology ++
        new BaseConfig
    )
