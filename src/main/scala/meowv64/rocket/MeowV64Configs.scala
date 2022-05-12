package meowv64.rocket

import freechips.rocketchip.config.{Config}
import freechips.rocketchip.system.DefaultConfig
import freechips.rocketchip.subsystem.WithNSmallCores
import freechips.rocketchip.system.DefaultSmallConfig

class MeowV64Config
    extends Config(
      new WithNMeowV64Cores(1) ++
        new DefaultConfig
    )

class RocketConfig
    extends Config(
      new DefaultSmallConfig
    )
