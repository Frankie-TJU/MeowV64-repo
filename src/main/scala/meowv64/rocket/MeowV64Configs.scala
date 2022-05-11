package meowv64.rocket

import freechips.rocketchip.config.{Config}
import freechips.rocketchip.system.DefaultConfig

class MeowV64Config
    extends Config(
      new WithNMeowV64Cores(1) ++
        new DefaultConfig
    )
