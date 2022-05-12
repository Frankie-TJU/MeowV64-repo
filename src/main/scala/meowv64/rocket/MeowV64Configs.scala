package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem.WithIncoherentBusTopology
import freechips.rocketchip.subsystem.WithJtagDTM
import freechips.rocketchip.subsystem.WithNoSlavePort
import freechips.rocketchip.system.BaseConfig
import meowv64.system.SingleCoreSystemDef
import freechips.rocketchip.subsystem.MemoryBusKey

class WithMemoryBeatBytes(beatBytes: Int)
    extends Config((_, _, up) => { case MemoryBusKey =>
      up(MemoryBusKey).copy(beatBytes = beatBytes)
    })

class MeowV64Config
    extends Config(
      new WithNMeowV64Cores(new SingleCoreSystemDef) ++
        new WithMemoryBeatBytes(32) ++
        new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithIncoherentBusTopology ++
        new BaseConfig
    )
