package meowv64.rocket

import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem.RocketTilesKey
import freechips.rocketchip.subsystem.WithCacheBlockBytes
import freechips.rocketchip.subsystem.WithCoherentBusTopology
import freechips.rocketchip.subsystem.WithDebugSBA
import freechips.rocketchip.subsystem.WithInclusiveCache
import freechips.rocketchip.subsystem.WithIncoherentBusTopology
import freechips.rocketchip.subsystem.WithIncoherentTiles
import freechips.rocketchip.subsystem.WithJtagDTM
import freechips.rocketchip.subsystem.WithNBigCores
import freechips.rocketchip.subsystem.WithNoSlavePort
import freechips.rocketchip.system.BaseConfig
import meowv64.system.DualCoreSystemDef
import meowv64.system.HexaCoreSystemDef
import meowv64.system.DecaCoreSystemDef
import meowv64.system.SingleCoreSystemDef
import freechips.rocketchip.subsystem.MasterPortParams
import freechips.rocketchip.subsystem.MemoryBusKey
import _root_.freechips.rocketchip.subsystem.WithDefaultSlavePort
import freechips.rocketchip.subsystem.WithNExtTopInterrupts

class WithCustomMemPort
    extends Config((site, _, _) => { case CustomExtMem =>
      Seq(
        MasterPortParams(
          // 0x8000_0000 ~ 0x2_0000_0000
          base = BigInt("80000000", 16),
          size = BigInt("180000000", 16),
          beatBytes = site(MemoryBusKey).beatBytes,
          idBits = 4
        )
      )
    })

class WithCustomMMIOPort
    extends Config((_, _, _) => { case CustomExtBus =>
      Seq(
        MasterPortParams(
          // 0x6000_0000 ~ 0x8000_0000
          base = BigInt("60000000", 16),
          size = BigInt("20000000", 16),
          beatBytes = 8,
          idBits = 4
        ),
        MasterPortParams(
          // 0x2_000_0000 ~ 0x3_0000_0000
          base = BigInt("200000000", 16),
          size = BigInt("100000000", 16),
          beatBytes = 8,
          idBits = 4
        )
      )
    })

class MeowV64BaseConfig
    extends Config(
      new WithCustomMemPort ++
        new WithCustomMMIOPort ++
        new WithDefaultSlavePort ++
        new WithJtagDTM ++
        new WithNoSlavePort ++
        new WithInclusiveCache ++
        new WithCoherentBusTopology ++
        new WithDebugSBA ++
        new BaseConfig ++
        new WithCacheBlockBytes(32)
    )

class MeowV64SingleCoreConfig
    extends Config(
      new WithMeowV64Cores(new SingleCoreSystemDef) ++
        new MeowV64BaseConfig
    )

class MeowV64FPGAConfig
    extends Config(
      new WithMeowV64Cores(
        new SingleCoreSystemDef,
        initVec = Some(0x10000)
      ) ++
        new WithNExtTopInterrupts(6) ++ // UART(1) + ETH(1+2) + I2C(1) + SPI(1)
        new MeowV64BaseConfig
    )

class WithDifftestMemPort
    extends Config((site, _, _) => { case CustomExtMem =>
      Seq(
        MasterPortParams(
          // 0x8000_0000 ~ 0x9000_0000
          base = BigInt("80000000", 16),
          size = BigInt("10000000", 16),
          beatBytes = site(MemoryBusKey).beatBytes,
          idBits = 4
        )
      )
    })

class WithDifftestMMIOPort
    extends Config((_, _, _) => { case CustomExtBus =>
      Seq(
        MasterPortParams(
          // 0x4000_0000 ~ 0x5000_0000
          base = BigInt("40000000", 16),
          size = BigInt("10000000", 16),
          beatBytes = 8,
          idBits = 4
        )
      )
    })

class MeowV64DifftestSingleCoreConfig
    extends Config(
      new WithMeowV64Cores(
        new SingleCoreSystemDef,
        enableDifftest = true
      ) ++
        new MeowV64BaseConfig
    )

class MeowV64DifftestDualCoreConfig
    extends Config(
      new WithMeowV64Cores(
        new DualCoreSystemDef,
        enableDifftest = true
      ) ++
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

class MeowV64DecaCoreConfig
    extends Config(
      new WithMeowV64Cores(new DecaCoreSystemDef) ++
        new MeowV64BaseConfig
    )

class MeowV64TapeOutConfig
    extends Config(
      new FlipMSB ++
        new WithNoSlavePort ++
        new WithMeowV64Cores(new HexaCoreSystemDef, initVec = Some(0x10000)) ++
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
