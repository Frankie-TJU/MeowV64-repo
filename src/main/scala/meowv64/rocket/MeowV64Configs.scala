package meowv64.rocket

import org.chipsalliance.cde.config.Config
import freechips.rocketchip.subsystem.WithCacheBlockBytes
import freechips.rocketchip.subsystem.WithCoherentBusTopology
import freechips.rocketchip.subsystem.WithDebugSBA
import freechips.rocketchip.subsystem.WithInclusiveCache
import freechips.rocketchip.subsystem.WithIncoherentBusTopology
import freechips.rocketchip.subsystem.WithIncoherentTiles
import freechips.rocketchip.subsystem.WithJtagDTM
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
          // 0x2_0000_0000 ~ 0x3_0000_0000
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
