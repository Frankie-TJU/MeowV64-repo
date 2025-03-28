package meowv64.rocket

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci.{ClockSinkParameters, ClockCrossingType}
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.RocketCrossingParams
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import meowv64.core.Core
import meowv64.core.CoreDebug
import meowv64.core.CoreDef
import chisel3.experimental.hierarchy.Definition
import chisel3.experimental.hierarchy.Instance

case class MeowV64CoreParams(
    coredef: CoreDef
) extends CoreParams {
  val bootFreqHz: BigInt = 0
  val useVM: Boolean = true
  val useHypervisor: Boolean = false
  val useUser: Boolean = true
  val useSupervisor: Boolean = true
  val useDebug: Boolean = true
  val useAtomics: Boolean = true
  val useAtomicsOnlyForIO: Boolean = false
  val useCompressed: Boolean = true
  override val useVector: Boolean = true
  val useSCIE: Boolean = false
  val useRVE: Boolean = false
  val useConditionalZero: Boolean = false
  val useZba: Boolean = false
  val useZbb: Boolean = false
  val useZbs: Boolean = false
  val mulDiv: Option[MulDivParams] = Some(MulDivParams())
  val fpu: Option[FPUParams] = Some(FPUParams())
  val fetchWidth: Int = 2
  val decodeWidth: Int = 2
  val retireWidth: Int = 2
  val instBits: Int = if (useCompressed) 16 else 32
  val nLocalInterrupts: Int = 0
  val useNMI: Boolean = false
  val nPMPs: Int = 0
  val pmpGranularity: Int = 4
  val nBreakpoints: Int = 0
  val useBPWatch: Boolean = false
  val mcontextWidth: Int = 0
  val scontextWidth: Int = 0
  val nPerfCounters: Int = 0
  val haveBasicCounters: Boolean = true
  val haveFSDirty: Boolean = false
  val misaWritable: Boolean = false
  val haveCFlush: Boolean = false
  val nL2TLBEntries: Int = 512
  val nL2TLBWays: Int = 1
  val nPTECacheEntries: Int = 8 // TODO: Check
  val mtvecInit: Option[BigInt] = Some(BigInt(0))
  val mtvecWritable: Boolean = true
  val traceHasWdata: Boolean = false
  val xLen: Int = 64
  val pgLevels: Int = 3

  override def lrscCycles: Int = 80

  override def vLen = 256
  override def eLen = 64

  override def vMemDataBits = 256
}

case class MeowV64TileAttachParams(
    tileParams: MeowV64TileParams,
    crossingParams: RocketCrossingParams
) extends CanAttachTile {
  type TileType = MeowV64Tile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))
}

case class MeowV64TileParams(
    coredef: CoreDef,
    tileId: Int,
    name: Option[String] = Some("meowv64_tile")
) extends InstantiableTileParams[MeowV64Tile] {
  val core: MeowV64CoreParams = MeowV64CoreParams(
    coredef
  )
  val icache: Option[ICacheParams] = Some(
    ICacheParams(nSets = coredef.L1I.LINE_PER_ASSOC, nWays = coredef.L1I.ASSOC)
  )
  val dcache: Option[DCacheParams] = Some(
    DCacheParams(nSets = coredef.L1D.LINE_PER_ASSOC, nWays = coredef.L1D.ASSOC)
  )
  val btb: Option[BTBParams] = Some(BTBParams())
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val clockSinkParams: ClockSinkParameters = ClockSinkParameters()

  def instantiate(
      crossing: HierarchicalElementCrossingParamsLike,
      lookup: LookupByHartIdImpl
  )(implicit
      p: Parameters
  ): MeowV64Tile = {
    new MeowV64Tile(this, crossing, lookup)
  }

  val baseName = "meowv64tile"
  val uniqueName = s"${baseName}_$tileId"
}

class MeowV64Tile private (
    val meowv64Params: MeowV64TileParams,
    crossing: ClockCrossingType,
    lookup: LookupByHartIdImpl,
    q: Parameters
) extends BaseTile(meowv64Params, crossing, lookup, q)
    with SinksExternalInterrupts
    with SourcesExternalNotifications {

  /** Setup parameters: Private constructor ensures altered LazyModule.p is used
    * implicitly
    */
  def this(
      params: MeowV64TileParams,
      crossing: HierarchicalElementCrossingParamsLike,
      lookup: LookupByHartIdImpl
  )(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = None
  val slaveNode = TLIdentityNode()
  val masterNode = visibilityNode

  tlOtherMastersNode := tlMasterXbar.node
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  val cpuDevice: SimpleDevice =
    new SimpleDevice("cpu", Seq("meow,meowv64", "riscv")) {
      override def parent = Some(ResourceAnchors.cpus)
      override def describe(resources: ResourceBindings): Description = {
        val Description(name, mapping) = super.describe(resources)
        Description(
          name,
          mapping ++
            cpuProperties ++
            nextLevelCacheProperty ++
            tileProperties
        )
      }
    }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(tileId))
  }

  override lazy val module = new MeowV64TileModuleImp(this)

  // adapt custom interface to TileLink Cached
  val adapter = LazyModule(new MeowV64TileLinkAdapter(meowv64Params.coredef))

  // buffets
  val buffets = LazyModule(
    new Buffets(
      BuffetsConfig(
        memoryBase = 0x5000000L,
        memorySize = p(BuffetsSizePerCore),
        configBase = 0x58000000L,
        beatBytes = 32
      )
    )
  )
  val addrGen = LazyModule(
    new AddressGeneration(
      AddressGenerationConfig(
        configBase = 0x59000000L,
        beatBytes = 32
      )
    )
  )

  // we use custom beatBytes from the client
  val node = TLIdentityNode()
  val innerBeatBytes = meowv64Params.coredef.L1_LINE_BYTES
  tlMasterXbar.node := node := TLBuffer() := TLWidthWidget(
    innerBeatBytes
  ) := adapter.icNode
  tlMasterXbar.node := node := TLBuffer() := TLWidthWidget(
    innerBeatBytes
  ) := adapter.dcNode
  tlMasterXbar.node := node := TLBuffer() := TLWidthWidget(
    innerBeatBytes
  ) := adapter.uiNode

  // create local crossbar for buffets & addrgen
  // share port with uncached to reduce clash
  val xbar = LazyModule(new TLXbar())
  xbar.node := adapter.ucNode
  xbar.node := addrGen.masterNode
  buffets.slaveNode := xbar.node
  buffets.registerNode := xbar.node
  addrGen.registerNode := xbar.node

  tlMasterXbar.node := node := TLBuffer() := TLWidthWidget(
    innerBeatBytes
  ) := xbar.node

  def connectMeowV64Interrupts(
      debug: Bool,
      meip: Bool,
      seip: Bool,
      mtip: Bool,
      msip: Bool
  ) {
    val (interrupts, _) = intSinkNode.in(0)
    // 0 is debug req
    debug := interrupts(0)
    // 1 is msip
    msip := interrupts(1)
    // 2 is mtip
    mtip := interrupts(2)
    // 3 is meip
    meip := interrupts(3)
    // 4 is seip
    seip := interrupts(4)
  }

  // expose debug
  val customDebugSourceNode =
    BundleBridgeSource(() => new CoreDebug()(meowv64Params.coredef))
  val customDebugNode: BundleBridgeOutwardNode[CoreDebug] =
    customDebugSourceNode
}

object MeowV64TileModuleImp {
  var definition: Definition[Core] = null
}

class MeowV64TileModuleImp(outer: MeowV64Tile)
    extends BaseTileModuleImp(outer) {
  // annotate the parameters
  Annotated.params(this, outer.meowv64Params)

  // connect the meowv64 core
  val coredef = outer.meowv64Params.core.coredef
  val core = Module(new Core()(coredef))

  // set hartid from outside to reuse core
  core.io.hartId := outer.meowv64Params.tileId.U

  // wire frontend io to adapter
  core.io.frontend <> outer.adapter.module.frontend

  outer.connectMeowV64Interrupts(
    core.io.dm.haltreq,
    core.io.int.meip,
    core.io.int.seip,
    core.io.int.mtip,
    core.io.int.msip
  )

  // expose debug
  outer.customDebugSourceNode.bundle := core.io.debug

  // connect addr gen and buffets
  outer.addrGen.module.egress <> outer.buffets.module.ingress

  // connect buffets fast path
  outer.buffets.module.fastpath <> core.io.toBuffets
}
