package meowv64.rocket

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci.ClockSinkParameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.RocketCrossingParams
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import meowv64.cache.DCWriteLen
import meowv64.cache.L1DCPort
import meowv64.cache.L1UCReq
import meowv64.core.Core
import meowv64.core.CoreDef
import meowv64.system.DefaultSystemDef
import meowv64.system.SystemDef

case class MeowV64CoreParams(
    coredef: CoreDef
) extends CoreParams {
  /* DO NOT CHANGE BELOW THIS */
  val bootFreqHz: BigInt = 0
  val useVM: Boolean = true
  val useHypervisor: Boolean = false
  val useUser: Boolean = true
  val useSupervisor: Boolean = true
  val useDebug: Boolean = true
  val useAtomics: Boolean = true
  val useAtomicsOnlyForIO: Boolean = false
  val useCompressed: Boolean = true
  override val useBitManip: Boolean = false
  override val useVector: Boolean = true
  val useSCIE: Boolean = false
  val useRVE: Boolean = false
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

  override def lrscCycles: Int = 80

  override def vLen = 256

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
    name: Option[String] = Some("meowv64_tile"),
    hartId: Int = 0,
    systemDef: SystemDef = new DefaultSystemDef
) extends InstantiableTileParams[MeowV64Tile] {
  val core: MeowV64CoreParams = MeowV64CoreParams(
    CoreDef.default(hartId, systemDef.INIT_VEC, systemDef.L2_LINE_BYTES)
  )
  val icache: Option[ICacheParams] = Some(ICacheParams())
  val dcache: Option[DCacheParams] = Some(DCacheParams())
  val btb: Option[BTBParams] = Some(BTBParams())
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val clockSinkParams: ClockSinkParameters = ClockSinkParameters()

  def instantiate(crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(
      implicit p: Parameters
  ): MeowV64Tile = {
    new MeowV64Tile(this, crossing, lookup)
  }
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
      crossing: TileCrossingParamsLike,
      lookup: LookupByHartIdImpl
  )(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = IntIdentityNode()
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
    Resource(cpuDevice, "reg").bind(ResourceAddress(staticIdForMetadataUseOnly))
  }

  override lazy val module = new MeowV64TileModuleImp(this)

  val node = TLIdentityNode()

  // dcache port
  val dcNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(name = "meowv64-dc", sourceId = IdRange(0, 1))
        )
      )
    )
  )

  tlMasterXbar.node := node := TLBuffer() := dcNode

  // icache port
  val icNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(name = "meowv64-ic", sourceId = IdRange(0, 1))
        )
      )
    )
  )

  tlMasterXbar.node := node := TLBuffer() := icNode

  // uncached port
  val ucNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(name = "meowv64-uc", sourceId = IdRange(0, 1))
        )
      )
    )
  )

  tlMasterXbar.node := node := TLBuffer() := ucNode

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
}

class MeowV64TileModuleImp(outer: MeowV64Tile)
    extends BaseTileModuleImp(outer) {
  // annotate the parameters
  Annotated.params(this, outer.meowv64Params)

  // connect the meowv64 core
  val coredef = outer.meowv64Params.core.coredef
  val core = Module(
    new Core()(coredef)
  )

  val s_ready :: s_active :: s_inflight :: Nil = Enum(3)

  // ic
  val (ic, ic_edge) = outer.icNode.out(0)
  val ic_state = RegInit(s_ready)
  val ic_addr = Reg(UInt(coredef.XLEN.W))
  switch(ic_state) {
    is(s_ready) {
      when(core.io.frontend.ic.read.valid) {
        ic_addr := core.io.frontend.ic.read.bits
        ic_state := s_active
      }
    }
    is(s_active) {
      when(ic.a.fire) {
        ic_state := s_inflight
      }
    }
    is(s_inflight) {
      when(ic.d.fire) {
        ic_state := s_ready
      }
    }
  }

  // a channel
  ic.a.valid := ic_state === s_active && ~reset.asBool
  ic.a.bits := ic_edge.Get(0.U, ic_addr, log2Ceil(coredef.L1_LINE_BYTES).U)._2

  // d channel
  ic.d.ready := true.B
  core.io.frontend.ic.stall := ~ic.d.valid
  core.io.frontend.ic.data := ic.d.bits.data

  // unused
  ic.b.valid := false.B
  ic.c.ready := true.B
  ic.e.ready := true.B

  // dc
  // TODO
  core.io.frontend.dc.l1stall := true.B
  core.io.frontend.dc.l2req := L1DCPort.L2Req.idle
  core.io.frontend.dc.l2addr := 0.U
  core.io.frontend.dc.l2data := 0.U

  // uncached
  // TODO
  core.io.frontend.uc.stall := true.B
  core.io.frontend.uc.rdata := 0.U

  val (uc, uc_edge) = outer.ucNode.out(0)
  val uc_state = RegInit(s_ready)
  val uc_addr = Reg(UInt(coredef.XLEN.W))
  val uc_read = Reg(Bool())
  val uc_wdata = Reg(UInt(coredef.XLEN.W))
  val uc_len = Reg(DCWriteLen())
  switch(uc_state) {
    is(s_ready) {
      switch(core.io.frontend.uc.req) {
        is(L1UCReq.read) {

          uc_addr := core.io.frontend.uc.addr
          uc_len := core.io.frontend.uc.len
          uc_state := s_active

          uc_read := true.B
        }
        is(L1UCReq.write) {

          uc_addr := core.io.frontend.uc.addr
          uc_len := core.io.frontend.uc.len
          uc_state := s_active

          uc_read := false.B
          uc_wdata := core.io.frontend.uc.wdata
        }
      }
    }
    is(s_active) {
      when(uc.a.fire) {
        uc_state := s_inflight
      }
    }
    is(s_inflight) {
      when(uc.d.fire) {
        uc_state := s_ready
      }
    }
  }

  // a channel
  uc.a.valid := uc_state === s_active && ~reset.asBool
  when(uc_read) {
    uc.a.bits := uc_edge.Get(0.U, uc_addr, uc_len.asUInt)._2
  }.otherwise {
    uc.a.bits := uc_edge.Put(0.U, uc_addr, uc_len.asUInt, uc_wdata)._2
  }

  // d channel
  uc.d.ready := true.B
  core.io.frontend.uc.stall := ~uc.d.valid
  core.io.frontend.uc.rdata := uc.d.bits.data

  // debug mode code
  // TODO
  core.io.dmCode.stall := true.B
  core.io.dmCode.data := 0.U

  // time
  // TODO
  core.io.time := 0.U

  outer.connectMeowV64Interrupts(
    core.io.dm.haltreq,
    core.io.int.meip,
    core.io.int.seip,
    core.io.int.mtip,
    core.io.int.msip
  )
}
