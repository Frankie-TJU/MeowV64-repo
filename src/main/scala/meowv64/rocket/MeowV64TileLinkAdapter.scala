package meowv64.rocket

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import meowv64.cache.DCWriteLen
import meowv64.cache.L1DCPort
import meowv64.cache.L1UCReq

class MeowV64TileLinkAdapter(val coredef: CoreDef)(implicit p: Parameters)
    extends LazyModule()(p) {
  override lazy val module = new MeowV64TileLinkAdapterModuleImp(this)

  // dcache port
  val lineSize = coredef.L1_LINE_BYTES
  val dcNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v2(
            name = "meowv64-dc",
            sourceId = IdRange(0, 1),
            supports = TLSlaveToMasterTransferSizes(
              probe = TransferSizes(lineSize, lineSize)
            ),
            emits = TLMasterToSlaveTransferSizes(
              acquireT = TransferSizes(lineSize, lineSize),
              acquireB = TransferSizes(lineSize, lineSize),
              get = TransferSizes(lineSize, lineSize),
              putFull = TransferSizes(lineSize, lineSize)
            )
          )
        )
      )
    )
  )

  // icache port
  val icNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v2(
            name = "meowv64-ic",
            sourceId = IdRange(0, 1),
            emits = TLMasterToSlaveTransferSizes(
              get = TransferSizes(lineSize, lineSize)
            )
          )
        )
      )
    )
  )

  // uncached port
  val ucNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v2(
            name = "meowv64-uc",
            sourceId = IdRange(0, 1),
            emits = TLMasterToSlaveTransferSizes(
              // from 1 to 8 bytes
              get = TransferSizes(1, 8),
              putFull = TransferSizes(1, 8)
            )
          )
        )
      )
    )
  )
}

class MeowV64TileLinkAdapterModuleImp(outer: MeowV64TileLinkAdapter)
    extends LazyModuleImp(outer) {

  val coredef = outer.coredef
  val frontend = IO(Flipped(new CoreFrontend()(coredef)))
  val s_ready :: s_active :: s_inflight :: Nil = Enum(3)

  // ic
  val (ic, ic_edge) = outer.icNode.out(0)
  val ic_state = RegInit(s_ready)
  val ic_addr = Reg(UInt(coredef.XLEN.W))
  switch(ic_state) {
    is(s_ready) {
      when(frontend.ic.read.valid) {
        ic_addr := frontend.ic.read.bits
        ic_state := s_active

        val offset =
          frontend.ic.read.bits(log2Ceil(coredef.L1_LINE_BYTES) - 1, 0)
        assert(offset === 0.U)
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
  ic.a.bits := ic_edge.Get(0.U, ic_addr, log2Ceil(outer.lineSize).U)._2

  // d channel
  ic.d.ready := true.B
  frontend.ic.stall := ~ic.d.valid
  frontend.ic.data := ic.d.bits.data

  // unused
  ic.b.valid := false.B
  ic.c.ready := true.B
  ic.e.ready := true.B

  // dc
  val (dc, dc_edge) = outer.dcNode.out(0)
  val s_dc_ready :: s_dc_read :: s_dc_modify :: s_dc_grant :: s_dc_grantack :: s_dc_writeback :: s_dc_probe :: s_dc_probeack :: Nil =
    Enum(8)
  val dc_state = RegInit(s_dc_ready)
  val dc_grant_d = Reg(dc.d.bits.cloneType)
  val dc_probe_b = Reg(dc.b.bits.cloneType)

  dc.a.valid := false.B
  dc.b.ready := false.B
  dc.c.valid := false.B
  dc.d.ready := false.B
  dc.e.valid := false.B

  frontend.dc.l1stall := true.B
  frontend.dc.l2req := L1DCPort.L2Req.idle
  frontend.dc.l2addr := 0.U
  frontend.dc.l2data := 0.U
  switch(dc_state) {
    is(s_dc_ready) {
      switch(frontend.dc.l1req) {
        is(L1DCPort.L1Req.read) {
          dc_state := s_dc_read
        }
        is(L1DCPort.L1Req.modify) {
          dc_state := s_dc_modify
        }
      }
      dc.b.ready := true.B
      when(dc.b.fire) {
        dc_state := s_dc_probe
        dc_probe_b := dc.b.bits
      }
    }
    is(s_dc_read, s_dc_modify) {
      dc.a.valid := true.B
      dc.a.bits := dc_edge
        .AcquireBlock(
          0.U,
          frontend.dc.l1addr,
          log2Ceil(outer.lineSize).U,
          Mux(dc_state === s_dc_read, TLPermissions.NtoB, TLPermissions.NtoT)
        )
        ._2
      when(dc.a.fire) {
        dc_state := s_dc_grant
      }
    }
    is(s_dc_grant) {
      dc.d.ready := true.B
      frontend.dc.l2data := dc.d.bits.data
      when(dc.d.fire && dc.d.bits.opcode === TLMessages.GrantData) {
        frontend.dc.l1stall := false.B
        dc_state := s_dc_grantack
        dc_grant_d := dc.d.bits
      }
    }
    is(s_dc_grantack) {
      dc.e.valid := true.B
      dc.e.bits := dc_edge.GrantAck(dc_grant_d)
      when(dc.e.fire) {
        dc_state := s_dc_ready
      }
    }
    is(s_dc_probe) {
      frontend.dc.l2req := L1DCPort.L2Req.flush
      frontend.dc.l2addr := dc.b.bits.address
      when(~frontend.dc.l2stall) {
        dc_state := s_dc_probeack
      }
    }
    is(s_dc_probeack) {
      dc.c.valid := true.B
      dc.c.bits := dc_edge.ProbeAck(dc_probe_b, TLPermissions.NtoN)
      when(dc.c.fire) {
        dc_state := s_dc_ready
      }
    }
  }

  // uncached
  val (uc, uc_edge) = outer.ucNode.out(0)
  val uc_state = RegInit(s_ready)
  val uc_addr = Reg(UInt(coredef.XLEN.W))
  val uc_read = Reg(Bool())
  val uc_wdata = Reg(UInt(coredef.XLEN.W))
  val uc_len = Reg(DCWriteLen())

  val uc_offset = frontend.uc
    .addr(log2Ceil(coredef.L1_LINE_BYTES) - 1, 0)
  val uc_shift = uc_offset << 3
  switch(uc_state) {
    is(s_ready) {
      switch(frontend.uc.req) {
        is(L1UCReq.read) {

          uc_addr := frontend.uc.addr
          uc_len := frontend.uc.len
          uc_state := s_active

          uc_read := true.B
        }
        is(L1UCReq.write) {

          uc_addr := frontend.uc.addr
          uc_len := frontend.uc.len
          uc_state := s_active

          uc_read := false.B

          // handle offset in addr
          uc_wdata := frontend.uc.wdata << uc_shift
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
  frontend.uc.stall := ~uc.d.valid
  // handle offset in addr
  frontend.uc.rdata := uc.d.bits.data >> uc_shift
}
