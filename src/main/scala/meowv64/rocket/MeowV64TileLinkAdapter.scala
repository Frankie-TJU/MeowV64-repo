package meowv64.rocket

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import meowv64.cache.DCWriteLen
import meowv64.cache.L1DCPort
import meowv64.cache.L1ICPort
import meowv64.cache.L1UCReq
import meowv64.core.CoreDef
import meowv64.core.CoreFrontend

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

  // uncached data port
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

  // uncached inst port
  val uiNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v2(
            name = "meowv64-ui",
            sourceId = IdRange(0, 1),
            emits = TLMasterToSlaveTransferSizes(
              get = TransferSizes(lineSize, lineSize)
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

  // connect L1ICPort to TileLInk
  def connectIC(port: L1ICPort, node: TLClientNode) = {
    val (ic, ic_edge) = node.out(0)
    val ic_state = RegInit(s_ready)
    val ic_addr = Reg(UInt(coredef.XLEN.W))
    switch(ic_state) {
      is(s_ready) {
        when(port.read.valid) {
          ic_addr := port.read.bits
          ic_state := s_active

          val offset =
            port.read.bits(log2Ceil(coredef.L1_LINE_BYTES) - 1, 0)
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
    port.stall := ~ic.d.valid
    port.data := ic.d.bits.data

    // unused
    ic.b.valid := false.B
    ic.c.ready := true.B
    ic.e.ready := true.B
  }

  // icache
  connectIC(frontend.ic, outer.icNode)

  // dcache
  val (dc, dc_edge) = outer.dcNode.out(0)

  dc.a.valid := false.B
  dc.b.ready := false.B
  dc.c.valid := false.B
  dc.d.ready := false.B
  dc.e.valid := false.B

  frontend.dc.l1stall := true.B
  frontend.dc.l1rdata := 0.U
  frontend.dc.l2req := L1DCPort.L2Req.idle
  frontend.dc.l2addr := 0.U

  // state for l1 & l2
  val s_l1_ready :: s_l1_read :: s_l1_modify :: s_l1_grant :: s_l1_grantack :: s_l1_writeback :: s_l1_releaseack :: Nil =
    Enum(7)
  val dc_l1_state = RegInit(s_l1_ready)

  val s_l2_ready :: s_l2_probe :: s_l2_probeack :: Nil =
    Enum(3)
  val dc_l2_state = RegInit(s_l2_ready)

  // l1 req
  val dc_grant_d = Reg(dc.d.bits.cloneType)
  val dc_l1_out_c = Wire(dc.c.cloneType)
  dc_l1_out_c.valid := false.B
  dc_l1_out_c.bits := 0.U.asTypeOf(dc_l1_out_c.bits)

  switch(dc_l1_state) {
    is(s_l1_ready) {
      switch(frontend.dc.l1req) {
        is(L1DCPort.L1Req.read) {
          dc_l1_state := s_l1_read
        }
        is(L1DCPort.L1Req.modify) {
          dc_l1_state := s_l1_modify
        }
        is(L1DCPort.L1Req.writeback) {
          // do not l1.writeback & l2.flush in the same cycle
          when(dc_l2_state === s_l2_ready) {
            dc_l1_state := s_l1_writeback
          }
        }
      }
    }
    is(s_l1_read, s_l1_modify) {
      // send AcquireBlock to retrieve a copy
      // and wait for Grant
      dc.a.valid := true.B
      dc.a.bits := dc_edge
        .AcquireBlock(
          0.U,
          frontend.dc.l1addr,
          log2Ceil(outer.lineSize).U,
          Mux(dc_l1_state === s_l1_read, TLPermissions.NtoB, TLPermissions.NtoT)
        )
        ._2
      when(dc.a.fire) {
        dc_l1_state := s_l1_grant
      }
    }
    is(s_l1_grant) {
      // wait for Grant
      // return data read to l1
      frontend.dc.l1rdata := dc.d.bits.data
      // If valid is LOW, the receiver must not expect the control or data signals to be a syntactically correct TileLink beat.
      when(dc.d.valid && dc.d.bits.opcode === TLMessages.GrantData) {
        dc.d.ready := true.B

        when(dc.d.fire) {
          frontend.dc.l1stall := false.B
          dc_l1_state := s_l1_grantack
          dc_grant_d := dc.d.bits
        }
      }
    }
    is(s_l1_grantack) {
      // send GrantAck
      dc.e.valid := true.B
      dc.e.bits := dc_edge.GrantAck(dc_grant_d)
      when(dc.e.fire) {
        dc_l1_state := s_l1_ready
      }
    }
    is(s_l1_writeback) {
      // send ReleaseData
      // M->I: T->N
      // writeback data(l1data)
      dc_l1_out_c.valid := true.B
      dc_l1_out_c.bits := dc_edge
        .Release(
          0.U,
          frontend.dc.l1addr,
          log2Ceil(coredef.L1_LINE_BYTES).U,
          TLPermissions.TtoN,
          frontend.dc.l1wdata
        )
        ._2
      when(dc_l1_out_c.fire) {
        dc_l1_state := s_l1_releaseack
      }
    }
    is(s_l1_releaseack) {
      // wait for ReleaseAck
      // If valid is LOW, the receiver must not expect the control or data signals to be a syntactically correct TileLink beat.
      when(dc.d.valid && dc.d.bits.opcode === TLMessages.ReleaseAck) {
        dc.d.ready := true.B

        when(dc.d.fire) {
          frontend.dc.l1stall := false.B
          dc_l1_state := s_l1_ready
        }
      }
    }
  }

  // l2 req
  val dc_l2_cache_valid = Reg(Bool())
  val dc_l2_cache_dirty = Reg(Bool())
  val dc_l2_cache_wdata = Reg(frontend.dc.l2wdata.cloneType)
  val dc_probe_b = Reg(dc.b.bits.cloneType)
  val dc_l2_out_c = Wire(dc.c.cloneType)
  dc_l2_out_c.valid := false.B
  dc_l2_out_c.bits := 0.U.asTypeOf(dc_l2_out_c.bits)
  switch(dc_l2_state) {
    is(s_l2_ready) {
      // block outer Probes when Release is inflight
      when(dc_l1_state === s_l1_writeback || dc_l1_state === s_l1_releaseack) {
        dc.b.ready := false.B
      }.otherwise {
        dc.b.ready := true.B
      }
      when(dc.b.fire) {
        dc_l2_state := s_l2_probe
        dc_probe_b := dc.b.bits
      }
    }
    is(s_l2_probe) {
      when(dc_probe_b.param === TLPermissions.toN) {
        // if toN, invalidate to I
        frontend.dc.l2req := L1DCPort.L2Req.invalidate
      }.elsewhen(dc_probe_b.param === TLPermissions.toB) {
        // if toB, flush to S
        frontend.dc.l2req := L1DCPort.L2Req.flush
      }.otherwise {
        // unsupported
        assert(false.B);
      }

      frontend.dc.l2addr := dc_probe_b.address
      when(~frontend.dc.l2stall) {
        dc_l2_state := s_l2_probeack
        dc_l2_cache_valid := frontend.dc.l2valid
        dc_l2_cache_dirty := frontend.dc.l2dirty
        dc_l2_cache_wdata := frontend.dc.l2wdata
      }
    }
    is(s_l2_probeack) {
      // Release: A master should not issue a Release if there is a pending Grant on the block.
      // Once the Release is issued, the master should not issue ProbeAcks, Acquires,
      // or further Releases until it receives a ReleaseAck from the slave acknowledging completion of the writeback.
      when(dc_l1_state === s_l1_writeback || dc_l1_state === s_l1_releaseack) {
        dc_l2_out_c.valid := false.B
      }.otherwise {
        dc_l2_out_c.valid := true.B
      }
      val perm = WireInit(0.U(TLPermissions.cWidth.W))
      when(dc_l2_cache_valid && dc_l2_cache_dirty) {
        // M -> S/I
        when(dc_probe_b.param === TLPermissions.toN) {
          perm := TLPermissions.TtoN
        }.elsewhen(dc_probe_b.param === TLPermissions.toB) {
          perm := TLPermissions.TtoB
        }.otherwise {
          assert(false.B)
        }

        // ProbeAckData
        dc_l2_out_c.bits := dc_edge.ProbeAck(
          dc_probe_b,
          perm,
          dc_l2_cache_wdata
        )
      }.elsewhen(dc_l2_cache_valid && !dc_l2_cache_dirty) {
        // S -> S/I
        when(dc_probe_b.param === TLPermissions.toN) {
          perm := TLPermissions.BtoN
        }.elsewhen(dc_probe_b.param === TLPermissions.toB) {
          perm := TLPermissions.BtoB
        }.otherwise {
          assert(false.B)
        }

        // ProbeAckData
        dc_l2_out_c.bits := dc_edge.ProbeAck(
          dc_probe_b,
          perm,
          dc_l2_cache_wdata
        )
      }.otherwise {
        when(dc_probe_b.param === TLPermissions.toN) {
          // I -> I
          perm := TLPermissions.NtoN
        }.elsewhen(dc_probe_b.param === TLPermissions.toB) {
          // L1DCache can silently drop non-dirty cache lines
          // I -> I
          perm := TLPermissions.NtoN
        }.otherwise {
          assert(false.B)
        }

        // ProbeAck
        dc_l2_out_c.bits := dc_edge.ProbeAck(dc_probe_b, perm)
      }
      when(dc_l2_out_c.fire) {
        dc_l2_state := s_l2_ready
      }
    }
  }

  // arbiter for c channel
  TLArbiter.robin(dc_edge, dc.c, dc_l1_out_c, dc_l2_out_c)

  // uncached data
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
        when(!uc.d.fire) {
          uc_state := s_inflight
        }.otherwise {
          // same cycle
          uc_state := s_ready
        }
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

  // uncached inst
  connectIC(frontend.ui, outer.uiNode)
}
