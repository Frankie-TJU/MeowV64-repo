package meowv64.exec.units

import chisel3._
import chisel3.util._
import meowv64.core.CoreDef
import meowv64.core.VState
import meowv64.exec._
import meowv64.instr.Decoder

class VectorALUExt(implicit val coredef: CoreDef) extends Bundle {
  val res = UInt(coredef.VLEN.W)

  val rs1Elements = MixedVec(
    coredef.INT_TYPES.map({ case (_, width) =>
      Vec(coredef.VLEN / width, UInt(width.W))
    })
  )
  val rs2Elements = MixedVec(
    coredef.INT_TYPES.map({ case (_, width) =>
      Vec(coredef.VLEN / width, UInt(width.W))
    })
  )
  val rs3Elements = MixedVec(
    coredef.INT_TYPES.map({ case (_, width) =>
      Vec(coredef.VLEN / width, UInt(width.W))
    })
  )
}

class VectorALU(override implicit val coredef: CoreDef)
    extends ExecUnit(1, new VectorALUExt, coredef.REG_VEC)
    with WithVState {

  val vState = IO(Input(new VState))
  def map(
      stage: Int,
      pipe: PipeInstr,
      last_ext: Option[VectorALUExt]
  ): (VectorALUExt, Bool) = {
    val ext = Wire(new VectorALUExt)
    ext := DontCare
    ext.res := 0.U

    for (((sew, width), idx) <- coredef.INT_TYPES.zipWithIndex) {
      val lanes = coredef.VLEN / width
      val simm = Wire(SInt(width.W))
      simm := pipe.instr.instr.simm5()
      val res = WireInit(VecInit.fill(lanes)(0.U(width.W)))

      if (stage == 0) {
        // stage 0: save values
        val rs1Elements = Wire(Vec(lanes, UInt(width.W)))
        rs1Elements := pipe.rs1val.asTypeOf(rs1Elements)
        val rs2Elements = Wire(Vec(lanes, UInt(width.W)))
        rs2Elements := pipe.rs2val.asTypeOf(rs2Elements)
        val rs3Elements = Wire(Vec(lanes, UInt(width.W)))
        rs3Elements := pipe.rs3val.asTypeOf(rs3Elements)
        ext.rs1Elements(idx) := rs1Elements
        ext.rs2Elements(idx) := rs2Elements
        ext.rs3Elements(idx) := rs3Elements
      } else {
        // stage 1: compute
        val rs1Elements = last_ext.get.rs1Elements(idx)
        val rs2Elements = last_ext.get.rs2Elements(idx)
        val rs3Elements = last_ext.get.rs3Elements(idx)

        when(vState.vtype.vsew === sew.U) {
          switch(pipe.instr.instr.funct6) {
            is(Decoder.VP_FUNC("VADD_V")) {
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VADD_VV
                  for (i <- 0 until lanes) {
                    res(i) := rs1Elements(i) + rs2Elements(i)
                  }
                }
                is(3.U) {
                  // VADD_VI
                  for (i <- 0 until lanes) {
                    res(i) := simm.asUInt + rs2Elements(i)
                  }
                }
                is(4.U) {
                  // VADD_VX
                  for (i <- 0 until lanes) {
                    res(i) := rs1Elements(0) + rs2Elements(i)
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VSUB_V")) {
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VSUB_VV
                  for (i <- 0 until lanes) {
                    res(i) := rs2Elements(i) - rs1Elements(i)
                  }
                }
                is(3.U) {
                  // VSUB_VX
                  for (i <- 0 until lanes) {
                    res(i) := rs2Elements(i) - rs1Elements(0)
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VRSUB_V")) {
              switch(pipe.instr.instr.funct3) {
                is(3.U) {
                  // VRSUB_VI
                  for (i <- 0 until lanes) {
                    res(i) := simm.asUInt - rs2Elements(i)
                  }
                }
                is(4.U) {
                  // VRSUB_VX
                  for (i <- 0 until lanes) {
                    res(i) := rs1Elements(0) - rs2Elements(i)
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VMINU_V")) {
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VMINU_VV
                  for (i <- 0 until lanes) {
                    when(rs1Elements(i) < rs2Elements(i)) {
                      res(i) := rs1Elements(i)
                    }.otherwise {
                      res(i) := rs2Elements(i)
                    }
                  }
                }
                is(4.U) {
                  // VMINU_VX
                  for (i <- 0 until lanes) {
                    when(rs1Elements(0) < rs2Elements(i)) {
                      res(i) := rs1Elements(0)
                    }.otherwise {
                      res(i) := rs2Elements(i)
                    }
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VMIN_V")) {
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VMIN_VV
                  for (i <- 0 until lanes) {
                    when(rs1Elements(i).asSInt < rs2Elements(i).asSInt) {
                      res(i) := rs1Elements(i)
                    }.otherwise {
                      res(i) := rs2Elements(i)
                    }
                  }
                }
                is(4.U) {
                  // VMIN_VX
                  for (i <- 0 until lanes) {
                    when(rs1Elements(0).asSInt < rs2Elements(i).asSInt) {
                      res(i) := rs1Elements(0)
                    }.otherwise {
                      res(i) := rs2Elements(i)
                    }
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VMAXU_V")) {
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VMAXU_VV
                  for (i <- 0 until lanes) {
                    when(rs1Elements(i) < rs2Elements(i)) {
                      res(i) := rs2Elements(i)
                    }.otherwise {
                      res(i) := rs1Elements(i)
                    }
                  }
                }
                is(4.U) {
                  // VMAXU_VX
                  for (i <- 0 until lanes) {
                    when(rs1Elements(0) < rs2Elements(i)) {
                      res(i) := rs2Elements(i)
                    }.otherwise {
                      res(i) := rs1Elements(0)
                    }
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VMAX_V")) {
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VMAXU_VV
                  for (i <- 0 until lanes) {
                    when(rs1Elements(i).asSInt < rs2Elements(i).asSInt) {
                      res(i) := rs2Elements(i)
                    }.otherwise {
                      res(i) := rs1Elements(i)
                    }
                  }
                }
                is(4.U) {
                  // VMAXU_VX
                  for (i <- 0 until lanes) {
                    when(rs1Elements(0).asSInt < rs2Elements(i).asSInt) {
                      res(i) := rs2Elements(i)
                    }.otherwise {
                      res(i) := rs1Elements(0)
                    }
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VAND_V")) {
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VAND_VV
                  for (i <- 0 until lanes) {
                    res(i) := rs1Elements(i) & rs2Elements(i)
                  }
                }
                is(3.U) {
                  // VAND_VI
                  for (i <- 0 until lanes) {
                    res(i) := simm.asUInt & rs2Elements(i)
                  }
                }
                is(4.U) {
                  // VAND_VX
                  for (i <- 0 until lanes) {
                    res(i) := rs1Elements(0) & rs2Elements(i)
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VOR_V")) {
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VOR_VV
                  for (i <- 0 until lanes) {
                    res(i) := rs1Elements(i) | rs2Elements(i)
                  }
                }
                is(3.U) {
                  // VOR_VI
                  for (i <- 0 until lanes) {
                    res(i) := simm.asUInt | rs2Elements(i)
                  }
                }
                is(4.U) {
                  // VOR_VX
                  for (i <- 0 until lanes) {
                    res(i) := rs1Elements(0) | rs2Elements(i)
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VXOR_V")) {
              switch(pipe.instr.instr.funct3) {
                is(0.U) {
                  // VXOR_VV
                  for (i <- 0 until lanes) {
                    res(i) := rs1Elements(i) ^ rs2Elements(i)
                  }
                }
                is(3.U) {
                  // VXOR_VI
                  for (i <- 0 until lanes) {
                    res(i) := simm.asUInt ^ rs2Elements(i)
                  }
                }
                is(4.U) {
                  // VXOR_VX
                  for (i <- 0 until lanes) {
                    res(i) := rs1Elements(0) ^ rs2Elements(i)
                  }
                }
              }
            }
            is(Decoder.VP_FUNC("VSLL_V")) {
              for (i <- 0 until lanes) {
                val shiftAmount = WireInit(0.U(log2Ceil(width).W))
                switch(pipe.instr.instr.funct3) {
                  is(0.U) {
                    // VSLL.VV
                    shiftAmount := rs1Elements(i)
                  }
                  is(3.U) {
                    // VSLL.VI
                    shiftAmount := simm.asUInt
                  }
                  is(4.U) {
                    // VSLL.VX
                    shiftAmount := pipe.rs1val
                  }
                }

                // check vm
                when(pipe.instr.instr.readVm() && ~pipe.vmval(i)) {
                  res(i) := rs3Elements(i)
                }.otherwise {
                  res(i) := rs2Elements(i) << shiftAmount
                }
              }
            }
          }

          ext.res := Cat(res.reverse)
        }
      }
    }

    (ext, false.B)
  }

  def finalize(pipe: PipeInstr, ext: VectorALUExt): RetireInfo = {
    val info = WireDefault(RetireInfo.vacant(regInfo))
    info.wb := ext.res

    info
  }

  init()
}
