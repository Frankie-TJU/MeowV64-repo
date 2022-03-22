package meowv64

import chisel3._
import chiseltest._
import chiseltest.simulator.IcarusBackendAnnotation
import meowv64.system.SystemDef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Paths
import scala.collection.mutable
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.AnnotationSeq
import chiseltest.simulator.VcsBackendAnnotation
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.Dependency
import chiseltest.internal.CachingAnnotation

object ExecDef extends SystemDef(coreCount = 1) {
  override val INIT_VEC = BigInt(0x80000000L)
}

class ExecTest(dut: system.RiscVSystem, file: String) {
  def doTest(bound: Int): Unit = {
    val beginTime = System.nanoTime

    // for timer interrupt test
    dut.clock.setTimeout(0)

    // avoid false assertions upon reset
    dut.io.axi.AWREADY.poke(false.B)
    dut.io.axi.WREADY.poke(false.B)
    dut.io.axi.BVALID.poke(false.B)
    dut.io.axi.ARREADY.poke(false.B)
    dut.io.axi.RVALID.poke(false.B)

    val axiDataWidth = ExecDef.L2.AXI_DATA_WIDTH
    val axiDataBytes = axiDataWidth / 8

    // reset for some time
    dut.reset.poke(true.B)
    dut.clock.step(16)
    dut.reset.poke(false.B)

    val mem: mutable.HashMap[Long, BigInt] = mutable.HashMap() // Addr to 4-byte
    var reading: Option[(Long, Long, Long, Long)] = None // ID, Ptr, Left, Size
    var writing: Option[(Long, Long, Long)] = None // Ptr, Left, Size
    var writingFinished = false

    // Load up memory
    val bytes = java.nio.file.Files.readAllBytes(Paths.get(file))
    val len = bytes.length
    val paddedLen = ((len + 3) / 4) * 4
    val padded = bytes ++ Array.fill(paddedLen - len)(0.asInstanceOf[Byte])
    val buffer = ByteBuffer.wrap(padded)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    val ints = buffer.asIntBuffer()
    var idx = 0
    while (ints.hasRemaining()) {
      var value = BigInt(ints.get())
      // make it positive because Java only has signed int
      if (value < 0) {
        value += BigInt(1) << 32
      }
      mem.put(idx * 4 + 0x80000000L, value)
      idx += 1
    }
    println(s"Initialized: $idx ints")
    // println(s"AXI bytes: $axiDataBytes")

    val axi = dut.io.axi
    val failed: mutable.HashSet[Long] = mutable.HashSet.empty

    // Test pass signal through tohost
    var finished = false
    for (i <- (0 until bound)) {
      // Always have interrupt 1 set at high
      dut.io.eints(1).poke(true.B)

      // println("Cycle: " + i)
      if (i % 10000 == 0) {
        println("Cycle: " + i)
        val mcycle = dut.io.debug(0).mcycle.peek.litValue
        val minstret = dut.io.debug(0).minstret.peek.litValue

        println(s"> mcycle: ${mcycle}")
        println(s"> minstret: ${minstret}")
      }
      dut.clock.step(1)

      // TODO: handles system
      if (dut.io.debug(0).pc.peek.litValue == 0x100000 || finished) {
        println(s"> Process ended at cycle ${i}")

        val mcycle = dut.io.debug(0).mcycle.peek.litValue
        val minstret = dut.io.debug(0).minstret.peek.litValue

        println(s"> mcycle: ${mcycle}")
        println(s"> minstret: ${minstret}")
        println(f"> IPC: ${BigDecimal(minstret) / BigDecimal(mcycle)}%.2f")

        val elapsed = System.nanoTime - beginTime
        val speed = BigDecimal(mcycle) * BigDecimal(1e9) / BigDecimal(elapsed)
        println(f"> Simulation speed: ${speed}%.2f mcycle/s")

        if (failed.size > 0) {
          throw new Error("Some testcases failed!")
        }

        return
      }

      // Simulate AXI
      // AR
      if (reading.isEmpty && axi.ARVALID.peek.litToBoolean == true) {
        axi.ARREADY.poke(true.B)
        reading = Some(
          (
            axi.ARID.peek.litValue.toLong,
            axi.ARADDR.peek.litValue.toLong,
            axi.ARLEN.peek.litValue.toLong,
            axi.ARSIZE.peek.litValue.toLong
          )
        )
        // println(s"Read: 0x${reading.get._2.toHexString}")
      } else {
        axi.ARREADY.poke(false.B)
      }

      // R
      if (reading.isDefined) {
        val (id, ptr, left, size) = reading.get
        axi.RVALID.poke(true.B)
        axi.RID.poke(id.U)
        val rdata = if (ptr == 0x10001014) { // LSR
          BigInt(1L << (32 + 5))
        } else {
          val aligned = (ptr / axiDataBytes) * axiDataBytes
          var res = BigInt(0)
          for (i <- 0 until axiDataBytes / 4) {
            val addr = aligned + i * 4
            val r = mem.get(addr).getOrElse(BigInt(0))
            res |= r << (i * 32)
          }
          res
        }
        val mask = (BigInt(1) << ((1 << size.toInt) * 8)) - 1L
        val shiftedMask =
          mask << ((ptr & (axiDataBytes - 1)) * 8).toInt
        // println(s"Raw: 0x${rdata.toString(16)}")
        // println(s"Mask: 0x${shiftedMask.toString(16)}")
        // println(s"Read: 0x${(rdata & shiftedMask).toString(16)}")
        axi.RDATA.poke((rdata & shiftedMask).U)
        axi.RLAST.poke((left == 0).B)
        // println(s"Returning: 0x${(shifted & mask).toHexString}")

        if (axi.RREADY.peek.litToBoolean == true) {
          if (left == 0) reading = None
          else reading = Some((id, ptr + (1L << size), left - 1, size))
        }
      } else {
        axi.RVALID.poke(false.B)
      }

      // AW
      if (writing.isEmpty && axi.AWVALID.peek.litToBoolean == true) {
        axi.AWREADY.poke(true.B)
        writing = Some(
          (
            axi.AWADDR.peek.litValue.toLong,
            axi.AWLEN.peek.litValue.toLong,
            axi.AWSIZE.peek.litValue.toLong
          )
        )
        writingFinished = false
        // println(s"Write: 0x${writing.get._1.toHexString}")
      } else {
        axi.AWREADY.poke(false.B)
      }

      // W
      if (writing.isDefined && !writingFinished) {
        val (ptr, left, size) = writing.get
        axi.WREADY.poke(true.B)
        val wdata = axi.WDATA.peek
        val wstrb = axi.WSTRB.peek

        if (axi.WVALID.peek.litToBoolean == true) {
          // compute byte mask from offset & size
          val mask = (BigInt(1) << (1 << size.toInt)) - 1L
          val shiftedMask =
            mask << (ptr & (axiDataBytes - 1)).toInt
          val aligned = (ptr / axiDataBytes) * axiDataBytes
          for (i <- 0 until axiDataBytes / 4) {
            val addr = aligned + i * 4
            val localWData = ((wdata.litValue >> (i * 32)) & 0xffffffffL).toLong
            val localWStrb = ((wstrb.litValue >> (i * 4)) & 0xf).toLong
            val localMask = ((shiftedMask >> (i * 4)) & 0xf).toLong
            if ((localMask & localWStrb) != 0) {
              val muxed = ExecTest.longMux(
                mem.get(addr).getOrElse(0),
                localWData,
                localMask & localWStrb
              )
              mem.put(addr, muxed)
              // println(s"Write ${muxed.toString(16)} to ${addr.toHexString}")
            }
          }

          writing match {
            case Some((addr, _, _)) if addr == 0x10001000L => {
              // Print to serial
              print((wdata.litValue & 0xff).toChar)
            }
            case Some((addr, _, _)) if addr == 0x60000000L => {
              // tohost in ISA testsuite
              val data = (wdata.litValue & 0xffffffff).toLong
              if (
                wdata.litValue == ((data & 0xff) | BigInt(
                  "0101000000000000",
                  16
                ))
              ) {
                // Is simple print
                print((data & 0xff).toChar)
              } else if (data == 1) {
                println("ISA testsuite pass.")
                finished = true
              } else if ((data & 1) == 1) {
                val c = data >> 1
                if (!failed.contains(c)) {
                  println(s"ISA testsuite failed case ${c}")
                  failed.add(c)
                  finished = true
                }
              } else {
                println(s"ISA testsuite tohost: ${data}")
              }
            }
            case _ => {}
          }

          writing = Some((ptr + (1L << size), left - 1, size))
          if (axi.WLAST.peek.litToBoolean == true) {
            assume(left == 0)
            writingFinished = true
          }
        }
      } else {
        axi.WREADY.poke(false.B)
      }

      if (writingFinished) {
        assume(writing.isDefined)
        axi.BVALID.poke(true.B)
        axi.BRESP.poke(0.U)
        axi.BID.poke(0.U)

        if (axi.BREADY.peek.litToBoolean == true) {
          writing = None
          writingFinished = false
        }
      } else {
        axi.BVALID.poke(false.B)
      }
    }

    throw new Error(s"Did not finished within ${bound} cycles")
  }

  doTest(100000)
}

object ExecTest {
  def longMux(
      base: BigInt,
      input: BigInt,
      be: Long
  ): BigInt = {
    var ret = BigInt(0)
    // println(s"Muxing: 0x${base.toString(16)} <- 0x${input.toString(16)} & ${be}")
    for (i <- (0 until 4)) {
      val strb = ((be >>> i) & 1)
      val sel = if (strb == 1) {
        (input >> (i * 8)) & 0xff
      } else {
        (base >> (i * 8)) & 0xff
      }
      ret = (sel << (i * 8)) | ret
    }
    // println(s"Result: 0x${ret.toString(16)}")

    ret
  }
}

object ExecSpec {
  val cases = List(
    ("Load/Store", "./testcases/meow/bin/load-store.bin"),
    ("Uncached Load/Store", "./testcases/meow/bin/load-store-uncached.bin")
  )
}

class ExecSpec extends AnyFlatSpec with Matchers with ChiselScalatestTester {
  behavior of "ExecSpec"

  it should s"run successfully" in {
    test(
      new system.RiscVSystem()(ExecDef)
    ).withAnnotations(Simulator.getAnnotations()) { dut =>
      for ((desc, file) <- ExecSpec.cases) {
        println("------------")
        println(s"Running file $file")
        new ExecTest(dut, file)
      }
    }
  }
}

object Simulator {

  /** check if vcs is found */
  def vcsFound(): Boolean = {
    os.proc("which", "vcs").call(check = false).exitCode == 0
  }

  /** check if icarus verilog is found */
  def icarusFound(): Boolean = {
    os.proc("which", "iverilog").call(check = false).exitCode == 0
  }

  /** check if verilator is found */
  def verilatorFound(): Boolean = {
    os.proc("which", "verilator").call(check = false).exitCode == 0
  }

  /** get annotations for chiseltest */
  def getAnnotations(
      useVCS: Boolean = false,
      useIcarus: Boolean = false,
      useVerilator: Boolean = true
  ): AnnotationSeq = {
    var annotations: AnnotationSeq = if (vcsFound && useVCS) {
      println("Using VCS")
      Seq(
        VcsBackendAnnotation
      )
    } else if (icarusFound && useIcarus) {
      println("Using Icarus Verilog")
      Seq(
        IcarusBackendAnnotation
      )
    } else if (verilatorFound && useVerilator) {
      println("Using Verilator")
      Seq(
        VerilatorBackendAnnotation,
        CachingAnnotation
      )
    } else {
      throw new RuntimeException("No usable simulator")
      Seq()
    }
    annotations = annotations ++
      Seq(
        RunFirrtlTransformAnnotation(Dependency(ZeroInit)) // for RRArbiter
      )
    // do not write vcd in ci by default
    if (
      System
        .getenv("GITHUB_ACTIONS") != null || System.getenv("GITLAB_CI") != null
    ) {
      println("CI detected")
      annotations
    } else {
      // fst is smaller than vcd
      annotations ++ Seq(WriteVcdAnnotation)
    }
  }
}
