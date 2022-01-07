import chiseltest.ChiselScalatestTester
import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.WriteVcdAnnotation
import meowv64.ExecDef
import meowv64.ExecTest
import meowv64.multicore.Multicore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

object RiscvTestsSpec {
  val knownFails = Seq("rv64mi-p-scall.bin")
  val cases = new File("./testcases/riscv-tests/isa").listFiles
    .filter(_.isFile)
    .filter(_.getName.endsWith(".bin"))
    .filter(f =>
      knownFails.foldLeft(true)((ret, cur) => ret && !f.getName().endsWith(cur))
    )
    .map(_.getPath)
    .toList
}

class RiscvTestsSpec
    extends AnyFlatSpec
    with Matchers
    with ChiselScalatestTester {
  behavior of "RiscvTestsSpec"

  for (file <- RiscvTestsSpec.cases) {
    it should s"run $file successfully" in {
      println(s"------------\nRunning file $file")
      test(
        new Multicore()(ExecDef)
      ).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) {
        new ExecTest(_, file)
      }
    }
  }
}

object RiscvTestsMain extends App {
  (new RiscvTestsSpec).execute(stats = true, shortstacks = true)
}
