package meowv64

import chisel3.stage.ChiselGeneratorAnnotation
import meowv64.system.RiscVSystem
import meowv64.system.SystemDef

object Main extends App {
  val (className, rest) = if (args.length > 0) {
    (args.head, args.tail)
  } else {
    ("meowv64.system.DefaultSystemDef", args)
  }

  println(s"Using config ${className}")
  val conf = Class
    .forName(className)
    .getDeclaredConstructor()
    .newInstance()
    .asInstanceOf[SystemDef]
  // generate verilog & system verilog
  val annotations =
    Seq(
      ChiselGeneratorAnnotation(() => new RiscVSystem()(conf)),
    )

  (new chisel3.stage.ChiselStage()).execute(
    Array("-X", "verilog", "-firw", "-gmv", "full") ++ rest,
    annotations
  )

  (new chisel3.stage.ChiselStage()).execute(
    Array("-X", "sverilog") ++ rest,
    annotations
  )
}
