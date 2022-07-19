package meowv64.sram

import firrtl._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.Dependency
import firrtl.passes.PassException
import firrtl.transforms.DedupModules

// adapted from https://github.com/chipsalliance/chisel3/issues/1059#issuecomment-814353578

/** Specifies a global prefix for all module names. */
case class ModulePrefix(prefix: String, main: String) extends NoTargetAnnotation

/** FIRRTL pass to add prefix to module names
  */
object PrefixModulesPass extends Transform with DependencyAPIMigration {
  // we run after deduplication to save some work
  override def prerequisites = Seq(Dependency[DedupModules])

  // we do not invalidate the results of any prior passes
  override def invalidates(a: Transform) = false

  // map module name to new
  def mapModuleName(prefix: String, main: String, orig: String): String = {
    if (orig == main) {
      prefix
    } else {
      prefix + '_' + orig
    }
  }

  override protected def execute(state: CircuitState): CircuitState = {
    val prefixes = state.annotations.collect { case a: ModulePrefix =>
      (a.prefix, a.main)
    }.distinct
    prefixes match {
      case Seq() =>
        logger.info("[PrefixModulesPass] No ModulePrefix annotation found.")
        state
      case Seq((prefix, main)) =>
        val c = state.circuit.mapModule(onModule(_, prefix, main))
        state.copy(circuit = c.copy(main = mapModuleName(prefix, main, c.main)))
      case other =>
        throw new PassException(
          s"[PrefixModulesPass] found more than one prefix annotation: $other"
        )
    }
  }

  private def onModule(m: ir.DefModule, prefix: String, main: String): ir.DefModule =
    m match {
      case e: ir.ExtModule => e.copy(name = mapModuleName(prefix, main, e.name))
      case mod: ir.Module =>
        val name = mapModuleName(prefix, main, mod.name)
        val body = onStmt(mod.body, prefix, main)
        mod.copy(name = name, body = body)
    }

  private def onStmt(s: ir.Statement, prefix: String, main: String): ir.Statement = s match {
    case i: ir.DefInstance => i.copy(module = mapModuleName(prefix, main, i.module))
    case other             => other.mapStmt(onStmt(_, prefix, main))
  }
}
