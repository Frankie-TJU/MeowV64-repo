import mill._
import mill.scalalib.publish._
import scalalib._
import scalafmt._
import coursier.maven.MavenRepository
import $ivy.`com.goyeau::mill-scalafix_mill0.10:0.2.8`
import com.goyeau.mill.scalafix.ScalafixModule

// learned from https://github.com/OpenXiangShan/fudian/blob/main/build.sc
val defaultVersions = Map(
  "chisel3" -> ("edu.berkeley.cs", "3.5.2", false),
  "chisel3-plugin" -> ("edu.berkeley.cs", "3.5.2", true),
  "paradise" -> ("org.scalamacros", "2.1.1", true),
  "json4s-jackson" -> ("org.json4s", "3.6.1", false),
  "chiseltest" -> ("edu.berkeley.cs", "0.5.1", false),
  "scalatest" -> ("org.scalatest", "3.2.10", false)
)

val commonScalaVersion = "2.12.13"

def getVersion(dep: String) = {
  val (org, ver, cross) = defaultVersions(dep)
  val version = sys.env.getOrElse(dep + "Version", ver)
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

trait CommonModule extends ScalaModule {
  def scalaVersion = commonScalaVersion

  // for snapshot dependencies
  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    )
  }

  override def scalacOptions = super.scalacOptions() ++
    Seq("-deprecation", "-unchecked", "-Xsource:2.11") ++ // for chisel3
    Seq("-Ywarn-unused", "-Ywarn-adapted-args", "-deprecation") // for scalafix

}

object hardfloat extends CommonModule with ScalaModule {
  override def millSourcePath = os.pwd / "submodules" / "berkeley-hardfloat"

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel3"),
    getVersion("scalatest")
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel3-plugin")
  )
}

object cde extends CommonModule with ScalaModule {
  override def millSourcePath =
    os.pwd / "submodules" / "cde" / "cde"
}

object rocketChipMacros extends CommonModule with ScalaModule {
  override def millSourcePath = os.pwd / "submodules" / "rocket-chip" / "macros"

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scala-lang:scala-reflect:$commonScalaVersion"
  )
}

object rocketChip extends CommonModule with SbtModule {
  override def millSourcePath = os.pwd / "submodules" / "rocket-chip"

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel3"),
    getVersion("json4s-jackson"),
    ivy"org.scala-lang:scala-reflect:$commonScalaVersion"
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel3-plugin"),
    getVersion("paradise")
  )

  override def moduleDeps =
    super.moduleDeps ++ Seq(hardfloat, rocketChipMacros, cde)
}

object meowv64
    extends CommonModule
    with ScalafmtModule
    with ScalafixModule
    with SbtModule {
  override def millSourcePath = os.pwd

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel3"),
    getVersion("scalatest"),
    getVersion("chiseltest")
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel3-plugin")
  )

  override def scalafixIvyDeps = Agg(
    ivy"com.github.liancheng::organize-imports:0.5.0"
  )

  override def moduleDeps = super.moduleDeps ++ Seq(hardfloat, rocketChip)

  object test
      extends Tests
      with TestModule.ScalaTest
      with ScalafmtModule
      with ScalafixModule {
    override def ivyDeps = super.ivyDeps() ++ Agg()

    override def scalafixIvyDeps = Agg(
      ivy"com.github.liancheng::organize-imports:0.5.0"
    )

    override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
      getVersion("chisel3-plugin")
    )
  }
}
