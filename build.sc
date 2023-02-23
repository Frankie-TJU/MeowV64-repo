import mill._
import mill.scalalib.publish._
import scalalib._
import scalafmt._
import coursier.maven.MavenRepository
import $ivy.`com.goyeau::mill-scalafix_mill0.10:0.2.8`
import com.goyeau.mill.scalafix.ScalafixModule

// learned from https://github.com/OpenXiangShan/fudian/blob/main/build.sc
val defaultVersions = Map(
  "chisel3" -> ("edu.berkeley.cs", "3.5.3", false),
  "chisel3-plugin" -> ("edu.berkeley.cs", "3.5.3", true),
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

trait CommonScalaModule extends ScalaModule {
  def scalaVersion = commonScalaVersion

  // for snapshot dependencies
  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    )
  }

  def scalacOptions = super.scalacOptions() ++
    Seq("-Ywarn-unused", "-Ywarn-adapted-args", "-deprecation") // for scalafix

}

trait CommonSbtModule extends SbtModule {
  def scalaVersion = commonScalaVersion

  // for snapshot dependencies
  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    )
  }

  def scalacOptions = super.scalacOptions() ++
    Seq("-Ywarn-unused", "-Ywarn-adapted-args", "-deprecation") // for scalafix

}

object hardfloat extends CommonSbtModule {
  override def millSourcePath = os.pwd / "submodules" / "berkeley-hardfloat"

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel3")
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel3-plugin")
  )
}

object cde extends CommonScalaModule {
  override def millSourcePath =
    os.pwd / "submodules" / "cde" / "cde"
}

object rocketChipMacros extends CommonScalaModule {
  override def millSourcePath = os.pwd / "submodules" / "rocket-chip" / "macros"

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scala-lang:scala-reflect:$commonScalaVersion"
  )
}

object rocketChip extends CommonSbtModule {
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

  override def scalacOptions = super.scalacOptions() ++
    Seq("-deprecation", "-unchecked", "-Xsource:2.11")
}

object inclusiveCache extends CommonScalaModule {
  override def millSourcePath =
    os.pwd / "submodules" / "block-inclusivecache-sifive" / "design" / "craft" / "inclusivecache"

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel3-plugin")
  )

  override def moduleDeps =
    super.moduleDeps ++ Seq(rocketChip)
}

object difftest extends CommonScalaModule {
  override def millSourcePath =
    os.pwd / "submodules" / "difftest"

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel3-plugin")
  )

  override def moduleDeps =
    super.moduleDeps ++ Seq(rocketChip)
}

object meowv64 extends CommonSbtModule with ScalafmtModule with ScalafixModule {
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

  override def moduleDeps =
    super.moduleDeps ++ Seq(hardfloat, cde, rocketChip, inclusiveCache, difftest)

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
