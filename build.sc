import mill._
import mill.scalalib.publish._
import scalalib._
import scalafmt._
import coursier.maven.MavenRepository
import $ivy.`com.goyeau::mill-scalafix::0.4.2`
import com.goyeau.mill.scalafix.ScalafixModule

// learned from https://github.com/OpenXiangShan/fudian/blob/main/build.sc
val defaultVersions = Map(
  "chisel" -> ("org.chipsalliance", "6.6.0", false),
  "chisel-plugin" -> ("org.chipsalliance", "6.6.0", true),
  "json4s-jackson" -> ("org.json4s", "4.0.6", false),
  "chiseltest" -> ("edu.berkeley.cs", "0.6.0-RC3", false),
  "scalatest" -> ("org.scalatest", "3.2.15", false),
  "sourcecode" -> ("com.lihaoyi", "0.3.1", false),
  "mainargs" -> ("com.lihaoyi", "0.5.0", false),
)

val commonScalaVersion = "2.13.15"

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
    Seq("-Ywarn-unused", "-deprecation") // for scalafix

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
    Seq("-Ywarn-unused", "-deprecation") // for scalafix

}

object hardfloat extends CommonSbtModule {
  override def millSourcePath = os.pwd / "submodules" / "berkeley-hardfloat" / "hardfloat"

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel")
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel-plugin")
  )
}

object cde extends CommonScalaModule {
  override def millSourcePath =
    os.pwd / "submodules" / "cde" / "cde"
}

object diplomacy extends CommonScalaModule {
  override def millSourcePath = os.pwd / "submodules" / "diplomacy" / "diplomacy"

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel"),
    getVersion("sourcecode"),
  )

  override def moduleDeps =
    super.moduleDeps ++ Seq(
      cde
    )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel-plugin")
  )
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
    getVersion("chisel"),
    getVersion("mainargs"),
    getVersion("json4s-jackson"),
    ivy"org.scala-lang:scala-reflect:$commonScalaVersion"
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel-plugin"),
  )

  override def moduleDeps =
    super.moduleDeps ++ Seq(hardfloat, rocketChipMacros, cde, diplomacy)

  override def scalacOptions = super.scalacOptions() ++
    Seq("-deprecation", "-unchecked")
}

object inclusiveCache extends CommonScalaModule {
  override def millSourcePath =
    os.pwd / "submodules" / "block-inclusivecache-sifive" / "design" / "craft" / "inclusivecache"

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel-plugin")
  )

  override def moduleDeps =
    super.moduleDeps ++ Seq(rocketChip)
}

object meowv64 extends CommonSbtModule with ScalafmtModule with ScalafixModule {
  override def millSourcePath = os.pwd

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel"),
    getVersion("scalatest"),
    getVersion("chiseltest")
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel-plugin"),
  )

  override def scalafixIvyDeps = Agg(
    ivy"com.github.liancheng::organize-imports:0.5.0"
  )

  override def moduleDeps =
    super.moduleDeps ++ Seq(hardfloat, cde, rocketChip, inclusiveCache)
}
