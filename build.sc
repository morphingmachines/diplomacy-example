// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import scalalib._
import publish._
// support BSP
import mill.bsp._

import scalafmt._
import $ivy.`com.goyeau::mill-scalafix::0.3.1`
import com.goyeau.mill.scalafix.ScalafixModule

import $file.^.playground.build
import $file.^.playground.dependencies.cde.build
import $file.^.playground.dependencies.`rocket-chip`.common

object ivys {
  val cv = ^.playground.build.ivys.cv
  // val cv = "5.1.0"
}

object macros extends ^.playground.dependencies.`rocket-chip`.common.MacrosModule with SbtModule {
  override def millSourcePath = os.pwd / os.up / "playground" / "dependencies" / "rocket-chip" / "macros"
  def scalaVersion: T[String] = T(^.playground.build.ivys.sv)
  def scalaReflectIvy = ^.playground.build.ivys.scalaReflect
}

object mycde extends ^.playground.dependencies.cde.build.CDE with PublishModule {
  override def millSourcePath = os.pwd / os.up / "playground" / "dependencies" / "cde" / "cde"
}

object myrocketchip extends ^.playground.dependencies.`rocket-chip`.common.RocketChipModule with SbtModule {

  override def millSourcePath = os.pwd / os.up / "playground" / "dependencies" / "rocket-chip"

  override def scalaVersion = ^.playground.build.ivys.sv

  def chiselModule = None

  def chiselPluginJar = None

  def chiselIvy = Some(^.playground.build.ivys.chiselCrossVersions(ivys.cv)._1)

  def chiselPluginIvy = Some(^.playground.build.ivys.chiselCrossVersions(ivys.cv)._2)

  override def ivyDeps             = T(super.ivyDeps() ++ chiselIvy)
  override def scalacPluginIvyDeps = T(super.scalacPluginIvyDeps() ++ chiselPluginIvy)

  def macrosModule = macros

  def hardfloatModule: ScalaModule = myhardfloat

  def cdeModule: ScalaModule = mycde

  def mainargsIvy = ^.playground.build.ivys.mainargs

  def json4sJacksonIvy = ^.playground.build.ivys.json4sJackson

}

object inclusivecache extends ^.playground.build.CommonModule {
  override def millSourcePath =
    os.pwd / os.up / "playground" / "dependencies" / "rocket-chip-inclusive-cache" / "design" / "craft" / "inclusivecache"
  override def moduleDeps = super.moduleDeps ++ Seq(myrocketchip)
}

object blocks extends ^.playground.build.CommonModule with SbtModule {
  override def millSourcePath = os.pwd / os.up / "playground" / "dependencies" / "rocket-chip-blocks"
  override def moduleDeps     = super.moduleDeps ++ Seq(myrocketchip)
}

object shells extends ^.playground.build.CommonModule with SbtModule {
  override def millSourcePath = os.pwd / os.up / "playground" / "dependencies" / "rocket-chip-fpga-shells"
  override def moduleDeps     = super.moduleDeps ++ Seq(myrocketchip, blocks)
}

// UCB
object myhardfloat extends ScalaModule with SbtModule with PublishModule {
  override def millSourcePath = os.pwd / os.up / "playground" / "dependencies" / "berkeley-hardfloat"
  def scalaVersion            = ^.playground.build.ivys.sv

  def chiselIvy = Some(^.playground.build.ivys.chiselCrossVersions(ivys.cv)._1)

  def chiselPluginIvy = Some(^.playground.build.ivys.chiselCrossVersions(ivys.cv)._2)

  override def ivyDeps             = T(super.ivyDeps() ++ chiselIvy)
  override def scalacPluginIvyDeps = T(super.scalacPluginIvyDeps() ++ chiselPluginIvy)
  // remove test dep
  override def allSourceFiles = T(
    super.allSourceFiles().filterNot(_.path.last.contains("Tester")).filterNot(_.path.segments.contains("test")),
  )

  def publishVersion = de.tobiasroeser.mill.vcs.version.VcsVersion.vcsState().format()

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "edu.berkeley.cs",
    url = "http://chisel.eecs.berkeley.edu",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("ucb-bar", "berkeley-hardfloat"),
    developers = Seq(
      Developer("jhauser-ucberkeley", "John Hauser", "https://www.colorado.edu/faculty/hauser/about/"),
      Developer("aswaterman", "Andrew Waterman", "https://aspire.eecs.berkeley.edu/author/waterman/"),
      Developer("yunsup", "Yunsup Lee", "https://aspire.eecs.berkeley.edu/author/yunsup/"),
    ),
  )
}

trait ScalacOptions extends ScalaModule {
  override def scalacOptions = T {
    super.scalacOptions() ++ Seq(
      "-unchecked",
      "-deprecation",
      "-language:reflectiveCalls",
      "-feature",
      "-Xcheckinit",
      "-Xfatal-warnings",
      "-Ywarn-dead-code",
      "-Ywarn-unused",
    )
  }
}

// Replace "adder" with your %PROJECT-NAME%
object adder
  extends ^.playground.build.CommonModule
  with SbtModule
  with ScalafmtModule
  with ScalafixModule
  with ScalacOptions {
  override def millSourcePath = os.pwd / "adder"

  override def moduleDeps = super.moduleDeps ++ Seq(myrocketchip)

  object test extends SbtModuleTests with ScalaTest with ScalafmtModule with ScalafixModule {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ^.playground.build.ivys.scalatest,
      ^.playground.build.ivys.chiseltestCrossVersions(ivys.cv),
      ^.playground.build.ivys.oslib,
    )

  }
}
