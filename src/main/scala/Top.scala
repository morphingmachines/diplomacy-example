package adder

import circt.stage.ChiselStage
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.ElaborationArtefacts
import org.chipsalliance.cde.config.{Config, Parameters}

import java.io._
import java.nio.file._

trait Toplevel {
  def topModule: chisel3.RawModule
  def topclass_name = topModule.getClass().getName().split("\\$").mkString(".")

  def generated_sv_dir = s"generated_sv_dir/${topclass_name}"

  /** For firtoolOpts run `firtool --help` There is an overlap between ChiselStage args and firtoolOpts.
    *
    * TODO: Passing "--Split-verilog" "--output-annotation-file" to firtool is not working.
    */

  lazy val chiselArgs   = Array("--full-stacktrace", "--target-dir", generated_sv_dir, "--split-verilog")
  lazy val firtroolArgs = Array("-dedup")

  def chisel2firrtl() = {
    val str_firrtl = ChiselStage.emitCHIRRTL(topModule, args = Array("--full-stacktrace"))
    Files.createDirectories(Paths.get("generated_sv_dir"))
    val pw = new PrintWriter(new File(s"${generated_sv_dir}.fir"))
    pw.write(str_firrtl)
    pw.close()
  }

  // Call this only after calling chisel2firrtl()
  def firrtl2sv() =
    os.proc(
      "firtool",
      s"${generated_sv_dir}.fir",
      "--disable-annotation-unknown",
      "--split-verilog",
      s"-o=${generated_sv_dir}",
      s"--output-annotation-file=${generated_sv_dir}/${topclass_name}.anno.json",
    ).call() // check additional options with "firtool --help"

}

trait LazyToplevel extends Toplevel {
  def lazyTop: LazyModule
  override def topModule     = lazyTop.module
  override def topclass_name = lazyTop.getClass().getName().split("\\$").mkString(".")

  def genDiplomacyGraph() = {
    ElaborationArtefacts.add("graphml", lazyTop.graphML)
    ElaborationArtefacts.files.foreach {
      case ("graphml", graphML) =>
        val fw = new FileWriter(new File(s"${generated_sv_dir}", s"${lazyTop.className}.graphml"))
        fw.write(graphML())
        fw.close()
      case _ =>
    }
  }

  def showModuleComposition(gen: => LazyModule) = {
    println("List of Diplomatic Nodes (Ports)")
    gen.getNodes.map(x => println(s"Class Type:  ${x.getClass.getName()} | node: ${x.name} (${x.description})"))
    println("")
    println("List of Sub Modules")
    gen.getChildren.map(x => println("Class Type: " + x.getClass.getName() + "| Instance name:" + x.name))
  }

}

/** To run from a terminal shell
  * {{{
  * mill adder.runMain adder.diplomacyExample
  * }}}
  */
object diplomacyExample extends App with LazyToplevel {

  val lazyTop = args(0) match {
    case "example1" => LazyModule(new example1.AdderTestHarness()(Parameters.empty))
    case "example2" => LazyModule(new example2.AdderTestHarness()(Parameters.empty))
    /** example3 has AdderTestHarness3 and AdderTestHarness2, with two nodes and three nodes of "AdderMonitor" module.
      */
    case "example3" => LazyModule(new example3.AdderTestHarness2()(Parameters.empty))
    /** Choose the number of diplomacy nodes for "AdderMonitor" module through an implicit config parameter
      */
    case "example4" =>
      LazyModule(
        new example4.AdderTestHarness()(
          new Config(new example4.DiplomacyExampleConfig).alterMap(Map((example4.NumMonitorNodes, 3))),
        ),
      )
    /** Choose the number of diplomacy nodes for "AdderMonitor" module through an defining the TopLevel class using
      * mixins
      */
    case "example5" =>
      LazyModule(
        new example5.AdderTestHarness()(new Config(new example5.DiplomacyExampleConfig)) with example5.HasOneNodeMonitor,
      )
  }
  chisel2firrtl()
  firrtl2sv()
  genDiplomacyGraph()

  showModuleComposition(lazyTop)
}


object SVGenTop extends App {
  ChiselStage.emitSystemVerilogFile(LazyModule(new example1.AdderTestHarness()(Parameters.empty)).module)
}