/** Extends the example2 with AdderMonitor that can have two sink nodes or three sink nodes, but implements the same
  * functionality.
  *
  * This demonstrates that each bind operator (:=) on node creates an edge with an index, and nodes have a Sequence/List
  * of edges. And sometimes, we should rely on the index of the edge to define how the interface gets driven or used.
  */

package adder.example3

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util.random.FibonacciLFSR
import chisel3.util.{Decoupled, DecoupledIO}
import freechips.rocketchip.diplomacy.{
  LazyModule,
  LazyModuleImp,
  NexusNode,
  RenderedEdge,
  SimpleNodeImp,
  SinkNode,
  SourceNode,
  ValName,
}
import org.chipsalliance.cde.config._

case class UpwardParam(width: Int)
case class DownwardParam(width: Int)
case class EdgeParam(width: Int)

object AdderNodeImp extends SimpleNodeImp[DownwardParam, UpwardParam, EdgeParam, DecoupledIO[UInt]] {

  def edge(pd: DownwardParam, pu: UpwardParam, p: Parameters, sourceInfo: SourceInfo) =
    if (pd.width > pu.width) EdgeParam(pu.width) else EdgeParam(pd.width)

  def bundle(e: EdgeParam): DecoupledIO[UInt] = Decoupled(UInt(e.width.W))

  override def render(e: EdgeParam): RenderedEdge = RenderedEdge("blue", s"width=${e.width}")
}

class AdderDriverNode(widths: Seq[DownwardParam])(implicit valName: ValName) extends SourceNode(AdderNodeImp)(widths)

class AdderDriver(width: Int, numOutputs: Int)(implicit p: Parameters) extends LazyModule {
  val node        = new AdderDriverNode(Seq.fill(numOutputs)(DownwardParam(width)))
  lazy val module = new AdderDriverModule(this)
  override lazy val desiredName: String = "AdderDriver"

}

class AdderDriverModule(outer: AdderDriver) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val opValid = Input(Bool())
  })
  val negotiatedWidths = outer.node.edges.out.map(_.width)
  require(negotiatedWidths.forall(_ == negotiatedWidths.head), "outputs of the driver node must have the same width")
  val finalWidth = negotiatedWidths.head

  // generate random addend
  val randomAddent =
    FibonacciLFSR.maxPeriod(finalWidth, increment = outer.node.out.unzip._1.map(_.ready).reduce(_ && _))

  outer.node.out.foreach { case (addend, _) => addend.bits := randomAddent }
  outer.node.out.foreach { case (addend, _) => addend.valid := io.opValid }

}

class AdderMonitorNode(width: UpwardParam)(implicit valName: ValName) extends SinkNode(AdderNodeImp)(Seq(width))

class AdderMonitorDriverNode(widths: Seq[UpwardParam])(implicit valName: ValName) extends SinkNode(AdderNodeImp)(widths)

class AdderMonitor(width: Int, numOperands: Int)(implicit p: Parameters) extends LazyModule {
  val nodeSeq                   = Seq.fill(numOperands)(new AdderMonitorNode(UpwardParam(width))(ValName("driver")))
  val nodeSum                   = new AdderMonitorNode(UpwardParam(width))(ValName("adder"))
  lazy val module               = new AdderMonitorModule(this)
  override lazy val desiredName = "AdderMonitor"
}

class AdderMonitorModule(outer: AdderMonitor) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val error = Output(Bool())
  })
  printf(outer.nodeSeq.map(node => p"${node.in.head._1}").reduce(_ + p" + " + _) + p"= ${outer.nodeSum.in.head._1}\n")
  io.error := outer.nodeSum.in.head._1.bits =/= outer.nodeSeq.map(_.in.head._1.bits).reduce(_ + _)
  val ports = (outer.nodeSeq.map(_.in.head._1) ++ Seq(outer.nodeSum.in.head._1)).zipWithIndex

  ports.foreach { case (in, i) =>
    in.ready := ports.filter { case (_, id) => id != i }.unzip._1.map(_.valid).reduce(_ && _)
  }
}

class AdderMonitor2Nodes(width: Int, numOperands: Int)(implicit p: Parameters) extends LazyModule {
  val nodeSeq                   = new AdderMonitorDriverNode(Seq.fill(numOperands)(UpwardParam(width)))
  val nodeSum                   = new AdderMonitorNode(UpwardParam(width))(ValName("adder"))
  lazy val module               = new AdderMonitor2NodesModule(this)
  override lazy val desiredName = "AdderMonitor2Nodes"
}

class AdderMonitor2NodesModule(outer: AdderMonitor2Nodes) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val error = Output(Bool())
  })
  printf(outer.nodeSeq.in.map(x => p"${x._1}").reduce(_ + p" + " + _) + p"= ${outer.nodeSum.in.head._1}\n")
  io.error := outer.nodeSum.in.head._1.bits =/= outer.nodeSeq.in.map(x => x._1.bits).reduce(_ + _)
  val ports = (outer.nodeSeq.in.map(_._1) ++ Seq(outer.nodeSum.in.head._1)).zipWithIndex

  ports.foreach { case (in, i) =>
    in.ready := ports.filter { case (_, id) => id != i }.unzip._1.map(_.valid).reduce(_ && _)
  }
}

class AdderNode(
  dFn: Seq[DownwardParam] => DownwardParam,
  uFn: Seq[UpwardParam] => UpwardParam,
)(
  implicit valName: ValName,
) extends NexusNode(AdderNodeImp)(dFn, uFn)

class Adder(implicit p: Parameters) extends LazyModule {
  val node = new AdderNode(
    { case dps: Seq[DownwardParam] =>
      dps.foreach(x => println(s"the width of the $x in ${x.width}"))
      require(dps.forall(dp => dp.width == dps.head.width), "inward, downward adder widths must be equivalent")
      dps.head
    },
    { case ups: Seq[UpwardParam] =>
      ups.foreach(x => println(s"the width of the $x in ${x.width}"))
      require(ups.forall(up => up.width == ups.head.width), "outward, upward adder width must be equivalent")
      ups.head
    },
  )
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size >= 2)
    node.out.head._1.bits  := node.in.unzip._1.map(_.bits).reduce(_ + _)
    node.out.head._1.valid := node.in.unzip._1.map(_.valid).reduce(_ && _)
    node.in.unzip._1.foreach(_.ready := node.out.head._1.fire)
  }

  override lazy val desiredName = "Adder"
}

class AdderTestHarness3()(implicit p: Parameters) extends LazyModule {
  println(s"Begin Test-Harness $p")
  val numOperands  = 2
  val driverWidth  = 7
  val monitorWidth = 9
  val adder        = LazyModule(new Adder())
  val drivers      = Seq.fill(numOperands)(LazyModule(new AdderDriver(width = driverWidth, numOutputs = numOperands)))

  val monitor = LazyModule(new AdderMonitor(width = monitorWidth, numOperands = numOperands))
  drivers.zip(monitor.nodeSeq).foreach { case (driver, operandMonitor) => operandMonitor := driver.node }
  drivers.foreach(driver => adder.node := driver.node)
  monitor.nodeSum := adder.node

  lazy val module = new AdderTestHarness3Imp(this)
  override lazy val desiredName: String = "AdderTestHarness"
  println(s"End Test-Harness $p")
}

class AdderTestHarness3Imp(outer: AdderTestHarness3) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val op1Valid = Input(Bool())
    val op2Valid = Input(Bool())
  })
  outer.drivers(0).module.io.opValid := io.op1Valid
  outer.drivers(1).module.io.opValid := io.op2Valid
  when(outer.monitor.module.io.error) {
    printf("something went wrong")
  }
}

class AdderTestHarness2()(implicit p: Parameters) extends LazyModule {
  println(s"Begin Test-Harness $p")
  val numOperands  = 2
  val driverWidth  = 7
  val monitorWidth = 9
  val adder        = LazyModule(new Adder())
  val drivers      = Seq.fill(numOperands)(LazyModule(new AdderDriver(width = driverWidth, numOutputs = numOperands)))

  val monitor = LazyModule(new AdderMonitor2Nodes(width = monitorWidth, numOperands = numOperands))
  drivers.foreach(driver => monitor.nodeSeq := driver.node)
  drivers.foreach(driver => adder.node := driver.node)
  monitor.nodeSum := adder.node

  lazy val module = new AdderTestHarness2Imp(this)
  override lazy val desiredName: String = "AdderTestHarness"
  println(s"End Test-Harness $p")
}
class AdderTestHarness2Imp(outer: AdderTestHarness2) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val op1Valid = Input(Bool())
    val op2Valid = Input(Bool())
  })
  outer.drivers(0).module.io.opValid := io.op1Valid
  outer.drivers(1).module.io.opValid := io.op2Valid
  when(outer.monitor.module.io.error) {
    printf("something went wrong")
  }
}
