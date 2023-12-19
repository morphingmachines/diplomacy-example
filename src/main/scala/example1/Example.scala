/** A simple example to demonstrate Diplomacy framework
  */

package adder.example1

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util.random.FibonacciLFSR
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
import org.chipsalliance.cde.config.Parameters

case class UpwardParam(width: Int)
case class DownwardParam(width: Int)
case class EdgeParam(width: Int)

/** Node implementation basically defines two things
  *
  * (1) How edge parameters are computed or negotiated from Downward and Upward flowing parameters
  *
  * (2) How hardware interface (Bundle) is defined from edge parameters at both input and out edges.
  *
  * A SimpleNodeImp resolves/negotiates the edge-parameters at all the end-points of its edges in the same way. The
  * negotiated parameters are called "Edge parameters".
  */
object AdderNodeImp extends SimpleNodeImp[DownwardParam, UpwardParam, EdgeParam, UInt] {

  /** Each edge has parameters flowing from "source to sink" (called Downward-flowing parameters) and "sink to source"
    * (called Upward-flowing parameters). The Edge parameter is computed/determined as a function of downward-flowing
    * and Upward-flowing parameters.
    *
    * In this node the "edge parameters" are computed/defined in the same way at both end-points of the edge. At the
    * sink end-point of the edge, we compute the Edge parameter of an input-port. At the source end-point of the edge,
    * we compute the Edge parameter of an output-port. Edge parameters at the source end are called Outward Edge
    * parameters (EO). Edge parameters at the sink end are called Inward Edge parameters (EI).
    *
    * Outward Edge parameters (EO) is computed as a function of Downward-flowing parameters generated at the output port
    * of the node (DO) and Upward-flowing parameters received at the output port (UO).
    *
    * Similarly, Inward Edge parameters (EI) is computed as a function of Downward-flowing parameters received at the
    * input port of the node (DI) and Upward-flowing parameters generated at the input port (UI).
    */
  def edge(pd: DownwardParam, pu: UpwardParam, p: Parameters, sourceInfo: SourceInfo) =
    if (pd.width > pu.width) EdgeParam(pu.width) else EdgeParam(pd.width)

  /** The negotiated edge parameter at the end-point is used to define the Bundle (chisel type) that represents the
    * hardware interface at the end-point.
    *
    * The bundle chisel-type is used as it is for outer side (source end-point) interface. If the interface is at the
    * inner side (sink end-point) then bundle gets automatically "Flipped" to match the IO direction.
    */
  def bundle(e: EdgeParam): UInt = UInt(e.width.W)

  override def render(e: EdgeParam): RenderedEdge = RenderedEdge("blue", s"width=${e.width}")
}

/** AdderDriverNode is a source node as it don't have any input diplomacy-interface.
  *
  * With only output diplomacy-interfaces, it only have edges coming out of this node. And it sends Downward-flowing
  * Outward parameters (DO) along the edges and receives Upward-flowing Outward parameters (UO) along the edges.
  *
  * For a source node the DO parameters must be explicitly specified as the part of node definition.
  *
  * The "AdderDriver" module has its output interfaces connected to both "Adder" module and "Monitor" module. For two
  * output interfaces, we will have two edges coming out of "AdderDriver" connecting to "Adder" and "Monitor".
  *
  * For the two edges connected to the outward-side of this node, we need two values of type DownwardParam, defined as
  * Seq[DownwardParam] type.
  */
class AdderDriverNode(widths: Seq[DownwardParam])(implicit valName: ValName) extends SourceNode(AdderNodeImp)(widths)

/** AdderMonitorNode is a sink node as it don't have any output diplomacy-interface.
  *
  * With only input diplomacy-interfaces, it only have edges coming into this node. And it sends Upward-flowing Inward
  * parameters (UI) and receive Downward-flowing Inward parameters (DI) along the edges.
  *
  * For a sink node the UI parameters must be explicitly specified as the part of node definition.
  *
  * The "AdderMonitor" module has its inputs coming from one "Adder" module and two instances of "AdderDriver" modules.
  * For three input diplomacy-interfaces, we will have three edges coming into the "AdderMonitor" from one "Adder" and
  * two instances of "AdderDriver".
  *
  * For the three edges connected to the inward-side of this node, we need three values of type UpwardParam, but all
  * three values are required to be same in this use case. Though all the edges carry same UpwardParam value along them,
  * the way we use the interfaces associated with these edges is different. The interface associated with edge
  * connecting "Adder" is the result that must be compared against the value computed using the interfaces associated
  * with "AdderDriver".
  *
  * In this case "AdderMonitor" module is implemented with three nodes (AdderMonitorNode) each with an edge.
  */
class AdderMonitorNode(width: UpwardParam)(implicit valName: ValName) extends SinkNode(AdderNodeImp)(Seq(width))

/** @param dFn
  *   Function that computes Downward flowing parameters on the output edges from the Downward flowing parameters
  *   received on the input edges
  * @param uFn
  *   Function that computes Upward flowing parameters on the input edges from the Upward flowing parameters received on
  *   the output edges
  */
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
      println("Adder downward params computed")
      dps.head
    },
    { case ups: Seq[UpwardParam] =>
      ups.foreach(x => println(s"the width of the $x in ${x.width}"))
      require(ups.forall(up => up.width == ups.head.width), "outward, upward adder width must be equivalent")
      println("Adder Upward params computed")
      ups.head
    },
  )
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size >= 2)
    node.out.head._1 := node.in.unzip._1.reduce(_ + _)
  }

  override lazy val desiredName = "Adder"
}

class AdderDriver(width: Int, numOutputs: Int)(implicit p: Parameters) extends LazyModule {
  val node = new AdderDriverNode(Seq.fill(numOutputs)(DownwardParam(width)))
  lazy val module = new LazyModuleImp(this) {
    val neogtiatedWidths = node.edges.out.map(_.width)
    require(neogtiatedWidths.forall(_ == neogtiatedWidths.head), "outputs of the driver node must have the same width")
    val finalWidth = neogtiatedWidths.head

    // generate random addend
    val randomAddent = FibonacciLFSR.maxPeriod(finalWidth)

    node.out.foreach { case (addend, _) => addend := randomAddent }
  }
  override lazy val desiredName: String = "AdderDriver"

}

class AdderMonitor(width: Int, numOperands: Int)(implicit p: Parameters) extends LazyModule {
  val nodeSeq                   = Seq.fill(numOperands)(new AdderMonitorNode(UpwardParam(width)))
  val nodeSum                   = new AdderMonitorNode(UpwardParam(width))
  lazy val module               = new AdderMonitorModule(this)
  override lazy val desiredName = "AdderMonitor"

}

class AdderMonitorModule(outer: AdderMonitor) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val error = Output(Bool())
  })
  printf(outer.nodeSeq.map(node => p"${node.in.head._1}").reduce(_ + p" + " + _) + p"= ${outer.nodeSum.in.head._1}\n")
  io.error := outer.nodeSum.in.head._1 =/= outer.nodeSeq.map(_.in.head._1).reduce(_ + _)
}

class AdderTestHarness()(implicit p: Parameters) extends LazyModule {
  println(s"Begin Test-Harness $p")
  val numOperands = 2
  // One adder-node with 2 incoming edges and 1 out going edge
  val adder = LazyModule(new Adder())

  // Two driver-nodes each with 2 edges
  val drivers = Seq.fill(numOperands)(LazyModule(new AdderDriver(width = 8, numOutputs = 2)))

  // Three monitor-node each with 1 edge
  val monitor = LazyModule(new AdderMonitor(width = 4, numOperands = 2))

  /* Below we draw edges between nodes. Whenever a binding operator (:=) connects two nodes,
   * an edge is drawn from rhs node to the lhs node. Note tha a node can have multiple incoming
   * and outgoing edges. Edges are indexed from 0. Whenever a node is used with binding operator, an edge
   * is added with current available index.
   */

  /** BIND(adder.node.edgeIn(0), drivers(0).node.edgeOut(0))
    *
    * BIND(adder.node.edgeIn(1), drivers(1).node.edgeOut(0))
    */
  drivers.foreach(driver => adder.node := driver.node)

  /** BIND(monitor.nodeSeq(0).edgeIn, drivers(0).node.edgeOut(1))
    *
    * BIND(monitor.nodeSeq(1).edgeIn, drivers(1).node.edgeOut(1))
    */
  drivers.zip(monitor.nodeSeq).foreach { case (driver, monitorNode) => monitorNode := driver.node }

  // BIND(monitor.nodeSum.edgeIn, adder.node.edgeOut)
  monitor.nodeSum := adder.node

  lazy val module = new LazyModuleImp(this) {
    when(monitor.module.io.error) {
      printf("something went wrong")
    }
  }
  override lazy val desiredName: String = "AdderTestHarness"
  println(s"End Test-Harness $p")
}
