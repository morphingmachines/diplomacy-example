/** Extends the example3 where you can choose the "AdderMonitor" module with 1 or 2 or 3 diplomacy nodes through a
  * parameter.
  *
  * Pattern matching is used to select the right "AdderMonitor" LazyModule based on the implicit parameter
  * configuration.
  */

package adder.example4

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

case object DriverWidth     extends Field[Int]
case object MonitorWidth    extends Field[Int]
case object NumMonitorNodes extends Field[Int]
case object NumOperands     extends Field[Int]

class DiplomacyExampleConfig
  extends Config((_, _, _) => {
    case NumOperands     => 2
    case DriverWidth     => 8
    case MonitorWidth    => 12
    case NumMonitorNodes => 3
  })

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

class AdderDriver(implicit p: Parameters) extends LazyModule {
  val node        = new AdderDriverNode(Seq.fill(p(NumOperands))(DownwardParam(p(DriverWidth))))
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

class AdderMonitorComplexNode(widths: Seq[UpwardParam])(implicit valName: ValName)
  extends SinkNode(AdderNodeImp)(widths)

class AdderMonitorModule(outer: AdderMonitor) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val error = Output(Bool())
  })
  printf(outer.input_ports.map(x => p"${x}").reduce(_ + p" + " + _) + p"= ${outer.output_port}\n")
  io.error := outer.output_port.bits =/= outer.input_ports.map(x => x.bits).reduce(_ + _)

  outer.ports.foreach { case (in, i) =>
    in.ready := outer.ports.filter { case (_, id) => id != i }.unzip._1.map(_.valid).reduce(_ && _)
  }
}

class AdderMonitor(implicit p: Parameters) extends LazyModule {
  val nodeSum: Option[AdderMonitorNode] = p(NumMonitorNodes) match {
    case 1 => None
    case _ => Some(new AdderMonitorNode(UpwardParam(p(MonitorWidth)))(ValName("adder")))
  }

  val nodeSeq = p(NumMonitorNodes) match {
    case 1 => new AdderMonitorComplexNode(Seq.fill(p(NumOperands) + 1)(UpwardParam(p(MonitorWidth))))
    case 2 => new AdderMonitorDriverNode(Seq.fill(p(NumOperands))(UpwardParam(p(MonitorWidth))))
    case 3 => Seq.fill(p(NumOperands))(new AdderMonitorNode(UpwardParam(p(MonitorWidth)))(ValName("driver")))
  }
  lazy val ports = p(NumMonitorNodes) match {
    case 1 => nodeSeq.asInstanceOf[AdderMonitorComplexNode].in.map(_._1).zipWithIndex
    case 2 => (nodeSeq.asInstanceOf[AdderMonitorDriverNode].in.map(_._1) ++ Seq(nodeSum.get.in.head._1)).zipWithIndex
    case 3 =>
      (nodeSeq.asInstanceOf[Seq[AdderMonitorNode]].map(_.in.head._1) ++ Seq(nodeSum.get.in.head._1)).zipWithIndex
  }
  lazy val input_ports          = ports.unzip._1.dropRight(1)
  lazy val output_port          = ports.unzip._1.last
  lazy val module               = new AdderMonitorModule(this)
  override lazy val desiredName = "AdderMonitor"
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

class AdderTestHarness(implicit p: Parameters) extends LazyModule {
  require(p(NumMonitorNodes) >= 1 || p(NumMonitorNodes) <= 3)
  println(s"Begin Test-Harness $p")
  val adder   = LazyModule(new Adder)
  val drivers = Seq.fill(p(NumOperands))(LazyModule(new AdderDriver))

  val monitor = LazyModule(new AdderMonitor)
  p(NumMonitorNodes) match {
    case 1 => {

      /** ! CAUTION: reordering the below node binding statements breaks the functional correctness When number of nodes
        * in monitor is 1. The same node has 3 ports, 2 for operands and 1 for result. AdderMonitor nodeSeq defines
        * first two ports as operands and the last port as result. So correctness depends on order of binding. In case
        * of monitor with 2 or 3 nodes, the ports for operands and result are uniquely determined by node names.
        */
      drivers.foreach(driver => monitor.nodeSeq.asInstanceOf[AdderMonitorComplexNode] := driver.node)
      monitor.nodeSeq.asInstanceOf[AdderMonitorComplexNode] := adder.node
    }
    case 2 => {
      drivers.foreach(driver => monitor.nodeSeq.asInstanceOf[AdderMonitorDriverNode] := driver.node)
      monitor.nodeSum.get := adder.node
    }
    case 3 => {
      drivers.zip(monitor.nodeSeq.asInstanceOf[Seq[AdderMonitorNode]]).foreach { case (driver, operandMonitor) =>
        operandMonitor := driver.node
      }
      monitor.nodeSum.get := adder.node
    }
  }
  drivers.foreach(driver => adder.node := driver.node)

  lazy val module = new AdderTestHarnessImp(this)
  override lazy val desiredName: String = "AdderTestHarness"
  println(s"End Test-Harness $p")
}

class AdderTestHarnessImp(outer: AdderTestHarness) extends LazyModuleImp(outer) {
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
