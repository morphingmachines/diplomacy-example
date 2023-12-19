/** Reimplements the example4 using "mixin's" instead of pattern matching.
  *
  * Use right mixin's to choose the appropriate diplomacy nodes for "AdderMonitor" module.
  */

package adder.example5

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
import freechips.rocketchip.subsystem.LazyScopeWithParameters
import org.chipsalliance.cde.config._

case object DriverWidth     extends Field[Int]
case object MonitorWidth    extends Field[Int]
case object NumMonitorNodes extends Field[Int]
case object NumOperands     extends Field[Int]

class DiplomacyExampleConfig
  extends Config((_, _, _) => {
    case NumOperands  => 2
    case DriverWidth  => 8
    case MonitorWidth => 12
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

trait BaseAdderMonitor extends LazyModule {
  type T
  def nodeSeq: T
  def nodeSum: Option[AdderMonitorNode]
  def ports:   Seq[(DecoupledIO[UInt], Int)]
}

abstract class AdderMonitor(implicit p: Parameters) extends BaseAdderMonitor {

  def adder_inputs              = ports.unzip._1.dropRight(1)
  def adder_output              = ports.unzip._1.last
  lazy val module               = new AdderMonitorModule(this)
  override lazy val desiredName = "AdderMonitor"
}

trait MonitorWithOneNode extends LazyScopeWithParameters { this: BaseAdderMonitor =>
  type T = AdderMonitorComplexNode
  val nodeSeq    = new AdderMonitorComplexNode(Seq.fill(p(NumOperands) + 1)(UpwardParam(p(MonitorWidth))))
  val nodeSum    = None
  lazy val ports = nodeSeq.in.map(_._1).zipWithIndex
}

trait MonitorWithTwoNodes extends LazyScopeWithParameters { this: BaseAdderMonitor =>
  type T = AdderMonitorDriverNode
  val nodeSeq    = new AdderMonitorDriverNode(Seq.fill(p(NumOperands))(UpwardParam(p(MonitorWidth))))
  val nodeSum    = Some(new AdderMonitorNode(UpwardParam(p(MonitorWidth)))(ValName("adder")))
  lazy val ports = (nodeSeq.in.map(_._1) ++ Seq(nodeSum.get.in.head._1)).zipWithIndex
}

trait MonitorWithThreeNodes extends LazyScopeWithParameters { this: BaseAdderMonitor =>
  type T = Seq[AdderMonitorNode]
  val nodeSeq    = Seq.fill(p(NumOperands))(new AdderMonitorNode(UpwardParam(p(MonitorWidth)))(ValName("driver")))
  val nodeSum    = Some(new AdderMonitorNode(UpwardParam(p(MonitorWidth)))(ValName("adder")))
  lazy val ports = (nodeSeq.map(_.in.head._1) ++ Seq(nodeSum.get.in.head._1)).zipWithIndex
}

class AdderMonitorModule(outer: AdderMonitor) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val error = Output(Bool())
  })
  printf(outer.adder_inputs.map(x => p"${x}").reduce(_ + p" + " + _) + p"= ${outer.adder_output}\n")
  io.error := outer.adder_output.bits =/= outer.adder_inputs.map(x => x.bits).reduce(_ + _)

  (outer.adder_inputs :+ outer.adder_output).zipWithIndex.foreach { case (in, i) =>
    in.ready := outer.ports.filter { case (_, id) => id != i }.unzip._1.map(_.valid).reduce(_ && _)
  }
}

abstract class AdderTestHarness(implicit p: Parameters) extends LazyModule {
  // require(p(NumMonitorNodes) >= 1 || p(NumMonitorNodes) <= 3)
  println(s"Begin Test-Harness $p")
  val adder   = LazyModule(new Adder)
  val drivers = Seq.fill(p(NumOperands))(LazyModule(new AdderDriver))

  drivers.foreach(driver => adder.node := driver.node)
  val monitor: AdderMonitor

  lazy val module = new AdderTestHarnessImp(this)
  override lazy val desiredName: String = "AdderTestHarness"
  println(s"End Test-Harness $p")
}

trait HasThreeNodeMonitor { this: AdderTestHarness =>
  val monitor = LazyModule(new AdderMonitor with MonitorWithThreeNodes)
  drivers.zip(monitor.nodeSeq.asInstanceOf[Seq[AdderMonitorNode]]).foreach { case (driver, operandMonitor) =>
    operandMonitor := driver.node
  }
  monitor.nodeSum.get := adder.node
}

trait HasTwoNodeMonitor { this: AdderTestHarness =>
  val monitor = LazyModule(new AdderMonitor with MonitorWithTwoNodes)
  drivers.foreach(driver => monitor.nodeSeq.asInstanceOf[AdderMonitorDriverNode] := driver.node)
  monitor.nodeSum.get := adder.node
}

trait HasOneNodeMonitor { this: AdderTestHarness =>
  val monitor = LazyModule(new AdderMonitor with MonitorWithOneNode)
  drivers.foreach(driver => monitor.nodeSeq.asInstanceOf[AdderMonitorComplexNode] := driver.node)
  monitor.nodeSeq.asInstanceOf[AdderMonitorComplexNode] := adder.node
}

class AdderTestHarnessImp(outer: AdderTestHarness) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val op1Valid = Input(Bool())
    val op2Valid = Input(Bool())
  })
  outer.drivers(0).module.io.opValid := io.op1Valid
  outer.drivers(1).module.io.opValid := io.op2Valid
  when(outer.monitor.module.io.error) {
    printf("something went wrong!! ")
  }
}
