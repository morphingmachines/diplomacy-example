/** Extends the example1, where diplomacy interface is changed from UInt to DecoupledIO[UInt].
  *
  * This makes the bundle type at the edge end-points DecoupledIO[UInt]. So now we have both Input and Output direction
  * signals as part of diplomacy interface.
  */

package adder.example2

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
import org.chipsalliance.cde.config.Parameters

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

class AdderMonitorNode(width: UpwardParam)(implicit valName: ValName) extends SinkNode(AdderNodeImp)(Seq(width))

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
      require(ups.forall(up => up.width == ups.head.width), "outward11, upward adder width must be equivalent")
      ups.head
    },
  )
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size >= 2)
    node.out.head._1.bits  := node.in.unzip._1.map(_.bits).reduce(_ + _)
    node.out.head._1.valid := node.in.unzip._1.map(_.valid).reduce(_ && _)
    node.in.unzip._1.foreach(_.ready := node.out.head._1.ready)
  }

  override lazy val desiredName = "Adder"
}

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

  io.error := outer.nodeSum.in.head._1.bits =/= outer.nodeSeq.map(_.in.head._1.bits).reduce(_ + _)
  val ports = (outer.nodeSeq.map(_.in.head._1) ++ Seq(outer.nodeSum.in.head._1)).zipWithIndex
  ports.foreach { case (in, i) =>
    in.ready := ports.filter { case (_, id) => id != i }.unzip._1.map(_.valid).reduce(_ && _)
  }
}

class AdderTestHarness()(implicit p: Parameters) extends LazyModule {
  println(s"Begin Test-Harness $p")
  val numOperands = 2
  val adder       = LazyModule(new Adder())
  val drivers     = Seq.fill(numOperands)(LazyModule(new AdderDriver(width = 8, numOutputs = 2)))

  val monitor = LazyModule(new AdderMonitor(width = 4, numOperands = 2))

  drivers.foreach(driver => adder.node := driver.node)
  drivers.zip(monitor.nodeSeq).foreach { case (driver, monitorNode) => monitorNode := driver.node }
  monitor.nodeSum := adder.node
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
