package adder

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

object AdderNodeImp extends SimpleNodeImp[DownwardParam, UpwardParam, EdgeParam, UInt] {
  def edge(pd: DownwardParam, pu: UpwardParam, p: Parameters, sourceInfo: SourceInfo) =
    if (pd.width > pu.width) EdgeParam(pu.width) else EdgeParam(pd.width)

  def bundle(e: EdgeParam): UInt = UInt(e.width.W)

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
  val adder       = LazyModule(new Adder())
  val drivers     = Seq.fill(numOperands)(LazyModule(new AdderDriver(width = 8, numOutputs = 2)))

  val monitor = LazyModule(new AdderMonitor(width = 4, numOperands = 2))

  drivers.foreach(driver => adder.node := driver.node)
  drivers.zip(monitor.nodeSeq).foreach { case (driver, monitorNode) => monitorNode := driver.node }
  monitor.nodeSum := adder.node
  lazy val module = new LazyModuleImp(this) {
    when(monitor.module.io.error) {
      printf("something went wrong")
    }
  }
  override lazy val desiredName: String = "AdderTestHarness"
  println(s"End Test-Harness $p")
}
