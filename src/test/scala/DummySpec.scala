package adder

import chiseltest._
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Config
import org.scalatest.freespec.AnyFreeSpec

/** This is a trivial example of how to run this Specification: From a terminal shell use:
  * {{{
  * mill adder.test.testOnly adder.DiplomaticAdder
  * }}}
  */
class DiplomaticAdder extends AnyFreeSpec with ChiselScalatestTester {

  "Diplomatic Adder must properly negotiate the width parameter" in {
    test(
      LazyModule(
        new example5.AdderTestHarness()(new Config(new example5.DiplomacyExampleConfig)) with example5.HasOneNodeMonitor,
      ).module,
    ).withAnnotations(
      Seq(
        WriteVcdAnnotation,
        VerilatorBackendAnnotation, // Uncomment to use the Verilator backend
      ),
    ) { dut =>
      dut.io.op1Valid.poke(true)
      dut.io.op2Valid.poke(true)
      dut.clock.step(10)
    }
  }
}
