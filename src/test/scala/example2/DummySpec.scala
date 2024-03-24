package adder.example2

import adder.example2

import chiseltest._
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import org.scalatest.freespec.AnyFreeSpec

/** This is a trivial example of how to run this Specification: From a terminal shell use:
  * {{{
  * mill adder.test.testOnly adder.example2.DiplomaticAdder
  * }}}
  */
class DiplomaticAdder extends AnyFreeSpec with ChiselScalatestTester {

  "Diplomatic Adder must properly negotiate the width parameter" in {
    test(
      LazyModule(
        new example2.AdderTestHarness()(Parameters.empty),
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
