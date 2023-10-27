package adder

import chiseltest._
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import org.scalatest.freespec.AnyFreeSpec

/** This is a trivial example of how to run this Specification: From a terminal shell use:
  * {{{
  * mill gcd.test.testOnly gcd.GCDSpec
  * }}}
  */
class DiplomaticAdder extends AnyFreeSpec with ChiselScalatestTester {

  "Diplomatic Adder must properly negotiate the wodth parameter" in {
    test(LazyModule(new AdderTestHarness()(Parameters.empty)).module).withAnnotations(
      Seq(
        WriteVcdAnnotation,
        VerilatorBackendAnnotation, // Uncomment to use the Verilator backend
      ),
    ) { dut =>
      dut.clock.step(10)

    }
  }
}
