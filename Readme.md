# Diplomatic Adder

This is template project to demonstrate [Diplomacy](https://github.com/chipsalliance/rocket-chip/blob/master/docs/src/diplomacy/adder_tutorial.md) in Chisel 5.0.0 with [playground](https://github.com/morphingmachines/playground.git) as a library. `playground` and `this project` directories should be at the same level, as shown below.
```
  workspace
  |-- playground
  |-- diplomacy-example
```
Make sure that you have a working [playground](https://github.com/morphingmachines/playground.git) project before proceeding further. And donot rename/modify `playground` directory structure.


## Generating Verilog

Verilog code can be generated from Chisel by using the `rtl` Makefile target.

```sh
make rtl
```

The output verilog files are generated in the `./generated_sv_dir` directory. This also generates a `graphml` file that visualizes the diplomacy graph of different components in the system. To view `graphml` file, as shown below, use [yEd](https://askubuntu.com/a/504178). 

![diplomacy_graph](./doc/figures/AdderTestHarness.jpg)
### Run tests

```sh
make test
```

The output VCD files are dumped in the `./test_run_dir` directory.

More targets can be listed by running `make`.

## Chisel Learning Resources

- [Chisel Book](https://github.com/schoeberl/chisel-book)
- [Chisel Documentation](https://www.chisel-lang.org/chisel3/)
- [Chisel API](https://www.chisel-lang.org/api/chisel/latest/)

