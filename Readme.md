# Diplomatic Adder

This is template project to demonstrate [Diplomacy](https://github.com/chipsalliance/rocket-chip/blob/master/docs/src/diplomacy/adder_tutorial.md) in Chisel 5.0.0 with [playground](https://github.com/morphingmachines/playground.git) as a library. `playground` and `this project` directories should be at the same level, as shown below.
```
  workspace
  |-- playground
  |-- diplomacy-example
```
Make sure that you have a working [playground](https://github.com/morphingmachines/playground.git) project before proceeding further. And donot rename/modify `playground` directory structure.


## Generating Verilog

Verilog code can be generated from Chisel by using the `chisel` Makefile target.

```sh
make rtl
```

The output verilog files are generated in the `./generated` directory.

Running tests can be done with:

```sh
make test
```

More targets can be listed by running `make`.

## Chisel Learning Resources

- [Chisel Book](https://github.com/schoeberl/chisel-book)
- [Chisel Documentation](https://www.chisel-lang.org/chisel3/)
- [Chisel API](https://www.chisel-lang.org/api/chisel/latest/)

