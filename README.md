MeowV64
=======================

MeowV64 is a synthesizable and configurable superscalar RISC-V CPU with out-of-order execution, L1/L2 caches and multicore support. MeowV64 implements the RV64IMAFDCSUV_Zicsr_Zifencei ISA & JTAG Debugging.

## Run simulation

Run single core system in verilator:

```shell
cd verilator/SingleCoreConfig
make
./VRiscVSystem /path/to/executable
```

Supports bare-metal executable in ELF or raw binary format.

## Testcases

Build testcases:

```shell
cd testcases
make
```

Run testcase in simulation:

```shell
$ cd verilator/SingleCoreConfig
$ ./VRiscVSystem -p -t ../../testcases/custom/bin/fib.bin
> Loaded 310 bytes from BIN ../../testcases/custom/bin/fib.bin
> Using tohost at 60000000
> Using fromhost at 60000040
> Enable tracing
> Simulation started
0
1
1
2
3
5
8
13
21
34
> ISA testsuite pass
> Simulation finished
```

Waveform is saved to `dump.fst`, you can view it with gktwave.

## RISC-VV Vector Missing Features

The following features are missing from vector extension:

- LMUL != 1 i.e. only use m1 in `vset{i}vl{i}`, pass `-mllvm -riscv-v-fixed-length-vector-lmul-max=1` to clang

## Authors

See `AUTHORS` file

## License

All code under this repository is released under the MIT license. See `LICENSE` file.
