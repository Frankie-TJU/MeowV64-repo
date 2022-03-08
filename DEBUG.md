# How to DEBUG

## Debug in simulation

Run simulator with remote bitbang jtag:

```shell
make && ./VRiscVSystem -p -j ../../testcases/riscv-tests/build/benchmarks/dhrystone.riscv
```

Run OpenOCD:

```shell
openocd -f openocd.cfg
```

Run GDB:

```shell
riscv64-unknown-elf-gdb -x gdbinit ~/MeowV64/testcases/riscv-tests/build/benchmarks/dhrystone.riscv
```
