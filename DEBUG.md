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

## Debugging tests

Run specific tests:

```shell
cd testcases/riscv-tests/debug
./gdbserver.py ./targets/Meow/MeowV64.py XXXXXX
```

Run all tests:

```
./gdbserver ./targets/Meow/MeowV64.py | tee /path/to/MeowV64/DEBUG_RESULT.txt
```

Read gdb & openocd output:

```shell
tail -f /tmp/gdb@*.log /tmp/openocd*.log
```