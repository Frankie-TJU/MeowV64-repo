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

Read gdb output:

```shell
tail -f /tmp/gdb@*.log
```

Tests:

- CheckMisa: 7s
- CustomRegisterTest: not applicable
- DebugBreakpoint: fail
- DebugChangeString: 237s
- DebugCompareSections: 289s
- DebugExit: 216s
- DebugSymbols: 227s
- DebugTurbostep: fail
- DisconnectTest: fail
- InfoTest: fail
- MemTest16: 10s
- MemTest32: 11s
- MemTest64: 11s
- MemTest8: 10s