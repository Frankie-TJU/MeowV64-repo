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

Read gdb & openocd output:

```shell
tail -f /tmp/gdb@*.log /tmp/openocd*.log
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
- DownloadTest: fail
- InfoTest: fail
- InstantChangePc: 39s
- InstantHaltTest: 55s
- MemTest16: 10s
- MemTest32: 11s
- MemTest64: 11s
- MemTest8: 10s
- MemTestBlock0: 78s
- MemTestBlock1: 78s
- MemTestBlock2: 78s
- PrivChange: 119s
- Registers: 303s
- Semihosting: 233s
- SimpleF18Test: 70s
- SimpleS0Test: 41s
- SimpleS1Test: 41s
- SimpleT0Test: 40s
- SimpleT1Test: 41s
- StepTest: 207s
- Sv32Test: 254s
- WriteCsrs: 143s
- WriteGprs: 176s