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

- CheckMisa: 4s
- DebugBreakpoint: 114s
- DebugChangeString: 99s
- DebugCompareSections: 108s
- DebugExit: 83s
- DebugFunctionCall: fail
- DebugSymbols: 86s
- DebugTurbostep: 118s
- DisconnectTest: fail
- DownloadTest: fail
- InfoTest: fail
- InstantChangePc: 15s
- InstantHaltTest: fail
- InterruptTest: fail
- MemTest16: 5s
- MemTest32: 5s
- MemTest64: 5s
- MemTest8: 5s
- MemTestBlock0: 30s
- MemTestBlock1: 30s
- MemTestBlock2: 33s
- MemTestReadInvalid: fail
- MemorySampleMixed: fail
- MemorySampleSingle: fail
- PrivChange: 45s
- PrivRw: fail
- ProgramHwWatchpoint: fail
- ProgramSwWatchpoint: 399s
- Registers: 109s
- RepeatReadTest: fail
- Semihosting: fail
- SimpleF18Test: fail
- SimpleNoExistTest: fail
- SimpleS0Test: 16s
- SimpleS1Test: 16s
- SimpleT0Test: 17s
- SimpleT1Test: 16s
- SimpleV13Test: 6s
- StepTest: 77s
- Sv39Test: 101s
- Sv48Test: 104s
- TooManyHwbp: 98s
- TriggerExecuteInstant: fail
- TriggerLoadAccessInstant: fail
- TriggerStoreAccessInstant: fail
- UserInterrupt: 93s
- WriteCsrs: 53s
- WriteGprs: 69s

ran 62 tests in 3335s
30 tests returned pass
14 tests returned not_applicable
14 tests returned exception