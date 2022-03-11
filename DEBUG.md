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

Failing tests:

- DisconnectTest: stopcount/stoptime not implemented
- InfoTest: not allowing floating point read when mstatus.FS=0 in debug mode
- InstantHaltTest: halt on reset not implemented
- InterruptTest: not investigated
- MemTestReadInvalid: not giving bad_address
- MemorySampleMixed: gdb command not exists
- MemorySampleSingle: gdb command not exists
- ProgramHwWatchpoint: trigger not implemented
- RepeatReadTest: gdb command not exists
- SimpleNoExistTest: not throwing exception when reading nonexisting csr
- TriggerExecuteInstant: trigger not implemented
- TriggerLoadAddressInstant: trigger not implemented
- TriggerStoreAddressInstant: trigger not implemented