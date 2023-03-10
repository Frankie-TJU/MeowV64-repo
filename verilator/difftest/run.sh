#!/bin/sh
make && LD_LIBRARY_PATH=~/prefix/riscv-isa-sim/lib/ numactl -C 0-7 ./VRiscVSystem -p ~/opensbi/build/platform/rocket-chip-vcu128/firmware/fw_payload.bin 2>log