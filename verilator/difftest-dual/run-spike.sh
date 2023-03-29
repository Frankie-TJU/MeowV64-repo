#!/bin/sh
~/prefix/riscv-isa-sim/bin/spike -p2 --isa=RV64IMAFDCV ~/opensbi/build/platform/rocket-chip-vcu128-dual-core/firmware/fw_payload.elf
