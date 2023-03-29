#!/bin/sh
~/prefix/riscv-isa-sim/bin/spike --isa=RV64IMAFDCV ~/opensbi/build/platform/rocket-chip-vcu128-single-core/firmware/fw_payload.elf
