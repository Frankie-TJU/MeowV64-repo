#!/bin/bash
set -x -e
make

for VLEN in 128 256 512 1024
do
	SPIKE="spike --varch=vlen:${VLEN},elen:64 --isa=rv64gcv -m0x00000000:0x40000000,0x60000000:0x1000,0x80000000:0x10000000"
	$SPIKE ./riscv-tests/build/isa/rv64uv-p-vle
	$SPIKE ./riscv-tests/build/isa/rv64uv-p-vse
	$SPIKE ./riscv-tests/build/isa/rv64uv-p-vluxei
	$SPIKE ./riscv-tests/build/isa/rv64uv-p-vfmv
	$SPIKE ./riscv-tests/build/isa/rv64uv-p-vsll
	$SPIKE ./riscv-tests/build/isa/rv64uv-p-vfadd
	$SPIKE ./riscv-tests/build/isa/rv64uv-p-vmvr
	$SPIKE ./riscv-tests/build/isa/rv64uv-p-vfmacc
	$SPIKE ./riscv-tests/build/isa/rv64uv-p-vfredosum
done