#!/bin/bash
set -x -e
make

for VLEN in 128 256 512 1024
do
	SPIKE="spike --varch=vlen:${VLEN},elen:64 --isa=rv64gcv -m0x00000000:0x40000000,0x60000000:0x1000,0x80000000:0x10000000"
	for TEST in vle vse vluxei vfmv vsll vfadd vmvr vfmacc vfredosum
	do
		$SPIKE ./riscv-tests/build/isa/rv64uv-p-${TEST}
	done
	$SPIKE ./riscv-tests/build/benchmarks/spmv.riscv
done