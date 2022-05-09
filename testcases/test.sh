#!/bin/bash
set -x -e
make

for VLEN in 128 256 512 1024
do
	SPIKE="spike --varch=vlen:${VLEN},elen:64 --isa=rv64gcv -m0x00000000:0x40000000,0x60000000:0x1000,0x80000000:0x10000000"
	for TEST in \
		vadd \
		vand \
		vfadd \
		vfmacc \
		vfmadd \
		vfmsac \
		vfmsub \
		vfmul \
		vfmv \
		vfnmacc \
		vfnmadd \
		vfnmsac \
		vfnmsub \
		vfredosum \
		vfredusum \
		vfsub \
		vle \
		vluxei \
		vmax \
		vmaxu \
		vmin \
		vminu \
		vmul \
		vmulh_32 \
		vmulhsu_32 \
		vmulhu_32 \
		vmv \
		vmvr \
		vor \
		vrsub \
		vse \
		vsetvl \
		vsll \
		vxor
	do
		$SPIKE ./riscv-tests/build/isa/rv64uv-p-${TEST}
	done
	$SPIKE ./riscv-tests/build/benchmarks/spmv.riscv
done