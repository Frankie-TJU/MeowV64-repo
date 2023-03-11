#!/bin/bash
set -e
export LD_LIBRARY_PATH=~/prefix/riscv-isa-sim/lib/

make
for filename in ../../testcases/riscv-tests/build/isa/*.bin; do
	if [[ "$filename" != *"breakpoint"* ]] && [[ "$filename" != *"ma_fetch"* ]] && [[ "$filename" != *"ma_data"* ]] && [[ "$filename" != *"dirty"* ]];then
		./VRiscVSystem $filename
	fi
done
