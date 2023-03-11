#!/bin/bash
set -e

make
for filename in ../../testcases/meow/bin/*.bin ../../testcases/riscv-tests/build/isa/*.bin; do
	if [[ "$filename" != *"ma_data"* ]]; then
		./VRiscVSystem $filename
	fi
done
