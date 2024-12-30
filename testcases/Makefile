##
# customV64 testcases
#
# @file
# @version 0.1

PREFIX ?= "riscv64-unknown-elf-"

.PHONY: all clean custom buffets rvv riscv-tests

all: custom buffets rvv riscv-tests

clean:
	make -C custom -f Makefile clean
	[ ! -d riscv-tests/build ] || make -C riscv-tests/build clean

custom:
	make -C custom -f Makefile all PREFIX=$(PREFIX)

buffets:
	make -C buffets -f Makefile all PREFIX=$(PREFIX)

rvv:
	make -C rvv -f Makefile all PREFIX=$(PREFIX)

riscv-tests:
	bash -c "cd riscv-tests && autoconf && mkdir -p build && cd build && ../configure"
	make -C riscv-tests/build -f Makefile RISCV_PREFIX=$(PREFIX)

# end
