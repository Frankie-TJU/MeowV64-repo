SRCS:=$(wildcard src/*.c)
BINS:=$(patsubst src/%.c,bin/%.bin,$(SRCS))
LINKEDS:=$(patsubst src/%.c,bin/%.linked,$(SRCS))
ASSEMBLED:=$(patsubst src/%.c,bin/%.assembled,$(SRCS))
HEXS:=$(patsubst src/%.c,hex/%.hex,$(SRCS))
DUMPS:=$(patsubst src/%.c,dump/%.S,$(SRCS))
LLVM_VERSION=18

PREFIX ?= riscv64-unknown-elf-

.PHONY: list all clean

all: $(BINS) $(HEXS) $(DUMPS) $(ASSEMBLED) $(LINKEDS)

bin/common.assembled: common/init.S Makefile
	mkdir -p bin
	$(PREFIX)cpp $< | $(PREFIX)as - -march=rv64imafdc_zicsr_zifencei -o $@

bin/%.assembled: src/%.c src/common.h common/init.S Makefile
	mkdir -p bin
	clang-$(LLVM_VERSION) -O2 -target riscv64-unknown-elf -fno-vectorize -nostdlib -mcmodel=medany -c $< -march=rv64imafdcv_zicsr_zifencei -o $@

bin/%.linked: bin/%.assembled bin/common.assembled
	$(PREFIX)ld $^ -T common/linker.lds -o $@

bin/%.bin: bin/%.linked
	cp $< $@
	$(PREFIX)objcopy -O binary -j .text -j .rodata -j .data $@

hex/%.hex: bin/%.bin Makefile
	mkdir -p hex
	hexdump -ve '1/1 "%02x" "\n"' $< > $@

dump/%.S: bin/%.linked
	mkdir -p dump
	llvm-objdump-$(LLVM_VERSION) --mattr=+V,+m -D -M no-aliases -S $^ > $@

list:
	@echo "Sources:"
	@echo $(SRCS)
	@echo
	@echo "Will generate:"
	@echo $(BINS)

clean:
	rm -f $(BINS) $(HEXS) $(TMPS) $(DUMPS)
