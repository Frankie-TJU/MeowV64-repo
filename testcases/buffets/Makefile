SRCS:=$(wildcard src/*.c)
BINS:=$(patsubst src/%.c,bin/%.bin,$(SRCS)) \
	bin/poisson_vector-64.bin \
	bin/poisson_vector-128.bin
LINKEDS:=$(patsubst src/%.c,bin/%.linked,$(SRCS))
ASSEMBLED:=$(patsubst src/%.c,bin/%.assembled,$(SRCS))
HEXS:=$(patsubst src/%.c,hex/%.hex,$(SRCS))
DUMPS:=$(patsubst src/%.c,dump/%.S,$(SRCS))

PREFIX ?= riscv64-unknown-elf-

.PHONY: list all clean

all: $(BINS) $(HEXS) $(DUMPS) $(ASSEMBLED) $(LINKEDS)

bin/common.assembled: common/init.S Makefile
	mkdir -p bin
	$(PREFIX)cpp $< | $(PREFIX)as - -march=rv64imafdcv_zicsr_zifencei -o $@

bin/%.assembled: src/%.c src/common.h common/init.S Makefile
	mkdir -p bin
	$(PREFIX)gcc -O2 -mcmodel=medany -fno-tree-loop-distribute-patterns -c $< -march=rv64imafdcv_zicsr_zifencei -o $@

bin/%.linked: bin/%.assembled bin/common.assembled
	$(PREFIX)ld $^ -T common/linker.lds -o $@

bin/%.bin: bin/%.linked
	cp $< $@
	$(PREFIX)objcopy -O binary -j .text -j .rodata -j .data $@

bin/%-64.bin: src/%.c bin/common.assembled common/linker.lds Makefile
	mkdir -p bin
	$(PREFIX)gcc -DN_OVERRIDE=64 -O2 -mcmodel=medany -fno-tree-loop-distribute-patterns -c $< -march=rv64imafdcv_zicsr_zifencei -o $(patsubst src/%.c,bin/%-64.assembled,$<)
	$(PREFIX)ld $(patsubst src/%.c,bin/%-64.assembled,$<) bin/common.assembled -T common/linker.lds -o $(patsubst src/%.c,bin/%-64.linked,$<)
	cp $(patsubst src/%.c,bin/%-64.linked,$<) $@
	$(PREFIX)objcopy -O binary -j .text -j .rodata -j .data $@

bin/%-128.bin: src/%.c bin/common.assembled common/linker.lds Makefile
	mkdir -p bin
	$(PREFIX)gcc -DN_OVERRIDE=128 -O2 -mcmodel=medany -fno-tree-loop-distribute-patterns -c $< -march=rv64imafdcv_zicsr_zifencei -o $(patsubst src/%.c,bin/%-128.assembled,$<)
	$(PREFIX)ld $(patsubst src/%.c,bin/%-128.assembled,$<) bin/common.assembled -T common/linker.lds -o $(patsubst src/%.c,bin/%-128.linked,$<)
	cp $(patsubst src/%.c,bin/%-128.linked,$<) $@
	$(PREFIX)objcopy -O binary -j .text -j .rodata -j .data $@

hex/%.hex: bin/%.bin Makefile
	mkdir -p hex
	hexdump -ve '1/1 "%02x" "\n"' $< > $@

dump/%.S: bin/%.linked
	mkdir -p dump
	$(PREFIX)objdump -D -M no-aliases -S $^ > $@

list:
	@echo "Sources:"
	@echo $(SRCS)
	@echo
	@echo "Will generate:"
	@echo $(BINS)

clean:
	rm -f $(BINS) $(HEXS) $(TMPS) $(DUMPS)
