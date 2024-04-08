GCC=riscv64-unknown-elf-gcc
OBJCOPY=riscv64-unknown-elf-objcopy

all: serial.elf serial.img serial.bin

%.bin: %.img
	python3 convert.py $< > $@

%.img: %.elf
	$(OBJCOPY) -O binary $< $@

%.elf: %.c init.S linker.ld
	$(GCC) -Os -Tlinker.ld init.S $< -ffreestanding -nostdlib -static -Wl,--no-gc-sections -o $@

clean:
	rm -rf *.img *.elf
