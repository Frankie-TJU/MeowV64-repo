debug.bin: debug.o
	riscv64-unknown-elf-objcopy -O binary -j .text $^ $@

debug.o: debug.S linker.ld
	riscv64-unknown-elf-gcc -ffreestanding -nostdlib -nostdinc -T linker.ld $< -o $@

clean:
	rm -rf *.bin *.o