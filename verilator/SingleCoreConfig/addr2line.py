import sys
import re
import os
import subprocess

for line in sys.stdin.readlines():
    m = re.match('^> pc: ([0-9a-f]+)', line)
    if m:
        addr = int(m[1], 16)
        if addr > 0x9ffc0000:
            addr = addr - 0x9ffca000 + 0x80029000
        binary = "~/u-boot/u-boot"
        if addr < 0x80020000:
            binary = "~/opensbi/build/platform/rocket-chip-vcu128/firmware/fw_payload.elf"
        name = subprocess.check_output(
            ["riscv64-unknown-elf-addr2line -e {} 0x{:x}".format(binary, addr)], shell=True, encoding='utf-8')
        print("{} @ {:x}".format(name.strip(), addr))
    print(line, end='')
