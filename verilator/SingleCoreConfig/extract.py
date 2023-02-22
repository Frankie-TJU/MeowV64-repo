import sys
import re
import os
import subprocess

cache = {}
last = ''
f = open('test.vcd', 'r')
accept = 0
addr_0 = 0
addr_1 = 0
while True:
    line = f.readline()
    if len(line) == 0:
        break
    m = re.search('\$var wire 2 (.) inflights_reader_accept', line)
    if m:
        accept_ch = m[1]
        #f.seek(int(370.980 * 1024 * 1024), os.SEEK_SET)
        f.seek(int(370 * 1024 * 1024), os.SEEK_SET)
    m = re.search('\$var wire 64 (.) inflights_reader_view_0_addr', line)
    if m:
        view_0_ch = m[1]
    m = re.search('\$var wire 64 (.) inflights_reader_view_1_addr', line)
    if m:
        view_1_ch = m[1]
    m = re.search('b([0-1]+) (.)', line)
    if m and m[2] == accept_ch:
        accept = int(m[1], 2)
    if m and m[2] == view_0_ch:
        addr_0 = int(m[1], 2)
    if m and m[2] == view_1_ch:
        addr_1 = int(m[1], 2)
    if line[0] == '#':
        time = int(line[1:])
        # new cycle
        if accept == 0:
            addrs = []
        elif accept == 1:
            addrs = [addr_0]
        else:
            addrs = [addr_1]
        for addr in addrs:
            if addr > 0x9ffc0000:
                addr = addr - 0x9ffca000 + 0x80029000

            print('pc @ {}: {:x}'.format(time, addr))
            if addr in cache:
                if cache[addr] != last:
                    print(cache[addr])
                last = cache[addr]
                continue
            binary = "~/u-boot/u-boot"
            if addr < 0x80020000:
                binary = "~/opensbi/build/platform/rocket-chip-vcu128/firmware/fw_payload.elf"
            name = subprocess.check_output(
                ["riscv64-unknown-elf-addr2line -f -e {} 0x{:x}".format(binary, addr)], shell=True, encoding='utf-8').strip()
            cache[addr] = name
            if name != last:
                print(name)
            last = name
