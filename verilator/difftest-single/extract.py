import sys
import re
import os
import subprocess

cache = {}
last = ''
f = open('dump.vcd', 'r')
valid = 0
while True:
    line = f.readline()
    if len(line) == 0:
        break
    valid_ch = '('
    m = re.search('\$var wire 64 (.) io_storeAddr', line)
    if m:
        store_addr_ch = m[1]
    m = re.search('\$var wire 64 (.) io_storeData', line)
    if m:
        store_data_ch = m[1]
    m = re.search('\$var wire 8 (.) io_storeMask', line)
    if m:
        store_mask_ch = m[1]
    m = re.search('b([0-1]+) (.)', line)
    if m and m[2] == store_addr_ch:
        store_addr = int(m[1], 2)
    if m and m[2] == store_data_ch:
        store_data = int(m[1], 2)
    if m and m[2] == store_mask_ch:
        store_mask = int(m[1], 2)
    if line[1] == valid_ch:
        valid = int(line[0], 2)
    if line[0] == '#':
        time = int(line[1:])
        # new cycle
        if time % 10 == 0 and valid == 1:
            print(f'mem[0x{store_addr:x}] = {store_data:x} (time {time} mask {store_mask:x})')
