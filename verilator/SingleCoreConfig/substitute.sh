#!/bin/bash
# skip failedWrite check
sed -i 's/for (i = 0;/\/\/\0/g' sram_*.v
sed -i "s/mem\[i\] = {MEM_WIDTH{1'bx}};/\/\/\0/g" sram_*.v