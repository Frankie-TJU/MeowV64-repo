#!/bin/bash
for file in sram_*.v; do
	# fix QA_update
	echo "\`define ARM_UD_MODEL" | cat - $file | sponge $file
	# skip failedWrite check
	sed -i 's/for (i = 0;/\/\/\0/g' $file
	sed -i "s/mem\[i\] = {MEM_WIDTH{1'bx}};/\/\/\0/g" $file
	# fix XQA
	sed -i "s/XQA = 1'b1/XQA = 1'b0/g" $file
done