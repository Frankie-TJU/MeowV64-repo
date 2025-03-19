#!/bin/sh
echo -n > cpu.log
echo -n > gpu.log
gcc -DNEURONS_PER_POPULATION=256 -g snn_int.c -o snn_int && ./snn_int | tee -a cpu.log
gcc -DNEURONS_PER_POPULATION=512 -g snn_int.c -o snn_int && ./snn_int | tee -a cpu.log
gcc -DNEURONS_PER_POPULATION=1024 -g snn_int.c -o snn_int && ./snn_int | tee -a cpu.log
CUDA_PATH=/usr/local/cuda python3 snn_int.py -n 256 | tee -a gpu.log
CUDA_PATH=/usr/local/cuda python3 snn_int.py -n 512 | tee -a gpu.log
CUDA_PATH=/usr/local/cuda python3 snn_int.py -n 1024 | tee -a gpu.log
