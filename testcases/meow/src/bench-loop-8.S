#include "common.h"

.section .text
.globl _start
_start:
    li sp, 0x88000000
    li ra, 0x100000
    li t0, 0x00001000
.LC0:
    addi t0, t0, -1
    li t1, 0
    li t2, 1
    li t3, 2
    li t4, 3
    li t5, 4
    li t6, 5
    bne t0, zero, .LC0
    
    SUCCESS
