#include "common.h"

.section .text
.globl _start
_start:
    li sp, 0x88000000
    li ra, 0x100000
    li t0, 0x00001000
.LC0:
    addi t0, t0, -1
    addi t1, t0, 0
    addi t2, t1, 1
    addi t3, t2, 2
    addi t4, t3, 3
    addi t5, t4, 4
    addi t6, t5, 5
    bne t0, zero, .LC0
    
    SUCCESS
