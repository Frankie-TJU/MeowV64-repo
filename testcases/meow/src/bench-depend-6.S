#include "common.h"

.section .text
.globl _start
_start:
    li sp, 0x88000000
    li ra, 0x100000
    li t0, 0x00001000
    .align 4
.LC0:
    addi t0, t0, -1
    addi t1, t0, 1
    addi t2, t1, 1
    addi t3, t2, 1
    addi t4, t3, 1
    bne t0, zero, .LC0

    SUCCESS
