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
    li t1, 0
    li t2, 1
    li t3, 2
    li t4, 3
    bne t0, zero, .LC0

    SUCCESS
