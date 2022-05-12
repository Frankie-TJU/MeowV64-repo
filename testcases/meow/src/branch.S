#include "common.h"

.section .text
.globl _start
  li sp, 0x88000000
	li ra, 0x100000
  li t0, 0x10
  li t1, 0x5
  li t2, 0x1
loop:
  bltu t0, t1, exit# if t0 < t1 then target
  sub t0, t0, t2 # t0 = t1 - t2
  j loop
exit:
  li a0, 0x4
  bne t0, a0, fail
  
  SUCCESS
fail:
  FAIL
