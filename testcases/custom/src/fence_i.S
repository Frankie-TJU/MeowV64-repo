#include "common.h"
.section .text
.globl _start
_start:
  li sp, 0x88000000
	li ra, 0x100000
  li t0, 0x1145141919810
  sd t0, 0(sp)
  fence.i
  ld t1, 0(sp)
  bne t0, t1, fail
  SUCCESS
fail:
  FAIL
