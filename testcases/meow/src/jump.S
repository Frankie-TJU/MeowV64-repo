#include "common.h"

.section .text
.globl _start
_start:
  li sp, 0x88000000
	li ra, 0x100000
  li t0, 0x1
  j skip
  li t0, 0x2
skip:
  li t1, 0x1

  # Check
  bne t0, t1, fail
  SUCCESS
fail:
  FAIL
