#include "common.h"

.section .text
.globl _start
_start:
  li sp, 0x88000000
	li ra, 0x100000
  csrrsi t0, misa, 0
  li t1, 0x800000000034112D
  bne t1, t0, fail

  li t1, 1145141919810
  csrrw zero, mtvec, t1
  csrrc t0, mtvec, zero
  bne t0, t1, fail
  SUCCESS
fail:
  FAIL
