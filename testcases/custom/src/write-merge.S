#include "common.h"

.section .text
.globl _start
_start:
  li sp, 0x88000000
	li ra, 0x100000
  li s0, 0x88000100
  li t0, 0x0123456789ABCDEF

  sd t0, 0(s0)
  li t0, 0xFF
  sb t0, 5(s0)
  li t0, 0xEEEE
  sh t0, 2(s0)
  li t0, 0xDDDDDDDD
  sw t0, 0(s0)

  ld t0, 0(s0)
  li t1, 0x0123FF67DDDDDDDD
  bne t0, t1, fail

  SUCCESS
fail:
  FAIL
