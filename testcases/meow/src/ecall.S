#include "common.h"

.section .text
  li sp, 0x88000000
	li ra, 0x100000
  j _start
  # For alignment
  addi zero, zero ,0
tvec:
  addi a0, a0, 1
  csrrsi t0, mepc, 0
  addi t0, t0, 4
  csrrw zero, mepc, t0
  mret

.globl _start
_start:
  la t0, tvec
  csrrw zero, mtvec, t0
  li a0, 1
  addi a0, a0, 1
  ecall
  addi a0, a0, 1
  li t0, 4
  bne a0, t0, fail
  SUCCESS
fail:
  FAIL
