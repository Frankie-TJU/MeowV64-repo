#include "common.h"

.section .text
.globl _start
_start:
  li sp, 0x88000000
  li ra, 0x100000
  li s0, 0x02004000
  li s1, 0x0200BFF8

  csrr a0, mhartid
  bnez a0, other_hart

  # Set mepc
  la t0, trap
  csrw mtvec, t0 # Because trap is 4-byte aligned, this also sets mode = direct

  # Set mtimecmp
  li t0, 2000
  sw t0, 0(s0)

  # Set mtime
  sd zero, 0(s1)

  # Enables MIE
  li t0, 1 << 3
  csrs mstatus, t0

  # Enables MTIE
  li t0, 1<<7
  csrs mie, t0

  # Spin
spin:
  j spin

  .align 4
trap:
  csrr t0, mcause
  li t1, 7 | (1 << 63)
  bne t0, t1, fail

  csrr t0, mepc
  la t1, spin
  bne t0, t1, fail

  SUCCESS

fail:
  FAIL

other_hart:
  j other_hart