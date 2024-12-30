#include "common.h"

.section .text
.globl _start
  # enable FPU and accelerator if present
  li t0, MSTATUS_FS | MSTATUS_XS | MSTATUS_VS
  csrs mstatus, t0

  # write all zeros to cached memory
  li sp, 0x80001000
  sd zero, 0(sp)
  # read from uncached memory
  flw f0, 0(sp)
  # f0 should equal NaN
  fmv.x.d t1, f0
  beq t1, zero, fail

  # write all zeros to uncached memory
  li sp, 0x60001000
  sd zero, 0(sp)
  # read from uncached memory
  flw f0, 0(sp)
  # f0 should equal NaN
  fmv.x.d t1, f0
  beq t1, zero, fail

  SUCCESS
fail:
  FAIL
