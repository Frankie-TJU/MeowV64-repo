#include "common.h"

.section .text
.globl _start
  # enable FPU and accelerator if present
  li t0, MSTATUS_FS | MSTATUS_XS | MSTATUS_VS
  csrs mstatus, t0

  # write 32 bytes to uncached memory
  li sp, 0x60000000
  li t1, 0x1234567812345678
  sd t1, 0(sp)
  sd t1, 8(sp)
  sd t1, 16(sp)
  sd t1, 24(sp)

  # read from uncached memory using vle
  vsetivli zero, 8, e32, ta, ma
  vle32.v v0, 0(sp)

  # save to cached memory
  li sp, 0x80000000
  vse32.v v0, 0(sp)

  # check data validity
  ld t2, 0(sp)
  bne t1, t2, fail
  ld t2, 8(sp)
  bne t1, t2, fail
  ld t2, 16(sp)
  bne t1, t2, fail
  ld t2, 24(sp)
  bne t1, t2, fail

  SUCCESS
fail:
  FAIL
