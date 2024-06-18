#include "common.h"

.section .text.init
.globl _start
_start:
  # enable FPU and accelerator if present
  li t0, MSTATUS_FS | MSTATUS_XS | MSTATUS_VS
  csrs mstatus, t0
  
  li sp, 0x88000000
  csrr a0, mhartid
  slli t0, a0, 20 # 1M Stack
  sub sp, sp, t0
  jal main
  bnez a0, fail
  
  SUCCESS
fail:
  FAIL
