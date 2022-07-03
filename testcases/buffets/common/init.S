#include "common.h"

.section .text.init
.globl _start
_start:
  li sp, 0x88000000
  jal main
  bnez a0, fail
  
  SUCCESS
fail:
  FAIL
