#define DRAM_TOP 0x80800000
#define DRAM_BASE 0x80000000

.section .text.start, "ax", @progbits
.globl _start
_start:
  li sp, DRAM_TOP
  la s0, bootloader
  csrr a0, mhartid
  bnez a0, boot_other_hart

  # add offset for fliped MSB
  li s1, 0x180000000
  add s0, s0, s1
  
  jr s0

boot_other_hart:

1:
  wfi
  j 1b