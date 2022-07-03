.section ".tohost","aw",@progbits
.align 6
.globl tohost
tohost: .dword 0
.align 6
.globl fromhost
fromhost: .dword 0

#define FAIL 2: \
  li a0, 3; \
  la a1, tohost; \
  sw a0, 0(a1); \
  j 2b;

#define SUCCESS 2: \
  li a0, 1; \
  la a1, tohost; \
  sw a0, 0(a1); \
  j 2b;
