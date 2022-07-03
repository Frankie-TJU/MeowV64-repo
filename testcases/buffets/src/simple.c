#include <stdint.h>

const uintptr_t ADDRGEN_BASE = 0x59000000;
volatile uint32_t *ADDRGEN_STATUS = (uint32_t *)(ADDRGEN_BASE + 0x00);
volatile uint32_t *ADDRGEN_CONTROL = (uint32_t *)(ADDRGEN_BASE + 0x20);
volatile uint32_t *ADDRGEN_ITERATIONS = (uint32_t *)(ADDRGEN_BASE + 0x40);
volatile uint32_t *ADDRGEN_INSTS = (uint32_t *)(ADDRGEN_BASE + 0x60);

volatile uint32_t *BUFFETS_DATA = (uint32_t *)0x5000000;

volatile uint32_t *BUFFETS_BASE = (uint32_t *)0x58000000;

uint32_t data[100];

int main()
{
  // prepare data
  int n = 100;
  for (int i = 0; i < 100; i++)
  {
    data[i] = i;
  }

  // setup address generation
  // 4 bytes per loop
  // stride = 4
  ADDRGEN_INSTS[0] = (0 << 31) | (4 << 20) | (4 << 0);
  uint64_t addr = (uint64_t)&data[0];
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;
  *ADDRGEN_ITERATIONS = 100;
  *ADDRGEN_CONTROL = 1;
  return 0;
}