#include "common.h"

#define N 100
uint32_t data[N];

int main() {
  // prepare data
  for (int i = 0; i < N; i++) {
    data[i] = i;
  }

  // setup address generation
  // 4 bytes per loop
  // stride = 4
  ADDRGEN_INSTS[0] = (0 << 31) | (4 << 20) | (4 << 0);
  uint64_t addr = (uint64_t)&data[0];
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;
  *ADDRGEN_ITERATIONS = N;
  *ADDRGEN_CONTROL = 1;

  // read data from buffets
  uint32_t sum = 0;
  for (int i = 0; i < N; i++) {
    uint32_t tmp = BUFFETS_DATA[i];
    if (tmp != data[i]) {
      return 1;
    }
    sum += tmp;
  }

  // size without shrink
  if (*BUFFETS_SIZE != N * 4) {
    return 1;
  }

  // validate
  if (sum != N * (N - 1) / 2) {
    return 1;
  }

  return 0;
}