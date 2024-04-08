#include "common.h"

#define N 100
uint32_t data[N];
uint32_t indices[N];

int mapping(int i) { return (17 * i) % N; }

int main() {
  // prepare data
  for (int i = 0; i < N; i++) {
    data[i] = i;
    // byte offset
    indices[i] = mapping(i);
  }

  // setup address generation
  // 4 bytes per loop
  // shift = 2 (4 bytes)
  // stride = 4
  ADDRGEN_INSTS[0] = (1 << 31) | (4 << 20) | (2 << 10) | (4 << 0);
  uint64_t addr = (uint64_t)&indices[0];
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;
  addr = (uint64_t)&data[0];
  ADDRGEN_INSTS[3] = addr >> 32;
  ADDRGEN_INSTS[4] = addr;
  *ADDRGEN_ITERATIONS = N;
  *ADDRGEN_CONTROL = 1;

  // read data from buffets
  for (int i = 0; i < N; i++) {
    // read and verify
    uint32_t read = BUFFETS_DATA[0];
    if (read != data[indices[i]]) {
      return 1;
    }
    *BUFFETS_SHRINK = 4;
  }

  // empty
  if (*BUFFETS_SIZE != 0) {
    return 1;
  }
  return 0;
}