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
  addrgen_strided(0, 4, 4, data);
  *ADDRGEN_ITERATIONS = N;
  *ADDRGEN_CONTROL = 1;

  // read data from buffets
  uint32_t sum = 0;
  for (int i = 0; i < N; i++) {
    uint32_t tmp = BUFFETS_DATA[0];
    if (tmp != data[i]) {
      return 1;
    }
    sum += tmp;
    // shrink
    *BUFFETS_SHRINK = 4;
  }

  // validate
  if (sum != N * (N - 1) / 2) {
    return 1;
  }

  // empty
  if (*BUFFETS_SIZE != 0) {
    return 1;
  }
  return 0;
}