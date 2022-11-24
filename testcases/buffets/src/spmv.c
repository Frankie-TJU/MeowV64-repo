#include "common.h"

#define N 10
#define NNZ 20
const double val[NNZ] = {
    1.0, 2.0, 3.0, 4.0, 5.0, 1.0, 2.0, 3.0, 4.0, 5.0,
    1.0, 2.0, 3.0, 4.0, 5.0, 1.0, 2.0, 3.0, 4.0, 5.0,
};

const uint64_t idx[NNZ] = {
    0, 1, 2, 4, 6, 9, 3, 6, 2, 4, 0, 1, 2, 4, 6, 9, 3, 6, 2, 4,
};

const double x[N] = {
    10.0, 20.0, 30.0, 40.0, 50.0, 10.0, 20.0, 30.0, 40.0, 50.0,
};

const uint64_t ptr[N + 1] = {0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20};

void spmv(int r, const double *val, const uint64_t *idx, const double *x,
          const uint64_t *ptr, double *y) {
  for (int i = 0; i < r; i++) {
    uint64_t k;
    double yi0 = 0;
    for (k = ptr[i]; k < ptr[i + 1]; k++) {
      yi0 += val[k] * x[idx[k]];
    }
    y[i] = yi0;
  }
}

void spmv_buffets(int r, const double *val, const uint64_t *idx,
                  const double *x, const uint64_t *ptr, double *y) {
  ADDRGEN_INSTS[0] = (1 << 31) | (8 << 20) | (8 << 0);
  uint64_t addr = (uint64_t)&idx[0];
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;
  addr = (uint64_t)&x[0];
  ADDRGEN_INSTS[3] = addr >> 32;
  ADDRGEN_INSTS[4] = addr;

  //*ADDRGEN_ITERATIONS = ptr[r + 1] - ptr[0];
  *ADDRGEN_ITERATIONS = 20;
  *ADDRGEN_CONTROL = 1;

  for (int i = 0; i < r; i++) {
    uint64_t k = ptr[i];
    double yi0 = 0;

    for (int j = 0; k < ptr[i + 1]; k++, j++) {
      yi0 += val[k] * ((volatile double *)BUFFETS_DATA)[j];
    }
    *BUFFETS_SHRINK = 8 * (ptr[i + 1] - ptr[i]);

    y[i] = yi0;
  }
}

int main() {
  double y[N];
  spmv(N, val, idx, x, ptr, y);
  spmv_buffets(N, val, idx, x, ptr, y);
  return 0;
}