#include "common.h"

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
  for (int i = 0; i < r; i++) {
    uint64_t k = ptr[i];
    double yi0 = 0;

    ADDRGEN_INSTS[0] = (1 << 31) | (8 << 20) | (8 << 0);
    uint64_t addr = (uint64_t)&idx[k];
    ADDRGEN_INSTS[1] = addr >> 32;
    ADDRGEN_INSTS[2] = addr;
    addr = (uint64_t)&x[0];
    ADDRGEN_INSTS[3] = addr >> 32;
    ADDRGEN_INSTS[4] = addr;

    *ADDRGEN_ITERATIONS = ptr[i + 1] - ptr[i];
    *ADDRGEN_CONTROL = 1;

    for (i = 0; k < ptr[i + 1]; k++, i++) {
      yi0 += val[k] * ((volatile double *)BUFFETS_DATA)[i];
    }
    *BUFFETS_SHRINK = 8 * (ptr[i + 1] - ptr[i]);

    y[i] = yi0;
  }
}

int main() { return 1; }