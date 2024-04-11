#include "common.h"

void __attribute__((noinline))
spmv(int r, const double *val, const uint64_t *idx, const double *x,
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

int __attribute__((noinline))
spmv_buffets(int r, const double *val, const uint64_t *idx, const double *x,
             const uint64_t *ptr, double *y) {
  // setup address generation
  // 8 bytes per loop
  // shift = 3 (8 bytes)
  // stride = 8
  ADDRGEN_INSTS[0] = (1 << 31) | (8 << 20) | (3 << 10) | (8 << 0);
  uint64_t addr = (uint64_t)&idx[0];
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;
  addr = (uint64_t)&x[0];
  ADDRGEN_INSTS[3] = addr >> 32;
  ADDRGEN_INSTS[4] = addr;

  *ADDRGEN_ITERATIONS = ptr[r] - ptr[0];
  *ADDRGEN_CONTROL = 1;

  for (int i = 0; i < r; i++) {
    uint64_t k = ptr[i];
    double yi0 = 0;

    for (int j = 0; k < ptr[i + 1]; k++, j++) {
      double tmp = ((volatile double *)BUFFETS_DATA)[j];
      if (tmp != x[idx[k]]) {
        return 1;
      }
      yi0 += val[k] * tmp;
    }
    *BUFFETS_SHRINK = 8 * (ptr[i + 1] - ptr[i]);

    y[i] = yi0;
  }
  return 0;
}

#define N 100
#define NNZ 200

double val[NNZ];
uint64_t idx[NNZ];
double x[N];
uint64_t ptr[N + 1];

int main() {
  double y1[N];
  double y2[N];

  for (int i = 0; i < NNZ; i++) {
    val[i] = (double)i;
  }
  for (int i = 0; i < NNZ; i++) {
    idx[i] = i % N;
  }
  for (int i = 0; i < N; i++) {
    x[i] = (double)i;
  }
  for (int i = 0; i < N; i++) {
    ptr[i] = i * (NNZ / N);
  }
  ptr[N] = NNZ;

  unsigned long before = read_csr(mcycle);
  spmv(N, val, idx, x, ptr, y1);
  unsigned long elapsed_scalar = read_csr(mcycle) - before;

  before = read_csr(mcycle);
  if (spmv_buffets(N, val, idx, x, ptr, y2) != 0) {
    return 1;
  }
  unsigned long elapsed_vector = read_csr(mcycle) - before;

  print(elapsed_scalar);
  print(elapsed_vector);

  for (int i = 0; i < N; i++) {
    if (y1[i] > y2[i] + 1e-5 || y1[i] < y2[i] - 1e-5) {
      return 1;
    }
  }

  return 0;
}