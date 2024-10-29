#include "common.h"
#include "printf.h"
#include <riscv_vector.h>

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
      // if (tmp != x[idx[k]]) {
      //   return 1;
      // }
      yi0 += val[k] * tmp;
    }
    *BUFFETS_SHRINK = 8 * (ptr[i + 1] - ptr[i]);

    y[i] = yi0;
  }
  return 0;
}

int __attribute__((noinline))
spmv_buffets_rvv(int r, const double *val, const uint64_t *idx, const double *x,
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

    // TODO: proper strip-mining
    size_t vl = __riscv_vsetvl_e64m1(2);
    vfloat64m1_t yi0;
    yi0 = __riscv_vfmv_v_f_f64m1(0.0, vl);

    for (int j = 0; k < ptr[i + 1]; k += 2, j++) {
      vfloat64m1_t valk = __riscv_vle64_v_f64m1(&val[k], vl);
      double tmp1 = ((volatile double *)BUFFETS_DATA)[j];
      double tmp2 = ((volatile double *)BUFFETS_DATA)[j + 1];
      double tmp_data[2] = {tmp1, tmp2};
      vfloat64m1_t tmp = __riscv_vle64_v_f64m1(tmp_data, vl);
      yi0 = __riscv_vfmacc_vv_f64m1(yi0, valk, tmp, vl);
    }
    *BUFFETS_SHRINK = 8 * (ptr[i + 1] - ptr[i]);

    vfloat64m1_t res;
    res = __riscv_vfmv_v_f_f64m1(0.0, vl);
    res = __riscv_vfredosum_vs_f64m1_f64m1(yi0, res, vl);
    y[i] = __riscv_vfmv_f_s_f64m1_f64(res);
  }
  return 0;
}

#define N 10000
#define NNZ 20000

double val[NNZ];
uint64_t idx[NNZ];
double x[N];
uint64_t ptr[N + 1];

static uint64_t lfsr63(uint64_t x) {
  uint64_t bit = (x ^ (x >> 1)) & 1;
  return (x >> 1) | (bit << 62);
}

int main() {
  for (int i = 0; i < 1000; i++)
    putstr(".");
  putstr("\r\n");

  double y1[N];
  double y2[N];
  double y3[N];
  printf_("Matrix: %dx%d with %d nnz\r\n", N, N, NNZ);

  printf_("Generate data\r\n");
  for (int i = 0; i < NNZ; i++) {
    val[i] = (double)i;
  }
  uint64_t seed = 1;
  for (int i = 0; i < NNZ; i++) {
    seed = lfsr63(seed);
    idx[i] = seed % N;
  }
  for (int i = 0; i < N; i++) {
    x[i] = (double)i;
  }
  for (int i = 0; i < N; i++) {
    ptr[i] = i * (NNZ / N);
  }
  ptr[N] = NNZ;

  printf_("Run spmv scalar\r\n");
  unsigned long before = read_csr(mcycle);
  spmv(N, val, idx, x, ptr, y1);
  unsigned long elapsed_scalar = read_csr(mcycle) - before;

  printf_("Run spmv scalar buffets\r\n");
  before = read_csr(mcycle);
  if (spmv_buffets(N, val, idx, x, ptr, y2) != 0) {
    return 1;
  }
  unsigned long elapsed_buffets = read_csr(mcycle) - before;

  printf_("Run spmv vector buffets\r\n");
  before = read_csr(mcycle);
  if (spmv_buffets_rvv(N, val, idx, x, ptr, y3) != 0) {
    return 1;
  }
  unsigned long elapsed_buffets_rvv = read_csr(mcycle) - before;

  printf_("Perf spmv scalar: %d cycles\r\n", elapsed_scalar);
  printf_("Perf spmv scalar buffets: %d cycles\r\n", elapsed_buffets);
  printf_("Perf spmv vector buffets: %d cycles\r\n", elapsed_buffets_rvv);

  for (int i = 0; i < N; i++) {
    if (y1[i] > y2[i] + 1e-5 || y1[i] < y2[i] - 1e-5) {
      printf_("Mismatch: %f vs %f\r\n", y1[i], y2[i]);
      return 1;
    }
  }

  for (int i = 0; i < N; i++) {
    if (y1[i] > y3[i] + 1e-5 || y1[i] < y3[i] - 1e-5) {
      printf_("Mismatch: %f vs %f\r\n", y1[i], y3[i]);
      return 1;
    }
  }

  return 0;
}