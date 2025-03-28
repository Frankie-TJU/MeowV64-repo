#include "common.h"
#include <riscv_vector.h>

void __attribute__((noinline)) spmv(int r, const double *val,
                                    const uint64_t *idx, const double *x,
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

int __attribute__((noinline)) spmv_buffets(int r, const double *val,
                                           const uint64_t *idx, const double *x,
                                           const uint64_t *ptr, double *y) {
  // setup address generation
  // 8 bytes per loop
  // shift = 3 (8 bytes)
  // stride = 8
  addrgen_indexed(0, 8, 3, 8, &idx[0], x);

  *ADDRGEN_ITERATIONS = ptr[r] - ptr[0];
  assert(*ADDRGEN_ITERATIONS == ptr[r] - ptr[0]);
  *ADDRGEN_CONTROL = 1;

  for (int i = 0; i < r; i++) {
    uint64_t k = ptr[i];
    uint64_t start = ptr[i];
    double yi0 = 0;

    for (int j = 0; k < ptr[i + 1]; k++, j++) {
      double tmp = ((volatile double *)BUFFETS_DATA)[j];
      // if (tmp != x[idx[k]]) {
      //   return 1;
      // }
      yi0 += val[k] * tmp;

      // avoid overflow
      if (j + 1 == 64) {
        *BUFFETS_SHRINK = 8 * (k + 1 - start);
        start = k + 1;
        j = -1;
      }
    }
    *BUFFETS_SHRINK = 8 * (ptr[i + 1] - start);

    y[i] = yi0;
  }
  return 0;
}

int __attribute__((noinline)) spmv_buffets_rvv(int r, const double *val,
                                               const uint64_t *idx,
                                               const double *x,
                                               const uint64_t *ptr, double *y) {
  // setup address generation
  // 8 bytes per loop
  // shift = 3 (8 bytes)
  // stride = 8
  addrgen_indexed(0, 8, 3, 8, &idx[0], x);

  *ADDRGEN_ITERATIONS = ptr[r] - ptr[0];
  assert(*ADDRGEN_ITERATIONS == ptr[r] - ptr[0]);
  *ADDRGEN_CONTROL = 1;

  for (int i = 0; i < r; i++) {
    uint64_t k = ptr[i];
    uint64_t start = ptr[i];

    // TODO: proper strip-mining
    size_t vl = __riscv_vsetvl_e64m1(2);
    vfloat64m1_t yi0;
    yi0 = __riscv_vfmv_v_f_f64m1(0.0, vl);

    for (int j = 0; k < ptr[i + 1]; k += 2, j += 2) {
      vfloat64m1_t valk = __riscv_vle64_v_f64m1(&val[k], vl);
      double tmp1 = ((volatile double *)BUFFETS_DATA)[j];
      double tmp2 = ((volatile double *)BUFFETS_DATA)[j + 1];
      double tmp_data[2] = {tmp1, tmp2};
      /*
      if (tmp1 != x[idx[k]]) {
        printf_("tmp1=%f\r\n", tmp1);
        printf_("x[idx[k]]=%f\r\n", x[idx[k]]);
        printf_("k=%d\r\n", k);
        printf_("i=%d\r\n", i);
        printf_("j=%d\r\n", j);
        printf_("start=%d\r\n", start);
        assert(false);
      }
      */
      vfloat64m1_t tmp = __riscv_vle64_v_f64m1(tmp_data, vl);
      yi0 = __riscv_vfmacc_vv_f64m1(yi0, valk, tmp, vl);

      // avoid overflow
      if (j + 2 == 64) {
        *BUFFETS_SHRINK = 8 * (k + 2 - start);
        start = k + 2;
        j = -2;
      }
    }
    // assert(k == ptr[i + 1]);
    *BUFFETS_SHRINK = 8 * (ptr[i + 1] - start);

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
    float diff = (y1[i] - y2[i]) / max(y1[i], y2[i]);
    if (diff > 1e-5 || diff < -1e-5) {
      printf_("y1 vs y2 Mismatch: %f vs %f\r\n", y1[i], y2[i]);
      return 1;
    }
  }

  for (int i = 0; i < N; i++) {
    float diff = (y1[i] - y3[i]) / max(y1[i], y3[i]);
    if (diff > 1e-5 || diff < -1e-5) {
      printf_("y1 vs y3 Mismatch: %f vs %f\r\n", y1[i], y3[i]);
      return 1;
    }
  }
  printf_("Result is validated\r\n");

  return 0;
}