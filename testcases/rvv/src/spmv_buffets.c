#include "common.h"
#include <riscv_vector.h>

#define N 10
#define NNZ 20
// compute the following spmv:
// A=
// [ 1.0 2.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 ]
// [ 0.0 0.0 3.0 0.0 4.0 0.0 0.0 0.0 0.0 0.0 ]
// [ 0.0 0.0 0.0 0.0 0.0 0.0 5.0 0.0 0.0 1.0 ]
// [ 0.0 0.0 0.0 2.0 0.0 0.0 3.0 0.0 0.0 0.0 ]
// [ 0.0 0.0 4.0 0.0 5.0 0.0 0.0 0.0 0.0 0.0 ]
// [ 1.0 2.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 ]
// [ 0.0 0.0 3.0 0.0 4.0 0.0 0.0 0.0 0.0 0.0 ]
// [ 0.0 0.0 0.0 0.0 0.0 0.0 5.0 0.0 0.0 1.0 ]
// [ 0.0 0.0 2.0 0.0 0.0 0.0 3.0 0.0 0.0 0.0 ]
// [ 0.0 0.0 4.0 0.0 5.0 0.0 0.0 0.0 0.0 0.0 ]
// x = [ 10.0 20.0 30.0 40.0 50.0 10.0 20.0 30.0 40.0 50.0 ]^T
// y = Ax = [ 50.0 290.0 150.0 140.0 370.0 50.0 290.0 150.0 140.0 370.0 ]^T
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

int main() {
  double y[N] = {50.0, 290.0, 150.0, 140.0, 370.0,
                 50.0, 290.0, 150.0, 140.0, 370.0};
  double y1[N];
  double y2[N];
  double y3[N];
  printf_("Matrix: %dx%d with %d nnz\r\n", N, N, NNZ);

  printf_("Run spmv scalar\r\n");
  unsigned long before = read_csr(mcycle);
  spmv(N, val, idx, x, ptr, y1);
  unsigned long elapsed_scalar = read_csr(mcycle) - before;

  for (int i = 0; i < N; i++) {
    if (y1[i] > y[i] + 1e-5 || y1[i] < y[i] - 1e-5) {
      return 1;
    }
  }

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
  unsigned long elapsed_buffets_vector = read_csr(mcycle) - before;

  printf_("Perf spmv scalar: %d cycles\r\n", elapsed_scalar);
  printf_("Perf spmv scalar buffets: %d cycles\r\n", elapsed_buffets);
  printf_("Perf spmv vector buffets: %d cycles\r\n", elapsed_buffets_vector);

  for (int i = 0; i < N; i++) {
    if (y1[i] > y2[i] + 1e-5 || y1[i] < y2[i] - 1e-5) {
      printf_("Mismatch: %f vs %f\r\n", y1[i], y2[i]);
      return 1;
    }
    if (y1[i] > y3[i] + 1e-5 || y1[i] < y3[i] - 1e-5) {
      printf_("Mismatch: %f vs %f\r\n", y1[i], y3[i]);
      return 1;
    }
  }

  return 0;
}