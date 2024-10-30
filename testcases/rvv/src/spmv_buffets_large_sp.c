#include "common.h"
#include <riscv_vector.h>

void __attribute__((noinline))
spmv(int r, const float *val, const uint32_t *idx, const float *x,
     const uint32_t *ptr, float *y) {
  for (int i = 0; i < r; i++) {
    uint32_t k;
    float yi0 = 0;
    for (k = ptr[i]; k < ptr[i + 1]; k++) {
      yi0 += val[k] * x[idx[k]];
    }
    y[i] = yi0;
  }
}

int __attribute__((noinline))
spmv_buffets(int r, const float *val, const uint32_t *idx, const float *x,
             const uint32_t *ptr, float *y) {
  // setup address generation
  // 4 bytes per loop
  // shift = 2 (4 bytes)
  // stride = 4
  ADDRGEN_INSTS[0] = (1 << 31) | (4 << 20) | (2 << 10) | (4 << 0);
  uint64_t addr = (uint64_t)&idx[0];
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;
  addr = (uint64_t)&x[0];
  ADDRGEN_INSTS[3] = addr >> 32;
  ADDRGEN_INSTS[4] = addr;

  *ADDRGEN_ITERATIONS = ptr[r] - ptr[0];
  assert(*ADDRGEN_ITERATIONS == ptr[r] - ptr[0]);
  *ADDRGEN_CONTROL = 1;

  for (int i = 0; i < r; i++) {
    uint64_t k = ptr[i];
    uint64_t start = ptr[i];
    float yi0 = 0;

    for (int j = 0; k < ptr[i + 1]; k++, j++) {
      float tmp = ((volatile float *)BUFFETS_DATA)[j];
      // if (tmp != x[idx[k]]) {
      //   return 1;
      // }
      yi0 += val[k] * tmp;

      // avoid overflow
      if (j + 1 == 64) {
        *BUFFETS_SHRINK = 4 * (k + 1 - start);
        start = k + 1;
        j = -1;
      }
    }
    *BUFFETS_SHRINK = 4 * (ptr[i + 1] - start);

    y[i] = yi0;
  }
  return 0;
}

int __attribute__((noinline))
spmv_buffets_rvv(int r, const float *val, const uint32_t *idx, const float *x,
                 const uint32_t *ptr, float *y) {
  // setup address generation
  // 4 bytes per loop
  // shift = 2 (4 bytes)
  // stride = 4
  ADDRGEN_INSTS[0] = (1 << 31) | (4 << 20) | (2 << 10) | (4 << 0);
  uint64_t addr = (uint64_t)&idx[0];
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;
  addr = (uint64_t)&x[0];
  ADDRGEN_INSTS[3] = addr >> 32;
  ADDRGEN_INSTS[4] = addr;

  *ADDRGEN_ITERATIONS = ptr[r] - ptr[0];
  assert(*ADDRGEN_ITERATIONS == ptr[r] - ptr[0]);
  *ADDRGEN_CONTROL = 1;

  for (int i = 0; i < r; i++) {
    uint64_t k = ptr[i];
    uint64_t start = ptr[i];

    // TODO: proper strip-mining
    size_t vl = __riscv_vsetvl_e32m1(4);
    vfloat32m1_t yi0;
    yi0 = __riscv_vfmv_v_f_f32m1(0.0, vl);

    for (int j = 0; k < ptr[i + 1]; k += 4, j += 4) {
      vfloat32m1_t valk = __riscv_vle32_v_f32m1(&val[k], vl);
      float tmp1 = ((volatile float *)BUFFETS_DATA)[j];
      float tmp2 = ((volatile float *)BUFFETS_DATA)[j + 1];
      float tmp3 = ((volatile float *)BUFFETS_DATA)[j + 2];
      float tmp4 = ((volatile float *)BUFFETS_DATA)[j + 3];
      float tmp_data[4] = {tmp1, tmp2, tmp3, tmp4};
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
      vfloat32m1_t tmp = __riscv_vle32_v_f32m1(tmp_data, vl);
      yi0 = __riscv_vfmacc_vv_f32m1(yi0, valk, tmp, vl);

      // avoid overflow
      if (j + 4 == 64) {
        *BUFFETS_SHRINK = 4 * (k + 4 - start);
        start = k + 4;
        j = -4;
      }
    }
    // assert(k == ptr[i + 1]);
    *BUFFETS_SHRINK = 4 * (ptr[i + 1] - start);

    vfloat32m1_t res;
    res = __riscv_vfmv_v_f_f32m1(0.0, vl);
    res = __riscv_vfredosum_vs_f32m1_f32m1(yi0, res, vl);
    y[i] = __riscv_vfmv_f_s_f32m1_f32(res);
  }
  return 0;
}

#define N 5000
#define NNZ 20000

float val[NNZ];
uint32_t idx[NNZ];
float x[N];
uint32_t ptr[N + 1];

static uint64_t lfsr63(uint64_t x) {
  uint64_t bit = (x ^ (x >> 1)) & 1;
  return (x >> 1) | (bit << 62);
}

int main() {
  for (int i = 0; i < 1000; i++)
    putstr(".");
  putstr("\r\n");

  float y1[N];
  float y2[N];
  float y3[N];
  printf_("Matrix: %dx%d with %d nnz\r\n", N, N, NNZ);

  printf_("Generate data\r\n");
  for (int i = 0; i < NNZ; i++) {
    val[i] = (float)i;
  }
  uint64_t seed = 1;
  for (int i = 0; i < NNZ; i++) {
    seed = lfsr63(seed);
    idx[i] = seed % N;
  }
  for (int i = 0; i < N; i++) {
    // avoid vectorization, it may use vid.v and vfcvt
    *(volatile float *)&x[i] = (float)i;
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