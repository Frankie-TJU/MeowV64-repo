#include "common.h"
#include <riscv_vector.h>

#define HART_CNT 10

typedef struct {
  __attribute__((aligned(64))) size_t progress;
} hart_t;

hart_t harts[HART_CNT] = {0};

void global_sync_nodata(size_t hartid) {
  volatile size_t spin = 0;
  if (hartid != 0) {
    size_t old_progress = harts[hartid].progress;
    __atomic_store_n(&harts[hartid].progress, old_progress + 1,
                     __ATOMIC_SEQ_CST);
    while (__atomic_load_n(&harts[0].progress, __ATOMIC_SEQ_CST) <=
           old_progress)
      ++spin; // Spin
  } else {
    size_t old_progress = harts[0].progress;
    for (int h = 1; h < HART_CNT; ++h)
      while (__atomic_load_n(&harts[h].progress, __ATOMIC_SEQ_CST) <=
             old_progress)
        ++spin; // Spin
    __atomic_store_n(&harts[0].progress, old_progress + 1, __ATOMIC_SEQ_CST);
  }
}

void __attribute__((noinline)) spmv(int hartid, int r, const float *val,
                                    const uint32_t *idx, const float *x,
                                    const uint32_t *ptr, float *y) {
  size_t group_residue = r % HART_CNT;
  size_t self_len = (r / HART_CNT) + (hartid < group_residue ? 1 : 0);
  size_t self_start = (r / HART_CNT) * hartid +
                      (hartid < group_residue ? hartid : group_residue);
  size_t self_end = self_start + self_len;
  for (int i = self_start; i < self_end; i++) {
    uint32_t k;
    float yi0 = 0;
    for (k = ptr[i]; k < ptr[i + 1]; k++) {
      yi0 += val[k] * x[idx[k]];
    }
    y[i] = yi0;
  }
}

int __attribute__((noinline))
spmv_buffets_rvv(int hartid, int r, const float *val, const uint32_t *idx,
                 const float *x, const uint32_t *ptr, float *y) {
  size_t group_residue = r % HART_CNT;
  size_t self_len = (r / HART_CNT) + (hartid < group_residue ? 1 : 0);
  size_t self_start = (r / HART_CNT) * hartid +
                      (hartid < group_residue ? hartid : group_residue);
  size_t self_end = self_start + self_len;
  // setup address generation
  // 4 bytes per loop
  // shift = 2 (4 bytes)
  // stride = 4
  ADDRGEN_INSTS[0] = (1 << 31) | (4 << 20) | (2 << 10) | (4 << 0);
  uint64_t addr = (uint64_t)&idx[ptr[self_start]];
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;
  addr = (uint64_t)&x[0];
  ADDRGEN_INSTS[3] = addr >> 32;
  ADDRGEN_INSTS[4] = addr;

  *ADDRGEN_ITERATIONS = ptr[self_end] - ptr[self_start];
  assert(*ADDRGEN_ITERATIONS == ptr[self_end] - ptr[self_start]);
  *ADDRGEN_CONTROL = 1;

  for (int i = self_start; i < self_end; i++) {
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
        printf_("hartid=%d\r\n", hartid);
        printf_("tmp1=%f\r\n", tmp1);
        printf_("tmp2=%f\r\n", tmp2);
        printf_("tmp3=%f\r\n", tmp3);
        printf_("tmp4=%f\r\n", tmp4);
        printf_("x[idx[k]]=%f\r\n", x[idx[k]]);
        printf_("x[idx[0]]=%f\r\n", x[idx[0]]);
        printf_("idx[0]=%d\r\n", idx[0]);
        printf_("idx[k]=%d\r\n", idx[k]);
        printf_("&idx[k]=%x\r\n", &idx[k]);
        printf_("&x[idx[k]]=%x\r\n", &x[idx[k]]);
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

float val[NNZ];
uint32_t idx[NNZ];
float x[N];
uint32_t ptr[N + 1];

static uint64_t lfsr63(uint64_t x) {
  uint64_t bit = (x ^ (x >> 1)) & 1;
  return (x >> 1) | (bit << 62);
}

[[noreturn]] void spin() {
  volatile size_t meow;
  while (1)
    ++meow;
}

float y1[N];
float y2[N];

int main(int hartid) {
  if (hartid >= HART_CNT)
    spin();
  if (hartid == 0) {
    printf_("Matrix: %dx%d with %d nnz\r\n", N, N, NNZ);

    printf_("Generate data\r\n");
    for (int i = 0; i < NNZ; i++) {
      val[i] = (float)i;
    }
    uint64_t seed = 1;
    for (int i = 0; i < NNZ; i++) {
      // seed = lfsr63(seed);
      // idx[i] = seed % N;
      idx[i] = i % N;
    }
    for (int i = 0; i < N; i++) {
      // avoid vectorization, it may use vid.v and vfcvt
      *(volatile float *)&x[i] = (float)i;
    }
    for (int i = 0; i < N; i++) {
      ptr[i] = i * (NNZ / N);
    }
    ptr[N] = NNZ;
  }

  // if (hartid == 0)
  //   printf_("Run spmv scalar\r\n");
  unsigned long before = read_csr(mcycle);
  global_sync_nodata(hartid);
  // spmv(hartid, N, val, idx, x, ptr, y1);
  global_sync_nodata(hartid);
  unsigned long elapsed_scalar = read_csr(mcycle) - before;

  if (hartid == 0)
    printf_("Run spmv vector buffets\r\n");
  before = read_csr(mcycle);
  global_sync_nodata(hartid);
  assert(spmv_buffets_rvv(hartid, N, val, idx, x, ptr, y2) == 0);
  global_sync_nodata(hartid);
  unsigned long elapsed_buffets_rvv = read_csr(mcycle) - before;

  if (hartid == 0) {
    // for (int i = 0; i < N; i++) {
    //   float diff = (y1[i] - y2[i]) / max(y1[i], y2[i]);
    //   if (diff > 1e-5 || diff < -1e-5) {
    //     printf_("y1 vs y2 Mismatch: %f vs %f\r\n", y1[i], y2[i]);
    //     return 1;
    //   }
    // }

    // printf_("Result is validated\r\n");

    // printf_("Perf spmv scalar: %d cycles\r\n", elapsed_scalar);
    printf_("Perf spmv vector buffets: %d cycles\r\n", elapsed_buffets_rvv);

    // printf_("Result: [%f", y2[0]);
    // for (int i = 1; i < N; i++) {
    //   printf_(", %f", y2[i]);
    // }
    // printf_("]\r\n");

    // printf_("Perf spmv scalar: %d cycles\r\n", elapsed_scalar);
    // printf_("Perf spmv vector buffets: %d cycles\r\n", elapsed_buffets_rvv);
  } else {
    spin();
  }
  return 0;
}
