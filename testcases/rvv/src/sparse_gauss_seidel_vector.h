#include "common.h"
#include <riscv_vector.h>

static unsigned long next = 1;

void my_srand(unsigned int seed) { next = seed; }

int my_rand(void) {
  next = next * 1103515245 + 12345;
  return (unsigned int)(next / 65536) % 32768;
}

// 生成[-1.0f, 1.0f]范围的随机浮点数
float my_rand_float(void) {
  // 获取0到32767的随机数
  int r = my_rand();

  // 得到[-1,1]
  return (float)r / 32767.0f * 2.0f - 1.0f;
}

float fabs(float f) { return (f < 0) ? -f : f; }

// 生成随机稀疏矩阵
void generateRandomSparseMatrix(float *matrix, int N) {
  my_srand(13000);

  // 设置目标非零元素数量（大约5%的稀疏度）
  int target_nnz = N * N / 20;
  int current_nnz = 0;

  // 矩阵清零
  for (int i = 0; i < N * N; ++i) {
    matrix[i] = 0.0;
  }

  // 先确保对角元素非零
  for (int i = 0; i < N; ++i) {
    matrix[i * N + i] = 10.0f + fabs(my_rand_float());
    current_nnz++;
  }

  // 随机填充剩余的非零元素
  while (current_nnz < target_nnz) {
    int i = my_rand() % N;
    int j = my_rand() % N;
    if (i != j && matrix[i * N + j] == 0.0f) {
      float val = my_rand_float();
      matrix[i * N + j] = val;
      current_nnz++;
    }
  }

  // 确保对角占优
  for (int i = 0; i < N; ++i) {
    float row_sum = 0.0f;
    for (int j = 0; j < N; ++j) {
      if (i != j) {
        row_sum += fabs(matrix[i * N + j]);
      }
    }
    if (row_sum >= fabs(matrix[i * N + i])) {
      matrix[i * N + i] = row_sum + 1.0f;
    }
  }
}

void generateExactSolution(float *x, int N) {
  my_srand(13000);
  for (int i = 0; i < N; i++) {
    x[i] = my_rand_float() * 4.0;
  }
}

const float EPS = 1e-3f;

void convertFromDense(int rows, int cols, float *dense_matrix, float *values,
                      int *out_nnz, int *col_indices, int *row_offsets,
                      float *diags) {
  int nnz = 0;
  row_offsets[0] = 0;

  for (int i = 0; i < rows; ++i) {
    bool has_diagonal = false;
    for (int j = 0; j < cols; ++j) {
      float val = dense_matrix[i * cols + j];
      if (fabs(val) > EPS) {
        if (i == j) {
          has_diagonal = true;
          diags[i] = val;
        } else {
          values[nnz] = val;
          col_indices[nnz] = j;
          nnz++;
        }
      }
    }
    row_offsets[i + 1] = nnz;

    assert(has_diagonal);
  }

  printf_("Matrix info: %dx%d, NNZ: %d\r\n", rows, cols, nnz + rows);
  printf_("Sparsity: %f%%\r\n", (float)(nnz + rows) / (rows * cols) * 100);
  *out_nnz = nnz;
}

void multiplyVector(int rows, int *row_offsets, float *values, int *col_indices,
                    float *x, float *b, float *diag) {
  for (int i = 0; i < rows; ++i) {
    b[i] = 0.0f;
    for (int j = row_offsets[i]; j < row_offsets[i + 1]; ++j) {
      b[i] += values[j] * x[col_indices[j]];
    }
    b[i] += diag[i] * x[i];
  }
}

// const int N = 512;
int nnz = 0;

// dense matrix
float matrix[N * N];
float exact_x[N];

const int MAX_NNZ = N * (N / 5 + 1);

// csr, diagonals are not saved
float val[MAX_NNZ];
int idx[MAX_NNZ];
int ptr[N + 1];
// diagonals
float diag[N];
// answer
float x[N];
// value on the rhs
float b[N];

// https://www.javatpoint.com/gauss-seidel-method-in-c
int main(int hartid) {
  if (hartid != 0) {
    spin();
  }

  for (int i = 0; i < 1000; i++)
    putstr(".");
  putstr("\r\n");

  int count, t, limit;
  float temp, error, a, sum = 0;

  printf_("Initialize A\r\n");
  generateRandomSparseMatrix(matrix, N);

  printf_("\r\n\r\nMatrix:\r\n\r\n");

  for (count = 0; count < N && count < 10; count++) {
    printf_("A[%d]:\t%f\r\n", count, matrix[count]);
  }

  printf_("Initialize x\r\n");
  generateExactSolution(exact_x, N);

  printf_("\r\n\r\nExact solution:\r\n\r\n");

  for (count = 0; count < N && count < 10; count++) {
    printf_("x[%d]:\t%f\r\n", count, exact_x[count]);
  }

  printf_("Initialize sparse A\r\n");
  convertFromDense(N, N, matrix, val, &nnz, idx, ptr, diag);
  printf_("Initialize b\r\n");
  multiplyVector(N, ptr, val, idx, exact_x, b, diag);

  for (count = 0; count < N; count++) {
    x[count] = 0.0;
  }

  int iter = 0;
  unsigned long total_elapsed = 0;
  do {
    unsigned long before = read_csr(mcycle);
    a = 0;

    for (count = 0; count < N; count++) {
      sum = 0;

      // spmv-like
      float d = diag[count]; // diagonal

      size_t vl = __riscv_vsetvl_e32m1(4);
      vfloat32m1_t yi0;
      yi0 = __riscv_vfmv_v_f_f32m1(0.0, vl);

      for (t = ptr[count]; t + 4 < ptr[count + 1]; t += 4) {
        // sum += val[t] * x[idx[t]];
        vfloat32m1_t valt = __riscv_vle32_v_f32m1(&val[t], vl);

        // compute byte offset
        vuint32m1_t idx_t = __riscv_vle32_v_u32m1(&idx[t], vl);
        vuint32m1_t idx_t_mul = __riscv_vsll_vx_u32m1(idx_t, 2, vl);

        // load x[idx[t]]
        vfloat32m1_t x_idx_t = __riscv_vluxei32(x, idx_t_mul, vl);

        yi0 = __riscv_vfmacc_vv_f32m1(yi0, valt, x_idx_t, vl);
      }
      vfloat32m1_t tmp = __riscv_vfmv_v_f_f32m1(0.0, vl);
      tmp = __riscv_vfredosum_vs_f32m1_f32m1(yi0, tmp, vl);

      // the remainder
      sum = __riscv_vfmv_f_s_f32m1_f32(tmp);
      for (; t < ptr[count + 1]; t++) {
        sum += val[t] * x[idx[t]];
      }

      temp = (b[count] - sum) / d;
      error = temp - x[count];
      if (error < 0) {
        error = -error;
      }

      if (error > a) {
        a = error;
      }

      x[count] = temp;
    }

    unsigned long elapsed = read_csr(mcycle) - before;
    total_elapsed += elapsed;
    printf_("Iteration %d, residual norm: %f, cycles: %ld\r\n", iter++, a,
            elapsed);
  } while (a >= EPS);
  printf_("Finished in cycles: %ld\r\n", total_elapsed);

  printf_("Result: [%f", x[0]);
  for (int i = 1; i < count; i++) {
    printf_(", %f", x[i]);
  }
  printf_("]\r\n");

  printf_("Finished in cycles: %ld\r\n", total_elapsed);
  return 0;
}
