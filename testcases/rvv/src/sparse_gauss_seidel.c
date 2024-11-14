#include "common.h"

static unsigned long next = 1;

void srand(unsigned int seed) { next = seed; }

int rand(void) {
  next = next * 1103515245 + 12345;
  return (unsigned int)(next / 65536) % 32768;
}

// 生成[-1.0f, 1.0f]范围的随机浮点数
float rand_float(void) {
  // 获取0到32767的随机数
  int r = rand();

  // 得到[-1,1]
  return (float)r / 32767.0f * 2.0f - 1.0f;
}

float fabs(float f) { return (f < 0) ? -f : f; }

// 生成随机稀疏矩阵
void generateRandomSparseMatrix(float *matrix, int N) {
  srand(13000);

  // 设置目标非零元素数量（大约5%的稀疏度）
  int target_nnz = N * N / 20;
  int current_nnz = 0;

  // 先确保对角元素非零
  for (int i = 0; i < N; ++i) {
    matrix[i * N + i] = 10.0f + fabs(rand_float());
    current_nnz++;
  }

  // 随机填充剩余的非零元素
  while (current_nnz < target_nnz) {
    int i = rand() % N;
    int j = rand() % N;
    if (i != j && matrix[i * N + j] == 0.0f) {
      float val = rand_float();
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

const int N = 100;
const int NNZ = 200;
const float err = 1e-3;
float y[N];
// save matrix in three parts:
// float matrix[N][N + 1];
// 1. diagonal
float diag[N];
// 2. non-diagonal sparse matrix in CSR format
float val[NNZ];
int idx[NNZ];
int ptr[N + 1];
// 3. value on the rhs
float rhs[N];

// https://www.javatpoint.com/gauss-seidel-method-in-c
int main() {
  int count, t, limit;
  float temp, error, a, sum = 0;

  printf_("Initialize data\r\n");
  for (count = 0; count < N; count++) {
    diag[count] = 1.0;
    rhs[count] = count;
    ptr[count] = 0;
  }

  for (count = 0; count < N; count++) {
    y[count] = 0;
  }

  do {
    a = 0;

    for (count = 0; count < N; count++) {
      sum = 0;

      // spmv
      for (t = ptr[count]; t < ptr[count + 1]; t++) {
        sum += val[t] * y[idx[t]];
      }

      temp = (rhs[count] - sum) / diag[count];
      error = temp - y[count];
      if (error < 0) {
        error = -error;
      }

      if (error > a) {
        a = error;
      }

      y[count] = temp;
    }
  } while (a >= err);

  printf_("\n\nSolution:\n\n");

  for (count = 0; count < N; count++) {
    printf_("Y[%d]:\t%f\n", count, y[count]);
  }

  return 0;
}