#include "common.h"

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