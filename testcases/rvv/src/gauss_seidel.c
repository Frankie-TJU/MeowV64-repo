#include "common.h"

const int N = 100;
const float err = 1e-3;
float matrix[N][N + 1];
float y[N];

// https://www.javatpoint.com/gauss-seidel-method-in-c
int main() {
  int count, t, limit;
  float temp, error, a, sum = 0;

  printf_("Initialize data\r\n");
  for (count = 0; count < N; count++) {
    for (t = 0; t < N + 1; t++) {
      if (t == count) {
        matrix[count][t] = 1.0;
      } else if (t == N) {
        matrix[count][t] = count;
      } else {
        matrix[count][t] = 0.0;
      }
    }
  }

  for (count = 0; count < N; count++) {
    y[count] = 0;
  }

  do {
    a = 0;

    for (count = 0; count < N; count++) {
      sum = 0;

      for (t = 0; t < N; t++) // Removed extra condition 't a'
      {
        if (t != count) {
          sum += matrix[count][t] * y[t];
        }
      }

      temp = (matrix[count][N] - sum) / matrix[count][count];
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