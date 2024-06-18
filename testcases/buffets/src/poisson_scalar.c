#include "common.h"
#include "printf.h"
#include <assert.h>

void diverg(data_t *field, data_t *result) {
  for (int i = 0; i < HEIGHT; ++i) {
    for (int j = 0; j < WIDTH; ++j) {
      _Bool solid_boundary = i == 0 || i == HEIGHT - 1;
      data_t cur = field[i * WIDTH + j] * (solid_boundary ? -3 : -4);

      if (j != 0)
        cur += field[i * WIDTH + (j - 1)];
      if (j != WIDTH - 1)
        cur += field[i * WIDTH + (j + 1)];
      if (i != 0)
        cur += field[(i - 1) * WIDTH + j];
      if (i != HEIGHT - 1)
        cur += field[(i + 1) * WIDTH + j];

      result[i * WIDTH + j] = cur;
    }
  }
}

data_t self_dot(data_t *field) {
  data_t accum = 0;
  for (int i = 0; i < WIDTH * HEIGHT; ++i)
    accum += field[i] * field[i];
  return accum;
}

data_t dot(data_t *a, data_t *b) {
  data_t accum = 0;
  for (int i = 0; i < WIDTH * HEIGHT; ++i)
    accum += a[i] * b[i];
  return accum;
}

void self_relaxiation(data_t *into, data_t *val, data_t mul) {
  for (int i = 0; i < WIDTH * HEIGHT; ++i)
    into[i] += val[i] * mul;
}

void relaxiation(data_t *into, data_t *from, data_t *val, data_t mul) {
  for (int i = 0; i < WIDTH * HEIGHT; ++i)
    into[i] = from[i] + val[i] * mul;
}

void init(data_t *field) {
  for (int i = 0; i < HEIGHT; ++i)
    for (int j = 0; j < WIDTH; ++j)
      field[i * WIDTH + j] = (j == 0) ? -1 : 0;
}

void *HEAP_BASE = 0x84000000;
void *alloc(size_t size) {
  void *ret = HEAP_BASE;
  HEAP_BASE = HEAP_BASE + ((size + 63) & (~63)); // Manually 64-byte alignment
  return ret;
}

int main() {
  for (int i = 0; i < 1000; i++)
    putstr(".");
  putstr("\r\n");

  __asm__ volatile("vsetvli t0, x0, e32" ::: "t0");

  data_t *r = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *x = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *p = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *div_p = alloc(sizeof(data_t) * WIDTH * HEIGHT);

  putstr("Initializing input data\r\n");
  init(p); // p = r
  init(r);
  zero(x, sizeof(data_t) * WIDTH * HEIGHT);

  data_t rr = self_dot(r);
  int round = 0;
  printf_("Start iterations until eps < %f\r\n", EPS);
  unsigned long long elapsed = 0;
  while (rr > EPS) {
    unsigned long before = read_csr(mcycle);
    diverg(p, div_p);
    data_t pAp = dot(p, div_p);
    data_t alpha = rr / pAp;

    self_relaxiation(x, p, alpha);
    self_relaxiation(r, div_p, -alpha);
    data_t rr_next = self_dot(r);

    data_t beta = rr_next / rr;
    relaxiation(p, r, p, beta);

    rr = rr_next;
    unsigned elapsed_round = read_csr(mcycle) - before;
    elapsed += elapsed_round;
    ++round;
    printf_("Round %d: error = %f in %ld cycles\r\n", round, rr, elapsed_round);
  }
  printf_("Finished at round %d after %lld cycles\r\n", round, elapsed);

  data_t l2_sum = 0;
  for (int i = 0; i < HEIGHT; ++i) {
    for (int j = 0; j < WIDTH; ++j)
      l2_sum += x[i * WIDTH + i] * x[i * WIDTH + i];
  }
  printf_("Sum of result squared: %f\r\n", l2_sum);
  return 0;
}
