#include "common.h"
#include "printf.h"
#include <assert.h>

void diverg(data_t *field, data_t *result) {
  for (int i = 0; i < HEIGHT; ++i) {
    _Bool solid_boundary = i == 0 || i == HEIGHT - 1;
    data_t multiplier = solid_boundary ? -3 : -4;

    for (int j = 0; j < WIDTH; j += GROUP_LEN) {
      data_t left = (j == 0) ? 0 : field[i * WIDTH + (j - 1)];
      data_t right =
          (j == WIDTH - GROUP_LEN) ? 0 : field[i * WIDTH + j + GROUP_LEN];

      __asm__ volatile("vle32.v v0, (%0)\n"
                       "vfslide1up.vf v1, v0, %1\n"
                       "vfslide1down.vf v2, v0, %2\n"
                       "vfmadd.vf v0, %3, v1\n"
                       "vfadd.vv v0, v0, v2\n"
                       :
                       : "r"(field + (i * WIDTH + j)), "f"(left), "f"(right),
                         "f"(multiplier));

      if (i != 0) {
        __asm__ volatile("vle32.v v1, (%0)\n"
                         "vfadd.vv v0, v0, v1\n"
                         :
                         : "r"(field + ((i - 1) * WIDTH + j)));
      }
      if (i != HEIGHT - 1) {
        __asm__ volatile("vle32.v v1, (%0)\n"
                         "vfadd.vv v0, v0, v1\n"
                         :
                         : "r"(field + ((i + 1) * WIDTH + j)));
      }

      __asm__ volatile("vse32.v v0, (%0)\n" : : "r"(result + (i * WIDTH + j)));
    }
  }
}

data_t self_dot(data_t *field) {
  static_assert(WIDTH * HEIGHT % GROUP_LEN == 0, "");
  __asm__ volatile("vmv.v.i v0, 0");
  for (int i = 0; i < WIDTH * HEIGHT; i += GROUP_LEN) {
    __asm__ volatile("vle32.v v1, 0(%0)\n"
                     "vfmacc.vv v0, v1, v1\n"
                     :
                     : "r"(&field[i])
                     : "memory");
  }

  data_t accum = 0;
  data_t buffer[GROUP_LEN];
  __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
  for (int i = 0; i < GROUP_LEN; ++i)
    accum += buffer[i];
  return accum;
}

data_t dot(data_t *a, data_t *b) {
  static_assert(WIDTH * HEIGHT % GROUP_LEN == 0, "");
  __asm__ volatile("vmv.v.i v0, 0");
  for (int i = 0; i < WIDTH * HEIGHT; i += GROUP_LEN) {
    __asm__ volatile("vle32.v v1, 0(%0)\n"
                     "vle32.v v2, 0(%1)\n"
                     "vfmacc.vv v0, v1, v2\n"
                     :
                     : "r"(&a[i]), "r"(&b[i])
                     : "memory");
  }

  data_t accum = 0;
  data_t buffer[GROUP_LEN];
  __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
  for (int i = 0; i < GROUP_LEN; ++i)
    accum += buffer[i];
  return accum;
}

void self_relaxiation(data_t *into, data_t *val, data_t mul) {
  static_assert(WIDTH * HEIGHT % GROUP_LEN == 0, "");
  for (int i = 0; i < WIDTH * HEIGHT; i += GROUP_LEN) {
    __asm__ volatile("vle32.v v0, 0(%0)\n"
                     "vle32.v v1, 0(%1)\n"
                     "vfmacc.vf v1, %2, v0\n"
                     "vse32.v v1, 0(%1)\n"
                     :
                     : "r"(&val[i]), "r"(&into[i]), "f"(mul)
                     : "memory");
  }
}

void reverse_relaxiation(data_t *into, data_t *from, data_t mul) {
  static_assert(WIDTH * HEIGHT % 8 == 0, "");
  for (int i = 0; i < WIDTH * HEIGHT; i += GROUP_LEN) {
    __asm__ volatile("vle32.v v0, 0(%0)\n"
                     "vle32.v v1, 0(%1)\n"
                     "vfmacc.vf v1, %2, v0\n"
                     "vse32.v v1, 0(%0)\n"
                     :
                     : "r"(&into[i]), "r"(&from[i]), "f"(mul)
                     : "memory");
  }
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
    reverse_relaxiation(p, r, beta);

    rr = rr_next;
    unsigned elapsed_round = read_csr(mcycle) - before;
    elapsed += elapsed_round;
    ++round;
    printf_("Round %d: error = %f in %ld cycles\r\n", round, rr, elapsed_round);
  }
  printf_("Finished computation of %dx%d with EPS %f at round %d after %lld "
          "cycles (%.2f seconds)\r\n",
          WIDTH, HEIGHT, EPS, round, elapsed, elapsed / FREQ);

  printf_("Result: [%f", x[0]);
  for (int i = 1; i < HEIGHT * WIDTH; i++) {
    printf_(", %f", x[i]);
  }
  printf_("]\r\n");

  data_t l2_sum = 0;
  for (int i = 0; i < HEIGHT; ++i) {
    for (int j = 0; j < WIDTH; ++j) {
      l2_sum += x[i * WIDTH + j] * x[i * WIDTH + j];
    }
  }
  printf_("Sum of result squared: %f\r\n", l2_sum);
  return 0;
}
