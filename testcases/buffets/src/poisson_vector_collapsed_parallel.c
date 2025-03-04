#include "common.h"
#include "printf.h"
#include "stdalign.h"
#include <assert.h>

#define PERSONALITY_CNT 10

#define COMPUTE_LIMIT(id)                                                      \
  size_t group_residue = GROUP_CNT % PERSONALITY_CNT;                          \
  size_t self_len =                                                            \
      (GROUP_CNT / PERSONALITY_CNT) + (id < group_residue ? 1 : 0);            \
  size_t my_grp_start = (GROUP_CNT / PERSONALITY_CNT) * id +                   \
                        (id < group_residue ? id : group_residue);             \
  size_t my_grp_end = my_grp_start + self_len;

const size_t GROUP_CNT = WIDTH / GROUP_LEN * HEIGHT;
const size_t ROW_GROUP_LEN = WIDTH / GROUP_LEN;

void diverg(data_t *field, data_t *result) {
  for (size_t hartid = 0; hartid < PERSONALITY_CNT; ++hartid) {
    COMPUTE_LIMIT(hartid);

    int gj = my_grp_start % ROW_GROUP_LEN;

    for (int i = my_grp_start / ROW_GROUP_LEN; i * ROW_GROUP_LEN < my_grp_end;
         ++i) {
      _Bool solid_boundary = i == 0 || i == HEIGHT - 1;
      data_t multiplier = solid_boundary ? -3 : -4;

      size_t remaining = my_grp_end - i * ROW_GROUP_LEN;

      for (; gj < ROW_GROUP_LEN && gj < remaining; ++gj) {
        int j = gj * GROUP_LEN;

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

        __asm__ volatile("vse32.v v0, (%0)\n"
                         :
                         : "r"(result + (i * WIDTH + j)));
      }

      gj = 0;
    }
  }
}

data_t self_dot(data_t *field) {
  data_t outer_accum = 0;
  for (size_t hartid = 0; hartid < PERSONALITY_CNT; ++hartid) {
    COMPUTE_LIMIT(hartid);

    // v1 is accumulator
    __asm__ volatile("vmv.v.i v0, 0");
    data_t *b = field + my_grp_start * GROUP_LEN;
    data_t *e = field + my_grp_end * GROUP_LEN;

    for (data_t *cur = b; cur < e; cur += GROUP_LEN) {
      __asm__ volatile("vle32.v v1, (%0)\n"
                       "vfmacc.vv v0, v1, v1\n"
                       :
                       : "r"(cur)
                       : "memory");
    }

    data_t accum = 0;
    data_t buffer[GROUP_LEN];
    __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
    for (int i = 0; i < GROUP_LEN; ++i)
      accum += buffer[i];
    outer_accum += accum;
  }

  return outer_accum;
}

data_t dot(data_t *a, data_t *b) {
  data_t outer_accum = 0;
  for (size_t hartid = 0; hartid < PERSONALITY_CNT; ++hartid) {
    COMPUTE_LIMIT(hartid);

    // v1 is accumulator
    __asm__ volatile("vmv.v.i v0, 0");

    for (int i = my_grp_start * GROUP_LEN; i < my_grp_end * GROUP_LEN;
         i += GROUP_LEN) {
      __asm__ volatile("vle32.v v1, (%0)\n"
                       "vle32.v v2, (%1)\n"
                       "vfmacc.vv v0, v1, v2\n"
                       :
                       : "r"(&a[i]), "r"(&b[i]));
    }

    data_t accum = 0;
    data_t buffer[GROUP_LEN];
    __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
    for (int i = 0; i < GROUP_LEN; ++i)
      accum += buffer[i];

    outer_accum += accum;
  }

  return outer_accum;
}

void self_relaxiation(data_t *into, data_t *val, data_t mul) {
  data_t outer_accum = 0;
  for (size_t hartid = 0; hartid < PERSONALITY_CNT; ++hartid) {
    COMPUTE_LIMIT(hartid);

    for (int i = my_grp_start * GROUP_LEN; i < my_grp_end * GROUP_LEN;
         i += GROUP_LEN) {
      __asm__ volatile("vle32.v v0, (%0)\n"
                       "vle32.v v1, (%1)\n"
                       "vfmacc.vf v1, %2, v0\n"
                       "vse32.v v1, (%1)\n"
                       :
                       : "r"(&val[i]), "r"(&into[i]), "f"(mul)
                       : "memory");
    }
  }
}

void reverse_relaxiation(data_t *into, data_t *from, data_t mul) {
  data_t outer_accum = 0;
  for (size_t hartid = 0; hartid < PERSONALITY_CNT; ++hartid) {
    COMPUTE_LIMIT(hartid);

    for (int i = my_grp_start * GROUP_LEN; i < my_grp_end * GROUP_LEN;
         i += GROUP_LEN) {
      __asm__ volatile("vle32.v v0, (%0)\n"
                       "vle32.v v1, (%1)\n"
                       "vfmacc.vf v1, %2, v0\n"
                       "vse32.v v1, (%0)\n"
                       :
                       : "r"(&into[i]), "r"(&from[i]), "f"(mul)
                       : "memory");
    }
  }
}

void init(data_t *field) {
  for (int i = 0; i < HEIGHT; ++i)
    for (int j = 0; j < WIDTH; ++j)
      field[i * WIDTH + j] = (j == 0) ? -1 : 0;
}

int main(int hartid) {
  if (hartid != 0)
    spin();

  __asm__ volatile("vsetvli t0, x0, e32" ::: "t0");

  void *heap = HEAP_BASE;
  data_t *r = heap_alloc(&heap, sizeof(data_t) * WIDTH * HEIGHT);
  data_t *x = heap_alloc(&heap, sizeof(data_t) * WIDTH * HEIGHT);
  data_t *p = heap_alloc(&heap, sizeof(data_t) * WIDTH * HEIGHT);
  data_t *div_p = heap_alloc(&heap, sizeof(data_t) * WIDTH * HEIGHT);

  if (hartid == 0) {
    putstr("Initializing input data\r\n");
    init(p); // p = r
    init(r);
    zero(x, sizeof(data_t) * WIDTH * HEIGHT);
  }

  data_t rr = self_dot(r);
  int round = 0;
  if (hartid == 0)
    printf_("Start iterations until eps < %f\r\n", EPS);
  unsigned long long elapsed = 0;
  bool early_eps_triggered = false;
  while (rr > EPS) {
    unsigned long before;
    if (hartid == 0)
      before = read_csr(mcycle);

    diverg(p, div_p);
    data_t pAp = dot(p, div_p);
    data_t alpha = rr / pAp;

    self_relaxiation(x, p, alpha);
    self_relaxiation(r, div_p, -alpha);
    data_t rr_next = self_dot(r);

    data_t beta = rr_next / rr;
    reverse_relaxiation(p, r, beta);

    rr = rr_next;
    ++round;
    if (hartid == 0) {
      unsigned elapsed_round = read_csr(mcycle) - before;
      elapsed += elapsed_round;
      printf_("Round %d: error = %.10f in %ld cycles\r\n", round, rr,
              elapsed_round);

      if (rr <= EARLY_EPS && !early_eps_triggered) {
        early_eps_triggered = true;
        printf_("Early EPS: Finished computation of %dx%d with EPS %.10f at "
                "round %d after %lld "
                "cycles (%.2f seconds)\r\n",
                WIDTH, HEIGHT, EPS, round, elapsed, elapsed / FREQ);
      }
    }
  }

  if (hartid == 0) {
    printf_(
        "Finished computation of %dx%d with EPS %.10f at round %d after %lld "
        "cycles (%.2f seconds)\r\n",
        WIDTH, HEIGHT, EPS, round, elapsed, elapsed / FREQ);

    data_t l2_sum = 0;
    for (int i = 0; i < HEIGHT; ++i) {
      for (int j = 0; j < WIDTH; ++j)
        l2_sum += x[i * WIDTH + i] * x[i * WIDTH + i];
    }
    printf_("Sum of result squared: %f\r\n", l2_sum);
    return 0;
  } else
    spin();
}
