#include "common.h"
#include "printf.h"

#define HART_CNT 2

const size_t GROUP_CNT = WIDTH / GROUP_LEN * HEIGHT;

typedef struct {
  size_t progress;
  data_t buffer;
  char _padding[64 - sizeof(size_t) - sizeof(data_t)];
} hart_t;

hart_t harts[HART_CNT];

void global_sync_nodata(size_t hartid) {
  volatile size_t spin = 0;
  if(hartid != 0) {
    size_t old_progress = harts[hartid].progress;
    __atomic_store_n(&harts[hartid].progress, old_progress + 1, __ATOMIC_SEQ_CST);
    while(__atomic_load_n(&harts[0].progress, __ATOMIC_SEQ_CST) <= old_progress) ++spin; // Spin
  } else {
    size_t old_progress = harts[0].progress;
    for(int h = 1; h < HART_CNT; ++h)
      while(__atomic_load_n(&harts[h].progress, __ATOMIC_SEQ_CST) <= old_progress) ++spin; // Spin
    __atomic_store_n(&harts[0].progress, old_progress + 1, __ATOMIC_SEQ_CST);
  }
}

data_t global_sync(size_t hartid, data_t my_data) {
  volatile size_t spin = 0;
  if(hartid != 0) {
    harts[hartid].buffer = my_data;
    size_t old_progress = harts[hartid].progress;
    __atomic_store_n(&harts[hartid].progress, old_progress + 1, __ATOMIC_SEQ_CST);
    while(__atomic_load_n(&harts[0].progress, __ATOMIC_SEQ_CST) <= old_progress) ++spin; // Spin
    // print_delim(hartid, "");
    return harts[0].buffer;
  } else {
    size_t old_progress = harts[0].progress;
    data_t accum = my_data;
    for(int h = 1; h < HART_CNT; ++h) {
      while(__atomic_load_n(&harts[h].progress, __ATOMIC_SEQ_CST) <= old_progress) ++spin; // Spin
      accum += harts[h].buffer;
    }
    harts[0].buffer = accum;
    __atomic_store_n(&harts[0].progress, old_progress + 1, __ATOMIC_SEQ_CST);
    // print_delim(hartid, "");
    return accum;
  }
}

const size_t ROW_GROUP_LEN = WIDTH / GROUP_LEN;

void diverg(data_t *field, data_t *result, size_t my_grp_start, size_t my_grp_end, size_t hartid) {
  int gj = my_grp_start % ROW_GROUP_LEN;

  for(int i = my_grp_start / ROW_GROUP_LEN; i * ROW_GROUP_LEN < my_grp_end; ++i) {
    _Bool solid_boundary = i == 0 || i == HEIGHT - 1;
    data_t multiplier = solid_boundary ? -3 : -4;

    size_t remaining = my_grp_end - i * ROW_GROUP_LEN;

    for(; gj < ROW_GROUP_LEN && gj < remaining; ++gj) {
      int j = gj * GROUP_LEN;

      data_t left = (j == 0) ? 0 : field[i * WIDTH + (j-1)];
      data_t right = (j == WIDTH - GROUP_LEN) ? 0 : field[i * WIDTH + j + GROUP_LEN];

      __asm__ volatile(
        "vle32.v v0, (%0)\n"
        "vfslide1up.vf v1, v0, %1\n"
        "vfslide1down.vf v2, v0, %2\n"
        "vfmadd.vf v0, %3, v1\n"
        "vfadd.vv v0, v0, v2\n"
        : 
        : "r" (field + (i * WIDTH + j)), "f" (left), "f" (right), "f"(multiplier)
      );

      if(i != 0) {
        __asm__ volatile(
          "vle32.v v1, (%0)\n"
          "vfadd.vv v0, v0, v1\n"
          : 
          : "r" (field + ((i - 1) * WIDTH + j))
        );
      }
      if(i != HEIGHT - 1) {
        __asm__ volatile(
          "vle32.v v1, (%0)\n"
          "vfadd.vv v0, v0, v1\n"
          : 
          : "r" (field + ((i + 1) * WIDTH + j))
        );
      }

      __asm__ volatile(
        "vse32.v v0, (%0)\n"
        : 
        : "r" (result + (i * WIDTH + j))
      );
    }

    gj = 0;
  }
}

data_t self_dot(data_t *field, size_t my_grp_start, size_t my_grp_end, size_t hartid) {
  // v1 is accumulator
  __asm__ volatile("vmv.v.i v0, 0");
  data_t *b = field + my_grp_start * GROUP_LEN;
  data_t *e = field + my_grp_end * GROUP_LEN;

  for(data_t *cur = b; cur < e; cur += GROUP_LEN) {
    __asm__ volatile(
      "vle32.v v1, (%0)\n"
      "vfmacc.vv v0, v1, v1\n"
      : 
      : "r" (cur)
    );
  }

  data_t accum = 0;
  data_t buffer[GROUP_LEN];
  __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
  for(int i = 0; i < GROUP_LEN; ++i) accum += buffer[i];

  return global_sync(hartid, accum);
}

data_t dot(data_t *a, data_t *b, size_t my_grp_start, size_t my_grp_end, size_t hartid) {
  // v1 is accumulator
  __asm__ volatile("vmv.v.i v0, 0");

  for(int i = my_grp_start * GROUP_LEN; i < my_grp_end * GROUP_LEN; i += GROUP_LEN) {
    __asm__ volatile(
      "vle32.v v1, (%0)\n"
      "vle32.v v2, (%1)\n"
      "vfmacc.vv v0, v1, v2\n"
      : 
      : "r" (&a[i]), "r" (&b[i])
    );
  }

  data_t accum = 0;
  data_t buffer[GROUP_LEN];
  __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
  for(int i = 0; i < GROUP_LEN; ++i) accum += buffer[i];

  return global_sync(hartid, accum);
}

void self_relaxiation(data_t *into, data_t *val, data_t mul, size_t my_grp_start, size_t my_grp_end, size_t hartid) {
  for(int i = my_grp_start * GROUP_LEN; i < my_grp_end * GROUP_LEN; i += GROUP_LEN) {
    __asm__ volatile(
      "vle32.v v0, (%0)\n"
      "vle32.v v1, (%1)\n"
      "vfmacc.vf v0, %2, v1\n"
      "vse32.v v0, (%0)\n"
      : 
      : "r" (&into[i]), "r" (&val[i]), "f" (mul)
      : "memory"
    );
  }
}

void reverse_relaxiation(data_t *into, data_t *from, data_t mul, size_t my_grp_start, size_t my_grp_end, size_t hartid) {
  for(int i = my_grp_start * GROUP_LEN; i < my_grp_end * GROUP_LEN; i += GROUP_LEN) {
    __asm__ volatile(
      "vle32.v v0, (%0)\n"
      "vle32.v v1, (%1)\n"
      "vfmacc.vf v1, %2, v0\n"
      "vse32.v v1, (%0)\n"
      : 
      : "r" (&into[i]), "r" (&from[i]), "f" (mul)
      : "memory"
    );
  }
}

void init(data_t *field) {
  for(int i = 0; i < HEIGHT; ++i)
    for(int j = 0; j < WIDTH; ++j)
      field[i * WIDTH + j] = (j == 0) ? -1 : 0;
}

void *HEAP_BASE = 0x84000000;
void *alloc(size_t size) {
  void *ret = HEAP_BASE;
  HEAP_BASE = HEAP_BASE + ((size + 63) & (~63)); // Manually 64-byte alignment
  return ret;
}

[[noreturn]]
void spin() {
  volatile size_t meow;
  while(1) ++meow;
}

int main(int hartid) {
  if(hartid >= HART_CNT)
    spin();

  // Temporarilly commented by meow
  // for (int i = 0;i < 1000;i++)
  //   putstr(".");
  // putstr("\r\n");

  size_t group_residue = GROUP_CNT % HART_CNT;
  size_t self_len = (GROUP_CNT / HART_CNT) + (hartid < group_residue ? 1 : 0);
  size_t self_start = (GROUP_CNT / HART_CNT) * hartid + (hartid < group_residue ? hartid : group_residue);
  size_t self_end = self_start + self_len;
//   volatile size_t meow = 0;
//   for(int i = 0; i < hartid * 20; ++i) ++meow;
//   print(self_start);
//   print(self_start + self_len);

  __asm__ volatile("vsetvli t0, x0, e32" ::: "t0");

  data_t *r = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *x = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *p = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *div_p = alloc(sizeof(data_t) * WIDTH * HEIGHT);

  if(hartid == 0) {
    putstr("Initializing input data\r\n");
    init(p); // p = r
    putstr("p\r\n");
    init(r);
    putstr("r\r\n");
    zero(x, sizeof(data_t) * WIDTH * HEIGHT);
    putstr("x\r\n");
  }

  if(hartid == 0) putstr("Pre-start\r\n");
  global_sync_nodata(hartid);
  if(hartid == 0) putstr("Start\r\n");

  data_t rr = self_dot(r, self_start, self_end, hartid);
  if(hartid == 0) printf_("rr = %.10f\n", rr);
  
  int round = 0;
  if(hartid == 0) putstr("Start iterations until eps < 1e-3\r\n");
  unsigned long long elapsed = 0;
  while(rr > EPS) {
    unsigned long before;
    if(hartid == 0) before = read_csr(mcycle);

    diverg(p, div_p, self_start, self_end, hartid);
    global_sync_nodata(hartid);
    data_t pAp = dot(p, div_p, self_start, self_end, hartid);
    data_t alpha = rr / pAp;
    // if(hartid == 0) {
    //   printf_("pAp = %.10f\n", pAp);
    //   printf_("alpha = %.10f\n", alpha);
    // }

    self_relaxiation(x, p, alpha, self_start, self_end, hartid);
    self_relaxiation(r, div_p, -alpha, self_start, self_end, hartid);
    global_sync_nodata(hartid);
    data_t rr_next = self_dot(r, self_start, self_end, hartid);
    // if(hartid == 0) printf_("rr = %.10f\n", rr_next);

    data_t beta = rr_next / rr;
    // if(hartid == 0) printf_("beta = %.10f\n", beta);
    reverse_relaxiation(p, r, beta, self_start, self_end, hartid);

    global_sync_nodata(hartid);

    rr = rr_next;
    ++round;
    if(hartid == 0) {
      unsigned elapsed_round = read_csr(mcycle) - before;
      elapsed += elapsed_round;
      printf_("Round %d: error = %f in %ld cycles\r\n", round, rr, elapsed_round);
    }
    // break;
    // if(round > 1) break;
  }

  global_sync(hartid, 0);

  if(hartid == 0) {
    printf_("Finished computation of %dx%d with EPS %f at round %d after %lld "
            "cycles (%.2f seconds)\r\n",
            WIDTH, HEIGHT, EPS, round, elapsed, elapsed / 50000000.0);

    data_t l2_sum = 0;
    for (int i = 0; i < HEIGHT; ++i) {
      for (int j = 0; j < WIDTH; ++j)
        l2_sum += x[i * WIDTH + i] * x[i * WIDTH + i];
    }
    printf_("Sum of result squared: %f\r\n", l2_sum);
    return 0;
  } else spin();
}
