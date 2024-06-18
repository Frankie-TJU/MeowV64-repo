#include "common.h"

#define HART_CNT 10

const size_t GROUP_CNT = WIDTH / GROUP_LEN * HEIGHT;

size_t MY_GROUP_START;
size_t MY_GROUP_END;

typedef struct {
  size_t progress;
  data_t buffer;
  char _padding[64 - sizeof(size_t) - sizeof(data_t)];
} hart_t;

hart_t harts[HART_CNT];

data_t global_sync(size_t hartid, data_t my_data) {
  print_delim(hartid, "");
  volatile size_t spin = 0;
  if (hartid != 0) {
    harts[hartid].buffer = my_data;
    size_t old_progress = harts[hartid].progress;
    __atomic_store_n(&harts[hartid].progress, old_progress + 1,
                     __ATOMIC_SEQ_CST);
    while (__atomic_load_n(&harts[0].progress, __ATOMIC_SEQ_CST) <=
           old_progress)
      ++spin; // Spin
    return harts[0].buffer;
  } else {
    size_t old_progress = harts[0].progress;
    data_t accum = my_data;
    for (int h = 1; h < HART_CNT; ++h) {
      while (__atomic_load_n(&harts[h].progress, __ATOMIC_SEQ_CST) <=
             old_progress)
        ++spin; // Spin
      accum += harts[h].buffer;
    }
    harts[0].buffer = accum;
    __atomic_store_n(&harts[0].progress, old_progress + 1, __ATOMIC_SEQ_CST);
    return accum;
  }
  size_t cur_progress = harts[hartid].progress;
}

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

void diverg_vector(data_t *field, data_t *result) {
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

data_t self_dot_vector(data_t *field, size_t my_grp_start, size_t my_grp_end,
                       size_t hartid) {
  // v1 is accumulator
  __asm__ volatile("vmv.v.i v0, 0");
  data_t *b = field + my_grp_start * GROUP_LEN;
  data_t *e = field + my_grp_end * GROUP_LEN;

  for (data_t *cur = b; cur < e; cur += GROUP_LEN) {
    __asm__ volatile("vle32.v v1, (%0)\n"
                     "vfmacc.vv v0, v1, v1\n"
                     :
                     : "r"(cur));
  }

  data_t accum = 0;
  data_t buffer[GROUP_LEN];
  __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
  for (int i = 0; i < GROUP_LEN; ++i)
    accum += buffer[i];

  return global_sync(hartid, accum);
}

data_t self_dot_buffet(data_t *field) {
  *ADDRGEN_CONTROL = 0;
  ADDRGEN_INSTS[0] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  uint64_t addr = (uint64_t)field;
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;

  ADDRGEN_INSTS[3] = 0;
  *ADDRGEN_ITERATIONS = WIDTH * HEIGHT;
  *ADDRGEN_CONTROL = 1;

  data_t accum = 0;
  for (int i = 0; i < WIDTH * HEIGHT; ++i) {
    data_t d = *((volatile data_t *)BUFFETS_DATA);
    accum += d * d;
    *BUFFETS_SHRINK = sizeof(data_t);
  }

  return accum;
}

data_t self_dot(data_t *field) {
  data_t accum = 0;
#pragma GCC unroll 8
  for (int i = 0; i < WIDTH * HEIGHT; ++i)
    accum += field[i] * field[i];
  return accum;
}

data_t dot_buffet(data_t *a, data_t *b) {
  *ADDRGEN_CONTROL = 0;
  ADDRGEN_INSTS[0] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  uint64_t addr = (uint64_t)a;
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;

  ADDRGEN_INSTS[3] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  addr = (uint64_t)b;
  ADDRGEN_INSTS[4] = addr >> 32;
  ADDRGEN_INSTS[5] = addr;

  ADDRGEN_INSTS[6] = 0;
  *ADDRGEN_ITERATIONS = WIDTH * HEIGHT;
  *ADDRGEN_CONTROL = 1;

  data_t accum = 0;
  for (int i = 0; i < WIDTH * HEIGHT; ++i) {
    data_t ad = *((volatile data_t *)BUFFETS_DATA);
    data_t bd = *(((volatile data_t *)BUFFETS_DATA) + 1);
    accum += ad * bd;
    *BUFFETS_SHRINK = sizeof(data_t) * 2;
  }

  return accum;
}

data_t dot(data_t *a, data_t *b) {
  data_t accum = 0;
#pragma GCC unroll 8
  for (int i = 0; i < WIDTH * HEIGHT; ++i)
    accum += a[i] * b[i];
  return accum;
}

data_t dot_vector(data_t *a, data_t *b) {
  // v1 is accumulator
  __asm__ volatile("vmv.v.i v0, 0");
  data_t *acur = a;
  data_t *bcur = b;

  for (int i = 0; i < WIDTH * HEIGHT / GROUP_LEN; ++i) {
    __asm__ volatile("vle32.v v1, (%0)\n"
                     "vle32.v v2, (%1)\n"
                     "vfmacc.vv v0, v1, v2\n"
                     :
                     : "r"(acur), "r"(bcur));
    acur += GROUP_LEN;
    bcur += GROUP_LEN;
  }

  data_t accum = 0;
  data_t buffer[GROUP_LEN];
  __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
  for (int i = 0; i < GROUP_LEN; ++i)
    accum += buffer[i];

  return accum;
}

void self_relaxiation(data_t *into, data_t *val, data_t mul) {
#pragma GCC unroll 8
  for (int i = 0; i < WIDTH * HEIGHT; ++i)
    into[i] += val[i] * mul;
}

void self_relaxiation_vector(data_t *into, data_t *val, data_t mul) {
  data_t *icur = into;
  data_t *vcur = val;
  for (int i = 0; i < WIDTH * HEIGHT / GROUP_LEN; ++i) {
    __asm__ volatile("vle32.v v0, (%0)\n"
                     "vle32.v v1, (%1)\n"
                     "vfmacc.vf v0, %2, v1\n"
                     "vse32.v v0, (%0)\n"
                     :
                     : "r"(icur), "r"(vcur), "f"(mul));
    icur += GROUP_LEN;
    vcur += GROUP_LEN;
  }
}

void relaxiation(data_t *into, data_t *from, data_t *val, data_t mul) {
#pragma GCC unroll 8
  for (int i = 0; i < WIDTH * HEIGHT; ++i)
    into[i] = from[i] + val[i] * mul;
}

void reverse_relaxiation_vector(data_t *into, data_t *from, data_t mul) {
  data_t *icur = into;
  data_t *fcur = from;
  for (int i = 0; i < WIDTH * HEIGHT / GROUP_LEN; ++i) {
    __asm__ volatile("vle32.v v0, (%0)\n"
                     "vle32.v v1, (%1)\n"
                     "vfmacc.vf v1, %2, v0\n"
                     "vse32.v v1, (%0)\n"
                     :
                     : "r"(icur), "r"(fcur), "f"(mul));
    icur += GROUP_LEN;
    fcur += GROUP_LEN;
  }
}

void relaxiation_buffet(data_t *into, data_t *from, data_t *val, data_t mul) {
  *ADDRGEN_CONTROL = 0;
  ADDRGEN_INSTS[0] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  uint64_t addr = (uint64_t)from;
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;

  ADDRGEN_INSTS[3] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  addr = (uint64_t)val;
  ADDRGEN_INSTS[4] = addr >> 32;
  ADDRGEN_INSTS[5] = addr;

  ADDRGEN_INSTS[6] = 0;
  *ADDRGEN_ITERATIONS = WIDTH * HEIGHT;
  *ADDRGEN_CONTROL = 1;

  for (int i = 0; i < WIDTH * HEIGHT; ++i) {
    data_t f = *((volatile data_t *)BUFFETS_DATA);
    data_t v = *(((volatile data_t *)BUFFETS_DATA) + 1);
    into[i] = f + v * mul;

    *BUFFETS_SHRINK = sizeof(data_t) * 2;
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

int main(int hartid) {
  if (hartid >= HART_CNT)
    return 0;

  // Temporarilly commented by meow
  // for (int i = 0;i < 1000;i++)
  //   putstr(".");
  // putstr("\r\n");

  size_t group_residue = GROUP_CNT % HART_CNT;
  size_t self_len = (GROUP_CNT / HART_CNT) + (hartid < group_residue ? 1 : 0);
  size_t self_start = (GROUP_CNT / HART_CNT) * hartid +
                      (hartid < group_residue ? hartid : group_residue);
  //   volatile size_t meow = 0;
  //   for(int i = 0; i < hartid * 20; ++i) ++meow;
  //   print(self_start);
  //   print(self_start + self_len);

  __asm__ volatile("vsetvli t0, x0, e32" ::: "t0");

  data_t *r = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *x = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *p = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *div_p = alloc(sizeof(data_t) * WIDTH * HEIGHT);

  if (hartid == 0) {
    putstr("Initializing input data\r\n");
    init(p); // p = r
    putstr("p\r\n");
    init(r);
    putstr("r\r\n");
    zero(x, sizeof(data_t) * WIDTH * HEIGHT);
    putstr("x\r\n");
  }

  if (hartid == 0)
    putstr("Pre-start\r\n");
  global_sync(hartid, 0);
  if (hartid == 0)
    putstr("Start\r\n");

  data_t rr = self_dot_vector(r, self_start, self_start + self_len, hartid);
  if (hartid == 0)
    print(rr * 100000);

  int round = 0;
  if (hartid == 0)
    putstr("Start iterations until eps < 1e-3\r\n");
  while (rr > EPS) {
    if (hartid == 0)
      putstr(".");
    diverg_vector(p, div_p);
    data_t pAp = dot_vector(p, div_p);
    data_t alpha = rr / pAp;

    self_relaxiation_vector(x, p, alpha);
    self_relaxiation_vector(r, div_p, -alpha);
    data_t rr_next =
        self_dot_vector(r, self_start, self_start + self_len, hartid);

    data_t beta = rr_next / rr;
    reverse_relaxiation_vector(p, r, beta);

    rr = rr_next;
    ++round;
    break;
    // if(round > 1) break;
  }

  global_sync(hartid, 0);

  if (hartid == 0) {
    putstr("\r\nFinished at round ");
    print(round);

    for (int i = 0; i < 16; ++i)
      print(x[i * WIDTH] * 100000);

    data_t l2_sum = 0;
    for (int i = 0; i < HEIGHT; ++i) {
      for (int j = 0; j < WIDTH; ++j)
        l2_sum += x[i * WIDTH + j] * x[i * WIDTH + j];
    }
    putstr("Sum of result squared: ");
    print(l2_sum * 100000);

    return 0;
  } else {
    volatile size_t meow;
    while (1)
      ++meow;
  }
}
