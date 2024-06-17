// #include <stdio.h>
// #include <string.h>
// #include <stdlib.h>
#include "common.h"
// #include "printf.h"

typedef long size_t;
typedef float data_t;
const size_t GROUP_LEN = 8;

const size_t WIDTH = 16;
const size_t HEIGHT = 16;
const data_t EPS = 1e-3;

void diverg(data_t *field, data_t *result) {
  for(int i = 0; i < HEIGHT; ++i) {
    for(int j = 0; j < WIDTH; ++j) {
      _Bool solid_boundary = i == 0 || i == HEIGHT - 1;
      data_t cur = field[i * WIDTH + j] * (solid_boundary ? -3 : -4);

      if(j != 0) cur += field[i * WIDTH + (j-1)];
      if(j != WIDTH - 1) cur += field[i * WIDTH + (j+1)];
      if(i != 0) cur += field[(i-1) * WIDTH + j];
      if(i != HEIGHT - 1) cur += field[(i+1) * WIDTH + j];

      result[i * WIDTH + j] = cur;
    }
  }
}

void diverg_vector(data_t *field, data_t *result) {
  for(int i = 0; i < HEIGHT; ++i) {
    _Bool solid_boundary = i == 0 || i == HEIGHT - 1;
    data_t multiplier = solid_boundary ? -3 : -4;

    for(int j = 0; j < WIDTH; j += GROUP_LEN) {
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
  }
}

data_t self_dot_vector(data_t *field) {
  // v1 is accumulator
  __asm__ volatile("vmv.v.i v0, 0");
  data_t *cur = field;

  for(int i = 0; i < WIDTH * HEIGHT / GROUP_LEN; ++i) {
    __asm__ volatile(
      "vle32.v v1, (%0)\n"
      "vfmacc.vv v0, v1, v1\n"
      : 
      : "r" (cur)
    );
    cur += GROUP_LEN;
  }

  data_t accum = 0;
  data_t buffer[GROUP_LEN];
  __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
  for(int i = 0; i < GROUP_LEN; ++i) accum += buffer[i];

  return accum;
}

data_t self_dot_buffet(data_t *field) {
  *ADDRGEN_CONTROL = 0;
  ADDRGEN_INSTS[0] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  uint64_t addr = (uint64_t) field;
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;

  ADDRGEN_INSTS[3] = 0;
  *ADDRGEN_ITERATIONS = WIDTH * HEIGHT;
  *ADDRGEN_CONTROL = 1;

  data_t accum = 0;
  for(int i = 0; i < WIDTH * HEIGHT; ++i) {
    data_t d = *((volatile data_t *) BUFFETS_DATA);
    accum += d * d;
    *BUFFETS_SHRINK = sizeof(data_t);
  }

  return accum;
}

data_t self_dot(data_t *field) {
  data_t accum = 0;
  #pragma GCC unroll 8
  for(int i = 0; i < WIDTH * HEIGHT; ++i) accum += field[i] * field[i];
  return accum;
}

data_t dot_buffet(data_t *a, data_t *b) {
  *ADDRGEN_CONTROL = 0;
  ADDRGEN_INSTS[0] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  uint64_t addr = (uint64_t) a;
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;

  ADDRGEN_INSTS[3] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  addr = (uint64_t) b;
  ADDRGEN_INSTS[4] = addr >> 32;
  ADDRGEN_INSTS[5] = addr;

  ADDRGEN_INSTS[6] = 0;
  *ADDRGEN_ITERATIONS = WIDTH * HEIGHT;
  *ADDRGEN_CONTROL = 1;

  data_t accum = 0;
  for(int i = 0; i < WIDTH * HEIGHT; ++i) {
    data_t ad = *((volatile data_t *) BUFFETS_DATA);
    data_t bd = *(((volatile data_t *) BUFFETS_DATA) + 1);
    accum += ad * bd;
    *BUFFETS_SHRINK = sizeof(data_t) * 2;
  }

  return accum;
}

data_t dot(data_t *a, data_t *b) {
  data_t accum = 0;
  #pragma GCC unroll 8
  for(int i = 0; i < WIDTH * HEIGHT; ++i) accum += a[i] * b[i];
  return accum;
}

data_t dot_vector(data_t *a, data_t *b) {
  // v1 is accumulator
  __asm__ volatile("vmv.v.i v0, 0");
  data_t *acur = a;
  data_t *bcur = b;

  for(int i = 0; i < WIDTH * HEIGHT / GROUP_LEN; ++i) {
    __asm__ volatile(
      "vle32.v v1, (%0)\n"
      "vle32.v v2, (%1)\n"
      "vfmacc.vv v0, v1, v2\n"
      : 
      : "r" (acur), "r" (bcur)
    );
    acur += GROUP_LEN;
    bcur += GROUP_LEN;
  }

  data_t accum = 0;
  data_t buffer[GROUP_LEN];
  __asm__ volatile("vse32.v v0, %0\n" : "=m"(buffer));
  for(int i = 0; i < GROUP_LEN; ++i) accum += buffer[i];

  return accum;
}

void self_relaxiation(data_t *into, data_t *val, data_t mul) {
  #pragma GCC unroll 8
  for(int i = 0; i < WIDTH * HEIGHT; ++i)
    into[i] += val[i] * mul;
}

void self_relaxiation_vector(data_t *into, data_t *val, data_t mul) {
  data_t *icur = into;
  data_t *vcur = val;
  for(int i = 0; i < WIDTH * HEIGHT / GROUP_LEN; ++i) {
    __asm__ volatile(
      "vle32.v v0, (%0)\n"
      "vle32.v v1, (%1)\n"
      "vfmacc.vf v0, %2, v1\n"
      "vse32.v v0, (%0)\n"
      : 
      : "r" (icur), "r" (vcur), "f" (mul)
    );
    icur += GROUP_LEN;
    vcur += GROUP_LEN;
  }
}

void relaxiation(data_t *into, data_t *from, data_t *val, data_t mul) {
  #pragma GCC unroll 8
  for(int i = 0; i < WIDTH * HEIGHT; ++i)
    into[i] = from[i] + val[i] * mul;
}

void reverse_relaxiation_vector(data_t *into, data_t *from, data_t mul) {
  data_t *icur = into;
  data_t *fcur = from;
  for(int i = 0; i < WIDTH * HEIGHT / GROUP_LEN; ++i) {
    __asm__ volatile(
      "vle32.v v0, (%0)\n"
      "vle32.v v1, (%1)\n"
      "vfmacc.vf v1, %2, v0\n"
      "vse32.v v1, (%0)\n"
      : 
      : "r" (icur), "r" (fcur), "f" (mul)
    );
    icur += GROUP_LEN;
    fcur += GROUP_LEN;
  }
}

void relaxiation_buffet(data_t *into, data_t *from, data_t *val, data_t mul) {
  *ADDRGEN_CONTROL = 0;
  ADDRGEN_INSTS[0] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  uint64_t addr = (uint64_t) from;
  ADDRGEN_INSTS[1] = addr >> 32;
  ADDRGEN_INSTS[2] = addr;

  ADDRGEN_INSTS[3] = (0 << 31) | (sizeof(data_t) << 20) | (sizeof(data_t) << 0);
  addr = (uint64_t) val;
  ADDRGEN_INSTS[4] = addr >> 32;
  ADDRGEN_INSTS[5] = addr;

  ADDRGEN_INSTS[6] = 0;
  *ADDRGEN_ITERATIONS = WIDTH * HEIGHT;
  *ADDRGEN_CONTROL = 1;

  for(int i = 0; i < WIDTH * HEIGHT; ++i) {
    data_t f = *((volatile data_t *) BUFFETS_DATA);
    data_t v = *(((volatile data_t *) BUFFETS_DATA) + 1);
    into[i] = f + v * mul;

    *BUFFETS_SHRINK = sizeof(data_t) * 2;
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

void zero(char *mem, int size) {
  for(int i = 0; i < size; ++i) mem[i] = 0;
}

void putstr(char *s) {
  for(; *s; ++s) _putchar(*s);
}

int main() {
  // Temporarilly commented by meow
  // for (int i = 0;i < 1000;i++)
  //   putstr(".");
  // putstr("\r\n");

  __asm__ volatile("vsetvli a0, x0, e32");

  data_t *r = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *x = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *p = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *div_p = alloc(sizeof(data_t) * WIDTH * HEIGHT);

  putstr("Initializing input data\r\n");
  init(p); // p = r
  init(r);
  zero(x, sizeof(data_t) * WIDTH * HEIGHT);

  data_t rr = self_dot_vector(r);
  // print(rr * 100000);
  int round = 0;
  putstr("Start iterations until eps < 1e-3\r\n");
  while(rr > EPS) {
    putstr(".");
    diverg_vector(p, div_p);
    data_t pAp = dot_vector(p, div_p);
    data_t alpha = rr / pAp;

    self_relaxiation_vector(x, p, alpha);
    self_relaxiation_vector(r, div_p, -alpha);
    data_t rr_next = self_dot_vector(r);

    data_t beta = rr_next / rr;
    reverse_relaxiation_vector(p, r, beta);

    rr = rr_next;
    ++round;
    // break;
    // if(round > 1) break;
  }
  putstr("\r\nFinished at round ");
  print(round);

  data_t l2_sum = 0;
  for(int i = 0; i < HEIGHT; ++i) {
    for(int j = 0; j < WIDTH; ++j)
      l2_sum += x[i * WIDTH + i] * x[i * WIDTH + i] ;
  }
  putstr("Sum of result squared: ");
  print(l2_sum * 100000);
  return 0;
}
