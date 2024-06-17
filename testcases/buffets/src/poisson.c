// #include <stdio.h>
// #include <string.h>
// #include <stdlib.h>
#include "common.h"
// #include "printf.h"

typedef long size_t;
typedef float data_t;

const size_t WIDTH = 10;
const size_t HEIGHT = 10;
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

data_t self_dot(data_t *field) {
  data_t accum = 0;
  for(int i = 0; i < WIDTH * HEIGHT; ++i)
    accum += field[i] * field[i];
  return accum;
}

data_t dot(data_t *a, data_t *b) {
  data_t accum = 0;
  for(int i = 0; i < WIDTH * HEIGHT; ++i)
    accum += a[i] * b[i];
  return accum;
}

void self_relaxiation(data_t *into, data_t *val, data_t mul) {
  for(int i = 0; i < WIDTH * HEIGHT; ++i)
    into[i] += val[i] * mul;
}

void relaxiation(data_t *into, data_t *from, data_t *val, data_t mul) {
  for(int i = 0; i < WIDTH * HEIGHT; ++i)
    into[i] = from[i] + val[i] * mul;
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
  data_t *r = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *x = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *p = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  data_t *div_p = alloc(sizeof(data_t) * WIDTH * HEIGHT);
  print_hex((int) r);
  print_hex((int) p);

  init(p); // p = r
  putstr("p init\n");
  init(r);
  putstr("r init\n");
  zero(x, sizeof(data_t) * WIDTH * HEIGHT);
  putstr("x init\n");

  data_t rr = self_dot(r);
  print(r[0] * 100000);
  print(rr * 100000);
  int round = 0;
  while(rr > EPS) {
    print(round);
    diverg(p, div_p);
    data_t pAp = dot(p, div_p);
    data_t alpha = rr / pAp;

    self_relaxiation(x, p, alpha);
    self_relaxiation(r, div_p, -alpha);
    data_t rr_next = dot(r, r);

    data_t beta = rr_next / rr;
    relaxiation(p, r, p, beta);

    rr = rr_next;
    ++round;
  }
  putstr("Rnd: ");
  print(round);

  for(int i = 0; i < 10; ++i) {
    for(int j = 0; j < 10; ++j)
      print(x[i * WIDTH + i] * 100000);
    _putchar('\n');
  }
  return 0;
}
