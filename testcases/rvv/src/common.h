#include <stddef.h>
#include <stdint.h>

#define SERIAL ((volatile char *)0x60001000ULL)

void print(int num) {
  int q;
  char tmp[10];
  char *cur = tmp;
  if (num == 0) {
    *SERIAL = '0';
    *SERIAL = '\n';
  } else {
    while (num) {
      *cur = (num % 10) + '0';
      ++cur;
      num = num / 10;
    }
    while (cur != tmp) {
      cur--;
      *SERIAL = *cur;
    }
    *SERIAL = '\n';
  }
}

void *memcpy(void *__restrict dest, const void *__restrict src, size_t n) {
  for (size_t i = 0; i < n; i++) {
    ((char *)dest)[i] = ((char *)src)[i];
  }
  return dest;
}
