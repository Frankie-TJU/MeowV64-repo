#include <stddef.h>
#include <stdint.h>

const uintptr_t ADDRGEN_BASE = 0x59000000;
volatile uint32_t *ADDRGEN_STATUS = (uint32_t *)(ADDRGEN_BASE + 0x00);
volatile uint32_t *ADDRGEN_CONTROL = (uint32_t *)(ADDRGEN_BASE + 0x20);
volatile uint32_t *ADDRGEN_ITERATIONS = (uint32_t *)(ADDRGEN_BASE + 0x40);
volatile uint32_t *ADDRGEN_INSTS = (uint32_t *)(ADDRGEN_BASE + 0x60);

volatile uint32_t *BUFFETS_DATA = (uint32_t *)0x5000000;

const uintptr_t BUFFETS_BASE = 0x58000000;
volatile uint32_t *BUFFETS_SIZE = (uint32_t *)(BUFFETS_BASE + 0x40);
volatile uint32_t *BUFFETS_SHRINK = (uint32_t *)(BUFFETS_BASE + 0x80);

const uintptr_t UART_BASE = 0x60200000;
volatile uint8_t *UART_THR = (uint8_t *)(UART_BASE + 0x1000);
volatile uint8_t *UART_LSR = (uint8_t *)(UART_BASE + 0x1014);

#define SERIAL ((volatile char *)0x60001000ULL)

#define read_csr(reg)                                                          \
  ({                                                                           \
    unsigned long __tmp;                                                       \
    asm volatile("csrr %0, " #reg : "=r"(__tmp));                              \
    __tmp;                                                                     \
  })

void puts(const char *s) {
  while (*s) {
    *SERIAL = *s;
    s++;
  }
}

void *memcpy(void *__restrict dest, const void *__restrict src, size_t n) {
  for (size_t i = 0; i < n; i++) {
    ((char *)dest)[i] = ((char *)src)[i];
  }
  return dest;
}

void *memset(void *__restrict dest, int ch, size_t n) {
  for (size_t i = 0; i < n; i++) {
    ((char *)dest)[i] = ch;
  }
  return dest;
}

float fabsf(float num) {
  if (num > 0)
    return num;
  else
    return -num;
}

static inline void _putchar(char c) {
  while (!(*UART_LSR & 0x40))
    ;
  *UART_THR = c;
}

static inline void _puts(char *s) {
  while (*s) {
    _putchar(*s++);
  }
}

void print_hex_delim(unsigned int num, char *delim) {
  int q;
  char tmp[10];
  char *cur = tmp;
  if (num == 0) {
    _putchar('0');
    _puts(delim);
  } else {
    while (num) {
      int d = num & 0xF;
      char c = (d < 10) ? (d + '0') : (d - 10 + 'A');
      *cur = c;
      ++cur;
      num >>= 4;
    }
    while (cur != tmp) {
      cur--;
      _putchar(*cur);
    }
    _puts(delim);
  }
}

void print_delim(unsigned int num, char *delim) {
  int q;
  char tmp[10];
  char *cur = tmp;
  if (num == 0) {
    _putchar('0');
    _puts(delim);
  } else {
    while (num) {
      *cur = (num % 10) + '0';
      ++cur;
      num = num / 10;
    }
    while (cur != tmp) {
      cur--;
      _putchar(*cur);
    }
    _puts(delim);
  }
}

void print_hex(int num) { print_hex_delim(num, "\r\n"); }

void print(int num) { print_delim(num, "\r\n"); }

void zero(char *mem, int size) {
  for (int i = 0; i < size; ++i)
    mem[i] = 0;
}

void putstr(char *s) {
  for (; *s; ++s)
    _putchar(*s);
}

#define assert(expr) assert_(expr, #expr)

#include "printf.h"

extern int printf_(const char *format, ...);
void assert_(int res, char *s) {
  if (!res) {
    printf_("Assertion failed: %s\r\n", s);
    for (;;) {
    }
  }
}

#define max(a, b) ((a) > (b) ? (a) : (b))

// helper to setup address generation
int addrgen_indexed(int offset, int bytes, int shift, int stride,
                    const void *indices, const void *data) {
  ADDRGEN_INSTS[offset++] =
      (1 << 27) | (bytes << 13) | (shift << 10) | (stride << 0);
  uint64_t addr = (uint64_t)indices;
  ADDRGEN_INSTS[offset++] = addr >> 32;
  ADDRGEN_INSTS[offset++] = addr;
  addr = (uint64_t)data;
  ADDRGEN_INSTS[offset++] = addr >> 32;
  ADDRGEN_INSTS[offset++] = addr;
  return offset;
}
