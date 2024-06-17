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

#define SERIAL ((volatile char *)0x60001000ULL)

#define read_csr(reg)                                                          \
  ({                                                                           \
    unsigned long __tmp;                                                       \
    asm volatile("csrr %0, " #reg : "=r"(__tmp));                              \
    __tmp;                                                                     \
  })

void _putchar(char c) {
  *SERIAL = c;
}

void print_hex_delim(unsigned int num, char delim) {
  int q;
  char tmp[10];
  char *cur = tmp;
  if (num == 0) {
    *SERIAL = '0';
    *SERIAL = delim;
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
      *SERIAL = *cur;
    }
    *SERIAL = delim;
  }
}

void print_delim(unsigned int num, char delim) {
  int q;
  char tmp[10];
  char *cur = tmp;
  if (num == 0) {
    *SERIAL = '0';
    *SERIAL = delim;
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
    *SERIAL = delim;
  }
}

void print_hex(int num) {
  print_hex_delim(num, '\n');
}

void print(int num) {
  print_delim(num, '\n');
}
