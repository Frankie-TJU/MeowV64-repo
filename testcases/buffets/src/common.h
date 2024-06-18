#include <stdint.h>
#include <stddef.h>

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

#define read_csr(reg)                                                          \
  ({                                                                           \
    unsigned long __tmp;                                                       \
    asm volatile("csrr %0, " #reg : "=r"(__tmp));                              \
    __tmp;                                                                     \
  })

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

void print_hex(int num) {
  print_hex_delim(num, "\r\n");
}

void print(int num) {
  print_delim(num, "\r\n");
}

void zero(char *mem, int size) {
  for(int i = 0; i < size; ++i) mem[i] = 0;
}

void putstr(char *s) {
  for(; *s; ++s) _putchar(*s);
}

// for poisson
typedef float data_t;
const size_t GROUP_LEN = 8;
const size_t WIDTH = 256;
const size_t HEIGHT = 256;
const data_t EPS = 1e-8;
const data_t EARLY_EPS = 1e-6;
const double FREQ = 500000000.0;
