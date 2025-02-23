#include <stddef.h>
#include <stdint.h>

const uintptr_t ADDRGEN_BASE = 0x59000000;
volatile uint32_t *ADDRGEN_STATUS = (uint32_t *)(ADDRGEN_BASE + 0x00);
volatile uint32_t *ADDRGEN_CONTROL = (uint32_t *)(ADDRGEN_BASE + 0x20);
volatile uint32_t *ADDRGEN_ITERATIONS = (uint32_t *)(ADDRGEN_BASE + 0x40);
volatile uint32_t *ADDRGEN_INSTS = (uint32_t *)(ADDRGEN_BASE + 0x60);
volatile uint64_t *ADDRGEN_PERF_BYTES_READ =
    (uint64_t *)(ADDRGEN_BASE + 0x1000);
volatile uint64_t *ADDRGEN_PERF_COUNT_READ =
    (uint64_t *)(ADDRGEN_BASE + 0x1020);
volatile uint64_t *ADDRGEN_PERF_BYTES_EGRESS =
    (uint64_t *)(ADDRGEN_BASE + 0x1040);
volatile uint64_t *ADDRGEN_PERF_COUNT_INST =
    (uint64_t *)(ADDRGEN_BASE + 0x1060);
volatile uint64_t *ADDRGEN_PERF_COUNT_INDEXED =
    (uint64_t *)(ADDRGEN_BASE + 0x1080);
volatile uint64_t *ADDRGEN_PERF_COUNT_STRIDED =
    (uint64_t *)(ADDRGEN_BASE + 0x10A0);
volatile uint64_t *ADDRGEN_PERF_COUNT_ACTIVE =
    (uint64_t *)(ADDRGEN_BASE + 0x10C0);
volatile uint64_t *ADDRGEN_PERF_COUNT_FULL =
    (uint64_t *)(ADDRGEN_BASE + 0x10E0);

volatile uint32_t *BUFFETS_DATA = (uint32_t *)0x5000000;
volatile uint32_t *BUFFETS_DATA_FASTPATH = (uint32_t *)0x51000000;

const uintptr_t BUFFETS_BASE = 0x58000000;
volatile uint32_t *BUFFETS_SIZE = (uint32_t *)(BUFFETS_BASE + 0x40);
volatile uint32_t *BUFFETS_SHRINK = (uint32_t *)(BUFFETS_BASE + 0x80);
volatile uint64_t *BUFFETS_PERF_BYTES_PUSHED =
    (uint64_t *)(BUFFETS_BASE + 0x1000);
volatile uint64_t *BUFFETS_PERF_COUNT_PUSHED =
    (uint64_t *)(BUFFETS_BASE + 0x1020);
volatile uint64_t *BUFFETS_PERF_BYTES_POPPED =
    (uint64_t *)(BUFFETS_BASE + 0x1040);
volatile uint64_t *BUFFETS_PERF_COUNT_POPPED =
    (uint64_t *)(BUFFETS_BASE + 0x1060);
volatile uint64_t *BUFFETS_PERF_BYTES_READ =
    (uint64_t *)(BUFFETS_BASE + 0x1080);
volatile uint64_t *BUFFETS_PERF_COUNT_READ =
    (uint64_t *)(BUFFETS_BASE + 0x10A0);

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

// for poisson
typedef float data_t;
const size_t GROUP_LEN = 8;
#ifdef N_OVERRIDE
const size_t WIDTH = N_OVERRIDE;
const size_t HEIGHT = N_OVERRIDE;
#else
const size_t WIDTH = 16;
const size_t HEIGHT = 16;
#endif
const data_t EPS = 1e-6;
const data_t EARLY_EPS = 1e-6;
const double FREQ = 500000000.0;

[[noreturn]] void spin() {
  volatile size_t meow;
  while (1)
    ++meow;
}

void *HEAP_BASE = (void *)0x84000000;
void *heap_bump(void *from, size_t size) {
  return from + ((size + 63) & (~63)); // Manually 64-byte alignment
}
void *heap_alloc(void **heap, size_t size) {
  void *ret = *heap;
  *heap = heap_bump(*heap, size);
  return ret;
}

// helper to setup address generation
int addrgen_indexed(int offset, int bytes, int shift, int stride,
                    const void *indices, const void *data) {
  ADDRGEN_INSTS[offset++] =
      (2 << 27) | (bytes << 13) | (shift << 10) | (stride << 0);
  uint64_t addr = (uint64_t)indices;
  ADDRGEN_INSTS[offset++] = addr >> 32;
  ADDRGEN_INSTS[offset++] = addr;
  addr = (uint64_t)data;
  ADDRGEN_INSTS[offset++] = addr >> 32;
  ADDRGEN_INSTS[offset++] = addr;
  return offset;
}

int addrgen_strided(int offset, int bytes, int stride, void *data) {
  ADDRGEN_INSTS[offset++] = (1 << 27) | (bytes << 13) | (stride << 0);
  uint64_t addr = (uint64_t)data;
  ADDRGEN_INSTS[offset++] = addr >> 32;
  ADDRGEN_INSTS[offset++] = addr;
  return offset;
}

#include "printf.h"

void dump_buffets() {
  printf_("AddrGen: %ld bytes read\r\n", *ADDRGEN_PERF_BYTES_READ);
  printf_("AddrGen: %ld times read\r\n", *ADDRGEN_PERF_COUNT_READ);
  printf_("AddrGen: %ld bytes egress\r\n", *ADDRGEN_PERF_BYTES_EGRESS);
  printf_("AddrGen: %ld insts\r\n", *ADDRGEN_PERF_COUNT_INST);
  printf_("AddrGen: %ld indexed insts\r\n", *ADDRGEN_PERF_COUNT_INDEXED);
  printf_("AddrGen: %ld strided insts\r\n", *ADDRGEN_PERF_COUNT_STRIDED);
  printf_("AddrGen: %ld active cycles\r\n", *ADDRGEN_PERF_COUNT_ACTIVE);
  printf_("AddrGen: %ld full cycles\r\n", *ADDRGEN_PERF_COUNT_FULL);

  printf_("Buffets: %ld bytes pushed\r\n", *BUFFETS_PERF_BYTES_PUSHED);
  printf_("Buffets: %ld times pushed\r\n", *BUFFETS_PERF_COUNT_PUSHED);
  printf_("Buffets: %ld bytes popped\r\n", *BUFFETS_PERF_BYTES_POPPED);
  printf_("Buffets: %ld times popped\r\n", *BUFFETS_PERF_COUNT_POPPED);
  printf_("Buffets: %ld bytes read\r\n", *BUFFETS_PERF_BYTES_READ);
  printf_("Buffets: %ld count read\r\n", *BUFFETS_PERF_COUNT_READ);
}