#include <stdint.h>

const uintptr_t UART_BASE = 0x2770B0000; // 0xF70B0000
volatile uint8_t *UART_RBR = (uint8_t *)(UART_BASE + 0x0000);
volatile uint8_t *UART_THR = (uint8_t *)(UART_BASE + 0x0000);
volatile uint8_t *UART_DLL = (uint8_t *)(UART_BASE + 0x0000); // LCR(7)=1
volatile uint8_t *UART_IER = (uint8_t *)(UART_BASE + 0x0004);
volatile uint8_t *UART_DLM = (uint8_t *)(UART_BASE + 0x0004); // LCR(7)=1
volatile uint8_t *UART_FCR = (uint8_t *)(UART_BASE + 0x0008);
volatile uint8_t *UART_LCR = (uint8_t *)(UART_BASE + 0x000C);
volatile uint8_t *UART_MCR = (uint8_t *)(UART_BASE + 0x0010);
volatile uint8_t *UART_LSR = (uint8_t *)(UART_BASE + 0x0014);

void init_serial() {
  // Enable 8 bytes FIFO
  *UART_FCR = 0x81;
  // LCR(7) = 1
  *UART_LCR = 0x80;
  // 115200: 100M / 16 / 115200 = 54
  *UART_DLL = 54;
  *UART_DLM = 0;
  // LCR(7) = 0, 8N1
  *UART_LCR = ~0x80 & 0x03;
  *UART_MCR = 0;
  *UART_IER = 0;
}

void putc(char ch) {
  while (!(*UART_LSR & 0x40))
    ;
  *UART_THR = ch;
}

void puts(char *s) {
  while (*s) {
    putc(*s++);
  }
}

void bootloader(int mhartid) {
  // Boot Hart
  init_serial();
  while(1) {
    puts("test serial\r\n");
  }
}
