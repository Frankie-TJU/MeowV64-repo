#include "VRiscVSystem.h"
#include <arpa/inet.h>
#include <fcntl.h>
#include <gmpxx.h>
#include <iostream>
#include <map>
#include <netinet/tcp.h>
#include <signal.h>
#include <string>
#include <sys/time.h>
#include <unistd.h>
#include <verilated.h>
#include <verilated_fst_c.h>

#ifdef __APPLE__
#include "elf-local.h"
#else
#include <elf.h>
#endif

// memory mapping
typedef uint32_t mem_t;
std::map<uint64_t, mem_t> memory;

// align to mem_t boundary
uint64_t align(uint64_t addr) { return (addr / sizeof(mem_t)) * sizeof(mem_t); }

VRiscVSystem *top;

vluint64_t main_time = 0;

double sc_time_stamp() { return main_time; }

bool finished = false;
bool jtag = false;
// use remote bitbang protocol
bool jtag_rbb = false;
// use jtag_vpi protocol
bool jtag_vpi = false;

// jtag_vpi state & definitions
enum JtagVpiState {
  CHECK_CMD,
  TAP_RESET,
  GOTO_IDLE,
  DO_TMS_SEQ,
  SCAN_CHAIN,
  FINISHED
} jtag_vpi_state;
bool jtag_vpi_tms_flip = false;

enum JtagVpiCommand {
  CMD_RESET,
  CMD_TMS_SEQ,
  CMD_SCAN_CHAIN,
  CMD_SCAN_CHAIN_FLIP_TMS,
  CMD_STOP_SIMU
};

struct jtag_vpi_cmd {
  uint32_t cmd;
  uint8_t buffer_out[512];
  uint8_t buffer_in[512];
  uint32_t length;
  uint32_t nb_bits;
};

int res = 0;

const uint64_t MEM_AXI_DATA_WIDTH = 128;
const uint64_t MEM_AXI_DATA_BYTES = MEM_AXI_DATA_WIDTH / 8;
const uint64_t MMIO_AXI_DATA_WIDTH = 64;
const uint64_t MMIO_AXI_DATA_BYTES = MMIO_AXI_DATA_WIDTH / 8;

// tohost/fromhost
// default at 0x60000000
uint64_t tohost_addr = 0x60000000;
uint64_t fromhost_addr = 0x60000040;

// serial
// default at 0x60001000
uint64_t serial_addr = 0x60001000;
uint64_t serial_fpga_addr = 0x60201000;

// signature generation for riscv-torture
uint64_t begin_signature = 0;
uint64_t begin_signature_override = 0;
uint64_t end_signature = 0;

void ctrlc_handler(int arg) {
  fprintf(stderr, "Received Ctrl-C\n");
  finished = true;
  res = 1;
}

// initialize signals
void init() {
  top->mem_axi4_AWREADY = 0;
  top->mem_axi4_WREADY = 0;
  top->mem_axi4_BVALID = 0;

  top->mem_axi4_ARREADY = 0;
  top->mem_axi4_RVALID = 0;

  top->mmio_axi4_AWREADY = 0;
  top->mmio_axi4_WREADY = 0;
  top->mmio_axi4_BVALID = 0;

  top->mmio_axi4_ARREADY = 0;
  top->mmio_axi4_RVALID = 0;

  // external interrupt
  top->interrupts = 0x3;
}

// step per clock fall
void step_mem() {
  // handle read
  static bool pending_read = false;
  static uint64_t pending_read_id = 0;
  static uint64_t pending_read_addr = 0;
  static uint64_t pending_read_len = 0;
  static uint64_t pending_read_size = 0;

  if (!pending_read) {
    if (top->mem_axi4_ARVALID) {
      top->mem_axi4_ARREADY = 1;
      pending_read = true;
      pending_read_id = top->mem_axi4_ARID;
      // flipped in msb: memory begins at 0x00000000
      pending_read_addr = top->mem_axi4_ARADDR ^ 0x80000000L;
      pending_read_len = top->mem_axi4_ARLEN;
      pending_read_size = top->mem_axi4_ARSIZE;
    }

    top->mem_axi4_RVALID = 0;
  } else {
    top->mem_axi4_ARREADY = 0;

    top->mem_axi4_RVALID = 1;
    top->mem_axi4_RID = pending_read_id;
    mpz_class r_data;

    uint64_t aligned =
        (pending_read_addr / MEM_AXI_DATA_BYTES) * MEM_AXI_DATA_BYTES;
    for (int i = 0; i < MEM_AXI_DATA_BYTES / sizeof(mem_t); i++) {
      uint64_t addr = aligned + i * sizeof(mem_t);
      mem_t r = memory[addr];
      mpz_class res = r;
      res <<= (i * (sizeof(mem_t) * 8));
      r_data += res;
    }

    mpz_class mask = 1;
    mask <<= (1L << pending_read_size) * 8;
    mask -= 1;

    mpz_class shifted_mask =
        mask << ((pending_read_addr & (MEM_AXI_DATA_BYTES - 1)) * 8);
    r_data &= shifted_mask;

    // top->mem_axi4_RDATA = r_data & shifted_mask;
    memset(top->mem_axi4_RDATA, 0, sizeof(top->mem_axi4_RDATA));
    mpz_export(top->mem_axi4_RDATA, NULL, -1, 4, -1, 0, r_data.get_mpz_t());
    top->mem_axi4_RLAST = pending_read_len == 0;

    // RREADY might be stale without eval()
    top->eval();
    if (top->mem_axi4_RREADY) {
      if (pending_read_len == 0) {
        pending_read = false;
      } else {
        pending_read_addr += 1 << pending_read_size;
        pending_read_len--;
      }
    }
  }

  // handle write
  static bool pending_write = false;
  static bool pending_write_finished = false;
  static uint64_t pending_write_addr = 0;
  static uint64_t pending_write_len = 0;
  static uint64_t pending_write_size = 0;
  static uint64_t pending_write_id = 0;
  if (!pending_write) {
    // idle
    if (top->mem_axi4_AWVALID) {
      top->mem_axi4_AWREADY = 1;
      pending_write = true;
      // flipped in msb: memory begins at 0x00000000
      pending_write_addr = top->mem_axi4_AWADDR ^ 0x80000000;
      pending_write_len = top->mem_axi4_AWLEN;
      pending_write_size = top->mem_axi4_AWSIZE;
      pending_write_id = top->mem_axi4_AWID;
      pending_write_finished = false;
    }
    top->mem_axi4_WREADY = 0;
    top->mem_axi4_BVALID = 0;
  } else if (!pending_write_finished) {
    // writing
    top->mem_axi4_AWREADY = 0;
    top->mem_axi4_WREADY = 1;

    // WVALID might be stale without eval()
    top->eval();
    if (top->mem_axi4_WVALID) {
      mpz_class mask = 1;
      mask <<= 1L << pending_write_size;
      mask -= 1;

      mpz_class shifted_mask =
          mask << (pending_write_addr & (MEM_AXI_DATA_BYTES - 1));
      mpz_class wdata;
      mpz_import(wdata.get_mpz_t(), MEM_AXI_DATA_BYTES / 4, -1, 4, -1, 0,
                 top->mem_axi4_WDATA);

      uint64_t aligned =
          pending_write_addr / MEM_AXI_DATA_BYTES * MEM_AXI_DATA_BYTES;
      for (int i = 0; i < MEM_AXI_DATA_BYTES / sizeof(mem_t); i++) {
        uint64_t addr = aligned + i * sizeof(mem_t);

        mpz_class local_wdata_mpz = wdata >> (i * (sizeof(mem_t) * 8));
        mem_t local_wdata = local_wdata_mpz.get_ui();

        uint64_t local_wstrb =
            (top->mem_axi4_WSTRB >> (i * sizeof(mem_t))) & 0xfL;

        mpz_class local_mask_mpz = shifted_mask >> (i * sizeof(mem_t));
        uint64_t local_mask = local_mask_mpz.get_ui() & 0xfL;
        if (local_mask & local_wstrb) {
          mem_t base = memory[addr];
          mem_t input = local_wdata;
          uint64_t be = local_mask & local_wstrb;

          mem_t muxed = 0;
          for (int i = 0; i < sizeof(mem_t); i++) {
            mem_t sel;
            if (((be >> i) & 1) == 1) {
              sel = (input >> (i * 8)) & 0xff;
            } else {
              sel = (base >> (i * 8)) & 0xff;
            }
            muxed |= (sel << (i * 8));
          }

          memory[addr] = muxed;
        }
      }

      uint64_t input = wdata.get_ui();
      pending_write_addr += 1L << pending_write_size;
      pending_write_len--;
      if (top->mem_axi4_WLAST) {
        assert(pending_write_len == -1);
        pending_write_finished = true;
      }
    }

    top->mem_axi4_BVALID = 0;
  } else {
    // finishing
    top->mem_axi4_AWREADY = 0;
    top->mem_axi4_WREADY = 0;
    top->mem_axi4_BVALID = 1;
    top->mem_axi4_BRESP = 0;
    top->mem_axi4_BID = pending_write_id;

    // BREADY might be stale without eval()
    top->eval();
    if (top->mem_axi4_BREADY) {
      pending_write = false;
      pending_write_finished = false;
    }
  }
}

// step per clock fall
void step_mmio() {
  // handle read
  static bool pending_read = false;
  static uint64_t pending_read_id = 0;
  static uint64_t pending_read_addr = 0;
  static uint64_t pending_read_len = 0;
  static uint64_t pending_read_size = 0;

  if (!pending_read) {
    if (top->mmio_axi4_ARVALID) {
      top->mmio_axi4_ARREADY = 1;
      pending_read = true;
      pending_read_id = top->mmio_axi4_ARID;
      pending_read_addr = top->mmio_axi4_ARADDR;
      pending_read_len = top->mmio_axi4_ARLEN;
      pending_read_size = top->mmio_axi4_ARSIZE;
    }

    top->mmio_axi4_RVALID = 0;
  } else {
    top->mmio_axi4_ARREADY = 0;

    top->mmio_axi4_RVALID = 1;
    top->mmio_axi4_RID = pending_read_id;
    mpz_class r_data;
    if (pending_read_addr == serial_addr + 0x14 ||
        pending_read_addr == serial_fpga_addr + 0x14) {
      // serial lsr
      // THRE | TEMT
      uint64_t lsr = (1L << 5) | (1L << 6);
      r_data = lsr << 32;
    } else {
      uint64_t aligned =
          (pending_read_addr / MMIO_AXI_DATA_BYTES) * MMIO_AXI_DATA_BYTES;
      for (int i = 0; i < MMIO_AXI_DATA_BYTES / sizeof(mem_t); i++) {
        uint64_t addr = aligned + i * sizeof(mem_t);
        mem_t r = memory[addr];
        mpz_class res = r;
        res <<= (i * (sizeof(mem_t) * 8));
        r_data += res;
      }
    }

    mpz_class mask = 1;
    mask <<= (1L << pending_read_size) * 8;
    mask -= 1;

    mpz_class shifted_mask =
        mask << ((pending_read_addr & (MMIO_AXI_DATA_BYTES - 1)) * 8);
    r_data &= shifted_mask;

    // top->mmio_axi4_RDATA = r_data & shifted_mask;
    memset(&top->mmio_axi4_RDATA, 0, sizeof(top->mmio_axi4_RDATA));
    mpz_export(&top->mmio_axi4_RDATA, NULL, -1, 4, -1, 0, r_data.get_mpz_t());
    top->mmio_axi4_RLAST = pending_read_len == 0;

    // RREADY might be stale without eval()
    top->eval();
    if (top->mmio_axi4_RREADY) {
      if (pending_read_len == 0) {
        pending_read = false;
      } else {
        pending_read_addr += 1 << pending_read_size;
        pending_read_len--;
      }
    }
  }

  // handle write
  static bool pending_write = false;
  static bool pending_write_finished = false;
  static uint64_t pending_write_addr = 0;
  static uint64_t pending_write_len = 0;
  static uint64_t pending_write_size = 0;
  static uint64_t pending_write_id = 0;
  if (!pending_write) {
    if (top->mmio_axi4_AWVALID) {
      top->mmio_axi4_AWREADY = 1;
      pending_write = 1;
      pending_write_addr = top->mmio_axi4_AWADDR;
      pending_write_len = top->mmio_axi4_AWLEN;
      pending_write_size = top->mmio_axi4_AWSIZE;
      pending_write_id = top->mmio_axi4_AWID;
      pending_write_finished = 0;
    }
    top->mmio_axi4_WREADY = 0;
    top->mmio_axi4_BVALID = 0;
  } else if (!pending_write_finished) {
    top->mmio_axi4_AWREADY = 0;
    top->mmio_axi4_WREADY = 1;

    // WVALID might be stale without eval()
    top->eval();
    if (top->mmio_axi4_WVALID) {
      mpz_class mask = 1;
      mask <<= 1L << pending_write_size;
      mask -= 1;

      mpz_class shifted_mask =
          mask << (pending_write_addr & (MMIO_AXI_DATA_BYTES - 1));
      mpz_class wdata;
      mpz_import(wdata.get_mpz_t(), MMIO_AXI_DATA_BYTES / 4, -1, 4, -1, 0,
                 &top->mmio_axi4_WDATA);

      uint64_t aligned =
          pending_write_addr / MMIO_AXI_DATA_BYTES * MMIO_AXI_DATA_BYTES;
      for (int i = 0; i < MMIO_AXI_DATA_BYTES / sizeof(mem_t); i++) {
        uint64_t addr = aligned + i * sizeof(mem_t);

        mpz_class local_wdata_mpz = wdata >> (i * (sizeof(mem_t) * 8));
        mem_t local_wdata = local_wdata_mpz.get_ui();

        uint64_t local_wstrb =
            (top->mmio_axi4_WSTRB >> (i * sizeof(mem_t))) & 0xfL;

        mpz_class local_mask_mpz = shifted_mask >> (i * sizeof(mem_t));
        uint64_t local_mask = local_mask_mpz.get_ui() & 0xfL;
        if (local_mask & local_wstrb) {
          mem_t base = memory[addr];
          mem_t input = local_wdata;
          uint64_t be = local_mask & local_wstrb;

          mem_t muxed = 0;
          for (int i = 0; i < sizeof(mem_t); i++) {
            mem_t sel;
            if (((be >> i) & 1) == 1) {
              sel = (input >> (i * 8)) & 0xff;
            } else {
              sel = (base >> (i * 8)) & 0xff;
            }
            muxed |= (sel << (i * 8));
          }

          memory[addr] = muxed;
        }
      }

      uint64_t input = wdata.get_ui();
      if (pending_write_addr == serial_addr ||
          pending_write_addr == serial_fpga_addr) {
        // serial
        printf("%c", input & 0xFF);
        fflush(stdout);
      } else if (pending_write_addr == tohost_addr) {
        // tohost
        uint32_t data = input & 0xFFFFFFFF;
        if (input == ((data & 0xFF) | 0x0101000000000000L)) {
          // serial
          printf("%c", input & 0xFF);
        } else if (data == 1) {
          // pass
          fprintf(stderr, "> ISA testsuite pass\n");
          if (!jtag) {
            finished = true;
          }
        } else if ((data & 1) == 1) {
          uint32_t c = data >> 1;
          fprintf(stderr, "> ISA testsuite failed case %d\n", c);
          if (!jtag) {
            finished = true;
          }
          res = 1;
        } else {
          fprintf(stderr, "> Unhandled tohost: %x\n", input);
        }
      } else if (pending_write_addr == fromhost_addr) {
        // write to fromhost
        // clear tohost
        for (int i = 0; i < MMIO_AXI_DATA_BYTES / sizeof(mem_t); i++) {
          uint64_t addr = tohost_addr + i * sizeof(mem_t);
          memory[addr] = 0;
        }
      }

      pending_write_addr += 1L << pending_write_size;
      pending_write_len--;
      if (top->mmio_axi4_WLAST) {
        assert(pending_write_len == -1);
        pending_write_finished = true;
      }
    }

    top->mmio_axi4_BVALID = 0;
  } else {
    // finishing
    top->mmio_axi4_AWREADY = 0;
    top->mmio_axi4_WREADY = 0;
    top->mmio_axi4_BVALID = 1;
    top->mmio_axi4_BRESP = 0;
    top->mmio_axi4_BID = pending_write_id;

    // BREADY might be stale without eval()
    top->eval();
    if (top->mmio_axi4_BREADY) {
      pending_write = false;
      pending_write_finished = false;
    }
  }
}

// load file
void load_file(const std::string &path) {
  size_t i = path.rfind('.');
  std::string ext;
  if (i != std::string::npos) {
    ext = path.substr(i);
  }
  if (ext == ".bin") {
    // load as bin
    FILE *fp = fopen(path.c_str(), "rb");
    assert(fp);
    uint64_t addr = 0x80000000;

    // read whole file and pad to multiples of mem_t
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    size_t padded_size = align(size + sizeof(mem_t) - 1);
    uint8_t *buffer = new uint8_t[padded_size];
    memset(buffer, 0, padded_size);

    size_t offset = 0;
    while (!feof(fp)) {
      ssize_t read = fread(&buffer[offset], 1, size - offset, fp);
      if (read <= 0) {
        break;
      }
      offset += read;
    }

    for (int i = 0; i < padded_size; i += sizeof(mem_t)) {
      memory[addr + i] = *((mem_t *)&buffer[i]);
    }
    fprintf(stderr, "> Loaded %ld bytes from BIN %s\n", size, path.c_str());
    fclose(fp);
    delete[] buffer;
  } else {
    // load as elf

    // read whole file
    FILE *fp = fopen(path.c_str(), "rb");
    assert(fp);
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    uint8_t *buffer = new uint8_t[size];
    memset(buffer, 0, size);

    size_t offset = 0;
    while (!feof(fp)) {
      ssize_t read = fread(&buffer[offset], 1, size - offset, fp);
      if (read <= 0) {
        break;
      }
      offset += read;
    }

    Elf64_Ehdr *hdr = (Elf64_Ehdr *)buffer;
    assert(hdr->e_ident[EI_MAG0] == ELFMAG0);
    assert(hdr->e_ident[EI_MAG1] == ELFMAG1);
    assert(hdr->e_ident[EI_MAG2] == ELFMAG2);
    assert(hdr->e_ident[EI_MAG3] == ELFMAG3);
    // 64bit
    assert(hdr->e_ident[EI_CLASS] == ELFCLASS64);
    // little endian
    assert(hdr->e_ident[EI_DATA] == ELFDATA2LSB);

    // https://github.com/eklitzke/parse-elf/blob/master/parse_elf.cc
    // iterate program header
    size_t total_size = 0;
    for (int i = 0; i < hdr->e_phnum; i++) {
      size_t offset = hdr->e_phoff + i * hdr->e_phentsize;
      Elf64_Phdr *hdr = (Elf64_Phdr *)&buffer[offset];
      if (hdr->p_type == PT_LOAD) {
        // load memory
        size_t size = hdr->p_filesz;
        size_t offset = hdr->p_offset;
        size_t dest = hdr->p_paddr;
        total_size += size;
        for (int i = 0; i < size; i += sizeof(mem_t)) {
          mem_t data = *(mem_t *)&buffer[offset + i];
          memory[dest + i] = data;
        }
      }
    }

    // find symbol table
    uint64_t symbol_table_offset = 0;
    uint64_t symbol_table_size = 0;
    uint64_t string_table = 0;
    for (int i = 0; i < hdr->e_shnum; i++) {
      size_t offset = hdr->e_shoff + i * hdr->e_shentsize;
      Elf64_Shdr *hdr = (Elf64_Shdr *)&buffer[offset];
      if (hdr->sh_type == SHT_SYMTAB) {
        symbol_table_offset = hdr->sh_offset;
        symbol_table_size = hdr->sh_size;
      } else if (hdr->sh_type == SHT_STRTAB) {
        if (!string_table) {
          string_table = hdr->sh_offset;
        }
      }
    }

    // iterate symbol table
    for (int i = 0; i < symbol_table_size; i += sizeof(Elf64_Sym)) {
      size_t offset = symbol_table_offset + i;
      Elf64_Sym *symbol = (Elf64_Sym *)&buffer[offset];
      std::string name = (char *)&buffer[string_table + symbol->st_name];
      if (name == "tohost") {
        tohost_addr = symbol->st_value;
      } else if (name == "fromhost") {
        fromhost_addr = symbol->st_value;
      } else if (name == "begin_signature") {
        begin_signature = symbol->st_value;
      } else if (name == "begin_signature_override") {
        begin_signature_override = symbol->st_value;
      } else if (name == "end_signature") {
        end_signature = symbol->st_value;
      }
    }

    fprintf(stderr, "> Loaded %ld bytes from ELF %s\n", size, path.c_str());
    fclose(fp);
    delete[] buffer;
  }
  fprintf(stderr, "> Using tohost at %x\n", tohost_addr);
  fprintf(stderr, "> Using fromhost at %x\n", fromhost_addr);
}

uint64_t get_time_us() {
  struct timeval tv = {};
  gettimeofday(&tv, NULL);
  return tv.tv_sec * 1000000 + tv.tv_usec;
}

int listen_fd = -1;
int client_fd = -1;

int jtag_rbb_init() {
  // ref rocket chip remote_bitbang.cc
  listen_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (listen_fd < 0) {
    perror("socket");
    return -1;
  }

  // set non blocking
  fcntl(listen_fd, F_SETFL, O_NONBLOCK);

  int reuseaddr = 1;
  if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &reuseaddr, sizeof(int)) <
      0) {
    perror("setsockopt");
    return -1;
  }

  int port = 12345;
  struct sockaddr_in addr = {};
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(port);

  if (bind(listen_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    perror("bind");
    return -1;
  }

  if (listen(listen_fd, 1) == -1) {
    perror("listen");
    return -1;
  }
  fprintf(stderr, "> Remote bitbang server listening at :12345\n");

  return 0;
}

void jtag_rbb_tick() {
  if (client_fd >= 0) {
    static char read_buffer[128];
    static size_t read_buffer_count = 0;
    static size_t read_buffer_offset = 0;

    if (read_buffer_offset == read_buffer_count) {
      ssize_t num_read = read(client_fd, read_buffer, sizeof(read_buffer));
      if (num_read > 0) {
        read_buffer_count = num_read;
        read_buffer_offset = 0;
      } else if (num_read == 0) {
        // remote socket closed
        fprintf(stderr, "> JTAG debugger detached\n");
        close(client_fd);
        client_fd = -1;
      }
    }

    if (read_buffer_offset < read_buffer_count) {
      char command = read_buffer[read_buffer_offset++];
      if ('0' <= command && command <= '7') {
        // set
        char offset = command - '0';
        top->jtag_TCK = (offset >> 2) & 1;
        top->jtag_TMS = (offset >> 1) & 1;
        top->jtag_TDI = (offset >> 0) & 1;
      } else if (command == 'R') {
        // read
        char send = top->jtag_TDO_data ? '1' : '0';

        while (1) {
          ssize_t sent = write(client_fd, &send, sizeof(send));
          if (sent > 0) {
            break;
          } else if (send < 0) {
            close(client_fd);
            client_fd = -1;
            break;
          }
        }
      } else if (command == 'r' || command == 's') {
        // trst = 0;
        // top->io_jtag_trstn = 1;
      } else if (command == 't' || command == 'u') {
        // trst = 1;
        // top->io_jtag_trstn = 0;
      }
    }
  } else {
    // accept connection
    client_fd = accept(listen_fd, NULL, NULL);
    if (client_fd > 0) {
      fcntl(client_fd, F_SETFL, O_NONBLOCK);

      // set nodelay
      int flags = 1;
      if (setsockopt(client_fd, IPPROTO_TCP, TCP_NODELAY, (void *)&flags,
                     sizeof(flags)) < 0) {
        perror("setsockopt");
      }
      fprintf(stderr, "> JTAG debugger attached\n");
    }
  }
}

int jtag_vpi_init() {
  listen_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (listen_fd < 0) {
    perror("socket");
    return -1;
  }

  // set non blocking
  fcntl(listen_fd, F_SETFL, O_NONBLOCK);

  int reuseaddr = 1;
  if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &reuseaddr, sizeof(int)) <
      0) {
    perror("setsockopt");
    return -1;
  }

  int port = 12345;
  struct sockaddr_in addr = {};
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(port);

  if (bind(listen_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    perror("bind");
    return -1;
  }

  if (listen(listen_fd, 1) == -1) {
    perror("listen");
    return -1;
  }
  fprintf(stderr, "> JTAG vpi server listening at :12345\n");

  jtag_vpi_state = JtagVpiState::CHECK_CMD;

  return 0;
}

bool write_socket_full(int fd, uint8_t *data, size_t count) {
  size_t num_sent = 0;
  while (num_sent < count) {
    ssize_t res = write(fd, &data[num_sent], count - num_sent);
    if (res > 0) {
      num_sent += res;
    } else if (count < 0) {
      return false;
    }
  }

  return true;
}

void jtag_vpi_tick() {
  // ref jtag_vpi project jtagServer.cpp

  static uint8_t jtag_vpi_buffer[sizeof(struct jtag_vpi_cmd)];
  static size_t jtag_vpi_recv = 0;
  // automatic tck clock
  static size_t tck_counter = 0;
  tck_counter++;
  static bool tck_en = false;
  top->jtag_TCK = tck_en ? (tck_counter % 2) : 0;

  struct jtag_vpi_cmd *cmd = (struct jtag_vpi_cmd *)jtag_vpi_buffer;

  if (jtag_vpi_state == JtagVpiState::CHECK_CMD) {
    if (client_fd >= 0) {
      ssize_t num_read = read(client_fd, &jtag_vpi_buffer[jtag_vpi_recv],
                              sizeof(jtag_vpi_cmd) - jtag_vpi_recv);
      if (num_read > 0) {
        jtag_vpi_recv += num_read;
      } else if (num_read == 0) {
        // remote socket closed
        fprintf(stderr, "> JTAG debugger detached\n");
        close(client_fd);
        client_fd = -1;
      }

      if (jtag_vpi_recv == sizeof(struct jtag_vpi_cmd)) {
        // cmd valid
        jtag_vpi_recv = 0;
        switch (cmd->cmd) {
        case CMD_RESET:
          jtag_vpi_state = TAP_RESET;
          break;
        case CMD_TMS_SEQ:
          jtag_vpi_state = DO_TMS_SEQ;
          break;
        case CMD_SCAN_CHAIN:
          memset(cmd->buffer_in, 0, sizeof(cmd->buffer_in));
          jtag_vpi_state = SCAN_CHAIN;
          jtag_vpi_tms_flip = false;
          break;
        case CMD_SCAN_CHAIN_FLIP_TMS:
          memset(cmd->buffer_in, 0, sizeof(cmd->buffer_in));
          jtag_vpi_state = SCAN_CHAIN;
          jtag_vpi_tms_flip = true;
          break;
        }
      }
    } else {
      client_fd = accept(listen_fd, NULL, NULL);
      if (client_fd > 0) {
        fcntl(client_fd, F_SETFL, O_NONBLOCK);

        // set nodelay
        int flags = 1;
        if (setsockopt(client_fd, IPPROTO_TCP, TCP_NODELAY, (void *)&flags,
                       sizeof(flags)) < 0) {
          perror("setsockopt");
        }
        fprintf(stderr, "> JTAG debugger attached\n");
      }
    }
  } else if (jtag_vpi_state == JtagVpiState::TAP_RESET) {
    // tap reset
    // tms=1 for five tck clocks
    static int reset_counter = 0;
    if (tck_counter % 2 == 0) {
      // tck fall
      if (reset_counter >= 5) {
        top->jtag_TMS = 0;
        reset_counter = 0;
        tck_en = false;
        jtag_vpi_state = JtagVpiState::GOTO_IDLE;
      } else {
        top->jtag_TMS = 1;
        tck_en = true;
      }
    } else if (tck_en) {
      // tck rise
      reset_counter++;
    }
  } else if (jtag_vpi_state == JtagVpiState::GOTO_IDLE) {
    // tap go to idle
    // tms=0 for one tck clock
    if (tck_counter % 2 == 0) {
      // tck fall
      top->jtag_TMS = 0;
      if (!tck_en) {
        tck_en = true;
      } else {
        tck_en = false;
        jtag_vpi_state = JtagVpiState::CHECK_CMD;
      }
    } else if (tck_en) {
      // tck rise
    }
  } else if (jtag_vpi_state == JtagVpiState::DO_TMS_SEQ) {
    // send tms
    static size_t progress = 0;

    if (tck_counter % 2 == 0) {
      // tck fall
      size_t byte_offset = progress / 8;
      size_t bit_offset = progress % 8;
      if (progress == cmd->nb_bits) {
        top->jtag_TMS = 0;
        progress = 0;
        tck_en = false;
        jtag_vpi_state = JtagVpiState::CHECK_CMD;
      } else {
        top->jtag_TMS = (cmd->buffer_out[byte_offset] >> bit_offset) & 1;
        tck_en = true;
      }
    } else if (tck_en) {
      // tck rise
      progress++;
    }
  } else if (jtag_vpi_state == JtagVpiState::SCAN_CHAIN) {
    // send tdi
    static size_t progress = 0;

    size_t byte_offset = progress / 8;
    size_t bit_offset = progress % 8;

    if (tck_counter % 2 == 0) {
      // tck fall
      if (progress < cmd->nb_bits) {
        top->jtag_TDI = (cmd->buffer_out[byte_offset] >> bit_offset) & 1;

        if (jtag_vpi_tms_flip && progress == cmd->nb_bits - 1) {
          // tms on last bit
          top->jtag_TMS = 1;
        }
        tck_en = true;
      } else {
        top->jtag_TDI = 0;
        top->jtag_TMS = 0;
        progress = 0;

        tck_en = false;
        jtag_vpi_state = JtagVpiState::CHECK_CMD;
        write_socket_full(client_fd, jtag_vpi_buffer,
                          sizeof(struct jtag_vpi_cmd));
      }
    } else if (tck_en) {
      // tck rise
      // capture tdo
      uint8_t bit = top->jtag_TDO_data & 1;
      cmd->buffer_in[byte_offset] |= bit << bit_offset;

      progress++;
    }
  }
}

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  signal(SIGINT, ctrlc_handler);

  // https://man7.org/linux/man-pages/man3/getopt.3.html
  int opt;
  bool trace = false;
  bool progress = false;
  const char *signature_path = "dump.sig";
  int signature_granularity = 16;
  while ((opt = getopt(argc, argv, "tpjvs:S:")) != -1) {
    switch (opt) {
    case 't':
      trace = true;
      break;
    case 'p':
      progress = true;
      break;
    case 'j':
      jtag = true;
      jtag_rbb = true;
      break;
    case 'v':
      jtag = true;
      jtag_vpi = true;
      break;
    case 's':
      signature_path = optarg;
      break;
    case 'S':
      sscanf(optarg, "%d", &signature_granularity);
      break;
    default: /* '?' */
      fprintf(stderr,
              "Usage: %s [-t] [-p] [-j] [-v] [-s signature] [-S granularity] "
              "name\n",
              argv[0]);
      return 1;
    }
  }

  if (optind < argc) {
    load_file(argv[optind]);
  }

  top = new VRiscVSystem;

  if (jtag) {
    if (jtag_rbb && jtag_rbb_init() < 0) {
      return -1;
    }
    if (jtag_vpi && jtag_vpi_init() < 0) {
      return -1;
    }

    // init
    top->jtag_TCK = 0;
    top->jtag_TMS = 0;
    top->jtag_TDI = 0;
    // top->io_jtag_trstn = 1;
  }

  VerilatedFstC *tfp = nullptr;
  if (trace) {
    Verilated::traceEverOn(true);
    tfp = new VerilatedFstC;
    top->trace(tfp, 99);
    tfp->open("dump.fst");
    fprintf(stderr, "> Enable tracing\n");
  }

  top->reset = 1;
  top->clock = 0;
  init();

  const size_t MAX_IQ_COUNT = 4;
  const size_t ISSUE_NUM = 2;
  size_t iq_empty_cycle_count[MAX_IQ_COUNT] = {};
  size_t iq_full_cycle_count[MAX_IQ_COUNT] = {};
  size_t cycles = 0;
  size_t issue_num_bounded_by_rob_size = 0;
  size_t issue_num_bounded_by_lsq_size = 0;
  size_t issue_num[ISSUE_NUM + 1] = {};
  size_t retire_num[ISSUE_NUM + 1] = {};

  fprintf(stderr, "> Simulation started\n");
  uint64_t begin = get_time_us();
  while (!Verilated::gotFinish() && !finished) {
    if (main_time > 50) {
      top->reset = 0;
    }
    if ((main_time % 10) == 0) {
      top->clock = 1;

      // log per 10000 mcycle
      if ((top->debug_0_mcycle % 10000) == 0 && top->debug_0_mcycle > 0 &&
          progress) {
        fprintf(stderr, "> mcycle: %ld\n", top->debug_0_mcycle);
        fprintf(stderr, "> minstret: %ld\n", top->debug_0_minstret);
        fprintf(stderr, "> pc: %lx\n", top->debug_0_pc);
      }

      if (top->debug_0_mcycle > 10000000 && !jtag) {
        // do not timeout in jtag mode
        fprintf(stderr, "> Timed out\n");
        finished = true;
        res = 1;
      }

      // accumulate rs free cycles
      for (int i = 0; i < MAX_IQ_COUNT; i++) {
        if ((top->debug_0_iqEmptyMask >> i) & 1) {
          iq_empty_cycle_count[i]++;
        }
        if ((top->debug_0_iqFullMask >> i) & 1) {
          iq_full_cycle_count[i]++;
        }
      }

      if (top->debug_0_issueNumBoundedByROBSize) {
        issue_num_bounded_by_rob_size++;
      }
      if (top->debug_0_issueNumBoundedByLSQSize) {
        issue_num_bounded_by_lsq_size++;
      }
      issue_num[top->debug_0_issueNum]++;
      retire_num[top->debug_0_retireNum]++;

      cycles++;
    }
    if ((main_time % 10) == 5) {
      top->clock = 0;
      step_mem();
      step_mmio();
    }

    if (jtag) {
      // jtag tick
      if (jtag_rbb) {
        jtag_rbb_tick();
      }
      if (jtag_vpi) {
        jtag_vpi_tick();
      }
    }

    top->eval();
    if (tfp) {
      tfp->dump(main_time);
      // tfp->flush();
    }
    main_time += 5;
  }
  uint64_t elapsed_us = get_time_us() - begin;
  fprintf(stderr, "> Simulation finished\n");
  fprintf(stderr, "> mcycle: %ld\n", top->debug_0_mcycle);
  fprintf(stderr, "> minstret: %ld\n", top->debug_0_minstret);
  fprintf(stderr, "> IPC: %.2lf\n",
          (double)top->debug_0_minstret / top->debug_0_mcycle);
  fprintf(stderr, "> Simulation speed: %.2lf mcycle/s\n",
          (double)top->debug_0_mcycle * 1000000 / elapsed_us);
  fprintf(stderr, "> Issue queue empty cycle:");
  for (int i = 0; i < MAX_IQ_COUNT; i++) {
    fprintf(stderr, " %.2lf%%", iq_empty_cycle_count[i] * 100.0 / cycles);
  }
  fprintf(stderr, "\n");
  fprintf(stderr, "> Issue queue full cycle:");
  for (int i = 0; i < MAX_IQ_COUNT; i++) {
    fprintf(stderr, " %.2lf%%", iq_full_cycle_count[i] * 100.0 / cycles);
  }
  fprintf(stderr, "\n");
  fprintf(stderr, "> Cycles when issue num is bounded by ROB size: %.2lf%%\n",
          issue_num_bounded_by_rob_size * 100.0 / cycles);
  fprintf(stderr, "> Cycles when issue num is bounded by LSQ size: %.2lf%%\n",
          issue_num_bounded_by_lsq_size * 100.0 / cycles);

  fprintf(stderr, "> Issue Num:");
  for (int i = 0; i <= ISSUE_NUM; i++) {
    fprintf(stderr, " %d=%.2lf%%", i, issue_num[i] * 100.0 / cycles);
  }
  fprintf(stderr, "\n");

  fprintf(stderr, "> Retire Num:");
  for (int i = 0; i <= ISSUE_NUM; i++) {
    fprintf(stderr, " %d=%.2lf%%", i, retire_num[i] * 100.0 / cycles);
  }
  fprintf(stderr, "\n");

  if (begin_signature && end_signature) {
    if (begin_signature_override) {
      // signature is copied
      end_signature =
          end_signature - begin_signature + begin_signature_override;
      begin_signature = begin_signature_override;
    }
    fprintf(stderr, "> Dumping signature(%lx:%lx) to dump.sig\n",
            begin_signature, end_signature);
    FILE *fp = fopen(signature_path, "w");
    for (uint64_t addr = begin_signature; addr < end_signature;
         addr += signature_granularity) {
      uint64_t words = signature_granularity / sizeof(mem_t);
      for (uint64_t i = 0; i < signature_granularity; i += sizeof(mem_t)) {
        fprintf(fp, "%08lx",
                memory[addr + signature_granularity - sizeof(mem_t) - i]);
      }
      fprintf(fp, "\n");
    }
    fclose(fp);
  }

  if (tfp) {
    tfp->flush();
    tfp->close();
  }
  top->final();
  delete top;
  return res;
}
