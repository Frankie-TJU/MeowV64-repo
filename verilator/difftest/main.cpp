#include "VRiscVSystem.h"
#include <arpa/inet.h>
#include <deque>
#include <fcntl.h>
#include <gmpxx.h>
#include <iostream>
#include <map>
#include <netinet/tcp.h>
#include <optional>
#include <riscv/cfg.h>
#include <riscv/devices.h>
#include <riscv/disasm.h>
#include <riscv/processor.h>
#include <riscv/sim.h>
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
typedef uint32_t meow_mem_t;
std::map<uint64_t, meow_mem_t> memory;

// align to meow_mem_t boundary
uint64_t align(uint64_t addr) {
  return (addr / sizeof(meow_mem_t)) * sizeof(meow_mem_t);
}

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

// machine timer interrupt pending
uint8_t mtip = 0;
// interrupt fire
uint8_t interrupt_fire = 0;
// supervisor external interrupt pending
uint8_t seip = 0;

struct fake_interrupt_controller : abstract_interrupt_controller_t {
  void set_interrupt_level(uint32_t interrupt_id, int level) {
    static int last_level = 0;
    if (level != last_level) {
      fprintf(stderr, "> intr %d = %d\n", interrupt_id, level);
      top->interrupts = level;
    }
    seip = level;
    last_level = level;
  }
  ~fake_interrupt_controller() {}
};

ns16550_t *sim_uart;
fake_interrupt_controller *sim_plic;

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

  sim_plic = new fake_interrupt_controller();
  sim_uart = new ns16550_t(NULL, sim_plic, 1, 2, 1);
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
    for (int i = 0; i < MEM_AXI_DATA_BYTES / sizeof(meow_mem_t); i++) {
      uint64_t addr = aligned + i * sizeof(meow_mem_t);
      meow_mem_t r = memory[addr];
      mpz_class res = r;
      res <<= (i * (sizeof(meow_mem_t) * 8));
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
      for (int i = 0; i < MEM_AXI_DATA_BYTES / sizeof(meow_mem_t); i++) {
        uint64_t addr = aligned + i * sizeof(meow_mem_t);

        mpz_class local_wdata_mpz = wdata >> (i * (sizeof(meow_mem_t) * 8));
        meow_mem_t local_wdata = local_wdata_mpz.get_ui();

        uint64_t local_wstrb =
            (top->mem_axi4_WSTRB >> (i * sizeof(meow_mem_t))) & 0xfL;

        mpz_class local_mask_mpz = shifted_mask >> (i * sizeof(meow_mem_t));
        uint64_t local_mask = local_mask_mpz.get_ui() & 0xfL;
        if (local_mask & local_wstrb) {
          meow_mem_t base = memory[addr];
          meow_mem_t input = local_wdata;
          uint64_t be = local_mask & local_wstrb;

          meow_mem_t muxed = 0;
          for (int i = 0; i < sizeof(meow_mem_t); i++) {
            meow_mem_t sel;
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
    if ((pending_read_addr >= serial_addr &&
         pending_read_addr <= serial_addr + 0x1000) ||
        (pending_read_addr >= serial_fpga_addr &&
         pending_read_addr <= serial_fpga_addr + 0x1000)) {
      // serial
      r_data = 0;
      uint64_t offset;
      if (pending_read_addr >= serial_addr &&
          pending_read_addr <= serial_addr + 0x1000) {
        offset = pending_read_addr - serial_addr;
      } else {
        offset = pending_read_addr - serial_fpga_addr;
      }
      uint8_t data;
      sim_uart->load(offset, 1, &data);
      r_data = data;
      r_data <<= (offset % 0x8) * 8;
    } else {
      uint64_t aligned =
          (pending_read_addr / MMIO_AXI_DATA_BYTES) * MMIO_AXI_DATA_BYTES;
      for (int i = 0; i < MMIO_AXI_DATA_BYTES / sizeof(meow_mem_t); i++) {
        uint64_t addr = aligned + i * sizeof(meow_mem_t);
        meow_mem_t r = memory[addr];
        mpz_class res = r;
        res <<= (i * (sizeof(meow_mem_t) * 8));
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
      for (int i = 0; i < MMIO_AXI_DATA_BYTES / sizeof(meow_mem_t); i++) {
        uint64_t addr = aligned + i * sizeof(meow_mem_t);

        mpz_class local_wdata_mpz = wdata >> (i * (sizeof(meow_mem_t) * 8));
        meow_mem_t local_wdata = local_wdata_mpz.get_ui();

        uint64_t local_wstrb =
            (top->mmio_axi4_WSTRB >> (i * sizeof(meow_mem_t))) & 0xfL;

        mpz_class local_mask_mpz = shifted_mask >> (i * sizeof(meow_mem_t));
        uint64_t local_mask = local_mask_mpz.get_ui() & 0xfL;
        if (local_mask & local_wstrb) {
          meow_mem_t base = memory[addr];
          meow_mem_t input = local_wdata;
          uint64_t be = local_mask & local_wstrb;

          meow_mem_t muxed = 0;
          for (int i = 0; i < sizeof(meow_mem_t); i++) {
            meow_mem_t sel;
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
      if ((pending_write_addr >= serial_addr &&
           pending_write_addr <= serial_addr + 0x1000) ||
          (pending_write_addr >= serial_fpga_addr &&
           pending_write_addr <= serial_fpga_addr + 0x1000)) {
        // serial
        uint64_t offset;
        if (pending_write_addr >= serial_addr &&
            pending_write_addr <= serial_addr + 0x1000) {
          offset = pending_write_addr - serial_addr;
        } else {
          offset = pending_write_addr - serial_fpga_addr;
        }
        uint8_t data;
        data = input >> ((offset % 0x8) * 8);
        sim_uart->store(offset, 1, &data);
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
        for (int i = 0; i < MMIO_AXI_DATA_BYTES / sizeof(meow_mem_t); i++) {
          uint64_t addr = tohost_addr + i * sizeof(meow_mem_t);
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
void load_file(const std::string &path, mem_t *m) {
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

    // read whole file and pad to multiples of meow_mem_t
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    size_t padded_size = align(size + sizeof(meow_mem_t) - 1);
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

    m->store(addr - 0x80000000, padded_size, buffer);
    for (int i = 0; i < padded_size; i += sizeof(meow_mem_t)) {
      memory[addr + i] = *((meow_mem_t *)&buffer[i]);
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
        m->store(dest - 0x80000000, size, &buffer[offset]);
        for (int i = 0; i < size; i += sizeof(meow_mem_t)) {
          meow_mem_t data = *(meow_mem_t *)&buffer[offset + i];
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

struct uncached_load_event {
  uint64_t addr;
  uint64_t len;
  uint64_t data;
};
std::deque<uncached_load_event> uncached_load_events;

uint64_t mtime = 0;

struct sim : simif_t {
  bus_t bus;
  cfg_t *cfg;
  std::map<size_t, processor_t *> m;

  // should return NULL for MMIO addresses
  char *addr_to_mem(reg_t paddr) {
    // taken from sim.cc
    auto desc = bus.find_device(paddr);
    if (auto mem = dynamic_cast<mem_t *>(desc.second))
      if (paddr - desc.first < mem->size())
        return mem->contents(paddr - desc.first);
    return NULL;
  }
  bool reservable(reg_t paddr) { return addr_to_mem(paddr); }
  // used for MMIO addresses
  bool mmio_fetch(reg_t paddr, size_t len, uint8_t *bytes) {
    return mmio_load(paddr, len, bytes);
  }
  bool mmio_load(reg_t paddr, size_t len, uint8_t *bytes) {
    if (paddr >= 0x80000000) {
      return bus.load(paddr, len, bytes);
    } else {
      // match rtl uncached load event
      if (!uncached_load_events.empty()) {
        uncached_load_event ev = uncached_load_events.front();
        if (ev.addr == paddr && ev.len == len) {
          memcpy(bytes, (char *)&ev.data, len);
          uncached_load_events.pop_front();
          return true;
        } else {
          fprintf(stderr, "> mismatched uncached load event!\n");
          fprintf(stderr, "> addr = %lx (expected %lx)\n", ev.addr, paddr);
          fprintf(stderr, "> len = %ld (len %ld)\n", ev.len, len);
        }
      } else {
        fprintf(stderr, "> mismatched uncached load event!\n");
        fprintf(stderr, "> expected addr = %ld\n", paddr);
        fprintf(stderr, "> expected len = %ld\n", len);
      }
      return false;
    }
  };
  bool mmio_store(reg_t paddr, size_t len, const uint8_t *bytes) {
    if (paddr >= 0x80000000) {
      return bus.store(paddr, len, bytes);
    } else {
      // fprintf(stderr, "> uncached store addr %lx len %ld\n", paddr, len);
      return true;
    }
  };
  // Callback for processors to let the simulation know they were reset.
  void proc_reset(unsigned id){};

  const cfg_t &get_cfg() const { return *cfg; };
  const std::map<size_t, processor_t *> &get_harts() const { return m; };

  const char *get_symbol(uint64_t paddr) { return NULL; };

  unsigned nprocs() const { return get_cfg().nprocs(); }

  ~sim() = default;
};

// to sync mtime in CLINT
extern "C" {
void set_time(long long cur_time) {
  fprintf(stderr, "> read mtime = %d\n", cur_time);
  mtime = cur_time;
}
void set_mtip(uint8_t ip) { mtip = ip; }
};

processor_t *proc;

struct commit_state {
  int valid;
  uint64_t pc;
  uint32_t inst;
};

enum csr {
  STATE_CSR_MSTATUS,
  STATE_CSR_SSTATUS,
  STATE_CSR_MEPC,
  STATE_CSR_SEPC,
  STATE_CSR_MTVAL,
  STATE_CSR_STVAL,
  STATE_CSR_MTVEC,
  STATE_CSR_STVEC,
  STATE_CSR_MCAUSE,
  STATE_CSR_SCAUSE,
  STATE_CSR_SATP,
  STATE_CSR_MIP,
  STATE_CSR_MIE,
  STATE_CSR_MSCRATCH,
  STATE_CSR_SSCRATCH,
  STATE_CSR_MIDELEG,
  STATE_CSR_MEDELEG,
  STATE_CSR_FCSR,
  STATE_CSR_VSTART,
  STATE_CSR_VCSR,
  STATE_CSR_VL,
  STATE_CSR_VTYPE,
  STATE_CSR_COUNT,
};
const char *csr_names[] = {
    "mstatus", "sstatus",  "mepc",     "sepc",    "mtval",   "stval",
    "mtvec",   "stvec",    "mcause",   "scause",  "satp",    "mip",
    "mie",     "mscratch", "sscratch", "mideleg", "medeleg", "fcsr",
    "vstart",  "vcsr",     "vl",       "vtype"};
const char *gpr_names[] = {"zero",  "ra", "sp",  "gp",  "tp", "t0", "t1", "t2",
                           "s0/fp", "s1", "a0",  "a1",  "a2", "a3", "a4", "a5",
                           "a6",    "a7", "s2",  "s3",  "s4", "s5", "s6", "s7",
                           "s8",    "s9", "s10", "s11", "t3", "t4", "t5", "t6"};
const char *fpr_names[] = {"ft0", "ft1", "ft2",  "ft3", "ft4", "ft5",  "ft6",
                           "ft7", "fs0", "fs1",  "fa0", "fa1", "fa2",  "fa3",
                           "fa4", "fa5", "fa6",  "fa7", "fs2", "fs3",  "fs4",
                           "fs5", "fs6", "fs7",  "fs8", "fs9", "fs10", "fs11",
                           "ft8", "ft9", "ft10", "ft11"};

const int history_size = 10;

struct history_entry {
  uint64_t pc;
  uint32_t inst;
};

struct cpu_state {
  struct commit_state insn[2];
  uint64_t csr_state[STATE_CSR_COUNT];
  uint64_t gpr[32];
  uint64_t fpr[32];
  std::deque<history_entry> history;
} cpu_state;

struct spike_state {
  uint64_t pc;
  uint64_t csr_state[STATE_CSR_COUNT];
  uint64_t gpr[32];
  uint64_t fpr[32];
  std::deque<uint64_t> pc_history;
} spike_state;

uint64_t last_pc = 0x80000000;
uint32_t last_inst = 0x00000000;

extern "C" {

#define DPIC_ARG_BIT uint8_t
#define DPIC_ARG_BYTE uint8_t
#define DPIC_ARG_INT uint32_t
#define DPIC_ARG_LONG uint64_t

void v_difftest_ArchIntRegState(
    DPIC_ARG_BYTE coreid, DPIC_ARG_LONG gpr_0, DPIC_ARG_LONG gpr_1,
    DPIC_ARG_LONG gpr_2, DPIC_ARG_LONG gpr_3, DPIC_ARG_LONG gpr_4,
    DPIC_ARG_LONG gpr_5, DPIC_ARG_LONG gpr_6, DPIC_ARG_LONG gpr_7,
    DPIC_ARG_LONG gpr_8, DPIC_ARG_LONG gpr_9, DPIC_ARG_LONG gpr_10,
    DPIC_ARG_LONG gpr_11, DPIC_ARG_LONG gpr_12, DPIC_ARG_LONG gpr_13,
    DPIC_ARG_LONG gpr_14, DPIC_ARG_LONG gpr_15, DPIC_ARG_LONG gpr_16,
    DPIC_ARG_LONG gpr_17, DPIC_ARG_LONG gpr_18, DPIC_ARG_LONG gpr_19,
    DPIC_ARG_LONG gpr_20, DPIC_ARG_LONG gpr_21, DPIC_ARG_LONG gpr_22,
    DPIC_ARG_LONG gpr_23, DPIC_ARG_LONG gpr_24, DPIC_ARG_LONG gpr_25,
    DPIC_ARG_LONG gpr_26, DPIC_ARG_LONG gpr_27, DPIC_ARG_LONG gpr_28,
    DPIC_ARG_LONG gpr_29, DPIC_ARG_LONG gpr_30, DPIC_ARG_LONG gpr_31) {
  cpu_state.gpr[0] = gpr_0;
  cpu_state.gpr[1] = gpr_1;
  cpu_state.gpr[2] = gpr_2;
  cpu_state.gpr[3] = gpr_3;
  cpu_state.gpr[4] = gpr_4;
  cpu_state.gpr[5] = gpr_5;
  cpu_state.gpr[6] = gpr_6;
  cpu_state.gpr[7] = gpr_7;
  cpu_state.gpr[8] = gpr_8;
  cpu_state.gpr[9] = gpr_9;
  cpu_state.gpr[10] = gpr_10;
  cpu_state.gpr[11] = gpr_11;
  cpu_state.gpr[12] = gpr_12;
  cpu_state.gpr[13] = gpr_13;
  cpu_state.gpr[14] = gpr_14;
  cpu_state.gpr[15] = gpr_15;
  cpu_state.gpr[16] = gpr_16;
  cpu_state.gpr[17] = gpr_17;
  cpu_state.gpr[18] = gpr_18;
  cpu_state.gpr[19] = gpr_19;
  cpu_state.gpr[20] = gpr_20;
  cpu_state.gpr[21] = gpr_21;
  cpu_state.gpr[22] = gpr_22;
  cpu_state.gpr[23] = gpr_23;
  cpu_state.gpr[24] = gpr_24;
  cpu_state.gpr[25] = gpr_25;
  cpu_state.gpr[26] = gpr_26;
  cpu_state.gpr[27] = gpr_27;
  cpu_state.gpr[28] = gpr_28;
  cpu_state.gpr[29] = gpr_29;
  cpu_state.gpr[30] = gpr_30;
  cpu_state.gpr[31] = gpr_31;
}

void v_difftest_ArchFpRegState(
    DPIC_ARG_BYTE coreid, DPIC_ARG_LONG fpr_0, DPIC_ARG_LONG fpr_1,
    DPIC_ARG_LONG fpr_2, DPIC_ARG_LONG fpr_3, DPIC_ARG_LONG fpr_4,
    DPIC_ARG_LONG fpr_5, DPIC_ARG_LONG fpr_6, DPIC_ARG_LONG fpr_7,
    DPIC_ARG_LONG fpr_8, DPIC_ARG_LONG fpr_9, DPIC_ARG_LONG fpr_10,
    DPIC_ARG_LONG fpr_11, DPIC_ARG_LONG fpr_12, DPIC_ARG_LONG fpr_13,
    DPIC_ARG_LONG fpr_14, DPIC_ARG_LONG fpr_15, DPIC_ARG_LONG fpr_16,
    DPIC_ARG_LONG fpr_17, DPIC_ARG_LONG fpr_18, DPIC_ARG_LONG fpr_19,
    DPIC_ARG_LONG fpr_20, DPIC_ARG_LONG fpr_21, DPIC_ARG_LONG fpr_22,
    DPIC_ARG_LONG fpr_23, DPIC_ARG_LONG fpr_24, DPIC_ARG_LONG fpr_25,
    DPIC_ARG_LONG fpr_26, DPIC_ARG_LONG fpr_27, DPIC_ARG_LONG fpr_28,
    DPIC_ARG_LONG fpr_29, DPIC_ARG_LONG fpr_30, DPIC_ARG_LONG fpr_31) {
  cpu_state.fpr[0] = fpr_0;
  cpu_state.fpr[1] = fpr_1;
  cpu_state.fpr[2] = fpr_2;
  cpu_state.fpr[3] = fpr_3;
  cpu_state.fpr[4] = fpr_4;
  cpu_state.fpr[5] = fpr_5;
  cpu_state.fpr[6] = fpr_6;
  cpu_state.fpr[7] = fpr_7;
  cpu_state.fpr[8] = fpr_8;
  cpu_state.fpr[9] = fpr_9;
  cpu_state.fpr[10] = fpr_10;
  cpu_state.fpr[11] = fpr_11;
  cpu_state.fpr[12] = fpr_12;
  cpu_state.fpr[13] = fpr_13;
  cpu_state.fpr[14] = fpr_14;
  cpu_state.fpr[15] = fpr_15;
  cpu_state.fpr[16] = fpr_16;
  cpu_state.fpr[17] = fpr_17;
  cpu_state.fpr[18] = fpr_18;
  cpu_state.fpr[19] = fpr_19;
  cpu_state.fpr[20] = fpr_20;
  cpu_state.fpr[21] = fpr_21;
  cpu_state.fpr[22] = fpr_22;
  cpu_state.fpr[23] = fpr_23;
  cpu_state.fpr[24] = fpr_24;
  cpu_state.fpr[25] = fpr_25;
  cpu_state.fpr[26] = fpr_26;
  cpu_state.fpr[27] = fpr_27;
  cpu_state.fpr[28] = fpr_28;
  cpu_state.fpr[29] = fpr_29;
  cpu_state.fpr[30] = fpr_30;
  cpu_state.fpr[31] = fpr_31;
}

void v_difftest_CSRState(
    DPIC_ARG_BYTE coreid, DPIC_ARG_BYTE privilegeMode, DPIC_ARG_LONG mstatus,
    DPIC_ARG_LONG sstatus, DPIC_ARG_LONG mepc, DPIC_ARG_LONG sepc,
    DPIC_ARG_LONG mtval, DPIC_ARG_LONG stval, DPIC_ARG_LONG mtvec,
    DPIC_ARG_LONG stvec, DPIC_ARG_LONG mcause, DPIC_ARG_LONG scause,
    DPIC_ARG_LONG satp, DPIC_ARG_LONG mip, DPIC_ARG_LONG mie,
    DPIC_ARG_LONG mscratch, DPIC_ARG_LONG sscratch, DPIC_ARG_LONG mideleg,
    DPIC_ARG_LONG medeleg, DPIC_ARG_LONG fcsr, DPIC_ARG_LONG vstart,
    DPIC_ARG_LONG vcsr, DPIC_ARG_LONG vl, DPIC_ARG_LONG vtype) {
  cpu_state.csr_state[STATE_CSR_MSTATUS] = mstatus;
  cpu_state.csr_state[STATE_CSR_SSTATUS] = sstatus;
  cpu_state.csr_state[STATE_CSR_MEPC] = mepc;
  cpu_state.csr_state[STATE_CSR_SEPC] = sepc;
  cpu_state.csr_state[STATE_CSR_MTVAL] = mtval;
  cpu_state.csr_state[STATE_CSR_STVAL] = stval;
  cpu_state.csr_state[STATE_CSR_MTVEC] = mtvec;
  cpu_state.csr_state[STATE_CSR_STVEC] = stvec;
  cpu_state.csr_state[STATE_CSR_MCAUSE] = mcause;
  cpu_state.csr_state[STATE_CSR_SCAUSE] = scause;
  cpu_state.csr_state[STATE_CSR_SATP] = satp;
  cpu_state.csr_state[STATE_CSR_MIP] = mip;
  cpu_state.csr_state[STATE_CSR_MIE] = mie;
  cpu_state.csr_state[STATE_CSR_MSCRATCH] = mscratch;
  cpu_state.csr_state[STATE_CSR_SSCRATCH] = sscratch;
  cpu_state.csr_state[STATE_CSR_MIDELEG] = mideleg;
  cpu_state.csr_state[STATE_CSR_MEDELEG] = medeleg;
  cpu_state.csr_state[STATE_CSR_FCSR] = fcsr;
  cpu_state.csr_state[STATE_CSR_VSTART] = vstart;
  cpu_state.csr_state[STATE_CSR_VCSR] = vcsr;
  cpu_state.csr_state[STATE_CSR_VL] = vl;
  cpu_state.csr_state[STATE_CSR_VTYPE] = vtype;
}

void v_difftest_InstrCommit(DPIC_ARG_BYTE coreid, DPIC_ARG_BYTE index,
                            DPIC_ARG_BIT valid, DPIC_ARG_BYTE special,
                            DPIC_ARG_BIT skip, DPIC_ARG_BIT isRVC,
                            DPIC_ARG_BIT rfwen, DPIC_ARG_BIT fpwen,
                            DPIC_ARG_INT wpdest, DPIC_ARG_BYTE wdest,
                            DPIC_ARG_LONG pc, DPIC_ARG_INT instr,
                            DPIC_ARG_INT robidx, DPIC_ARG_BYTE lqidx,
                            DPIC_ARG_BYTE sqidx, DPIC_ARG_BIT isLoad,
                            DPIC_ARG_BIT isStore) {
  if (valid) {
    cpu_state.insn[index].valid = 1;
    cpu_state.insn[index].pc = pc;
    cpu_state.insn[index].inst = instr;
  }
}

void v_difftest_ArchEvent(DPIC_ARG_BYTE coreid, DPIC_ARG_INT intrNo,
                          DPIC_ARG_INT cause, DPIC_ARG_LONG exceptionPC,
                          DPIC_ARG_INT exceptionInst) {
  if (intrNo > 0) {
    fprintf(stderr, "> %ld: interrupt %d\n", main_time, intrNo);
    interrupt_fire = intrNo;
  }
}

void v_difftest_TrapEvent() {}

struct store_event {
  uint64_t addr;
  mpz_class data;
  uint32_t len;
};
std::deque<store_event> store_events;

void v_difftest_StoreEvent(DPIC_ARG_BYTE coreid, DPIC_ARG_BYTE index,
                           DPIC_ARG_BIT valid, DPIC_ARG_LONG storeAddr,
                           DPIC_ARG_LONG storeData_0, DPIC_ARG_LONG storeData_1,
                           DPIC_ARG_LONG storeData_2, DPIC_ARG_LONG storeData_3,
                           DPIC_ARG_INT storeMask) {
  if (valid) {
    store_event ev;
    ev.addr = storeAddr;
    ev.data = storeData_0;
    ev.data |= ((mpz_class)storeData_1) << 64;
    ev.data |= ((mpz_class)storeData_2) << 128;
    ev.data |= ((mpz_class)storeData_3) << 192;
    if (storeMask == 0x1) {
      ev.len = 1;
      ev.data &= 0xffL;
    } else if (storeMask == 0x3) {
      ev.len = 2;
      ev.data &= 0xffffL;
    } else if (storeMask == 0xf) {
      ev.len = 4;
      ev.data &= 0xffffffffL;
    } else if (storeMask == 0xff) {
      ev.len = 8;
      ev.data &= (((mpz_class)1) << 64) - 1;
    } else if (storeMask == 0xffff) {
      ev.len = 16;
      ev.data &= (((mpz_class)1) << 128) - 1;
    } else {
      ev.len = 0;
      fprintf(stderr, "> Unexpected write mask: %d\n", storeMask);
    }
    store_events.push_back(ev);
  }
}

void v_difftest_UncachedLoadEvent(DPIC_ARG_BYTE coreid, DPIC_ARG_BIT valid,
                                  DPIC_ARG_LONG addr, DPIC_ARG_LONG len,
                                  DPIC_ARG_LONG data) {
  if (valid) {
    uncached_load_event ev;
    ev.addr = addr;
    ev.data = data;
    ev.len = 1 << len;
    uncached_load_events.push_back(ev);
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

  mem_t m(0x20000000);
  if (optind < argc) {
    load_file(argv[optind], &m);
  }

  const char *isa = "RV64IMAFDCV_Zfh";
  cfg_t cfg(/*default_initrd_bounds=*/std::make_pair((reg_t)0, (reg_t)0),
            /*default_bootargs=*/nullptr,
            /*default_isa=*/isa,
            /*default_priv=*/DEFAULT_PRIV,
            /*default_varch=*/"vlen:128,elen:64",
            /*default_misaligned=*/false,
            /*default_endianness=*/endianness_little,
            /*default_pmpregions=*/0,
            /*default_mem_layout=*/
            std::vector<mem_cfg_t>{mem_cfg_t(0x80000000, 0x20000000)},
            /*default_hartids=*/std::vector<size_t>(),
            /*default_real_time_clint=*/false,
            /*default_trigger_count=*/4);
  sim s;
  s.cfg = &cfg;
  s.bus.add_device(0x80000000, &m);
  // add dummy device for reading dtb
  // mem_t m_zero(0x1000);
  // s.bus.add_device(0x00000000, &m_zero);
  // add dummy device for tohost/fromhost/signature
  // mem_t m_htif(0x10000000);
  // s.bus.add_device(0x60000000, &m_htif);

  isa_parser_t isa_parser(isa, DEFAULT_PRIV);
  processor_t p(&isa_parser, &cfg, &s, 0, true, stderr, std::cerr);
  s.m[0] = &p;
  // only enable sv39 and sv48, disable sv57
  p.set_impl(IMPL_MMU_SV57, false);
  // asid not implemented
  p.set_impl(IMPL_MMU_ASID, false);
  p.difftest_mode = true;

  // add plic, clint and uart
  std::vector<processor_t *> procs;
  procs.push_back(&p);

  p.reset();
  p.get_state()->pc = 0x80000000;
  // p.enable_log_commits();
  // p.debug = true;
  proc = &p;

  disassembler_t disassembler(&isa_parser);

  auto difftest_failed = [&]() {
    for (int i = 0; i < 32; i++) {
      if (spike_state.gpr[i] == cpu_state.gpr[i]) {
        fprintf(stderr, "> gpr[%d, %s] = %016lx\n", i, gpr_names[i],
                cpu_state.gpr[i]);
      } else {
        fprintf(stderr, "> gpr[%d, %s] = %016lx (expected %016lx)\n", i,
                gpr_names[i], cpu_state.gpr[i], spike_state.gpr[i]);
      }
    }
    for (int i = 0; i < 32; i++) {
      if (spike_state.fpr[i] == cpu_state.fpr[i]) {
        fprintf(stderr, "> fpr[%d, %s] = %016lx\n", i, fpr_names[i],
                cpu_state.fpr[i]);
      } else {
        fprintf(stderr, "> fpr[%d, %s] = %016lx (expected %016lx)\n", i,
                fpr_names[i], cpu_state.fpr[i], spike_state.fpr[i]);
      }
    }
    for (int i = 0; i < STATE_CSR_COUNT; i++) {
      if (spike_state.csr_state[i] == cpu_state.csr_state[i]) {
        fprintf(stderr, "> csr[%s] = %016lx\n", csr_names[i],
                cpu_state.csr_state[i]);
      } else {
        fprintf(stderr, "> csr[%s] = %016lx (expected %016lx)\n", csr_names[i],
                cpu_state.csr_state[i], spike_state.csr_state[i]);
      }
    }
    fprintf(stderr, "> cpu history:\n");
    for (auto hist : cpu_state.history) {
      fprintf(stderr, "> pc=%016lx inst=%08lx %s\n", hist.pc, hist.inst,
              disassembler.disassemble(hist.inst).c_str());
    }
    fprintf(stderr, "> spike pc history:\n");
    for (auto pc : spike_state.pc_history) {
      fprintf(stderr, "> %016lx\n", pc);
    }

    finished = true;
    res = 1;
  };

  // step and save csr, gpr & fpr state
  auto step_spike = [&]() {
    proc->step(1);

    auto &log_mem_write = proc->get_state()->log_mem_write;
    if (log_mem_write.size() > 0) {
      int i = 0;
      while (i < log_mem_write.size()) {
        if (store_events.empty()) {
          uint64_t addr, val, len;
          std::tie(addr, val, len) = log_mem_write[i];
          fprintf(stderr,
                  "> %ld: Missing store event @ pc %lx (expected addr %lx data "
                  "%lx len %ld)\n",
                  main_time, proc->get_state()->last_inst_pc, addr, val, len);
          difftest_failed();
          break;
        } else {
          store_event ev = store_events.front();
          uint64_t addr, temp, len;
          std::tie(addr, temp, len) = log_mem_write[i];
          mpz_class val = temp;

          // attempt to merge requests
          if (len < ev.len) {
            while (i + 1 < log_mem_write.size() && len < ev.len) {
              i++;
              uint64_t addr1, val1, len1;
              std::tie(addr1, val1, len1) = log_mem_write[i];
              if (addr1 != addr + len) {
                fprintf(stderr, "> %ld: failed to merge write\n", main_time);
                break;
              }
              val |= ((mpz_class)val1) << (len * 8);
              len += len1;
            }
          }

          std::string val_str = val.get_str(16);
          std::string data_str = ev.data.get_str(16);
          if (ev.addr != addr || ev.data != val || ev.len != len) {
            fprintf(stderr,
                    "> %ld: Mismatch store event @ pc %lx addr %lx (expected "
                    "%lx) data %s (expected %s) len %lx (expected %ld)\n",
                    main_time, proc->get_state()->last_inst_pc, ev.addr, addr,
                    data_str.c_str(), val_str.c_str(), ev.len, len);
            difftest_failed();
          }
          i++;
          store_events.pop_front();
        }
      }
    }

    const csr_t *csrs[] = {
        proc->get_state()->mstatus.get(),
        proc->get_state()->sstatus.get(),
        proc->get_state()->mepc.get(),
        proc->get_state()->sepc.get(),
        proc->get_state()->mtval.get(),
        proc->get_state()->stval.get(),
        proc->get_state()->mtvec.get(),
        proc->get_state()->stvec.get(),
        proc->get_state()->mcause.get(),
        proc->get_state()->scause.get(),
        proc->get_state()->satp.get(),
        proc->get_state()->mip.get(),
        proc->get_state()->mie.get(),
        proc->get_state()->csrmap[CSR_MSCRATCH].get(),
        proc->get_state()->csrmap[CSR_SSCRATCH].get(),
        proc->get_state()->mideleg.get(),
        proc->get_state()->medeleg.get(),
        proc->get_state()->csrmap[CSR_FCSR].get(),
        proc->VU.vstart.get(),
        proc->get_state()->csrmap[CSR_VCSR].get(),
        proc->VU.vl.get(),
        proc->VU.vtype.get(),
    };
    for (int i = 0; i < 32; i++) {
      spike_state.gpr[i] = proc->get_state()->XPR[i];
    }
    for (int i = 0; i < 32; i++) {
      spike_state.fpr[i] = proc->get_state()->FPR[i].v[0];
    }
    for (int i = 0; i < STATE_CSR_COUNT; i++) {
      spike_state.csr_state[i] = csrs[i]->read();
    }
    // patch mtip
    if (mtip) {
      spike_state.csr_state[STATE_CSR_MIP] |= MIP_MTIP;
    }
    spike_state.pc = proc->get_state()->last_inst_pc;
    spike_state.pc_history.push_back(spike_state.pc);
    if (spike_state.pc_history.size() > history_size) {
      spike_state.pc_history.pop_front();
    }
  };

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

      if (0) {
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

      // sync mtime and mcycle
      proc->get_state()->mcycle->write(top->debug_0_mcycle - 3);

      // handle trap
      if (interrupt_fire) {
        trap_t trap = 0x8000000000000000 | interrupt_fire;
        proc->take_trap(trap, proc->get_state()->pc);
        fprintf(stderr, "> %ld: take trap %d\n", main_time, interrupt_fire);
        interrupt_fire = 0;
      }

      bool any_valid = false;
      for (int index = 0; index < 2; index++) {
        if (cpu_state.insn[index].valid) {
          any_valid = true;

          history_entry hist;
          uint64_t cur_pc = cpu_state.insn[index].pc;
          hist.pc = cur_pc;
          hist.inst = cpu_state.insn[index].inst;
          cpu_state.history.push_back(hist);

          if (cpu_state.history.size() > history_size) {
            cpu_state.history.pop_front();
          }

          // debugging
          if (0) {
            fprintf(stderr, "> %ld: commit @ pc %lx inst %08lx\n", main_time,
                    cur_pc, cpu_state.insn[index].inst);
            for (int i = 0; i < 32; i++) {
              fprintf(stderr, "> gpr[%d, %s] = %016lx\n", i, gpr_names[i],
                      cpu_state.gpr[i]);
            }
            for (int i = 0; i < 32; i++) {
              fprintf(stderr, "> fpr[%d, %s] = %016lx\n", i, fpr_names[i],
                      cpu_state.fpr[i]);
            }
            fprintf(stderr, "> cpu history:\n");
            for (auto hist : cpu_state.history) {
              fprintf(stderr, "> pc=%016lx inst=%08lx %s\n", hist.pc, hist.inst,
                      disassembler.disassemble(hist.inst).c_str());
            }
          }

          step_spike();

          uint64_t exp_pc = spike_state.pc;
          if (cur_pc != exp_pc) {
            fprintf(
                stderr,
                "> %ld: Mismatch commit @ pc %lx (expected %lx) inst %08lx\n",
                main_time, cur_pc, exp_pc, cpu_state.insn[index].inst);
            difftest_failed();
          }

          last_pc = cur_pc;
          last_inst = cpu_state.insn[index].inst;

          cpu_state.insn[index].valid = 0;
        }
      }

      if (any_valid) {
        for (int i = 0; i < STATE_CSR_COUNT; i++) {
          uint64_t actual = cpu_state.csr_state[i];
          uint64_t expected = spike_state.csr_state[i];
          if (expected != actual) {
            if (last_inst == 0x10200073 || last_inst == 0x30200073) {
              // sret/mret
              // csr update is async with inst commit
              continue;
            }

            fprintf(stderr,
                    "> %ld: Mismatch csr @ pc %lx inst %08lx %s %lx (expected "
                    "%lx)\n",
                    main_time, last_pc, last_inst, csr_names[i], actual,
                    expected);
            // skip transient error
            if (i != STATE_CSR_MIP && i != STATE_CSR_MSTATUS &&
                i != STATE_CSR_SSTATUS && i != STATE_CSR_FCSR &&
                i != STATE_CSR_VL && i != STATE_CSR_VTYPE &&
                i != STATE_CSR_VCSR) {
              difftest_failed();
            }
          }
        }

        for (int i = 0; i < 32; i++) {
          uint64_t actual = cpu_state.gpr[i];
          uint64_t expected = spike_state.gpr[i];
          if (expected != actual) {
            fprintf(stderr,
                    "> %ld: Mismatch gpr @ pc %lx inst %08lx gpr[%d]=%016lx "
                    "(expected "
                    "%016lx)\n",
                    main_time, last_pc, last_inst, i, actual, expected);
            difftest_failed();
          }

          actual = cpu_state.fpr[i];
          expected = spike_state.fpr[i];
          if (expected != actual) {
            fprintf(stderr,
                    "> %ld: Mismatch fpr @ pc %lx inst %08lx fpr[%d]=%016lx "
                    "(expected "
                    "%016lx)\n",
                    main_time, last_pc, last_inst, i, actual, expected);
            difftest_failed();
          }
        }
      }
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
      if (1 || main_time > 21032000000) {
        tfp->dump(main_time);
      }
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
      uint64_t words = signature_granularity / sizeof(meow_mem_t);
      for (uint64_t i = 0; i < signature_granularity; i += sizeof(meow_mem_t)) {
        fprintf(fp, "%08lx",
                memory[addr + signature_granularity - sizeof(meow_mem_t) - i]);
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
