VERILATOR = verilator
GMP_CFLAGS = $(shell pkg-config --cflags gmp)
GMP_LDFLAGS = $(shell pkg-config --libs gmp)
GMPXX_CFLAGS = $(shell pkg-config --cflags gmpxx)
GMPXX_LDFLAGS = $(shell pkg-config --libs gmpxx)
VERILATOR_FLAGS = -Wno-fatal --trace-fst -CFLAGS "$(GMP_CFLAGS) $(GMPXX_CFLAGS)" -LDFLAGS "$(GMP_LDFLAGS) $(GMPXX_LDFLAGS)"
CURRENT_DIR = $(shell pwd)
VERILOG_SRCS = meowv64.rocket.MeowV64Config.v plusarg_reader.v EICG_wrapper.v

all: VRiscVSystem

$(VERILOG_SRCS):
	cp ../../build/$@ .

VRiscVSystem: ../rocket/main.cpp $(VERILOG_SRCS)
	$(VERILATOR) $(VERILATOR_FLAGS) --top-module RiscVSystem --cc $(VERILOG_SRCS) --exe $<
	make -j8 -C obj_dir -f VRiscVSystem.mk VRiscVSystem
	cp obj_dir/VRiscVSystem .

clean:
	rm -rf $(VERILOG_SRCS) obj_dir