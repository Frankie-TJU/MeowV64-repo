CONFIG = meowv64.rocket.MeowV64TapeOutConfig
VERILATOR = verilator
ZLIB_CFLAGS = $(shell pkg-config --cflags zlib)
ZLIB_LDFLAGS = $(shell pkg-config --libs zlib)
GMP_CFLAGS = $(shell pkg-config --cflags gmp)
GMP_LDFLAGS = $(shell pkg-config --libs gmp)
GMPXX_CFLAGS = $(shell pkg-config --cflags gmpxx)
GMPXX_LDFLAGS = $(shell pkg-config --libs gmpxx)
VERILATOR_FLAGS = -O3 -Wno-fatal --trace-fst -threads 4 -CFLAGS "-march=native -O3 $(ZLIB_CFLAGS) $(GMP_CFLAGS) $(GMPXX_CFLAGS)" -LDFLAGS "$(ZLIB_LDFLAGS) $(GMP_LDFLAGS) $(GMPXX_LDFLAGS)"
CURRENT_DIR = $(shell pwd)
VERILOG_SRCS = $(CONFIG).v plusarg_reader.v EICG_wrapper.v icDataArray_ext.v btbEntries_ext.v cc_dir_ext.v cc_banks_0_ext.v data_ext.v

all: VRiscVSystem

firrtl_black_box_resource_files.f:
	cd ../../ && CONFIG=$(CONFIG) ./generate.sh
	cp ../../build/$(CONFIG)/$@ .

%.v: firrtl_black_box_resource_files.f
	cp ../../build/$(CONFIG)/$@ .

VRiscVSystem: ../tapeout/main.cpp $(VERILOG_SRCS)
	$(VERILATOR) $(VERILATOR_FLAGS) --top-module RiscVSystem --cc $(VERILOG_SRCS) --exe $<
	make -j8 -C obj_dir -f VRiscVSystem.mk VRiscVSystem
	cp obj_dir/VRiscVSystem .

clean-verilator:
	rm -rf obj_dir VRiscVSystem

clean:
	rm -rf $(VERILOG_SRCS) obj_dir *.f
