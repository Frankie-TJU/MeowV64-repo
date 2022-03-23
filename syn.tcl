# usage: dc_shell -f syn.tcl

# load library if config exists
set rc [file exist ~/library.tcl]
if {$rc == 1} {
	source ~/library.tcl
}

# args
set input_verilog [list RiscVSystem.v]
set input_vhdl [list ]
#set toplevel_name RiscVSystem
set toplevel_name Core

# load design
read_file -format verilog $input_verilog
#read_file -format vhdl $input_vhdl
# check module exists
set rc [llength [get_designs -exact $toplevel_name]]
if {$rc == 0} {
	quit
}
current_design $toplevel_name

# setup
set_host_options -max_cores 16

# timing
# 300MHz clock
create_clock clock -period 3.3333
create_clock clk -period 3.3333
# dff clock to output: 0.14ns
# assume all input comes from output of dff
set_input_delay 0.14 -clock clock [all_inputs]
set_input_delay 0.14 -clock clk [all_inputs]
# dff setup time: 0.02ns
# assume all output goes to dff
set_output_delay 0.02 -clock clock [all_outputs]
set_output_delay 0.02 -clock clk [all_outputs]

# synthesis flow
link
uniquify
set_optimize_registers -check_design -verbose \
        -designs [get_designs { FMA FloatDivSqrt }]
set_ungroup [get_designs { INToRecFN* \
	MulAddRecFNToRaw_postMul* \
	MulAddRecFNToRaw_preMul* \
	RoundAnyRawFNToRecFN* \
	RoundRawFNToRecFN* \
	DivSqrtRawFN_small* \
	DivSqrtRecFN_small* \
	DivSqrtRecFNToRaw_small* \
	RecFNToIN* \
	RecFNToRecFN* \
	CompareRecFN* }]
compile_ultra -no_autoungroup -retime

# export
write -format ddc -hierarchy -output [format "%s%s" $toplevel_name ".ddc"]
write_sdf -version 1.0 [format "%s%s" $toplevel_name ".sdf"]
write -format verilog -hierarchy -output [format "%s%s" $toplevel_name ".syn.v"]
write_sdc [format "%s%s" $toplevel_name ".sdc"]

# reports
check_timing > ${toplevel_name}_check_timing.txt
check_design > ${toplevel_name}_check_design.txt
report_design > ${toplevel_name}_report_design.txt
report_area -hierarchy > ${toplevel_name}_report_area.txt
report_power -hierarchy > ${toplevel_name}_report_power.txt
report_cell > ${toplevel_name}_report_cell.txt
report_timing -delay_type max -max_paths 10 > ${toplevel_name}_report_timing_setup.txt
report_timing -delay_type min -max_paths 10 > ${toplevel_name}_report_timing_hold.txt
report_constraint -all_violators > ${toplevel_name}_report_constraint.txt
report_qor > ${toplevel_name}_report_qor.txt

# quit
quit
