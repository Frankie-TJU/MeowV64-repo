open_hw_manager
catch connect_hw_server
open_hw_target
set_property PROGRAM.FILE {./proj/fpga.runs/impl_1/system_wrapper.bit} [lindex [get_hw_devices] 0]
set_property PROBES.FILE {./proj/fpga.runs/impl_1/system_wrapper.ltx} [lindex [get_hw_devices] 0]
program_hw_device
