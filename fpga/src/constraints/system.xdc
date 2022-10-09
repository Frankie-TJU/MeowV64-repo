set_property -dict {PACKAGE_PIN BM29 IOSTANDARD LVCMOS12} [get_ports reset]

#
# easy to confuse rxd/txd here
# uart0_txd
set_property -dict {PACKAGE_PIN BP26 IOSTANDARD LVCMOS18} [get_ports jtag_TCK]
# uart0_rxd
set_property -dict {PACKAGE_PIN BN26 IOSTANDARD LVCMOS18} [get_ports jtag_TDI]
# uart0_rts
set_property -dict {PACKAGE_PIN BP22 IOSTANDARD LVCMOS18} [get_ports jtag_TDO]
# uart0_cts
set_property -dict {PACKAGE_PIN BP23 IOSTANDARD LVCMOS18} [get_ports jtag_TMS]

# assume 10MHz jtag
# ref https://github.com/pulp-platform/pulp/blob/master/fpga/pulp-vcu118/constraints/vcu118.xdc
# intel jtag timing: https://www.intel.com/content/www/us/en/docs/programmable/683301/current/jtag-configuration-timing.html
# ft4232 timing: https://ftdichip.com/wp-content/uploads/2020/08/DS_FT4232H.pdf
create_clock -period 100.000 -name jtag_TCK [get_ports jtag_TCK]
set_input_jitter jtag_TCK 1.000
set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets jtag_TCK_IBUF_inst/O]

