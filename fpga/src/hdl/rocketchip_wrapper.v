//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 11/04/2019 05:46:57 PM
// Design Name: 
// Module Name: rocketchip_wrapper
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module rocketchip_wrapper(
    clk,
    reset,
    interrupts,

    // debug
    debug_0_pc,

    // MEM
    M_AXI_awready,
    M_AXI_awvalid,
    M_AXI_awid,
    M_AXI_awaddr,
    M_AXI_awlen,
    M_AXI_awsize,
    M_AXI_awburst,
    M_AXI_awlock,
    M_AXI_awcache,
    M_AXI_awprot,
    M_AXI_awqos,

    M_AXI_wready,
    M_AXI_wvalid,
    M_AXI_wdata,
    M_AXI_wstrb,
    M_AXI_wlast,

    M_AXI_bready,
    M_AXI_bvalid,
    M_AXI_bid,
    M_AXI_bresp,

    M_AXI_arready,
    M_AXI_arvalid,
    M_AXI_arid,
    M_AXI_araddr,
    M_AXI_arlen,
    M_AXI_arsize,
    M_AXI_arburst,
    M_AXI_arlock,
    M_AXI_arcache,
    M_AXI_arprot,
    M_AXI_arqos,

    M_AXI_rready,
    M_AXI_rvalid,
    M_AXI_rid,
    M_AXI_rdata,
    M_AXI_rresp,
    M_AXI_rlast,

    // MMIO
    M_AXI_MMIO_awready,
    M_AXI_MMIO_awvalid,
    M_AXI_MMIO_awid,
    M_AXI_MMIO_awaddr,
    M_AXI_MMIO_awlen,
    M_AXI_MMIO_awsize,
    M_AXI_MMIO_awburst,
    M_AXI_MMIO_awlock,
    M_AXI_MMIO_awcache,
    M_AXI_MMIO_awprot,
    M_AXI_MMIO_awqos,

    M_AXI_MMIO_wready,
    M_AXI_MMIO_wvalid,
    M_AXI_MMIO_wdata,
    M_AXI_MMIO_wstrb,
    M_AXI_MMIO_wlast,

    M_AXI_MMIO_bready,
    M_AXI_MMIO_bvalid,
    M_AXI_MMIO_bid,
    M_AXI_MMIO_bresp,

    M_AXI_MMIO_arready,
    M_AXI_MMIO_arvalid,
    M_AXI_MMIO_arid,
    M_AXI_MMIO_araddr,
    M_AXI_MMIO_arlen,
    M_AXI_MMIO_arsize,
    M_AXI_MMIO_arburst,
    M_AXI_MMIO_arlock,
    M_AXI_MMIO_arcache,
    M_AXI_MMIO_arprot,
    M_AXI_MMIO_arqos,

    M_AXI_MMIO_rready,
    M_AXI_MMIO_rvalid,
    M_AXI_MMIO_rid,
    M_AXI_MMIO_rdata,
    M_AXI_MMIO_rresp,
    M_AXI_MMIO_rlast,

    // slave port for dma
    S_AXI_awready,
    S_AXI_awvalid,
    S_AXI_awid,
    S_AXI_awaddr,
    S_AXI_awlen,
    S_AXI_awsize,
    S_AXI_awburst,
    S_AXI_awlock,
    S_AXI_awcache,
    S_AXI_awprot,
    S_AXI_awqos,

    S_AXI_wready,
    S_AXI_wvalid,
    S_AXI_wdata,
    S_AXI_wstrb,
    S_AXI_wlast,

    S_AXI_bready,
    S_AXI_bvalid,
    S_AXI_bid,
    S_AXI_bresp,

    S_AXI_arready,
    S_AXI_arvalid,
    S_AXI_arid,
    S_AXI_araddr,
    S_AXI_arlen,
    S_AXI_arsize,
    S_AXI_arburst,
    S_AXI_arlock,
    S_AXI_arcache,
    S_AXI_arprot,
    S_AXI_arqos,

    S_AXI_rready,
    S_AXI_rvalid,
    S_AXI_rid,
    S_AXI_rdata,
    S_AXI_rresp,
    S_AXI_rlast,

    // JTAG
    jtag_TCK,
    jtag_TMS,
    jtag_TDI,
    jtag_TDO
    );

    input clk;
    input reset;
    input [5:0] interrupts;

    // debug
    output [63:0] debug_0_pc;

    // MEM
    input M_AXI_awready;
    output M_AXI_awvalid;
    output [3:0] M_AXI_awid;
    output [63:0] M_AXI_awaddr;
    output [7:0] M_AXI_awlen;
    output [2:0] M_AXI_awsize;
    output [1:0] M_AXI_awburst;
    output M_AXI_awlock;
    output [3:0] M_AXI_awcache;
    output [2:0] M_AXI_awprot;
    output [3:0] M_AXI_awqos;

    input M_AXI_wready;
    output M_AXI_wvalid;
    output [127:0] M_AXI_wdata;
    output [15:0] M_AXI_wstrb;
    output M_AXI_wlast;

    output M_AXI_bready;
    input M_AXI_bvalid;
    input [3:0] M_AXI_bid;
    input [1:0] M_AXI_bresp;

    input M_AXI_arready;
    output M_AXI_arvalid;
    output [3:0] M_AXI_arid;
    output [63:0] M_AXI_araddr;
    output [7:0] M_AXI_arlen;
    output [2:0] M_AXI_arsize;
    output [1:0] M_AXI_arburst;
    output M_AXI_arlock;
    output [3:0] M_AXI_arcache;
    output [2:0] M_AXI_arprot;
    output [3:0] M_AXI_arqos;

    output M_AXI_rready;
    input M_AXI_rvalid;
    input [3:0] M_AXI_rid;
    input [127:0] M_AXI_rdata;
    input [1:0] M_AXI_rresp;
    input M_AXI_rlast;

    // MMIO
    input M_AXI_MMIO_awready;
    output M_AXI_MMIO_awvalid;
    output [3:0] M_AXI_MMIO_awid;
    output [63:0] M_AXI_MMIO_awaddr;
    output [7:0] M_AXI_MMIO_awlen;
    output [2:0] M_AXI_MMIO_awsize;
    output [1:0] M_AXI_MMIO_awburst;
    output M_AXI_MMIO_awlock;
    output [3:0] M_AXI_MMIO_awcache;
    output [2:0] M_AXI_MMIO_awprot;
    output [3:0] M_AXI_MMIO_awqos;

    input M_AXI_MMIO_wready;
    output M_AXI_MMIO_wvalid;
    output [63:0] M_AXI_MMIO_wdata;
    output [7:0] M_AXI_MMIO_wstrb;
    output M_AXI_MMIO_wlast;

    output M_AXI_MMIO_bready;
    input M_AXI_MMIO_bvalid;
    input [3:0] M_AXI_MMIO_bid;
    input [1:0] M_AXI_MMIO_bresp;

    input M_AXI_MMIO_arready;
    output M_AXI_MMIO_arvalid;
    output [3:0] M_AXI_MMIO_arid;
    output [63:0] M_AXI_MMIO_araddr;
    output [7:0] M_AXI_MMIO_arlen;
    output [2:0] M_AXI_MMIO_arsize;
    output [1:0] M_AXI_MMIO_arburst;
    output M_AXI_MMIO_arlock;
    output [3:0] M_AXI_MMIO_arcache;
    output [2:0] M_AXI_MMIO_arprot;
    output [3:0] M_AXI_MMIO_arqos;

    output M_AXI_MMIO_rready;
    input M_AXI_MMIO_rvalid;
    input [3:0] M_AXI_MMIO_rid;
    input [63:0] M_AXI_MMIO_rdata;
    input [1:0] M_AXI_MMIO_rresp;
    input M_AXI_MMIO_rlast;

    // slave
    output S_AXI_awready;
    input S_AXI_awvalid;
    input [7:0] S_AXI_awid;
    input [63:0] S_AXI_awaddr;
    input [7:0] S_AXI_awlen;
    input [2:0] S_AXI_awsize;
    input [1:0] S_AXI_awburst;
    input S_AXI_awlock;
    input [3:0] S_AXI_awcache;
    input [2:0] S_AXI_awprot;
    input [3:0] S_AXI_awqos;

    output S_AXI_wready;
    input S_AXI_wvalid;
    input [63:0] S_AXI_wdata;
    input [7:0] S_AXI_wstrb;
    input S_AXI_wlast;

    input S_AXI_bready;
    output S_AXI_bvalid;
    output [7:0] S_AXI_bid;
    output [1:0] S_AXI_bresp;

    output S_AXI_arready;
    input S_AXI_arvalid;
    input [7:0] S_AXI_arid;
    input [63:0] S_AXI_araddr;
    input [7:0] S_AXI_arlen;
    input [2:0] S_AXI_arsize;
    input [1:0] S_AXI_arburst;
    input S_AXI_arlock;
    input [3:0] S_AXI_arcache;
    input [2:0] S_AXI_arprot;
    input [3:0] S_AXI_arqos;

    input S_AXI_rready;
    output S_AXI_rvalid;
    output [7:0] S_AXI_rid;
    output [63:0] S_AXI_rdata;
    output [1:0] S_AXI_rresp;
    output S_AXI_rlast;

    assign M_AXI_araddr[63:32] = 0;
    assign M_AXI_awaddr[63:32] = 0;
    assign M_AXI_MMIO_araddr[63:32] = 0;
    assign M_AXI_MMIO_awaddr[63:32] = 0;

    // jtag
    input jtag_TCK;
    input jtag_TMS;
    input jtag_TDI;
    output jtag_TDO;

    RiscVSystem top (
        .clock(clk),
        .reset(reset),

        .debug_0_pc(debug_0_pc),

        .mem_axi4_ARVALID (M_AXI_arvalid),
        .mem_axi4_ARREADY (M_AXI_arready),
        .mem_axi4_ARADDR (M_AXI_araddr[31:0]),
        .mem_axi4_ARID (M_AXI_arid),
        .mem_axi4_ARSIZE (M_AXI_arsize),
        .mem_axi4_ARLEN (M_AXI_arlen),
        .mem_axi4_ARBURST (M_AXI_arburst),
        .mem_axi4_ARCACHE (M_AXI_arcache),
        .mem_axi4_ARLOCK (M_AXI_arlock),
        .mem_axi4_ARPROT (M_AXI_arprot),
        .mem_axi4_ARQOS (M_AXI_arqos),
        .mem_axi4_AWVALID (M_AXI_awvalid),
        .mem_axi4_AWREADY (M_AXI_awready),
        .mem_axi4_AWADDR (M_AXI_awaddr[31:0]),
        .mem_axi4_AWID (M_AXI_awid),
        .mem_axi4_AWSIZE (M_AXI_awsize),
        .mem_axi4_AWLEN (M_AXI_awlen),
        .mem_axi4_AWBURST (M_AXI_awburst),
        .mem_axi4_AWCACHE (M_AXI_awcache),
        .mem_axi4_AWLOCK (M_AXI_awlock),
        .mem_axi4_AWPROT (M_AXI_awprot),
        .mem_axi4_AWQOS (M_AXI_awqos),
        .mem_axi4_WVALID (M_AXI_wvalid),
        .mem_axi4_WREADY (M_AXI_wready),
        .mem_axi4_WSTRB (M_AXI_wstrb),
        .mem_axi4_WDATA (M_AXI_wdata),
        .mem_axi4_WLAST (M_AXI_wlast),
        .mem_axi4_BVALID (M_AXI_bvalid),
        .mem_axi4_BREADY (M_AXI_bready),
        .mem_axi4_BRESP (M_AXI_bresp),
        .mem_axi4_BID (M_AXI_bid),
        .mem_axi4_RVALID (M_AXI_rvalid),
        .mem_axi4_RREADY (M_AXI_rready),
        .mem_axi4_RRESP (M_AXI_rresp),
        .mem_axi4_RID (M_AXI_rid),
        .mem_axi4_RDATA (M_AXI_rdata),
        .mem_axi4_RLAST (M_AXI_rlast),

        .mmio_axi4_ARVALID (M_AXI_MMIO_arvalid),
        .mmio_axi4_ARREADY (M_AXI_MMIO_arready),
        .mmio_axi4_ARADDR (M_AXI_MMIO_araddr[31:0]),
        .mmio_axi4_ARID (M_AXI_MMIO_arid),
        .mmio_axi4_ARSIZE (M_AXI_MMIO_arsize),
        .mmio_axi4_ARLEN (M_AXI_MMIO_arlen),
        .mmio_axi4_ARBURST (M_AXI_MMIO_arburst),
        .mmio_axi4_ARCACHE (M_AXI_MMIO_arcache),
        .mmio_axi4_ARLOCK (M_AXI_MMIO_arlock),
        .mmio_axi4_ARPROT (M_AXI_MMIO_arprot),
        .mmio_axi4_ARQOS (M_AXI_MMIO_arqos),
        .mmio_axi4_AWVALID (M_AXI_MMIO_awvalid),
        .mmio_axi4_AWREADY (M_AXI_MMIO_awready),
        .mmio_axi4_AWADDR (M_AXI_MMIO_awaddr[31:0]),
        .mmio_axi4_AWID (M_AXI_MMIO_awid),
        .mmio_axi4_AWSIZE (M_AXI_MMIO_awsize),
        .mmio_axi4_AWLEN (M_AXI_MMIO_awlen),
        .mmio_axi4_AWBURST (M_AXI_MMIO_awburst),
        .mmio_axi4_AWCACHE (M_AXI_MMIO_awcache),
        .mmio_axi4_AWLOCK (M_AXI_MMIO_awlock),
        .mmio_axi4_AWPROT (M_AXI_MMIO_awprot),
        .mmio_axi4_AWQOS (M_AXI_MMIO_awqos),
        .mmio_axi4_WVALID (M_AXI_MMIO_wvalid),
        .mmio_axi4_WREADY (M_AXI_MMIO_wready),
        .mmio_axi4_WSTRB (M_AXI_MMIO_wstrb),
        .mmio_axi4_WDATA (M_AXI_MMIO_wdata),
        .mmio_axi4_WLAST (M_AXI_MMIO_wlast),
        .mmio_axi4_BVALID (M_AXI_MMIO_bvalid),
        .mmio_axi4_BREADY (M_AXI_MMIO_bready),
        .mmio_axi4_BRESP (M_AXI_MMIO_bresp),
        .mmio_axi4_BID (M_AXI_MMIO_bid),
        .mmio_axi4_RVALID (M_AXI_MMIO_rvalid),
        .mmio_axi4_RREADY (M_AXI_MMIO_rready),
        .mmio_axi4_RRESP (M_AXI_MMIO_rresp),
        .mmio_axi4_RID (M_AXI_MMIO_rid),
        .mmio_axi4_RDATA (M_AXI_MMIO_rdata),
        .mmio_axi4_RLAST (M_AXI_MMIO_rlast),

        .slave_axi4_ARVALID (S_AXI_arvalid),
        .slave_axi4_ARREADY (S_AXI_arready),
        .slave_axi4_ARADDR (S_AXI_araddr),
        .slave_axi4_ARID (S_AXI_arid),
        .slave_axi4_ARSIZE (S_AXI_arsize),
        .slave_axi4_ARLEN (S_AXI_arlen),
        .slave_axi4_ARBURST (S_AXI_arburst),
        .slave_axi4_ARCACHE (S_AXI_arcache),
        .slave_axi4_ARLOCK (S_AXI_arlock),
        .slave_axi4_ARPROT (S_AXI_arprot),
        .slave_axi4_ARQOS (S_AXI_arqos),
        .slave_axi4_AWVALID (S_AXI_awvalid),
        .slave_axi4_AWREADY (S_AXI_awready),
        .slave_axi4_AWADDR (S_AXI_awaddr),
        .slave_axi4_AWID (S_AXI_awid),
        .slave_axi4_AWSIZE (S_AXI_awsize),
        .slave_axi4_AWLEN (S_AXI_awlen),
        .slave_axi4_AWBURST (S_AXI_awburst),
        .slave_axi4_AWCACHE (S_AXI_awcache),
        .slave_axi4_AWLOCK (S_AXI_awlock),
        .slave_axi4_AWPROT (S_AXI_awprot),
        .slave_axi4_AWQOS (S_AXI_awqos),
        .slave_axi4_WVALID (S_AXI_wvalid),
        .slave_axi4_WREADY (S_AXI_wready),
        .slave_axi4_WSTRB (S_AXI_wstrb),
        .slave_axi4_WDATA (S_AXI_wdata),
        .slave_axi4_WLAST (S_AXI_wlast),
        .slave_axi4_BVALID (S_AXI_bvalid),
        .slave_axi4_BREADY (S_AXI_bready),
        .slave_axi4_BRESP (S_AXI_bresp),
        .slave_axi4_BID (S_AXI_bid),
        .slave_axi4_RVALID (S_AXI_rvalid),
        .slave_axi4_RREADY (S_AXI_rready),
        .slave_axi4_RRESP (S_AXI_rresp),
        .slave_axi4_RID (S_AXI_rid),
        .slave_axi4_RDATA (S_AXI_rdata),
        .slave_axi4_RLAST (S_AXI_rlast),

        .interrupts(interrupts),

        .jtag_TCK(jtag_TCK),
        .jtag_TMS(jtag_TMS),
        .jtag_TDI(jtag_TDI),
        .jtag_TDO_data(jtag_TDO)
    );
endmodule
