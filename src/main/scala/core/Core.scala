package core

import chisel3._
import _root_.data._
import _root_.instr._
import _root_.cache._
import _root_.reg._
import exec.Exec
import paging.PTW

class Core(implicit val coredef: CoreDef = DefaultDef) extends Module {
  val io = IO(new Bundle {
    val axi = new AXI(coredef.XLEN, coredef.PADDR_WIDTH)
    val eint = Input(Bool())

    // Debug
    val pc = Output(UInt(coredef.XLEN.W))
    val minstret = Output(UInt(coredef.XLEN.W))
    val mcycle = Output(UInt(coredef.XLEN.W))
  })

  assert(coredef.FETCH_NUM % 2 == 0, "issue num can only be multiples of two, because we need to support compressed instructions")

  val ctrl = Module(new Ctrl)
  
  // Caches
  val l2 = Module(new L2Cache(coredef.L2))
  val l1i = Module(new L1IC(coredef.L1I))
  val l1d = Module(new L1DC(coredef.L1D))

  l2.axi <> io.axi
  l2.ic(0) <> l1i.toL2
  l2.dc(0) <> l1d.toL2

  // TODO: attach DTLB
  val ptw = Module(new PTW)
  l1d.ptw <> ptw.dc

  val fetch = Module(new InstrFetch)
  val bpu = Module(new BPU)
  val exec = Module(new Exec)
  val reg = Module(new RegFile(coredef.XLEN, 32, coredef.ISSUE_NUM * 2, coredef.RETIRE_NUM))

  val (csrWriter, csr) = CSR.gen(coredef.XLEN, coredef.HART_ID)

  fetch.toIC <> l1i.toCPU
  fetch.toCtrl <> ctrl.toIF
  fetch.toCore.ptw <> ptw.itlb

  bpu.toExec <> exec.toBPU
  bpu.toFetch <> fetch.toBPU

  exec.toIF <> fetch.toExec
  exec.rr <> reg.io.reads
  exec.rw <> reg.io.writes
  exec.csrWriter <> csrWriter

  exec.toDC.r <> l1d.mr
  exec.toDC.w <> l1d.w
  exec.toDC.fs <> l1d.fs
  exec.toDC.u <> l2.directs(0)
  
  exec.toCtrl.ctrl <> ctrl.toExec.ctrl
  exec.toCtrl.tlbrst := ctrl.toExec.tlbrst

  exec.toCore.ptw <> ptw.dtlb

  ctrl.br.req <> exec.toCtrl.branch
  ctrl.br.tval <> exec.toCtrl.tval

  ctrl.toExec.retCnt := exec.toCtrl.retCnt
  ctrl.toExec.nepc := exec.toCtrl.nepc
  ctrl.toExec.int <> exec.toCtrl.int
  ctrl.toExec.intAck := exec.toCtrl.intAck
  ctrl.toExec.priv <> exec.toCtrl.priv
  ctrl.toExec.status <> exec.toCtrl.status

  ctrl.eint := io.eint

  io.pc := fetch.debug.pc

  // CSR
  CSRHelper.defaults(csr)
  csr.const("mhartid") := coredef.HART_ID.U
  csr.attach("mcycle").connect(ctrl.csr.mcycle)
  csr.attach("minstret").connect(ctrl.csr.minstret)
  csr.attach("mstatus").connect(ctrl.csr.mstatus)
  csr.attach("mtvec").connect(ctrl.csr.mtvec)
  csr.attach("mcause").connect(ctrl.csr.mcause)
  csr.attach("mtval").connect(ctrl.csr.mtval)
  csr.attach("mepc").connect(ctrl.csr.mepc)
  csr.attach("mie").connect(ctrl.csr.mie)
  csr.attach("mip").connect(ctrl.csr.mip)
  csr.attach("mcountinhibit").connect(ctrl.csr.mcountinhibit)
  csr.attach("mideleg").connect(ctrl.csr.mideleg)
  csr.attach("medeleg").connect(ctrl.csr.medeleg)

  csr.attach("sstatus").connect(ctrl.csr.sstatus)
  csr.attach("stvec").connect(ctrl.csr.stvec)
  csr.attach("sie").connect(ctrl.csr.sie)
  csr.attach("sip").connect(ctrl.csr.sip)
  csr.attach("scause").connect(ctrl.csr.scause)
  csr.attach("sepc").connect(ctrl.csr.sepc)
  csr.attach("stval").connect(ctrl.csr.stval)

  val mscratch = RegInit(0.U(coredef.XLEN.W))
  csr.attach("mscratch").connect(CSRPort.fromReg(coredef.XLEN, mscratch))
  val sscratch = RegInit(0.U(coredef.XLEN.W))
  csr.attach("sscratch").connect(CSRPort.fromReg(coredef.XLEN, sscratch))

  val satp = RegInit(Satp.empty)
  csr.attach("satp").connect(satp.port)

  ptw.satp := satp
  fetch.toCore.satp := satp
  exec.toCore.satp := satp

  io.mcycle := ctrl.csr.mcycle.rdata
  io.minstret := ctrl.csr.minstret.rdata
}
