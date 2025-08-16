package KXCore.superscalar

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import KXCore.superscalar.core._

class core_top(implicit params: CoreParameters) extends RawModule {
  import params.{commonParams, axiParams, backendParams}
  val aclk    = IO(Input(Clock()))
  val aresetn = IO(Input(AsyncReset()))
  val intrpt  = IO(Input(UInt(8.W)))

  val awready = IO(Input(Bool()))
  val awvalid = IO(Output(Bool()))
  val awid    = IO(Output(UInt(axiParams.idBits.W)))
  val awaddr  = IO(Output(UInt(axiParams.addrBits.W)))
  val awlen   = IO(Output(UInt(axiParams.lenBits.W)))
  val awsize  = IO(Output(UInt(axiParams.sizeBits.W)))
  val awburst = IO(Output(UInt(axiParams.burstBits.W)))
  val awlock  = IO(Output(UInt(axiParams.lockBits.W)))
  val awcache = IO(Output(UInt(axiParams.cacheBits.W)))
  val awprot  = IO(Output(UInt(axiParams.protBits.W)))
  val wready  = IO(Input(Bool()))
  val wvalid  = IO(Output(Bool()))
  val wid     = IO(Output(UInt(axiParams.idBits.W)))
  val wdata   = IO(Output(UInt(axiParams.dataBits.W)))
  val wstrb   = IO(Output(UInt(axiParams.wstrBits.W)))
  val wlast   = IO(Output(UInt(axiParams.wlastBits.W)))
  val bready  = IO(Output(Bool()))
  val bvalid  = IO(Input(Bool()))
  val bid     = IO(Input(UInt(axiParams.idBits.W)))
  val bresp   = IO(Input(UInt(axiParams.brespBits.W)))
  val arready = IO(Input(Bool()))
  val arvalid = IO(Output(Bool()))
  val arid    = IO(Output(UInt(axiParams.idBits.W)))
  val araddr  = IO(Output(UInt(axiParams.addrBits.W)))
  val arlen   = IO(Output(UInt(axiParams.lenBits.W)))
  val arsize  = IO(Output(UInt(axiParams.sizeBits.W)))
  val arburst = IO(Output(UInt(axiParams.burstBits.W)))
  val arlock  = IO(Output(UInt(axiParams.lockBits.W)))
  val arcache = IO(Output(UInt(axiParams.cacheBits.W)))
  val arprot  = IO(Output(UInt(axiParams.protBits.W)))
  val rready  = IO(Output(Bool()))
  val rvalid  = IO(Input(Bool()))
  val rid     = IO(Input(UInt(axiParams.idBits.W)))
  val rdata   = IO(Input(UInt(axiParams.dataBits.W)))
  val rresp   = IO(Input(UInt(axiParams.rrespBits.W)))
  val rlast   = IO(Input(UInt(axiParams.rlastBits.W)))

  val break_point = IO(Input(Bool()))
  val infor_flag  = IO(Input(Bool()))
  val reg_num     = IO(Input(UInt(backendParams.lregWidth.W)))
  val ws_valid    = IO(Output(Bool()))
  val rf_rdata    = IO(Output(UInt(commonParams.dataWidth.W)))
  val debug0      = IO(Output(new DebugInfo))
  val debug1      = IO(Output(new DebugInfo))

  val core = withClockAndReset(aclk, !aresetn.asBool) { Module(new Core) }
  core.io.intrpt      := intrpt
  core.io.break_point := break_point
  core.io.infor_flag  := infor_flag
  core.io.reg_num     := reg_num

  core.io.axi.aw.ready    := awready
  awvalid                 := core.io.axi.aw.valid
  awid                    := core.io.axi.aw.bits.id
  awaddr                  := core.io.axi.aw.bits.addr
  awlen                   := core.io.axi.aw.bits.len
  awsize                  := core.io.axi.aw.bits.size
  awburst                 := core.io.axi.aw.bits.burst
  awlock                  := core.io.axi.aw.bits.lock
  awcache                 := core.io.axi.aw.bits.cache
  awprot                  := core.io.axi.aw.bits.prot
  core.io.axi.w.ready     := wready
  wvalid                  := core.io.axi.w.valid
  wid                     := core.io.axi.w.bits.id
  wdata                   := core.io.axi.w.bits.data
  wstrb                   := core.io.axi.w.bits.strb
  wlast                   := core.io.axi.w.bits.last
  bready                  := core.io.axi.b.ready
  core.io.axi.b.valid     := bvalid
  core.io.axi.b.bits.id   := bid
  core.io.axi.b.bits.resp := bresp
  core.io.axi.ar.ready    := arready
  arvalid                 := core.io.axi.ar.valid
  arid                    := core.io.axi.ar.bits.id
  araddr                  := core.io.axi.ar.bits.addr
  arlen                   := core.io.axi.ar.bits.len
  arsize                  := core.io.axi.ar.bits.size
  arburst                 := core.io.axi.ar.bits.burst
  arlock                  := core.io.axi.ar.bits.lock
  arcache                 := core.io.axi.ar.bits.cache
  arprot                  := core.io.axi.ar.bits.prot
  rready                  := core.io.axi.r.ready
  core.io.axi.r.valid     := rvalid
  core.io.axi.r.bits.id   := rid
  core.io.axi.r.bits.data := rdata
  core.io.axi.r.bits.resp := rresp
  core.io.axi.r.bits.last := rlast

  ws_valid := core.io.ws_valid
  rf_rdata := core.io.rf_rdata
  debug0   := core.io.debug0
  debug1   := core.io.debug1
}
