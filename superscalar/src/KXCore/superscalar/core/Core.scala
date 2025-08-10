package KXCore.superscalar.core

import chisel3._
import chisel3.util._
import KXCore.common.peripheral._
import KXCore.common.Privilege._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core.frontend._
import KXCore.superscalar.core.backend._
import KXCore.superscalar.core.backend.CSRAddr.DMW0

class DebugInfo(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, backendParams}
  import commonParams.{vaddrWidth, dataWidth, instWidth}
  import backendParams.{lregWidth, pregWidth}
  val wb_pc       = UInt(vaddrWidth.W)
  val wb_rf_wen   = UInt((dataWidth / 8).W)
  val wb_rf_wnum  = UInt(lregWidth.W)
  val wb_rf_wdata = UInt(dataWidth.W)
  val wb_inst     = UInt(instWidth.W)
}

class CoreIO(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, backendParams, axiParams}
  import commonParams.{vaddrWidth, dataWidth, instWidth}
  import backendParams.{lregWidth, pregWidth}
  val intrpt      = Input(UInt(8.W))
  val axi         = new AXIBundle(axiParams)
  val break_point = Input(Bool())
  val infor_flag  = Input(Bool())
  val reg_num     = Input(UInt(lregWidth.W))
  val ws_valid    = Output(Bool())
  val rf_rdata    = Output(UInt(dataWidth.W))
  val debug0      = Output(new DebugInfo)
}

class Core(implicit params: CoreParameters) extends Module {
  import params.{commonParams, axiParams}
  val io = IO(new CoreIO)

  val tlb      = Module(new TLB)
  val csr      = Module(new CSR)
  val frontend = Module(new FrontEnd)
  val backend  = Module(new BackEnd)

  tlb.io.mode.da   := csr.io.tlb.da
  tlb.io.mode.pg   := csr.io.tlb.pg
  tlb.io.mode.dmw  := csr.io.tlb.dmw
  tlb.io.mode.asid := csr.io.tlb.asid
  tlb.io.mode.plv  := csr.io.priv
  tlb.io.mode.matf := csr.io.tlb.matf
  tlb.io.mode.matd := csr.io.tlb.matd

  tlb.io.transReq0 := frontend.io.itlbReq
  // tlb.io.transReq0.asid := csr.io.tlb.asid
  // tlb.io.transReq0.plv  := csr.io.priv
  tlb.io.transReq1 := backend.io.dtlbReq
  // tlb.io.transReq0.asid := csr.io.tlb.asid
  // tlb.io.transReq0.plv  := csr.io.priv
  tlb.io.cmd_in := DontCare

  csr.io.raddr                := backend.io.csr_access.raddr
  backend.io.csr_access.rdata := csr.io.rdata

  csr.io.we    := backend.io.csr_access.we
  csr.io.waddr := backend.io.csr_access.waddr
  csr.io.wdata := backend.io.csr_access.wdata
  csr.io.wmask := backend.io.csr_access.wmask

  backend.io.csr_access.counterID := csr.io.counterID
  backend.io.csr_access.cntvh     := csr.io.cntvh
  backend.io.csr_access.cntvl     := csr.io.cntvl

  csr.io.pc                    := backend.io.csr_access.pc
  csr.io.ecode                 := backend.io.csr_access.ecode
  csr.io.ecode_sub             := backend.io.csr_access.ecode_sub
  csr.io.badv                  := backend.io.csr_access.badv
  csr.io.excp_en               := backend.io.csr_access.excp_en
  backend.io.csr_access.eentry := csr.io.eentry

  csr.io.eret_en                     := backend.io.csr_access.eret_en
  backend.io.csr_access.era          := csr.io.era
  backend.io.csr_access.intr_pending := csr.io.interrupt.pending
  csr.io.interrupt.externel_sample   := io.intrpt

  AXIInterconnect(axiParams, Seq(backend.io.axi, frontend.io.axi), Seq(io.axi), Seq(Seq(AddressSet(0, -1))), Seq(false))

  frontend.io.icacheReq.valid   := false.B
  frontend.io.icacheReq.bits    := DontCare
  frontend.io.itlbResp          := tlb.io.transResp0
  frontend.io.fetchPacket.ready := backend.io.fetchPacket.ready
  frontend.io.getPC(0).ftqIdx   := backend.io.getPC(0).ftqIdx
  frontend.io.getPC(1).ftqIdx   := backend.io.getPC(1).ftqIdx
  frontend.io.getPC(2).ftqIdx   := backend.io.getPC(2).ftqIdx
  frontend.io.commit.valid      := backend.io.commit.valid
  frontend.io.commit.bits       := backend.io.commit.bits

  backend.io.dtlbResp          := tlb.io.transResp1
  backend.io.fetchPacket.valid := frontend.io.fetchPacket.valid
  backend.io.fetchPacket.bits  := frontend.io.fetchPacket.bits
  backend.io.getPC(0).info     := frontend.io.getPC(0).info
  backend.io.getPC(1).info     := frontend.io.getPC(1).info
  backend.io.getPC(2).info     := frontend.io.getPC(2).info

  io.ws_valid := false.B
  io.rf_rdata := 0.U
  io.debug0   := 0.U.asTypeOf(new DebugInfo)

  if (params.debug) {
    dontTouch(backend.io.debug)
    val difftestInstrCommit = Module(new DifftestInstrCommit)
    val difftestGRegState   = Module(new DifftestGRegState)
    val difftestCSRRegState = Module(new DifftestCSRRegState)
    val difftestTrapEvent   = Module(new DifftestTrapEvent)
    val difftestExcpEvent   = Module(new DifftestExcpEvent)
    val difftestStoreEvent  = Module(new DifftestStoreEvent)
    val difftestLoadEvent   = Module(new DifftestLoadEvent)

    val commit_idx = OHToUInt(VecInit(backend.io.debug.commit_uops.map(_.valid)).asUInt)
    difftestInstrCommit.io.clock          := clock.asBool
    difftestInstrCommit.io.coreid         := 0.U
    difftestInstrCommit.io.index          := 0.U
    difftestInstrCommit.io.valid          := RegNext(backend.io.commit.valid && !backend.io.csr_access.excp_en, 0.U)
    difftestInstrCommit.io.pc             := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.pc, 0.U)
    difftestInstrCommit.io.instr          := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.inst, 0.U)
    difftestInstrCommit.io.skip           := RegNext(false.B, false.B)
    difftestInstrCommit.io.is_TLBFILL     := RegNext(false.B, false.B)
    difftestInstrCommit.io.TLBFILL_index  := RegNext(false.B, false.B)
    difftestInstrCommit.io.is_CNTinst     := RegNext(false.B, false.B)
    difftestInstrCommit.io.timer_64_value := RegNext(csr.io.cntvh ## csr.io.cntvl, 0.U)
    difftestInstrCommit.io.wen            := RegNext(backend.io.debug.commit_uops(commit_idx).bits.ldst =/= 0.U, 0.U)
    difftestInstrCommit.io.wdest          := RegNext(backend.io.debug.commit_uops(commit_idx).bits.ldst, 0.U)
    difftestInstrCommit.io.wdata          := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.wdata, 0.U)
    difftestInstrCommit.io.csr_rstat      := RegNext(false.B, false.B)
    difftestInstrCommit.io.csr_data       := RegNext(0.U, 0.U)

    difftestExcpEvent.io.clock         := clock.asBool
    difftestExcpEvent.io.coreid        := 0.U
    difftestExcpEvent.io.excp_valid    := RegNext(backend.io.csr_access.excp_en, 0.U)
    difftestExcpEvent.io.eret          := RegNext(backend.io.csr_access.eret_en, 0.U)
    difftestExcpEvent.io.intrNo        := csr.io.debug.estat(12, 2)
    difftestExcpEvent.io.cause         := RegNext(ECODE.getEcode(backend.io.csr_access.ecode), 0.U)
    difftestExcpEvent.io.exceptionPC   := RegNext(backend.io.csr_access.pc, 0.U)
    difftestExcpEvent.io.exceptionInst := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.inst, 0.U)

    difftestTrapEvent.io.clock    := clock.asBool
    difftestTrapEvent.io.coreid   := 0.U
    difftestTrapEvent.io.valid    := 0.U
    difftestTrapEvent.io.code     := 0.U
    difftestTrapEvent.io.pc       := 0.U
    difftestTrapEvent.io.cycleCnt := 0.U
    difftestTrapEvent.io.instrCnt := 0.U

    difftestStoreEvent.io.clock      := clock.asBool
    difftestStoreEvent.io.coreid     := 0.U
    difftestStoreEvent.io.index      := 0.U
    difftestStoreEvent.io.valid      := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.store & Fill(8, backend.io.commit.valid), 0.U)
    difftestStoreEvent.io.storePAddr := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.storePaddr, 0.U)
    difftestStoreEvent.io.storeVAddr := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.storeVaddr, 0.U)
    difftestStoreEvent.io.storeData  := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.storeData, 0.U)

    difftestLoadEvent.io.clock  := clock.asBool
    difftestLoadEvent.io.coreid := 0.U
    difftestLoadEvent.io.index  := 0.U
    difftestLoadEvent.io.valid  := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.load & Fill(8, backend.io.commit.valid), 0.U)
    difftestLoadEvent.io.paddr  := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.loadVaddr, 0.U)
    difftestLoadEvent.io.vaddr  := RegNext(backend.io.debug.commit_uops(commit_idx).bits.debug.loadPaddr, 0.U)

    difftestGRegState.io.clock  := clock.asBool
    difftestGRegState.io.coreid := 0.U
    difftestGRegState.io.gpr_0  := backend.io.debug.regs(0)
    difftestGRegState.io.gpr_1  := backend.io.debug.regs(1)
    difftestGRegState.io.gpr_2  := backend.io.debug.regs(2)
    difftestGRegState.io.gpr_3  := backend.io.debug.regs(3)
    difftestGRegState.io.gpr_4  := backend.io.debug.regs(4)
    difftestGRegState.io.gpr_5  := backend.io.debug.regs(5)
    difftestGRegState.io.gpr_6  := backend.io.debug.regs(6)
    difftestGRegState.io.gpr_7  := backend.io.debug.regs(7)
    difftestGRegState.io.gpr_8  := backend.io.debug.regs(8)
    difftestGRegState.io.gpr_9  := backend.io.debug.regs(9)
    difftestGRegState.io.gpr_10 := backend.io.debug.regs(10)
    difftestGRegState.io.gpr_11 := backend.io.debug.regs(11)
    difftestGRegState.io.gpr_12 := backend.io.debug.regs(12)
    difftestGRegState.io.gpr_13 := backend.io.debug.regs(13)
    difftestGRegState.io.gpr_14 := backend.io.debug.regs(14)
    difftestGRegState.io.gpr_15 := backend.io.debug.regs(15)
    difftestGRegState.io.gpr_16 := backend.io.debug.regs(16)
    difftestGRegState.io.gpr_17 := backend.io.debug.regs(17)
    difftestGRegState.io.gpr_18 := backend.io.debug.regs(18)
    difftestGRegState.io.gpr_19 := backend.io.debug.regs(19)
    difftestGRegState.io.gpr_20 := backend.io.debug.regs(20)
    difftestGRegState.io.gpr_21 := backend.io.debug.regs(21)
    difftestGRegState.io.gpr_22 := backend.io.debug.regs(22)
    difftestGRegState.io.gpr_23 := backend.io.debug.regs(23)
    difftestGRegState.io.gpr_24 := backend.io.debug.regs(24)
    difftestGRegState.io.gpr_25 := backend.io.debug.regs(25)
    difftestGRegState.io.gpr_26 := backend.io.debug.regs(26)
    difftestGRegState.io.gpr_27 := backend.io.debug.regs(27)
    difftestGRegState.io.gpr_28 := backend.io.debug.regs(28)
    difftestGRegState.io.gpr_29 := backend.io.debug.regs(29)
    difftestGRegState.io.gpr_30 := backend.io.debug.regs(30)
    difftestGRegState.io.gpr_31 := backend.io.debug.regs(31)

    difftestCSRRegState.io.clock     := clock.asBool
    difftestCSRRegState.io.coreid    := 0.U
    difftestCSRRegState.io.crmd      := csr.io.debug.crmd
    difftestCSRRegState.io.prmd      := csr.io.debug.prmd
    difftestCSRRegState.io.euen      := 0.U
    difftestCSRRegState.io.ecfg      := csr.io.debug.ecfg
    difftestCSRRegState.io.estat     := csr.io.debug.estat
    difftestCSRRegState.io.era       := csr.io.debug.era
    difftestCSRRegState.io.badv      := csr.io.debug.badv
    difftestCSRRegState.io.eentry    := csr.io.debug.eentry
    difftestCSRRegState.io.tlbidx    := tlb.io.cmd_out.tlb_idx
    difftestCSRRegState.io.tlbehi    := tlb.io.cmd_out.tlb_ehi
    difftestCSRRegState.io.tlbelo0   := tlb.io.cmd_out.tlb_elo0
    difftestCSRRegState.io.tlbelo1   := tlb.io.cmd_out.tlb_elo1
    difftestCSRRegState.io.asid      := tlb.io.cmd_out.tlb_asid
    difftestCSRRegState.io.pgdl      := 0.U
    difftestCSRRegState.io.pgdh      := 0.U
    difftestCSRRegState.io.save0     := csr.io.debug.saved0
    difftestCSRRegState.io.save1     := csr.io.debug.saved1
    difftestCSRRegState.io.save2     := csr.io.debug.saved2
    difftestCSRRegState.io.save3     := csr.io.debug.saved3
    difftestCSRRegState.io.tid       := csr.io.debug.tid
    difftestCSRRegState.io.tcfg      := csr.io.debug.tcfg
    difftestCSRRegState.io.tval      := csr.io.debug.tval
    difftestCSRRegState.io.ticlr     := 0.U
    difftestCSRRegState.io.llbctl    := 0.U
    difftestCSRRegState.io.tlbrentry := csr.io.debug.tlbrentry
    difftestCSRRegState.io.dmw0      := csr.io.debug.dmw0
    difftestCSRRegState.io.dmw1      := csr.io.debug.dmw1

    dontTouch(csr.io.debug)
    dontTouch(difftestInstrCommit.io)
    dontTouch(difftestExcpEvent.io)
    dontTouch(difftestTrapEvent.io)
    dontTouch(difftestStoreEvent.io)
    dontTouch(difftestLoadEvent.io)
    dontTouch(difftestGRegState.io)
    dontTouch(difftestCSRRegState.io)
  }
}
