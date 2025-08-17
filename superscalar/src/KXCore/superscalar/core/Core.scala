package KXCore.superscalar.core

import chisel3._
import chisel3.util._
import KXCore.common.peripheral._
import KXCore.common.Privilege._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core.frontend._
import KXCore.superscalar.core.backend._

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
  val debug1      = Output(new DebugInfo)
}

class Core(implicit params: CoreParameters) extends Module {
  import params.{commonParams, axiParams}
  val io = IO(new CoreIO)

  val tlb      = Module(new TLB)
  val csr      = Module(new CSRUnit)
  val frontend = Module(new FrontEnd)
  val backend  = Module(new BackEnd)

  tlb.io.mode.crmd    := csr.io.crmd
  tlb.io.mode.estat   := csr.io.estat
  tlb.io.mode.asid    := csr.io.tlb.asid
  tlb.io.mode.dmw     := csr.io.tlb.dmw
  tlb.io.mode.tlbidx  := csr.io.tlb.tlbidx
  tlb.io.mode.tlbehi  := csr.io.tlb.tlbehi
  tlb.io.mode.tlbelo0 := csr.io.tlb.tlbelo0
  tlb.io.mode.tlbelo1 := csr.io.tlb.tlbelo1

  tlb.io.tlbCmd := backend.io.tlbCmd

  tlb.io.transReq0 := frontend.io.itlbReq
  tlb.io.transReq1 := backend.io.dtlbReq

  csr.io.raddr                := backend.io.csr_access.raddr
  backend.io.csr_access.rdata := csr.io.rdata

  csr.io.we    := backend.io.csr_access.we
  csr.io.waddr := backend.io.csr_access.waddr
  csr.io.wdata := backend.io.csr_access.wdata
  csr.io.wmask := backend.io.csr_access.wmask

  csr.io.tlbUpdate := tlb.io.tlbUpdate

  backend.io.csr_access.counterID := csr.io.counterID
  backend.io.csr_access.cntvh     := csr.io.cntvh
  backend.io.csr_access.cntvl     := csr.io.cntvl

  csr.io.epc                   := backend.io.csr_access.epc
  csr.io.ecode                 := backend.io.csr_access.ecode
  csr.io.ecode_sub             := backend.io.csr_access.ecode_sub
  csr.io.badv                  := backend.io.csr_access.badv
  csr.io.excp_en               := backend.io.csr_access.excp_en
  backend.io.csr_access.eentry := csr.io.eentry

  csr.io.eret_en                     := backend.io.csr_access.eret_en
  backend.io.csr_access.era          := csr.io.era
  backend.io.csr_access.intr_pending := csr.io.interrupt.pending
  csr.io.interrupt.externel_sample   := io.intrpt

  val axiAribter = Module(new AXIArbiter(axiParams, Seq(params.backendParams.dcacheParams.id, params.frontendParams.icacheParams.id)))
  axiAribter.io.in(0) <> backend.io.axi
  axiAribter.io.in(1) <> frontend.io.axi
  axiAribter.io.out   <> io.axi

  frontend.io.icacheClear       := false.B
  frontend.io.icacheReq.valid   := backend.io.icacheReq.valid
  frontend.io.icacheReq.bits    := backend.io.icacheReq.bits
  frontend.io.itlbResp          := tlb.io.transResp0
  frontend.io.fetchPacket.ready := backend.io.fetchPacket.ready
  frontend.io.ftqReqs(0)        := backend.io.ftqReqs(0)
  frontend.io.ftqReqs(1)        := backend.io.ftqReqs(1)
  frontend.io.ftqReqs(2)        := backend.io.ftqReqs(2)
  frontend.io.commit            := backend.io.commit
  frontend.io.redirect          := backend.io.redirect

  backend.io.dtlbResp          := tlb.io.transResp1
  backend.io.fetchPacket.valid := frontend.io.fetchPacket.valid
  backend.io.fetchPacket.bits  := frontend.io.fetchPacket.bits
  backend.io.ftqResps(0)       := frontend.io.ftqResps(0)
  backend.io.ftqResps(1)       := frontend.io.ftqResps(1)
  backend.io.ftqResps(2)       := frontend.io.ftqResps(2)
  backend.io.icacheReq.ready   := frontend.io.icacheReq.ready

  io.ws_valid := false.B
  io.rf_rdata := 0.U
  io.debug0   := 0.U.asTypeOf(new DebugInfo)
  io.debug1   := 0.U.asTypeOf(new DebugInfo)

  if (params.debug) {
    import KXCore.common.Instruction._
    dontTouch(backend.io.debug)
    dontTouch(csr.io.debug)

    val commit_uops = WireInit(backend.io.debug.rob_commit.uop)

    io.debug0.wb_pc       := commit_uops(0).debug.pc
    io.debug0.wb_inst     := commit_uops(0).debug.inst
    io.debug0.wb_rf_wen   := commit_uops(0).debug.wen
    io.debug0.wb_rf_wnum  := commit_uops(0).debug.wdest
    io.debug0.wb_rf_wdata := commit_uops(0).debug.wdata

    io.debug1.wb_pc       := commit_uops(1).debug.pc
    io.debug1.wb_inst     := commit_uops(1).debug.inst
    io.debug1.wb_rf_wen   := commit_uops(1).debug.wen
    io.debug1.wb_rf_wnum  := commit_uops(1).debug.wdest
    io.debug1.wb_rf_wdata := commit_uops(1).debug.wdata

    if (params.difftest) {
      val difftestInstrCommit0 = Module(new DifftestInstrCommit)
      val difftestInstrCommit1 = Module(new DifftestInstrCommit)
      val difftestGRegState    = Module(new DifftestGRegState)
      val difftestCSRRegState  = Module(new DifftestCSRRegState)
      val difftestTrapEvent    = Module(new DifftestTrapEvent)
      val difftestExcpEvent    = Module(new DifftestExcpEvent)
      val difftestStoreEvent0  = Module(new DifftestStoreEvent)
      val difftestStoreEvent1  = Module(new DifftestStoreEvent)
      val difftestLoadEvent0   = Module(new DifftestLoadEvent)
      val difftestLoadEvent1   = Module(new DifftestLoadEvent)

      val commit_val0 = WireInit(backend.io.debug.rob_commit.valids(0))
      val commit_uop0 = WireInit(commit_uops(0))
      val commit_val1 = WireInit(backend.io.debug.rob_commit.valids(1))
      val commit_uop1 = WireInit(commit_uops(1))
      when(!backend.io.debug.rob_commit.valids(0) && backend.io.debug.rob_commit.valids(1)) {
        commit_val0 := backend.io.debug.rob_commit.valids(1)
        commit_uop0 := commit_uops(1)

        commit_val1 := backend.io.debug.rob_commit.valids(0)
        commit_uop1 := commit_uops(0)
      }

      difftestInstrCommit0.io.clock          := clock.asBool
      difftestInstrCommit0.io.coreid         := 0.U
      difftestInstrCommit0.io.index          := 0.U
      difftestInstrCommit0.io.valid          := RegNext(commit_val0, 0.U)
      difftestInstrCommit0.io.pc             := RegNext(commit_uop0.debug.pc, 0.U)
      difftestInstrCommit0.io.instr          := RegNext(commit_uop0.debug.inst, 0.U)
      difftestInstrCommit0.io.skip           := RegNext(false.B, false.B)
      difftestInstrCommit0.io.is_TLBFILL     := RegNext(commit_uop0.debug.is_TLBFILL, false.B)
      difftestInstrCommit0.io.TLBFILL_index  := RegNext(commit_uop0.debug.TLBFILL_index, false.B)
      difftestInstrCommit0.io.is_CNTinst     := RegNext(commit_uop0.debug.is_CNTinst, false.B)
      difftestInstrCommit0.io.timer_64_value := RegNext(commit_uop0.debug.timer_64_value, 0.U)
      difftestInstrCommit0.io.wen            := RegNext(commit_uop0.debug.wen, 0.U)
      difftestInstrCommit0.io.wdest          := RegNext(commit_uop0.debug.wdest, 0.U)
      difftestInstrCommit0.io.wdata          := RegNext(commit_uop0.debug.wdata, 0.U)
      difftestInstrCommit0.io.csr_rstat      := RegNext(commit_uop0.debug.csr_rstat, false.B)
      difftestInstrCommit0.io.csr_data       := RegNext(commit_uop0.debug.csr_data, 0.U)

      difftestInstrCommit1.io.clock          := clock.asBool
      difftestInstrCommit1.io.coreid         := 0.U
      difftestInstrCommit1.io.index          := 1.U
      difftestInstrCommit1.io.valid          := RegNext(commit_val1, 0.U)
      difftestInstrCommit1.io.pc             := RegNext(commit_uop1.debug.pc, 0.U)
      difftestInstrCommit1.io.instr          := RegNext(commit_uop1.debug.inst, 0.U)
      difftestInstrCommit1.io.skip           := RegNext(false.B, false.B)
      difftestInstrCommit1.io.is_TLBFILL     := RegNext(commit_uop1.debug.is_TLBFILL, false.B)
      difftestInstrCommit1.io.TLBFILL_index  := RegNext(commit_uop1.debug.TLBFILL_index, false.B)
      difftestInstrCommit1.io.is_CNTinst     := RegNext(commit_uop1.debug.is_CNTinst, false.B)
      difftestInstrCommit1.io.timer_64_value := RegNext(commit_uop1.debug.timer_64_value, 0.U)
      difftestInstrCommit1.io.wen            := RegNext(commit_uop1.debug.wen, 0.U)
      difftestInstrCommit1.io.wdest          := RegNext(commit_uop1.debug.wdest, 0.U)
      difftestInstrCommit1.io.wdata          := RegNext(commit_uop1.debug.wdata, 0.U)
      difftestInstrCommit1.io.csr_rstat      := RegNext(commit_uop1.debug.csr_rstat, false.B)
      difftestInstrCommit1.io.csr_data       := RegNext(commit_uop1.debug.csr_data, 0.U)

      val exception = backend.io.debug.rob_exception
      difftestExcpEvent.io.clock         := clock.asBool
      difftestExcpEvent.io.coreid        := 0.U
      difftestExcpEvent.io.excp_valid    := RegNext(exception.valid, 0.U)
      difftestExcpEvent.io.eret          := RegNext(csr.io.eret_en, 0.U)
      difftestExcpEvent.io.intrNo        := csr.io.debug.estat(12, 2)
      difftestExcpEvent.io.cause         := RegNext(exception.ecode, 0.U)
      difftestExcpEvent.io.exceptionPC   := RegNext(exception.debug.exceptionPC, 0.U)
      difftestExcpEvent.io.exceptionInst := RegNext(exception.debug.exceptionInst, 0.U)

      difftestTrapEvent.io.clock    := clock.asBool
      difftestTrapEvent.io.coreid   := 0.U
      difftestTrapEvent.io.valid    := 0.U
      difftestTrapEvent.io.code     := 0.U
      difftestTrapEvent.io.pc       := 0.U
      difftestTrapEvent.io.cycleCnt := 0.U
      difftestTrapEvent.io.instrCnt := 0.U

      difftestStoreEvent0.io.clock      := clock.asBool
      difftestStoreEvent0.io.coreid     := 0.U
      difftestStoreEvent0.io.index      := 0.U
      difftestStoreEvent0.io.valid      := RegNext(commit_uop0.debug.store & Fill(8, commit_val0), 0.U)
      difftestStoreEvent0.io.storePAddr := RegNext(commit_uop0.debug.storePaddr, 0.U)
      difftestStoreEvent0.io.storeVAddr := RegNext(commit_uop0.debug.storeVaddr, 0.U)
      difftestStoreEvent0.io.storeData  := RegNext(commit_uop0.debug.storeData, 0.U)

      difftestStoreEvent1.io.clock      := clock.asBool
      difftestStoreEvent1.io.coreid     := 0.U
      difftestStoreEvent1.io.index      := 1.U
      difftestStoreEvent1.io.valid      := RegNext(commit_uop1.debug.store & Fill(8, commit_val1), 0.U)
      difftestStoreEvent1.io.storePAddr := RegNext(commit_uop1.debug.storePaddr, 0.U)
      difftestStoreEvent1.io.storeVAddr := RegNext(commit_uop1.debug.storeVaddr, 0.U)
      difftestStoreEvent1.io.storeData  := RegNext(commit_uop1.debug.storeData, 0.U)

      difftestLoadEvent0.io.clock  := clock.asBool
      difftestLoadEvent0.io.coreid := 0.U
      difftestLoadEvent0.io.index  := 0.U
      difftestLoadEvent0.io.valid  := RegNext(commit_uop0.debug.load & Fill(8, commit_val0), 0.U)
      difftestLoadEvent0.io.paddr  := RegNext(commit_uop0.debug.loadVaddr, 0.U)
      difftestLoadEvent0.io.vaddr  := RegNext(commit_uop0.debug.loadPaddr, 0.U)

      difftestLoadEvent1.io.clock  := clock.asBool
      difftestLoadEvent1.io.coreid := 0.U
      difftestLoadEvent1.io.index  := 1.U
      difftestLoadEvent1.io.valid  := RegNext(commit_uop1.debug.load & Fill(8, commit_val1), 0.U)
      difftestLoadEvent1.io.paddr  := RegNext(commit_uop1.debug.loadVaddr, 0.U)
      difftestLoadEvent1.io.vaddr  := RegNext(commit_uop1.debug.loadPaddr, 0.U)

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
      difftestCSRRegState.io.coreid    := csr.io.debug.cpuid
      difftestCSRRegState.io.crmd      := csr.io.debug.crmd
      difftestCSRRegState.io.prmd      := csr.io.debug.prmd
      difftestCSRRegState.io.euen      := csr.io.debug.euen
      difftestCSRRegState.io.ecfg      := csr.io.debug.ecfg
      difftestCSRRegState.io.estat     := csr.io.debug.estat
      difftestCSRRegState.io.era       := csr.io.debug.era
      difftestCSRRegState.io.badv      := csr.io.debug.badv
      difftestCSRRegState.io.eentry    := csr.io.debug.eentry
      difftestCSRRegState.io.tlbidx    := csr.io.debug.tlbidx
      difftestCSRRegState.io.tlbehi    := csr.io.debug.tlbehi
      difftestCSRRegState.io.tlbelo0   := csr.io.debug.tlbelo0
      difftestCSRRegState.io.tlbelo1   := csr.io.debug.tlbelo1
      difftestCSRRegState.io.asid      := csr.io.debug.asid
      difftestCSRRegState.io.pgdl      := csr.io.debug.pgdl
      difftestCSRRegState.io.pgdh      := csr.io.debug.pgdh
      difftestCSRRegState.io.save0     := csr.io.debug.saved0
      difftestCSRRegState.io.save1     := csr.io.debug.saved1
      difftestCSRRegState.io.save2     := csr.io.debug.saved2
      difftestCSRRegState.io.save3     := csr.io.debug.saved3
      difftestCSRRegState.io.tid       := csr.io.debug.tid
      difftestCSRRegState.io.tcfg      := csr.io.debug.tcfg
      difftestCSRRegState.io.tval      := csr.io.debug.tval
      difftestCSRRegState.io.ticlr     := csr.io.debug.ticlr
      difftestCSRRegState.io.llbctl    := csr.io.debug.llbctl
      difftestCSRRegState.io.tlbrentry := csr.io.debug.tlbrentry
      difftestCSRRegState.io.dmw0      := csr.io.debug.dmw0
      difftestCSRRegState.io.dmw1      := csr.io.debug.dmw1

      dontTouch(difftestInstrCommit0.io)
      dontTouch(difftestInstrCommit1.io)
      dontTouch(difftestExcpEvent.io)
      dontTouch(difftestTrapEvent.io)
      dontTouch(difftestStoreEvent0.io)
      dontTouch(difftestStoreEvent1.io)
      dontTouch(difftestLoadEvent0.io)
      dontTouch(difftestLoadEvent1.io)
      dontTouch(difftestGRegState.io)
      dontTouch(difftestCSRRegState.io)
    }
  }
}
