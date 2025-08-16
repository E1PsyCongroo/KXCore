package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.utils._
import KXCore.common.Privilege._
import KXCore.superscalar._
import KXCore.superscalar.REDIRECTType._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._

class RoBAllocIO(implicit params: CoreParameters) extends Bundle {
  val valid = Input(Bool())
  val uop   = Input(new MicroOp)
  val idx   = Output(UInt(params.backendParams.robIdxWidth.W))
  val ready = Output(Bool())
}

class RoBCommitIO(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, frontendParams, backendParams}
  import commonParams.{vaddrWidth}
  import frontendParams.{ftqIdxWidth}
  import backendParams.{coreWidth}

  val valids = Vec(coreWidth, Bool())
  val uop    = Vec(coreWidth, new MicroOp)
}

class RoBRedirectIO(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, frontendParams}
  import commonParams.{vaddrWidth}
  import frontendParams.{ftqIdxWidth}

  val valid      = Bool()
  val target     = UInt(vaddrWidth.W)
  val ftqIdx     = UInt(ftqIdxWidth.W)
  val brRecovery = new BrRecoveryInfo
}

class RoBExceptionIO(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams}
  import commonParams.{vaddrWidth}

  val valid     = Bool()
  val epc       = UInt(vaddrWidth.W) // pc for exception handling
  val ecode     = UInt(6.W)          // exception code
  val ecode_sub = UInt(9.W)
  val badv      = UInt(vaddrWidth.W) // bad virtual address for exception about addr
  val debug = new Bundle {
    val exceptionPC   = UInt(32.W)
    val exceptionInst = UInt(32.W)
  }
}

class ReorderBuffer(implicit params: CoreParameters) extends Module {
  import params.{fetchBytes, commonParams, frontendParams, backendParams}
  import commonParams.{dataWidth, vaddrWidth, instBytes}
  import frontendParams.{fetchWidth, ftqIdxWidth}
  import backendParams.{coreWidth, robRowNum, robIdxWidth, retireWidth, lregWidth, pregWidth, wbPortNum}

  val io = IO(new Bundle {
    val alloc = Vec(coreWidth, new RoBAllocIO)
    val empty = Output(Bool())

    val write = Vec(wbPortNum, Flipped(Valid(new ExeUnitResp)))
    val brInfo = Vec(
      backendParams.intIQParams.issueWidth,
      Input(new Bundle {
        val uop            = new MicroOp
        val brRecoveryInfo = new BrRecoveryInfo
      }),
    )
    val xcepInfo = Vec(2, Flipped(Valid(new MicroOp)))

    val ftqReq  = Output(UInt(ftqIdxWidth.W))
    val ftqResp = Input(new FTQInfo)

    val commit    = Output(new RoBCommitIO)
    val redirect  = Output(new RoBRedirectIO)
    val exception = Output(new RoBExceptionIO)

    val intr_pending = Input(Bool())
    val eentry       = Input(UInt(dataWidth.W))
    val era          = Input(UInt(dataWidth.W))
    val ertn         = Output(Bool())
  })

  val rob_flush     = Wire(Bool())
  val rob_do_unique = RegInit(false.B)
  val rob_xcep_val  = RegInit(false.B)
  val rob_xcep_info = Reg(new Bundle {
    val robIdx = UInt(robIdxWidth.W)
    val ecode  = UInt(ECODE.getWidth.W)
    val badv   = UInt(vaddrWidth.W)
  })
  val rob_redirect_val = RegInit(false.B)
  val rob_redirect_info = Reg(new Bundle {
    val robIdx         = UInt(robIdxWidth.W)
    val isErtn         = Bool()
    val brRecoveryInfo = new BrRecoveryInfo
  })

  val rob_head      = RegInit(0.U(log2Ceil(robRowNum).W))
  val rob_head_idx  = if (coreWidth == 1) rob_head else Cat(rob_head, 0.U(log2Ceil(coreWidth).W))
  val rob_tail      = RegInit(0.U(log2Ceil(robRowNum).W))
  val rob_tail_free = RegInit(UInt(coreWidth.W), Fill(coreWidth, 1.B))
  val rob_tail_idx  = if (coreWidth == 1) rob_tail else Cat(rob_tail, OHToUInt(PriorityEncoderOH(rob_tail_free)))

  val full  = Wire(Bool())
  val empty = Wire(Bool())

  val can_commit     = Wire(Vec(coreWidth, Bool()))
  val can_throw_xcep = Wire(Vec(coreWidth, Bool()))
  val can_redirect   = Wire(Vec(coreWidth, Bool()))
  val will_commit    = Wire(Vec(coreWidth, Bool()))

  val rob_head_vals = Wire(Vec(coreWidth, Bool())) // are the instructions at the head valid?

  def GetRowIdx(rob_idx: UInt): UInt = {
    if (coreWidth == 1) return rob_idx
    else return rob_idx(robIdxWidth - 1, log2Ceil(coreWidth))
  }
  def GetBankIdx(rob_idx: UInt): UInt = {
    if (coreWidth == 1) { return 0.U }
    else { return rob_idx(log2Ceil(coreWidth) - 1, 0).asUInt }
  }

  class RoBCompactUop extends Bundle {
    val pcLow     = UInt(log2Ceil(fetchBytes).W)
    val ftqIdx    = UInt(ftqIdxWidth.W)
    val ldst      = UInt(lregWidth.W)
    val pdst      = UInt(pregWidth.W)
    val stalePdst = UInt(pregWidth.W)
  }
  def compact_to_uop(compact: RoBCompactUop, uop: MicroOp): MicroOp = {
    val out = WireInit(uop)
    out.pcLow     := compact.pcLow
    out.ftqIdx    := compact.ftqIdx
    out.ldst      := compact.ldst
    out.pdst      := compact.pdst
    out.stalePdst := compact.stalePdst
    out
  }
  def uop_to_compact(uop: MicroOp): RoBCompactUop = {
    val out = Wire(new RoBCompactUop)
    out.pcLow     := uop.pcLow
    out.ftqIdx    := uop.ftqIdx
    out.ldst      := uop.ldst
    out.pdst      := uop.pdst
    out.stalePdst := uop.stalePdst
    out
  }

  for (w <- 0 until coreWidth) {
    def MatchBank(bank_idx: UInt): Bool = (bank_idx === w.U)

    // one bank
    val rob_val         = RegInit(VecInit(Seq.fill(robRowNum) { false.B }))
    val rob_uop         = Reg(Vec(robRowNum, new MicroOp()))
    val rob_compact_uop = Reg(Vec(robRowNum, new RoBCompactUop))
    val rob_bsy         = Reg(Vec(robRowNum, Bool()))
    val rob_exception   = Reg(Vec(robRowNum, Bool()))
    val rob_redirect    = Reg(Vec(robRowNum, Bool()))

    // -----------------------------------------------
    // Dispatch: Add Entry to ROB

    when(io.alloc(w).valid && io.alloc(w).ready) {
      rob_val(rob_tail)         := true.B
      rob_bsy(rob_tail)         := io.alloc(w).uop.busy
      rob_compact_uop(rob_tail) := uop_to_compact(io.alloc(w).uop)
      rob_uop(rob_tail)         := io.alloc(w).uop
      rob_exception(rob_tail)   := io.alloc(w).uop.exception
      rob_redirect(rob_tail)    := io.alloc(w).uop.flushOnCommit
      when(!rob_xcep_val && io.alloc(w).uop.exception) {
        rob_xcep_val         := true.B
        rob_xcep_info.robIdx := io.alloc(w).uop.robIdx
        rob_xcep_info.ecode  := io.alloc(w).uop.ecode
        rob_xcep_info.badv   := io.alloc(w).uop.badv
      }
      when(!rob_redirect_val && io.alloc(w).uop.flushOnCommit) {
        rob_redirect_val                       := true.B
        rob_redirect_info.robIdx               := io.alloc(w).uop.robIdx
        rob_redirect_info.isErtn               := io.alloc(w).uop.isErtn
        rob_redirect_info.brRecoveryInfo       := DontCare
        rob_redirect_info.brRecoveryInfo.valid := false.B
      }
      assert(rob_val(rob_tail) === false.B, "[rob] overwriting a valid entry.")
    }

    io.alloc(w).idx   := Mux((coreWidth == 1).B, rob_tail, Cat(rob_tail, w.U(log2Ceil(coreWidth).W)))
    io.alloc(w).ready := rob_tail_free(w) && !full && !rob_do_unique

    // -----------------------------------------------
    // Writeback

    for (i <- 0 until wbPortNum) {
      val wb_resp = io.write(i)
      val wb_uop  = wb_resp.bits.uop
      val row_idx = GetRowIdx(wb_uop.robIdx)
      when(wb_resp.valid && MatchBank(GetBankIdx(wb_uop.robIdx))) {
        rob_bsy(row_idx) := false.B
        rob_uop(row_idx) := wb_uop
      }
    }

    for (i <- 0 until backendParams.intIQParams.issueWidth) {
      val br_info = io.brInfo(i)
      val br_uop  = br_info.uop
      val row_idx = GetRowIdx(br_uop.robIdx)
      when(br_info.brRecoveryInfo.valid && MatchBank(GetBankIdx(br_uop.robIdx))) {
        rob_redirect(row_idx) := true.B
      }
    }
    val oldest_br_info =
      io.brInfo.reduce((oldest, info) =>
        Mux(!oldest.brRecoveryInfo.valid || (info.brRecoveryInfo.valid && IsOlder(info.uop.robIdx, oldest.uop.robIdx, rob_head_idx)), info, oldest),
      )
    when(oldest_br_info.brRecoveryInfo.valid && (!rob_redirect_val || IsOlder(oldest_br_info.uop.robIdx, rob_redirect_info.robIdx, rob_head_idx))) {
      rob_redirect_val                 := true.B
      rob_redirect_info.robIdx         := oldest_br_info.uop.robIdx
      rob_redirect_info.isErtn         := false.B
      rob_redirect_info.brRecoveryInfo := oldest_br_info.brRecoveryInfo
    }

    for (i <- 0 until io.xcepInfo.length) {
      val xcep_info = io.xcepInfo(i)
      val xcep_uop  = xcep_info.bits
      val row_idx   = GetRowIdx(xcep_uop.robIdx)
      when(xcep_info.valid && MatchBank(GetBankIdx(xcep_uop.robIdx))) {
        rob_exception(row_idx) := true.B
      }
    }
    val oldest_xcep_info =
      io.xcepInfo.reduce((oldest, info) => Mux(!oldest.valid || (info.valid && IsOlder(info.bits.robIdx, oldest.bits.robIdx, rob_head_idx)), info, oldest))
    when(oldest_xcep_info.valid && (!rob_xcep_val || IsOlder(oldest_xcep_info.bits.robIdx, rob_xcep_info.robIdx, rob_head_idx))) {
      rob_xcep_val         := true.B
      rob_xcep_info.robIdx := oldest_xcep_info.bits.robIdx
      rob_xcep_info.ecode  := oldest_xcep_info.bits.ecode
      rob_xcep_info.badv   := oldest_xcep_info.bits.badv
    }

    // -----------------------------------------------
    // Commit

    can_commit(w)     := rob_val(rob_head) && !rob_bsy(rob_head) && !io.intr_pending
    can_throw_xcep(w) := rob_val(rob_head) && (rob_exception(rob_head) || io.intr_pending)
    can_redirect(w)   := can_commit(w) && rob_redirect(rob_head)

    io.commit.valids(w) := will_commit(w)
    io.commit.uop(w)    := compact_to_uop(rob_compact_uop(rob_head), rob_uop(rob_head))

    when(will_commit(w)) {
      rob_val(rob_head) := false.B
    }

    // -----------------------------------------------
    // Outputs

    rob_head_vals(w) := rob_val(rob_head)

    // -----------------------------------------------
    // Flush

    when(rob_flush) {
      for (i <- 0 until robRowNum) {
        rob_val(i) := false.B
      }
    }

    if (params.debug) {
      dontTouch(rob_val)
      dontTouch(rob_bsy)
      dontTouch(rob_redirect)
    }
  } // for (w <- 0 until coreWidth)

  // -----------------------------------------------
  // Commit Logic

  var block_commit    = false.B
  var will_throw_xcep = false.B
  var will_redirect   = false.B
  var block_redirect  = false.B
  var commit_count    = 0.U

  for (w <- 0 until coreWidth) {
    will_throw_xcep = will_throw_xcep || (can_throw_xcep(w) && !block_commit && !block_redirect)
    will_redirect = will_redirect || (can_redirect(w) && !block_commit && !block_redirect)
    will_commit(w) := can_commit(w) && !block_commit

    commit_count = Mux(will_commit(w), commit_count + 1.U, commit_count)
    block_commit = block_commit || (rob_head_vals(w) && (!can_commit(w) || can_throw_xcep(w) || can_redirect(w)))
    if (retireWidth != coreWidth) {
      block_commit = block_commit || (commit_count === retireWidth.U)
    }
    block_redirect = will_commit(w)
  }

  val redirect_uop = PriorityMux(rob_head_vals, io.commit.uop)
  io.ftqReq := redirect_uop.ftqIdx
  val redirect_uop_fetchPC = params.fetchAlign(io.ftqResp.entry.fetchPC)
  val redirect_uop_pc      = redirect_uop_fetchPC | redirect_uop.pcLow
  io.redirect.valid            := will_throw_xcep || will_redirect
  io.redirect.ftqIdx           := redirect_uop.ftqIdx
  io.redirect.brRecovery       := rob_redirect_info.brRecoveryInfo
  io.redirect.brRecovery.valid := rob_redirect_info.brRecoveryInfo.valid && !will_throw_xcep
  io.redirect.target := MuxCase(
    redirect_uop_pc + instBytes.U,
    Seq(
      will_throw_xcep                                           -> io.eentry,
      (will_redirect && rob_redirect_info.isErtn)               -> io.era,
      (will_redirect && rob_redirect_info.brRecoveryInfo.valid) -> rob_redirect_info.brRecoveryInfo.target,
    ),
  )
  io.ertn := will_redirect && rob_redirect_info.isErtn && !will_throw_xcep

  val ecode = Mux(io.intr_pending, ECODE.INT.asUInt, rob_xcep_info.ecode)
  io.exception.valid     := will_throw_xcep
  io.exception.epc       := redirect_uop_pc
  io.exception.ecode     := ECODE.getEcode(ecode)
  io.exception.ecode_sub := ECODE.getEsubCode(ecode)
  io.exception.badv      := rob_xcep_info.badv

  io.exception.debug.exceptionPC   := io.exception.epc
  io.exception.debug.exceptionInst := redirect_uop.debug.inst

  // -----------------------------------------------
  // Exception & Redirect Tracking Logic
  // only store the oldest exception & redirect, since only one can happen!

  // -----------------------------------------------
  // ROB Head Logic

  val finished_committing_row = io.commit.valids.reduce(_ || _) &&
    (rob_head_vals.asUInt === will_commit.asUInt) && (rob_head =/= rob_tail)

  when(finished_committing_row) {
    rob_head := WrapInc(rob_head, robRowNum)
  }

  // -----------------------------------------------
  // ROB Tail Logic

  val next_rob_tail_free = rob_tail_free & ~VecInit(io.alloc.map(a => a.valid && a.ready)).asUInt
  when(next_rob_tail_free === 0.U) {
    rob_tail      := WrapInc(rob_tail, robRowNum)
    rob_tail_free := Fill(coreWidth, 1.B)
  }.otherwise {
    rob_tail_free := next_rob_tail_free
  }

  // -----------------------------------------------
  // Full/Empty Logic

  full     := WrapInc(rob_tail, robRowNum) === rob_head
  empty    := (rob_head === rob_tail) && (rob_head_vals.asUInt === 0.U)
  io.empty := empty

  // -----------------------------------------------
  // Unique Logic

  for (w <- 0 until coreWidth) {
    when(io.alloc(w).valid && io.alloc(w).ready && io.alloc(w).uop.isUnique) {
      assert(empty)
      rob_do_unique := true.B
    }
  }
  when(will_commit.reduce(_ || _)) {
    rob_do_unique := false.B
  }

  // -----------------------------------------------
  // Flush Logic

  rob_flush := io.redirect.valid
  when(rob_flush) {
    rob_head         := 0.U
    rob_tail         := 0.U
    rob_tail_free    := Fill(coreWidth, 1.B)
    rob_do_unique    := false.B
    rob_xcep_val     := false.B
    rob_redirect_val := false.B
  }

  // -----------------------------------------------
  // Debug

  if (params.debug) {
    dontTouch(io)
    dontTouch(can_commit)
    dontTouch(can_redirect)
    dontTouch(will_commit)
    dontTouch(will_throw_xcep)
    dontTouch(will_redirect)
    dontTouch(rob_head_vals)
  }

  // -----------------------------------------------
  // -----------------------------------------------
}
