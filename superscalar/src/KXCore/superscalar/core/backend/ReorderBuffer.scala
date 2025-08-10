package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.utils._
import KXCore.common.Privilege._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._

class RoBAllocIO(implicit params: CoreParameters) extends Bundle {
  val valid = Input(Bool())
  val uop   = Input(new MicroOp)
  val idx   = Output(UInt(params.backendParams.robIdxWidth.W))
  val ready = Output(Bool())
}

class RoBWriteIO(implicit params: CoreParameters) extends Bundle {
  val uop    = new MicroOp
  val brInfo = Valid(new BrUpdateInfo)
}
class RoBCommitIO(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, frontendParams, backendParams}
  import commonParams.{vaddrWidth, instBytes}
  import frontendParams.{ftqIdxWidth}
  import backendParams.{coreWidth, robRowNum, robIdxWidth, retireWidth, lregWidth, pregWidth, wbPortNum}

  val valids   = Vec(coreWidth, Bool())
  val uop      = Vec(coreWidth, new MicroOp)
  val ftqIdx   = UInt(ftqIdxWidth.W)
  val brInfo   = Valid(new BrUpdateInfo)
  val redirect = Valid(UInt(vaddrWidth.W))
  val exception = Valid(new Bundle {
    val pc        = Output(UInt(vaddrWidth.W)) // Program counter for exception handling
    val ecode     = Output(UInt(6.W))          // Exception code
    val ecode_sub = Output(UInt(9.W))
    val badv      = Output(UInt(vaddrWidth.W)) // Bad virtual address for exception
  })
  val ertn = Output(Bool())
}

class ReorderBuffer(implicit params: CoreParameters) extends Module {
  import params.{commonParams, frontendParams, backendParams}
  import commonParams.{dataWidth, vaddrWidth, instBytes}
  import frontendParams.{ftqIdxWidth}
  import backendParams.{coreWidth, robRowNum, robIdxWidth, retireWidth, lregWidth, pregWidth, wbPortNum}

  val io = IO(new Bundle {
    val alloc = Vec(coreWidth, new RoBAllocIO)
    val empty = Output(Bool())

    val write = Vec(wbPortNum, Flipped(Valid(new RoBWriteIO)))

    val getPC  = Flipped(new GetPCFromFtqIO)
    val commit = Output(new RoBCommitIO)
    val eentry = Input(UInt(dataWidth.W)) // Exception entry address
    val era    = Input(UInt(dataWidth.W))
  })

  val rob_flush = Wire(Bool())

  val rob_exception_info = Reg(Valid(new Bundle {
    val robIdx = UInt(robIdxWidth.W)
    val ecode  = UInt(ECODE.getWidth.W)
    val badv   = UInt(vaddrWidth.W) // Bad virtual address for exception
  }))
  when(reset.asBool) { rob_exception_info.valid := false.B }

  val rob_head = RegInit(0.U(log2Ceil(robRowNum).W))
  val rob_tail = RegInit(0.U(log2Ceil(robRowNum).W))

  val next_rob_head = WireInit(rob_head)
  rob_head := next_rob_head

  val full  = Wire(Bool())
  val empty = Wire(Bool())

  val will_commit          = Wire(Vec(coreWidth, Bool()))
  val will_flush           = Wire(Vec(coreWidth, Bool()))
  val will_throw_exception = Wire(Vec(coreWidth, Bool()))
  val can_commit           = Wire(Vec(coreWidth, Bool()))
  val can_flush            = Wire(Vec(coreWidth, Bool()))
  val can_throw_exception  = Wire(Vec(coreWidth, Bool()))

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
    val idx       = UInt(log2Ceil(frontendParams.fetchWidth).W)
    val ldst      = UInt(lregWidth.W)
    val pdst      = UInt(pregWidth.W)
    val stalePdst = UInt(pregWidth.W)
    val flush     = Bool()
    val ertn      = Bool()
  }
  def compact_to_uop(compact: RoBCompactUop, uop: MicroOp): MicroOp = {
    val out = WireInit(uop)
    out.idx       := compact.idx
    out.ldst      := compact.ldst
    out.pdst      := compact.pdst
    out.stalePdst := compact.stalePdst
    out.flush     := compact.flush
    out.ertn      := compact.ertn
    out
  }
  def uop_to_compact(uop: MicroOp): RoBCompactUop = {
    val out = Wire(new RoBCompactUop)
    out.idx       := uop.idx
    out.ldst      := uop.ldst
    out.pdst      := uop.pdst
    out.stalePdst := uop.stalePdst
    out.flush     := uop.flush
    out.ertn      := uop.ertn
    out
  }

  val rob_row_ftq    = Reg(Vec(robRowNum, UInt(ftqIdxWidth.W)))
  val rob_row_brInfo = Reg(Vec(if (coreWidth == retireWidth) 1 else coreWidth, Vec(robRowNum, Valid(new BrUpdateInfo))))
  rob_row_brInfo.foreach(_.foreach(b => when(reset.asBool) { b.valid := false.B }))
  for (w <- 0 until coreWidth) {
    def MatchBank(bank_idx: UInt): Bool = (bank_idx === w.U)

    // one bank
    val rob_row_brInfo_idx = if (coreWidth == retireWidth) 0 else w
    val rob_val            = RegInit(VecInit(Seq.fill(robRowNum) { false.B }))
    val rob_uop            = Reg(Vec(robRowNum, new MicroOp()))
    val rob_compact_uop    = Reg(Vec(robRowNum, new RoBCompactUop))
    val rob_bsy            = Reg(Vec(robRowNum, Bool()))
    val rob_exception      = Reg(Vec(robRowNum, Bool()))

    // -----------------------------------------------
    // Dispatch: Add Entry to ROB

    when(io.alloc(w).valid && io.alloc(w).ready) {
      rob_val(rob_tail)                                  := true.B
      rob_bsy(rob_tail)                                  := io.alloc(w).uop.busy
      rob_compact_uop(rob_tail)                          := uop_to_compact(io.alloc(w).uop)
      rob_uop(rob_tail)                                  := io.alloc(w).uop
      rob_exception(rob_tail)                            := io.alloc(w).uop.exception
      rob_row_ftq(rob_tail)                              := io.alloc(w).uop.ftqIdx
      rob_row_brInfo(rob_row_brInfo_idx)(rob_tail).valid := false.B
      when(!rob_exception_info.valid && io.alloc(w).uop.exception) {
        rob_exception_info.valid       := io.alloc(w).uop.exception
        rob_exception_info.bits.badv   := io.alloc(w).uop.badv
        rob_exception_info.bits.ecode  := io.alloc(w).uop.ecode
        rob_exception_info.bits.robIdx := io.alloc(w).uop.robIdx
      }
      assert(rob_val(rob_tail) === false.B, "[rob] overwriting a valid entry.")
    }

    io.alloc(w).idx   := Mux((coreWidth == 1).B, rob_tail, Cat(rob_tail, w.U(log2Ceil(coreWidth).W)))
    io.alloc(w).ready := !full

    // -----------------------------------------------
    // Writeback

    // judge idx1 is older than idx2
    def is_old_rob(idx1: UInt, idx2: UInt) = !((rob_head <= rob_tail) ^ (idx1 < idx2))

    var oldest_excp_idx = rob_exception_info.bits.robIdx
    for (i <- 0 until wbPortNum) {
      val wb_resp    = io.write(i)
      val wb_uop     = wb_resp.bits.uop
      val wb_brInfo  = wb_resp.bits.brInfo
      val row_idx    = GetRowIdx(wb_uop.robIdx)
      val rob_brInfo = rob_row_brInfo(rob_row_brInfo_idx)(row_idx)
      when(wb_resp.valid && MatchBank(GetBankIdx(wb_uop.robIdx))) {
        rob_bsy(row_idx)       := false.B
        rob_uop(row_idx)       := wb_uop
        rob_exception(row_idx) := wb_uop.exception
        // TODO: Make this better
        when(
          !rob_brInfo.valid || !rob_brInfo.bits.cfiIdx.valid ||
            (wb_brInfo.bits.cfiIdx.valid && wb_brInfo.bits.cfiIdx.bits < rob_brInfo.bits.cfiIdx.bits),
        ) {
          // printf("[ROB] row_id: %x, write brInfo with valid %d and mispred %d\n", wb_uop.robIdx, wb_brInfo.valid, wb_brInfo.bits.mispredict)
          rob_brInfo := wb_brInfo
        }
      }
      when(
        wb_uop.exception &&
          (!rob_exception_info.valid || (rob_exception_info.valid
            && is_old_rob(wb_uop.robIdx, oldest_excp_idx))),
      ) {
        oldest_excp_idx = wb_uop.robIdx
        rob_exception_info.valid      := wb_uop.exception
        rob_exception_info.bits.ecode := wb_uop.ecode
        rob_exception_info.bits.badv  := wb_uop.badv
      }
    }

    // -----------------------------------------------
    // Commit
    can_commit(w)          := rob_val(rob_head) && !rob_bsy(rob_head)
    can_throw_exception(w) := rob_val(rob_head) && rob_exception(rob_head)
    can_flush(w)           := can_commit(w) && rob_compact_uop(rob_head).flush

    io.commit.valids(w) := will_commit(w)
    io.commit.uop(w)    := compact_to_uop(rob_compact_uop(rob_head), rob_uop(rob_head))
    io.commit.ftqIdx    := rob_row_ftq(rob_head)

    when(will_commit(w)) {
      rob_val(rob_head)                                  := false.B
      rob_row_brInfo(rob_row_brInfo_idx)(rob_head).valid := false.B
    }

    // -----------------------------------------------
    // Outputs

    rob_head_vals(w) := rob_val(rob_head)

    // -----------------------------------------------
    // Flush

    when(rob_flush) {
      rob_exception_info.valid := false.B
      for (i <- 0 until robRowNum) {
        rob_val(i)                                  := false.B
        rob_row_brInfo(rob_row_brInfo_idx)(i).valid := false.B
      }
    }

    dontTouch(rob_val)
    dontTouch(rob_bsy)
  } // for (w <- 0 until coreWidth)

  // -----------------------------------------------
  // Commit Logic

  var block_commit    = false.B
  var block_flush     = false.B
  var block_exception = false.B
  var will_etrn       = false.B
  var commit_count    = 0.U

  io.commit.brInfo.valid := false.B
  io.commit.brInfo.bits  := DontCare
  for (w <- 0 until coreWidth) {
    will_commit(w)          := can_commit(w) && !block_commit
    will_flush(w)           := can_flush(w) && !block_commit && !block_flush
    will_throw_exception(w) := can_throw_exception(w) && !block_commit && !block_exception

    will_etrn = (can_commit(w) && io.commit.uop(w).ertn && !block_commit) || will_etrn

    if (retireWidth == coreWidth) {
      io.commit.brInfo := rob_row_brInfo(0)(rob_head)
      block_commit = block_commit || (rob_head_vals(w) &&
        (!can_commit(w) ||
          (io.commit.brInfo.valid && io.commit.brInfo.bits.mispredict) ||
          can_throw_exception(w) ||
          can_flush(w) || io.commit.uop(w).ertn))
    } else {
      val brInfo = rob_row_brInfo(w)(rob_head)
      // TODO: Make this valid when retiredWidth > 1
      when(will_commit(w) && brInfo.valid) {
        io.commit.brInfo := brInfo
      }
      commit_count = Mux(will_commit(w), commit_count + 1.U, commit_count)
      block_commit = block_commit || (commit_count === retireWidth.U) ||
        (rob_head_vals(w) &&
          (!can_commit(w) ||
            (brInfo.valid && brInfo.bits.mispredict) ||
            can_throw_exception(w) ||
            can_flush(w) || io.commit.uop(w).ertn))
    }
    block_flush = will_commit(w)
    block_exception = will_commit(w)
  }

  io.getPC.ftqIdx := io.commit.ftqIdx

  val alignedFetchPC = params.fetchAlign(io.getPC.info.entry.fetchPC)
  val pcs = VecInit((0 until coreWidth).map { w =>
    alignedFetchPC | Cat(io.commit.uop(w).idx, Fill(log2Ceil(instBytes), 0.U))
  })

  val mispred       = io.commit.brInfo.valid && io.commit.brInfo.bits.mispredict
  val flush         = will_flush.reduce(_ || _)
  val flush_idx     = OHToUInt(will_flush)
  val flushPC       = pcs(flush_idx) + instBytes.U
  val exception     = will_throw_exception.reduce(_ || _)
  val exception_idx = OHToUInt(will_throw_exception)
  io.commit.redirect.valid := mispred | flush | exception | will_etrn
  io.commit.redirect.bits := MuxCase(
    DontCare,
    Seq(
      exception -> io.eentry,
      will_etrn -> io.era,
      flush     -> flushPC,
      mispred   -> io.commit.brInfo.bits.target,
    ),
  )
  io.commit.exception.valid          := will_throw_exception.reduce(_ || _)
  io.commit.exception.bits.pc        := Mux(rob_exception_info.bits.ecode === ECODE.ADEF.asUInt, io.getPC.info.entry.fetchPC, pcs(exception_idx))
  io.commit.exception.bits.badv      := rob_exception_info.bits.badv
  io.commit.exception.bits.ecode     := ECODE.getEcode(rob_exception_info.bits.ecode)
  io.commit.exception.bits.ecode_sub := ECODE.getEsubCode(rob_exception_info.bits.ecode)
  io.commit.ertn                     := will_etrn

  // -----------------------------------------------
  // ROB Head Logic

  val finished_committing_row =
    (io.commit.valids.reduce(_ || _)) &&
      ((will_commit.asUInt ^ rob_head_vals.asUInt) === 0.U)

  when(finished_committing_row) {
    next_rob_head := WrapInc(rob_head, robRowNum)
  }

  // -----------------------------------------------
  // ROB Tail Logic

  when(!full && io.alloc.map(_.valid).reduce(_ || _)) {
    rob_tail := WrapInc(rob_tail, robRowNum)
  }

  // -----------------------------------------------
  // Full/Empty Logic

  full  := (rob_head === rob_tail) && (rob_head_vals.reduce(_ || _))
  empty := (rob_head === rob_tail) && (rob_head_vals.asUInt === 0.U)

  io.empty := empty

  // -----------------------------------------------
  // Flush Logic

  rob_flush := io.commit.valids.reduce(_ || _) && io.commit.redirect.valid

  when(rob_flush) {
    rob_head := 0.U
    rob_tail := 0.U
  }

  // -----------------------------------------------
  // -----------------------------------------------
  dontTouch(io.commit)
  dontTouch(can_commit)
  dontTouch(can_flush)
  dontTouch(can_throw_exception)
  dontTouch(will_commit)
  dontTouch(will_flush)
  dontTouch(will_throw_exception)
  dontTouch(will_etrn)
  dontTouch(rob_head_vals)
}
