package KXCore.superscalar.core.frontend

import scala.math.max
import chisel3._
import chisel3.util._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.common.Elaborate.commonParams
import os.read

/** Buffer to hold fetched packets and convert them into a vector of MicroOps to give the Decode stage
  */
class FetchBuffer(implicit params: CoreParameters) extends Module {
  import params.{commonParams, frontendParams, backendParams}
  import frontendParams.{fetchWidth, fbNum}
  import backendParams.{coreWidth}

  require(fetchWidth % coreWidth == 0)

  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new FetchBundle()))
    val deq = Decoupled(new FetchBufferResp())

    // Was the pipeline redirected? flush the fetchbuffer.
    val flush = Input(Bool())
  })

  val nBanks = fetchWidth / coreWidth

  val valids = RegInit(VecInit(Seq.fill(fbNum)(0.U(fetchWidth.W))))
  val uops   = Reg(Vec(fbNum, Vec(fetchWidth, new MicroOp)))

  val head_ptr = RegInit(0.U(log2Ceil(fbNum).W)) // deq_ptr
  val bank_ptr = RegInit(0.U((1 max log2Ceil(nBanks)).W))
  val tail_ptr = RegInit(0.U(log2Ceil(fbNum).W)) // enq_ptr

  val maybe_full = valids(head_ptr).orR
  val full       = (head_ptr === tail_ptr) && maybe_full
  val empty      = (head_ptr === tail_ptr) && !maybe_full

  val do_enq = io.enq.fire
  val do_deq = io.deq.fire

  // -------------------------------------------------------------
  // **** Enqueue Uops ****
  // -------------------------------------------------------------
  io.enq.ready := !full

  val in_mask = Wire(Vec(fetchWidth, Bool()))
  val in_uops = Wire(Vec(fetchWidth, new MicroOp()))

  for (i <- 0 until fetchWidth) {
    val pc = io.enq.bits.pcs(i)
    in_uops(i)            := DontCare
    in_mask(i)            := io.enq.valid && (io.enq.bits.mask(i) || io.enq.bits.exception.valid)
    in_uops(i).exception  := io.enq.bits.exception.valid
    in_uops(i).ecode      := io.enq.bits.exception.bits
    in_uops(i).badv       := io.enq.bits.pc
    in_uops(i).debug.pc   := pc
    in_uops(i).debug.inst := io.enq.bits.insts(i)
    in_uops(i).idx        := i.U
    in_uops(i).ftqIdx     := io.enq.bits.ftqIdx
    in_uops(i).inst       := io.enq.bits.insts(i)
  }

  when(do_enq) {
    valids(tail_ptr) := in_mask.asUInt
    uops(tail_ptr)   := in_uops
  }

  // -------------------------------------------------------------
  // **** Dequeue Uops ****
  // -------------------------------------------------------------

  val row_valids = Wire(Vec(nBanks, UInt(coreWidth.W)))
  val row_uops   = Wire(Vec(nBanks, Vec(coreWidth, new MicroOp)))
  for (b <- 0 until nBanks) {
    row_valids(b) := valids(head_ptr)((b + 1) * coreWidth - 1, b * coreWidth)
    (0 until coreWidth).map { i =>
      val idx = b * coreWidth + i
      row_uops(b)(i) := uops(head_ptr)(idx)
    }
  }

  io.deq.valid := row_valids(bank_ptr).orR
  (0 until coreWidth).map { i =>
    io.deq.bits.uops(i).valid := row_valids(bank_ptr)(i) && !io.flush
    io.deq.bits.uops(i).bits  := row_uops(bank_ptr)(i)
  }

  when(io.deq.fire) {
    val bank_mask = (0 until nBanks)
      .map { i => Mux(bank_ptr === i.U, ((1 << coreWidth) - 1).U << (i * coreWidth), 0.U) }
      .reduce(_ | _)
    valids(head_ptr) := valids(head_ptr) & ~bank_mask
  }

  // -------------------------------------------------------------
  // **** Update State ****
  // -------------------------------------------------------------

  tail_ptr := Mux(do_enq, WrapInc(tail_ptr, fbNum), tail_ptr)

  val next_bank_ptr  = bank_ptr + 1.U
  val read_next_bank = (!empty && !io.deq.valid) || io.deq.fire
  bank_ptr := Mux(read_next_bank, next_bank_ptr, bank_ptr)
  head_ptr := Mux(next_bank_ptr === 0.U && read_next_bank, WrapInc(head_ptr, fbNum), head_ptr)

  when(io.flush) {
    head_ptr := 0.U
    bank_ptr := 0.U
    tail_ptr := 0.U
    (0 until fbNum).map { i => valids(i) := 0.U }
  }

}
