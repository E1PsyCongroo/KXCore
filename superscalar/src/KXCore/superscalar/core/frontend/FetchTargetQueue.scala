package KXCore.superscalar.core.frontend

import chisel3._
import chisel3.util._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.backend._

/** Queue to store the fetch PC and other relevant branch predictor signals that are inflight in the processor.
  *
  * @param num_entries
  *   # of entries in the FTQ
  */
class FetchTargetQueue(implicit params: CoreParameters) extends Module {
  import params.{commonParams, frontendParams, backendParams}
  import commonParams.{vaddrWidth}
  import frontendParams.{ftqNum, ftqIdxWidth}
  import backendParams.{coreWidth}

  val io = IO(new Bundle {
    // Enqueue one entry for every fetch cycle.
    val enq = Flipped(Decoupled(new FetchBundle()))
    // Pass to FetchBuffer (newly fetched instructions).
    val enqIdx = Output(UInt(ftqIdxWidth.W))
    // ROB tells us the youngest committed ftq_idx to remove from FTQ.
    val deq = Flipped(Valid(UInt(ftqIdxWidth.W)))
    val redirect = Input(new Bundle {
      val valid      = Bool()
      val idx        = UInt(ftqIdxWidth.W)
      val brRecovery = new BrRecoveryInfo
    })

    // Give PC info to BranchUnit.
    val reqs  = Input(Vec(3, UInt(ftqIdxWidth.W)))
    val resps = Output(Vec(3, new FTQInfo))

    val bpuUpdate = Output(new BranchPredictionUpdate)
  })

  val bpu_ptr    = RegInit(0.U(ftqIdxWidth.W))
  val deq_ptr    = RegInit(0.U(ftqIdxWidth.W))
  val enq_ptr    = RegInit(0.U(ftqIdxWidth.W))
  val maybe_full = RegInit(false.B)

  val full = maybe_full && enq_ptr === bpu_ptr
  io.enq.ready := !full

  val ram = Reg(Vec(ftqNum, new FTQBundle))

  val do_enq = io.enq.fire

  when(do_enq) {
    val new_entry = Wire(new FTQBundle)
    new_entry.fetchPC      := io.enq.bits.pc
    new_entry.brMask       := io.enq.bits.brMask
    new_entry.cfiIdx.valid := io.enq.bits.cfiIdx.valid
    new_entry.cfiIdx.bits  := io.enq.bits.cfiIdx.bits
    new_entry.cfiIsB       := io.enq.bits.bMask(io.enq.bits.cfiIdx.bits)
    new_entry.cfiIsBr      := io.enq.bits.brMask(io.enq.bits.cfiIdx.bits)
    new_entry.cfiIsJirl    := io.enq.bits.jirlMask(io.enq.bits.cfiIdx.bits)
    new_entry.meta.bim     := io.enq.bits.bpuMeta.bim
    new_entry.meta.btb     := io.enq.bits.bpuMeta.btb
    ram(enq_ptr)           := new_entry
    enq_ptr                := WrapInc(enq_ptr, ftqNum)
    maybe_full             := true.B
  }

  io.enqIdx := enq_ptr

  when(io.deq.valid) {
    deq_ptr    := io.deq.bits
    maybe_full := false.B
  }

  val redirect_entry     = ram(io.redirect.idx)
  val redirect_new_entry = WireInit(redirect_entry)
  when(io.redirect.valid) {
    enq_ptr    := WrapInc(io.redirect.idx, ftqNum)
    maybe_full := false.B

    when(io.redirect.brRecovery.valid) {
      redirect_new_entry.brMask := Mux(
        io.redirect.brRecovery.cfiIdx.valid,
        MaskLower(UIntToOH(io.redirect.brRecovery.cfiIdx.bits)) & redirect_entry.brMask,
        redirect_entry.brMask,
      )
      redirect_new_entry.cfiIdx    := io.redirect.brRecovery.cfiIdx
      redirect_new_entry.cfiIsB    := io.redirect.brRecovery.cfiIsB
      redirect_new_entry.cfiIsBr   := io.redirect.brRecovery.cfiIsBr
      redirect_new_entry.cfiIsJirl := io.redirect.brRecovery.cfiIsJirl
    }
    ram(io.redirect.idx) := redirect_new_entry
  }

  when(bpu_ptr =/= deq_ptr) {
    bpu_ptr := WrapInc(bpu_ptr, ftqNum)

    val bpuEntry = ram(bpu_ptr)
    val target   = ram(WrapInc(bpu_ptr, ftqNum)).fetchPC

    io.bpuUpdate.valid     := bpuEntry.cfiIdx.valid || bpuEntry.brMask =/= 0.U
    io.bpuUpdate.fetchPC   := bpuEntry.fetchPC
    io.bpuUpdate.brMask    := bpuEntry.brMask
    io.bpuUpdate.cfiIdx    := bpuEntry.cfiIdx
    io.bpuUpdate.cfiIsB    := bpuEntry.cfiIsB
    io.bpuUpdate.cfiIsBr   := bpuEntry.cfiIsBr
    io.bpuUpdate.cfiIsJirl := bpuEntry.cfiIsJirl
    io.bpuUpdate.target    := target
    io.bpuUpdate.meta      := bpuEntry.meta
  }

  // -------------------------------------------------------------
  // **** Core Read PC ****
  // -------------------------------------------------------------

  for (i <- 0 until 3) {
    val idx = io.reqs(i)
    io.resps(i).valid := idx =/= enq_ptr || io.enq.fire
    io.resps(i).entry := ram(idx)
    when(idx === enq_ptr) {
      io.resps(i).entry.fetchPC := io.enq.bits.pc
    }
  }
}
