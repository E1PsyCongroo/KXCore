package KXCore.superscalar.core.frontend

import chisel3._
import chisel3.util._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._

/** Queue to store the fetch PC and other relevant branch predictor signals that are inflight in the processor.
  *
  * @param num_entries
  *   # of entries in the FTQ
  */
class FetchTargetQueue(implicit params: CoreParameters) extends Module {
  import params.{commonParams, frontendParams, backendParams}
  import commonParams.{vaddrWidth}
  import frontendParams.{ftqNum}
  import backendParams.{coreWidth}
  private val idxWidth = log2Ceil(ftqNum)

  val io = IO(new Bundle {
    // Enqueue one entry for every fetch cycle.
    val enq = Flipped(Decoupled(new FetchBundle()))
    // Pass to FetchBuffer (newly fetched instructions).
    val enqIdx = Output(UInt(idxWidth.W))
    // ROB tells us the youngest committed ftq_idx to remove from FTQ.
    val deq = Flipped(Valid(new Bundle {
      val idx      = UInt(idxWidth.W)
      val redirect = Bool()
      val brUpdate = Valid(new BrUpdateInfo)
    }))

    // Give PC info to BranchUnit.
    val getPC = Vec(3, new GetPCFromFtqIO())

    val bpuUpdate = Output(Valid(new BranchPredictionUpdate))

  })
  val deq_ptr = RegInit(0.U(idxWidth.W))
  val enq_ptr = RegInit(1.U(idxWidth.W))

  val full = deq_ptr === WrapInc(enq_ptr, ftqNum)
  io.enq.ready := !full

  val ram = Reg(Vec(ftqNum, new FTQBundle))

  val do_enq = io.enq.fire

  when(do_enq) {
    val new_entry = Wire(new FTQBundle)
    new_entry.fetchPC  := io.enq.bits.pc
    new_entry.taken    := io.enq.bits.cfiIdx.valid
    new_entry.cfiIdx   := io.enq.bits.cfiIdx.bits
    new_entry.meta.bim := io.enq.bits.bpuMeta.bim
    new_entry.meta.btb := io.enq.bits.bpuMeta.btb
    ram(enq_ptr)       := new_entry
    enq_ptr            := WrapInc(enq_ptr, ftqNum)
  }

  io.enqIdx := enq_ptr

  when(io.deq.valid) {
    deq_ptr := io.deq.bits.idx
    when(io.deq.bits.redirect) {
      enq_ptr := WrapInc(io.deq.bits.idx, ftqNum)
    }
  }

  val bpuUpdate = Wire(new BranchPredictionUpdate)
  bpuUpdate.fetchPC   := ram(io.deq.bits.idx).fetchPC
  bpuUpdate.cfiIdx    := io.deq.bits.brUpdate.bits.cfiIdx.bits
  bpuUpdate.cfiIsBr   := io.deq.bits.brUpdate.bits.cfiIsBr
  bpuUpdate.cfiIsB    := io.deq.bits.brUpdate.bits.cfiIsB
  bpuUpdate.cfiIsJirl := io.deq.bits.brUpdate.bits.cfiIsJirl
  bpuUpdate.target    := io.deq.bits.brUpdate.bits.target
  bpuUpdate.cfiTaken  := io.deq.bits.brUpdate.bits.cfiIdx.valid
  bpuUpdate.meta      := ram(io.deq.bits.idx).meta

  io.bpuUpdate.valid := io.deq.valid && io.deq.bits.brUpdate.valid
  io.bpuUpdate.bits  := bpuUpdate

  // -------------------------------------------------------------
  // **** Core Read PC ****
  // -------------------------------------------------------------

  for (i <- 0 until 3) {
    val idx = io.getPC(i).ftqIdx
    io.getPC(i).info.valid := idx =/= enq_ptr
    io.getPC(i).info.entry := ram(idx)
  }
}
