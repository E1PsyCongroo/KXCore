package KXCore.superscalar.core.frontend

import scala.math.max
import chisel3._
import chisel3.util._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._

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

  val rowNum = fbNum / coreWidth

  val uops    = Reg(Vec(fbNum, new MicroOp))
  val deq_vec = Wire(Vec(rowNum, Vec(coreWidth, new MicroOp)))

  val head      = RegInit(1.U(rowNum.W)) // deq_ptr one-hot
  val tail      = RegInit(1.U(fbNum.W))  // enq_ptr one-hot
  val maybeFull = RegInit(false.B)

  // -------------------------------------------------------------
  // **** Enqueue Uops ****
  // -------------------------------------------------------------
  // Step 1: Convert FetchPacket into a vector of MicroOps.
  // Step 2: Generate one-hot write indices.
  // Step 3: Write MicroOps into the RAM.

  def rotateLeft(in: UInt, k: Int) = {
    val n = in.getWidth
    Cat(in(n - k - 1, 0), in(n - 1, n - k))
  }

  val mightHitHead = (1 until fetchWidth)
    .map(k => VecInit(rotateLeft(tail, k).asBools.zipWithIndex.filter { case (e, i) => i % coreWidth == 0 }.map { case (e, i) => e }).asUInt)
    .map(tail => head & tail)
    .reduce(_ | _)
    .orR
  val atHead = (VecInit(
    tail.asBools.zipWithIndex
      .filter { case (e, i) => i % coreWidth == 0 }
      .map { case (e, i) => e },
  ).asUInt & head).orR

  val do_enq = !(atHead && maybeFull || mightHitHead)

  io.enq.ready := do_enq

  val in_mask = Wire(Vec(fetchWidth, Bool()))
  val in_uops = Wire(Vec(fetchWidth, new MicroOp()))

  for (i <- 0 until fetchWidth) {
    val pc = io.enq.bits.pcs(i)
    in_uops(i)      := DontCare
    in_mask(i)      := io.enq.valid && io.enq.bits.mask(i)
    in_uops(i).idx  := i.U
    in_uops(i).inst := io.enq.bits.insts(i)

    in_uops(i).ftqIdx := io.enq.bits.ftqIdx
    in_uops(i).isBr   := io.enq.bits.brMask(i)
    in_uops(i).isB    := io.enq.bits.bMask(i)
    in_uops(i).isJirl := io.enq.bits.jirlMask(i)

    in_uops(i).exception := io.enq.bits.exception.valid
    in_uops(i).ecode     := io.enq.bits.exception.bits
    in_uops(i).badv      := io.enq.bits.pc

    in_uops(i).debug.pc   := pc
    in_uops(i).debug.inst := io.enq.bits.insts(i)
  }

  // Step 2. Generate one-hot write indices.
  val enq_idxs = Wire(Vec(fetchWidth, UInt(fbNum.W)))

  def inc(ptr: UInt) = {
    val n = ptr.getWidth
    Cat(ptr(n - 2, 0), ptr(n - 1))
  }

  var enq_idx = tail
  for (i <- 0 until fetchWidth) {
    enq_idxs(i) := enq_idx
    enq_idx = Mux(in_mask(i), inc(enq_idx), enq_idx)
  }

  // Step 3: Write MicroOps into the RAM.
  for (i <- 0 until fetchWidth) {
    for (j <- 0 until fbNum) {
      when(do_enq && in_mask(i) && enq_idxs(i)(j)) {
        uops(j) := in_uops(i)
      }
    }
  }

  // -------------------------------------------------------------
  // **** Dequeue Uops ****
  // -------------------------------------------------------------

  val tail_collisions    = VecInit((0 until fbNum).map(i => head(i / coreWidth) && (!maybeFull || (i % coreWidth != 0).B))).asUInt & tail
  val slot_will_hit_tail = (0 until rowNum).map(i => tail_collisions((i + 1) * coreWidth - 1, i * coreWidth)).reduce(_ | _)
  val will_hit_tail      = slot_will_hit_tail.orR

  val do_deq = io.deq.ready && !will_hit_tail

  val deq_valids = (~MaskUpper(slot_will_hit_tail)).asBools

  // Generate vec for dequeue read port.
  for (i <- 0 until fbNum) {
    deq_vec(i / coreWidth)(i % coreWidth) := uops(i)
  }

  io.deq.bits.uops zip deq_valids map { case (d, v) => d.valid := v }
  io.deq.bits.uops zip Mux1H(head, deq_vec) map { case (d, q) => d.bits := q }
  io.deq.valid := deq_valids.reduce(_ || _)

  // -------------------------------------------------------------
  // **** Update State ****
  // -------------------------------------------------------------

  when(do_enq) {
    tail := enq_idx
    when(in_mask.reduce(_ || _)) {
      maybeFull := true.B
    }
  }

  when(do_deq) {
    head      := inc(head)
    maybeFull := false.B
  }

  when(io.flush) {
    head      := 1.U
    tail      := 1.U
    maybeFull := false.B
  }

}
