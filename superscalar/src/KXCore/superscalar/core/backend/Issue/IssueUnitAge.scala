package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core._

/** Specific type of issue unit
  *
  * @param issueParams
  *   issue queue params
  */
class IssueUnitCollapsing(issueParams: IssueParams)(implicit params: CoreParameters) extends IssueUnit(issueParams) {
  import issueParams._
  // -------------------------------------------------------------
  // Figure out how much to shift entries by

  val maxShift  = dispatchWidth
  val vacants   = issue_slots.map(!_.valid) ++ io.dis_uops.map(!_.valid)
  val shamts_oh = Wire(Vec(numEntries + dispatchWidth, UInt(maxShift.W)))
  // track how many to shift up this entry by counting previous vacant spots
  def SaturatingCounterOH(count_oh: UInt, inc: Bool, max: Int): UInt = {
    val next = Wire(UInt(max.W))
    next := count_oh
    when(count_oh === 0.U && inc) {
      next := 1.U
    }.elsewhen(!count_oh(max - 1) && inc) {
      next := (count_oh << 1.U)
    }
    next
  }
  shamts_oh(0) := 0.U
  for (i <- 1 until numEntries + dispatchWidth) {
    shamts_oh(i) := SaturatingCounterOH(shamts_oh(i - 1), vacants(i - 1), maxShift)
  }

  // -------------------------------------------------------------

  // which entries' uops will still be next cycle? (not being issued and vacated)
  val issue_slots_will_be_valid = issue_slots.map(slot => slot.valid && (!slot.request || !slot.grant))
  val will_be_valid             = issue_slots_will_be_valid ++ io.dis_uops.map(_.valid)

  val uops = issue_slots.map(s => s.out_uop) ++ dis_uops.map(s => s)
  for (i <- 0 until numEntries) {
    issue_slots(i).in_uop.valid := false.B
    issue_slots(i).in_uop.bits  := uops(i + 1)
    for (j <- 1 to maxShift) {
      when(shamts_oh(i + j) === (1 << (j - 1)).U) {
        issue_slots(i).in_uop.valid := will_be_valid(i + j)
        issue_slots(i).in_uop.bits  := uops(i + j)
      }
    }
    issue_slots(i).clear := shamts_oh(i) =/= 0.U
  }

  // -------------------------------------------------------------
  // Dispatch/Entry Logic
  // did we find a spot to slide the new dispatched uops into?

  val will_be_available = RegNext(
    VecInit((issue_slots zip issue_slots_will_be_valid).map { case (slot, will_be_valid) =>
      (!will_be_valid || slot.clear) && !slot.in_uop.valid
    }),
  )
  val num_available = PopCount(will_be_available)
  for (w <- 0 until dispatchWidth) {
    io.dis_uops(w).ready := num_available > w.U
  }

  // -------------------------------------------------------------
  // Issue Select Logic

  // set default
  for (w <- 0 until issueWidth) {
    io.iss_uops(w).valid := false.B
    io.iss_uops(w).bits  := DontCare
  }

  val requests    = issue_slots.map(s => s.request)
  val port_issued = Array.fill(issueWidth) { false.B }
  for (i <- 0 until numEntries) {
    issue_slots(i).grant := false.B
    var uop_issued = false.B

    for (w <- 0 until issueWidth) {
      val can_allocate = (issue_slots(i).uop.fuType & io.fu_types(w)) =/= 0.U
      when(requests(i) && !uop_issued && can_allocate && !port_issued(w)) {
        issue_slots(i).grant := io.iss_uops(w).ready
        io.iss_uops(w).valid := requests(i)
        io.iss_uops(w).bits  := issue_slots(i).uop
      }
      val was_port_issued_yet = port_issued(w)
      port_issued(w) = (requests(i) && !uop_issued && can_allocate) | port_issued(w)
      uop_issued = (requests(i) && can_allocate && !was_port_issued_yet) | uop_issued
    }
  }
}
