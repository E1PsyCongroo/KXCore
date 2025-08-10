package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core._

/** IO bundle to interact with the issue unit
  */
class IssueUnitIO(issueParams: IssueParams)(implicit params: CoreParameters) extends Bundle {
  import params.{backendParams}
  import backendParams.{pregWidth, wbPortNum}
  import issueParams.{dispatchWidth, issueWidth, numEntries}

  val flush        = Input(Bool())
  val dis_uops     = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))
  val iss_uops     = Vec(issueWidth, Decoupled(new MicroOp()))
  val wakeup_ports = Flipped(Vec(wbPortNum, Valid(UInt(pregWidth.W))))
  // tell the issue unit what each execution pipeline has in terms of functional units
  val fu_types = Input(Vec(issueWidth, UInt(FUType.getWidth.W)))

  // val event_empty = Output(Bool()) // used by HPM events; is the issue unit empty?
}

/** Abstract top level issue unit
  */
abstract class IssueUnit(issueParams: IssueParams)(implicit params: CoreParameters) extends Module {
  import issueParams.{dispatchWidth, issueWidth, numEntries, iqType}

  val io = IO(new IssueUnitIO(issueParams))

  // -------------------------------------------------------------
  // Set up the dispatch uops
  // special case "storing" 2 uops within one issue slot.

  val dis_uops = Wire(Vec(dispatchWidth, new MicroOp()))
  for (w <- 0 until dispatchWidth) {
    dis_uops(w) := io.dis_uops(w).bits
  }

  // -------------------------------------------------------------
  // Issue Table

  val slots       = Seq.fill(numEntries)(Module(new IssueSlot))
  val issue_slots = VecInit(slots.map(_.io))

  for (i <- 0 until numEntries) {
    issue_slots(i).wakeup_ports := io.wakeup_ports
    issue_slots(i).kill         := io.flush
  }

  // io.event_empty := !(issue_slots.map(s => s.valid).reduce(_ | _))

  val count = PopCount(slots.map(_.io.valid))
  dontTouch(count)

  assert(PopCount(issue_slots.map(s => s.grant)) <= issueWidth.U, "[issue] window giving out too many grants.")
}
