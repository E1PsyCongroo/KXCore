package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.superscalar._
import KXCore.superscalar.core._

class BusyResp extends Bundle {
  val prs1Busy = Bool()
  val prs2Busy = Bool()
}

class RenameBusyTable(val bypass: Boolean)(implicit params: CoreParameters) extends Module {
  import params.{commonParams, backendParams}
  import backendParams.{coreWidth, pregNum, pregWidth, wbPortNum}

  val io = IO(new Bundle {
    val uopReqs    = Input(Vec(coreWidth, new MicroOp))
    val busyResps  = Output(Vec(coreWidth, new BusyResp))
    val rebusyReqs = Input(Vec(coreWidth, Bool()))

    val wbPdsts  = Input(Vec(wbPortNum, UInt(pregWidth.W)))
    val wbValids = Input(Vec(wbPortNum, Bool()))

    val debug = Output(UInt(pregNum.W))
  })

  val busyTable = RegInit(0.U(pregNum.W))
  // Unbusy written back registers.
  val busyTableWb = busyTable & ~(io.wbPdsts zip io.wbValids)
    .map { case (pdst, valid) => UIntToOH(pdst) & Fill(pregNum, valid.asUInt) }
    .reduce(_ | _)
  // Rebusy newly allocated registers.
  val busyTableNext = busyTableWb | (io.uopReqs zip io.rebusyReqs)
    .map { case (uop, req) => UIntToOH(uop.pdst) & Fill(pregNum, req.asUInt) }
    .reduce(_ | _)

  busyTable := busyTableNext

  // Read the busy table.
  for (i <- 0 until coreWidth) {
    val prs1Bypassed = (0 until i).map(j => io.uopReqs(i).lrs1 === io.uopReqs(j).ldst && io.rebusyReqs(j)).foldLeft(false.B)(_ || _)
    val prs2Bypassed = (0 until i).map(j => io.uopReqs(i).lrs2 === io.uopReqs(j).ldst && io.rebusyReqs(j)).foldLeft(false.B)(_ || _)

    io.busyResps(i).prs1Busy := busyTable(io.uopReqs(i).prs1) || (prs1Bypassed && bypass.B)
    io.busyResps(i).prs2Busy := busyTable(io.uopReqs(i).prs2) || (prs2Bypassed && bypass.B)
  }

  io.debug := busyTable
}
