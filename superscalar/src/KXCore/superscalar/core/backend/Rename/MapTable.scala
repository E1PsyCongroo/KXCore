package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.superscalar._
import KXCore.superscalar.core._

class MapReq(implicit val params: CoreParameters) extends Bundle {
  val lrs1 = UInt(params.backendParams.lregWidth.W)
  val lrs2 = UInt(params.backendParams.lregWidth.W)
  val ldst = UInt(params.backendParams.lregWidth.W)
}

class MapResp(implicit val params: CoreParameters) extends Bundle {
  val prs1      = UInt(params.backendParams.pregWidth.W)
  val prs2      = UInt(params.backendParams.pregWidth.W)
  val stalePdst = UInt(params.backendParams.pregWidth.W)
}

class RemapReq(implicit val params: CoreParameters) extends Bundle {
  val ldst  = UInt(params.backendParams.lregWidth.W)
  val pdst  = UInt(params.backendParams.pregWidth.W)
  val valid = Bool()
}

class RenameMapTable(val bypass: Boolean)(implicit params: CoreParameters) extends Module {
  import params.{commonParams, backendParams}
  import backendParams.{coreWidth, lregNum, pregNum, pregWidth}

  val io = IO(new Bundle {
    // Logical sources -> physical sources.
    val mapReqs  = Input(Vec(coreWidth, new MapReq))
    val mapResps = Output(Vec(coreWidth, new MapResp))

    // Remapping an ldst to a newly allocated pdst?
    val renRemapReqs = Input(Vec(coreWidth, new RemapReq))
    val comRemapReqs = Input(Vec(coreWidth, new RemapReq))

    // Signals for restoring state
    val rollback = Input(Bool())

    val debug = Output(Vec(lregNum, UInt(pregWidth.W)))
  })

  // The map table register array
  val renMapTable = RegInit(VecInit((0 until lregNum) map { _.U(pregWidth.W) }))
  val comMapTable = RegInit(VecInit((0 until lregNum) map { _.U(pregWidth.W) }))

  // The intermediate states of the map table following modification by each pipeline slot.
  val renRemapTable = Wire(Vec(coreWidth + 1, Vec(lregNum, UInt(pregWidth.W))))
  val comRemapTable = Wire(Vec(coreWidth + 1, Vec(lregNum, UInt(pregWidth.W))))

  // Uops requesting changes to the map table.
  val renRemapPdsts   = io.renRemapReqs.map(_.pdst)
  val renRemapLdstsOH = io.renRemapReqs.map(req => UIntToOH(req.ldst) & Fill(lregNum, req.valid.asUInt))

  val comRemapPdsts   = io.comRemapReqs.map(_.pdst)
  val comRemapLdstsOH = io.comRemapReqs.map(req => UIntToOH(req.ldst) & Fill(lregNum, req.valid.asUInt))

  // Figure out the new mappings seen by each pipeline slot.
  for (i <- 0 until lregNum) {
    val renRemappedRow = (renRemapLdstsOH.map(ldst => ldst(i)) zip renRemapPdsts)
      .scanLeft(renMapTable(i)) { case (pdst, (ldst, new_pdst)) => Mux(ldst, new_pdst, pdst) }

    val comRemappedRow = (comRemapLdstsOH.map(ldst => ldst(i)) zip comRemapPdsts)
      .scanLeft(comMapTable(i)) { case (pdst, (ldst, new_pdst)) => Mux(ldst, new_pdst, pdst) }

    for (j <- 0 until coreWidth + 1) {
      renRemapTable(j)(i) := renRemappedRow(j)
      comRemapTable(j)(i) := comRemappedRow(j)
    }
  }

  // Update mappings.
  renMapTable := Mux(io.rollback, comRemapTable(coreWidth), renRemapTable(coreWidth))
  comMapTable := comRemapTable(coreWidth)

  // Read out mappings.
  for (i <- 0 until coreWidth) {
    io.mapResps(i).prs1 := (0 until coreWidth).foldLeft(renMapTable(io.mapReqs(i).lrs1))((p, k) =>
      Mux(bypass.B && io.renRemapReqs(k).valid && io.renRemapReqs(k).ldst === io.mapReqs(i).lrs1, io.renRemapReqs(k).pdst, p),
    )
    io.mapResps(i).prs2 := (0 until coreWidth).foldLeft(renMapTable(io.mapReqs(i).lrs2))((p, k) =>
      Mux(bypass.B && io.renRemapReqs(k).valid && io.renRemapReqs(k).ldst === io.mapReqs(i).lrs2, io.renRemapReqs(k).pdst, p),
    )
    io.mapResps(i).stalePdst := (0 until coreWidth).foldLeft(renMapTable(io.mapReqs(i).ldst))((p, k) =>
      Mux(bypass.B && io.renRemapReqs(k).valid && io.renRemapReqs(k).ldst === io.mapReqs(i).ldst, io.renRemapReqs(k).pdst, p),
    )
  }

  io.renRemapReqs.map(req => (req.pdst, req.valid)).foreach { case (p, r) =>
    assert(!r || !renMapTable.contains(p), "[maptable] Trying to write a duplicate mapping.")
  }

  io.debug := comMapTable

}
