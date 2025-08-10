package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.superscalar._
import KXCore.common.utils._
import KXCore.superscalar.core._

class RenameFreeList(
    val allocWidth: Int,
    val deallocWidth: Int,
)(implicit params: CoreParameters)
    extends Module {
  import params.{commonParams, backendParams}
  import backendParams.{lregNum, pregNum, pregWidth}
  private val n = pregNum

  val io = IO(new Bundle {
    // Physical register requests.
    val allocPregs = Vec(allocWidth, Decoupled(UInt(pregWidth.W)))

    // Pregs returned by the ROB.
    val despec  = Input(Vec(deallocWidth, Valid(UInt(pregWidth.W))))
    val dealloc = Input(Vec(deallocWidth, Valid(UInt(pregWidth.W))))

    val rollback = Input(Bool())

    val debug = Output(UInt(pregNum.W))
  })

  // The free list register array and its allocation lists.
  val freeList      = RegInit(UInt(pregNum.W), Cat(~(0.U((pregNum - lregNum).W)), 0.U(lregNum.W)))
  val specAllocList = RegInit(0.U(pregNum.W))

  // Select pregs from the free list.
  val selects    = SelectFirstN(freeList, allocWidth)
  val selectFire = Wire(Vec(allocWidth, Bool()))

  // Masks that modify the freelist array.
  val selectMask = (selects zip selectFire).map { case (s, f) => s & Fill(n, f) }.reduce(_ | _)
  val allocMask  = selectMask

  val comDeallocs      = io.dealloc.map(d => UIntToOH(d.bits)(n - 1, 0) & Fill(n, d.valid)).reduce(_ | _)
  val rollbackDeallocs = specAllocList & Fill(n, io.rollback)
  val comDespec        = io.despec.map(d => UIntToOH(d.bits)(n - 1, 0) & Fill(n, d.valid)).reduce(_ | _)
  val deallocMask      = comDeallocs | rollbackDeallocs

  // Update the free list.
  freeList      := ((freeList & ~allocMask) | deallocMask) & ~comDespec
  specAllocList := ((specAllocList | allocMask) & ~deallocMask) & ~comDespec

  for (w <- 0 until allocWidth) {
    val valid  = selects(w).orR
    val select = OHToUInt(selects(w))

    selectFire(w) := valid && io.allocPregs(w).ready

    io.allocPregs(w).bits  := select
    io.allocPregs(w).valid := valid && !io.rollback
  }

  io.debug := freeList | io.allocPregs.map(p => UIntToOH(p.bits)(n - 1, 0) & Fill(n, p.valid)).reduce(_ | _)

  assert(!(io.debug & deallocMask).orR, "[freelist] Returning a free physical register.")
  assert(!freeList(0), "[freelist] Preg number 0 never be free.")
}
