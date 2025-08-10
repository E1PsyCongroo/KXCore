package KXCore.superscalar.core.frontend

import chisel3._
import chisel3.util._
import chisel3.util.random._
import KXCore.superscalar._
import KXCore.superscalar.core._

class FAMicroBTB(implicit params: CoreParameters) extends Module {
  import params.{commonParams, frontendParams, fetchIdx}
  import commonParams.{vaddrWidth, instWidth}
  import frontendParams.{fetchWidth, faubtbParams}
  import faubtbParams._
  require(tagWidth <= vaddrWidth - 2)

  def getTag(fetchPC: UInt): UInt = {
    fetchIdx(fetchPC)(tagWidth - 1, 0)
  }

  def bimWrite(v: UInt, taken: Bool): UInt = {
    val old_bim_sat_taken  = v === 3.U
    val old_bim_sat_ntaken = v === 0.U
    Mux(old_bim_sat_taken && taken, 3.U, Mux(old_bim_sat_ntaken && !taken, 0.U, Mux(taken, v + 1.U, v - 1.U)))
  }

  class MicroBTBMeta extends Bundle {
    val tag     = UInt(tagWidth.W)
    val ctr     = UInt(2.W)
    val cfiIdx  = UInt(log2Ceil(fetchWidth).W)
    val brMask  = UInt(fetchWidth.W)
    val jmpMask = UInt(fetchWidth.W)
  }

  class BTBPredictMeta extends Bundle {
    val writeWay = UInt(log2Ceil(nWays).W)
  }

  val io = IO(new Bundle {
    val req    = Flipped(Decoupled(UInt(vaddrWidth.W)))
    val resp   = Decoupled(Vec(fetchWidth, new BranchPrediction))
    val update = Flipped(Valid(new BranchPredictionUpdate))
  })

  val tag = getTag(io.req.bits)
  io.req.ready := io.resp.ready

  val valids = RegInit(VecInit(0.U(nWays.W).asBools))
  val meta   = Reg(Vec(nWays, new MicroBTBMeta))
  val btb    = Reg(Vec(nWays, Vec(if (useDualEntries) 2 else 1, UInt(vaddrWidth.W)))) // (jmpTarg, brTarg)

  val hitOH = (0 until nWays) map { w => valids(w) && meta(w).tag === tag }
  val hit   = hitOH.reduce(_ || _)

  val hitMeta = Mux1H(hitOH, meta)
  val hitBtb  = Mux1H(hitOH, btb)
  for (i <- 0 until fetchWidth) {
    val isCfi = hitMeta.cfiIdx === i.U
    io.resp.bits(i).target.valid := hit && ((isCfi && hitMeta.brMask(i)) || hitMeta.jmpMask(i))
    io.resp.bits(i).target.bits  := (if (useDualEntries) hitBtb(hitMeta.brMask(i)) else hitBtb(0))
    io.resp.bits(i).isBr         := hit && hitMeta.brMask(i)
    io.resp.bits(i).isJmp        := hit && hitMeta.jmpMask(i)
    io.resp.bits(i).taken        := hit && hitMeta.ctr(1)
  }
  io.resp.valid := io.req.valid

  val updateTag = getTag(io.update.bits.fetchPC)
  val updateHitOH = (0 until nWays) map { w =>
    meta(w).tag === updateTag && valids(w)
  }
  val updateHit      = updateHitOH.reduce(_ || _)
  val updateWriteWay = Mux(updateHit, OHToUInt(updateHitOH), GaloisLFSR.maxPeriod(log2Ceil(nWays)))

  when(io.update.valid) {
    if (useDualEntries) {
      btb(updateWriteWay)(io.update.bits.cfiIsBr) := io.update.bits.target
    } else {
      btb(updateWriteWay)(0) := io.update.bits.target
    }
    valids(updateWriteWay) := true.B
    val rmeta = meta(updateWriteWay)
    val wmeta = Wire(new MicroBTBMeta)
    wmeta.tag := updateTag
    when(!updateHit) {
      wmeta.brMask  := UIntToOH(io.update.bits.cfiIdx) & Fill(fetchWidth, io.update.bits.cfiIsBr)
      wmeta.ctr     := Mux(io.update.bits.cfiTaken, 3.U, 0.U)
      wmeta.jmpMask := UIntToOH(io.update.bits.cfiIdx) & Fill(fetchWidth, io.update.bits.cfiIsB || io.update.bits.cfiIsJirl)
    }.otherwise {
      wmeta.brMask  := rmeta.brMask | (UIntToOH(io.update.bits.cfiIdx) & Fill(fetchWidth, io.update.bits.cfiIsBr))
      wmeta.ctr     := Mux(io.update.bits.cfiIdx === rmeta.cfiIdx, bimWrite(rmeta.ctr, io.update.bits.cfiTaken), Mux(io.update.bits.cfiTaken, 3.U, 0.U))
      wmeta.jmpMask := rmeta.jmpMask | (UIntToOH(io.update.bits.cfiIdx) & Fill(fetchWidth, io.update.bits.cfiIsB || io.update.bits.cfiIsJirl))
    }
    wmeta.cfiIdx         := io.update.bits.cfiIdx
    meta(updateWriteWay) := wmeta
  }
}
