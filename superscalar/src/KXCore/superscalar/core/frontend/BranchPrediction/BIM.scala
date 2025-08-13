package KXCore.superscalar.core.frontend

import chisel3._
import chisel3.util._
import chisel3.util.random._
import KXCore.superscalar._
import KXCore.superscalar.core._

class BIM(implicit params: CoreParameters) extends Module {
  import params._
  import commonParams.{vaddrWidth}
  import frontendParams._
  import bimParams._

  def getSet(fetchPC: UInt): UInt = {
    fetchIdx(fetchPC)(log2Ceil(nSets) - 1, 0)
  }

  def bimWrite(v: UInt, taken: Bool): UInt = {
    val old_bim_sat_taken  = v === 3.U
    val old_bim_sat_ntaken = v === 0.U
    Mux(old_bim_sat_taken && taken, 3.U, Mux(old_bim_sat_ntaken && !taken, 0.U, Mux(taken, v + 1.U, v - 1.U)))
  }

  class BIMMeta(implicit params: CoreParameters) extends Bundle {
    val bims = Vec(fetchWidth, UInt(2.W))
  }

  class BIMStage0to1 extends Module {
    val io = IO(new Bundle {
      val flush  = Input(Bool())
      val req    = Flipped(Decoupled(UInt(vaddrWidth.W)))
      val resp   = Decoupled(new BIMMeta)
      val update = Flipped(Valid(new BranchPredictionUpdate))
    })

    val doingReset = RegInit(true.B)
    val resetIdx   = RegInit(0.U(log2Ceil(nSets).W))
    resetIdx := resetIdx + doingReset
    when(resetIdx === (nSets - 1).U) { doingReset := false.B }

    val en     = RegInit(false.B)
    val nextEn = WireDefault(en & ~io.resp.ready)
    en            := Mux(io.req.ready, io.req.valid, nextEn)
    io.req.ready  := ((nextEn === 0.U) || io.flush) && !doingReset
    io.resp.valid := en && !io.flush

    val holdReqReg = RegEnable(io.req.bits, io.req.ready)
    val readSet    = getSet(Mux(io.req.ready, io.req.bits, holdReqReg))

    val metas = SyncReadMem(nSets, Vec(fetchWidth, UInt(2.W)))
    val bims  = metas.read(readSet)

    val updateWdata = Wire(Vec(fetchWidth, UInt(2.W)))
    val updateWmask = Wire(Vec(fetchWidth, Bool()))
    val updateBits  = io.update.bits
    val updateIndex = getSet(updateBits.fetchPC)
    val updateMeta  = updateBits.meta.bim

    for (w <- 0 until fetchWidth) {
      updateWmask(w) := false.B
      updateWdata(w) := 2.U

      // TODO: fix this logic
      when(io.update.valid && io.update.bits.cfiIdx === w.U) {
        val isTaken = io.update.bits.cfiTaken
        updateWmask(w) := true.B
        updateWdata(w) := bimWrite(updateMeta(w), isTaken)
      }
    }

    when(doingReset || io.update.valid) {
      metas.write(
        Mux(doingReset, resetIdx, updateIndex),
        Mux(doingReset, VecInit(Seq.fill(fetchWidth) { 2.U }), updateWdata),
        Mux(doingReset, Fill(fetchWidth, true.B), updateWmask.asUInt).asBools,
      )
    }

    io.resp.bits.bims := bims
  }

  class BIMStage1 extends Module {
    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new BIMMeta))
      val resp = Decoupled(new Bundle {
        val pred = Vec(fetchWidth, new BranchPrediction)
        val meta = Vec(fetchWidth, UInt(2.W))
      })
    })

    io.req.ready  := io.resp.ready
    io.resp.valid := io.req.valid
    for (i <- 0 until fetchWidth) {
      io.resp.bits.pred(i)       := DontCare
      io.resp.bits.pred(i).taken := io.req.bits.bims(i)(1)
      io.resp.bits.meta(i)       := io.req.bits.bims(i)
    }
  }

  val io = IO(new Bundle {
    val flush = Input(Bool())
    val req   = Flipped(Decoupled(UInt(vaddrWidth.W)))
    val resp = Decoupled(new Bundle {
      val pred = Vec(fetchWidth, new BranchPrediction)
      val meta = Vec(fetchWidth, UInt(2.W))
    })
    val update = Flipped(Valid(new BranchPredictionUpdate))
  })

  val stage0to1 = Module(new BIMStage0to1)
  val stage1    = Module(new BIMStage1)

  stage0to1.io.flush  := io.flush
  stage0to1.io.update := io.update

  io.req         <> stage0to1.io.req
  stage1.io.req  <> stage0to1.io.resp
  stage1.io.resp <> io.resp
}
