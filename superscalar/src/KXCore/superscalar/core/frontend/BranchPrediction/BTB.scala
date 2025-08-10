package KXCore.superscalar.core.frontend

import chisel3._
import chisel3.util._
import chisel3.util.random._
import KXCore.superscalar._
import KXCore.superscalar.core._

class BTB(implicit params: CoreParameters) extends Module {
  import params._
  import commonParams.{vaddrWidth, instBytes}
  import frontendParams._
  import btbParams._
  private val tagWidth = vaddrWidth - log2Ceil(nSets) - log2Ceil(instBytes)

  def getSet(fetchPC: UInt): UInt = {
    fetchIdx(fetchPC)(log2Ceil(nSets) - 1, 0)
  }

  def getTag(fetchPC: UInt): UInt = {
    fetchIdx(fetchPC) >> log2Ceil(nSets)
  }

  class BTBEntry extends Bundle {
    val offset   = SInt(offsetWidth.W)
    val extended = Bool()
  }

  class BTBMeta extends Bundle {
    val isBr = Bool()
    val tag  = UInt(tagWidth.W)
  }

  class BTBPredictMeta extends Bundle {
    val hits     = Vec(fetchWidth, Bool())
    val writeWay = UInt(log2Ceil(nWays).W)
  }

  class BTBStage0to1 extends Module {
    val io = IO(new Bundle {
      val flush = Input(Bool())
      val req   = Flipped(Decoupled(UInt(vaddrWidth.W)))
      val resp = Decoupled(new Bundle {
        val meta = Vec(nWays, Vec(fetchWidth, new BTBMeta))
        val btb  = Vec(nWays, Vec(fetchWidth, new BTBEntry))
        val ebtb = UInt(vaddrWidth.W)
      })
      val keepRead = Input(UInt(vaddrWidth.W))
      val update   = Flipped(Valid(new BranchPredictionUpdate))
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

    val meta = Seq.fill(nWays) { SyncReadMem(nSets, Vec(fetchWidth, new BTBMeta)) }
    val btb  = Seq.fill(nWays) { SyncReadMem(nSets, Vec(fetchWidth, new BTBEntry)) }
    val ebtb = SyncReadMem(extendedNSets, UInt(vaddrWidth.W))

    val readEn  = io.req.fire || nextEn
    val readSet = getSet(Mux(io.req.fire, io.req.bits, io.keepRead))

    val rmeta = VecInit(meta.map(m => m.read(readSet, readEn)))
    val rbtb  = VecInit(btb.map(m => m.read(readSet, readEn)))
    val rebtb = ebtb.read(readSet, readEn)

    val updateBits       = io.update.bits
    val updateSet        = getSet(updateBits.fetchPC)
    val updateTag        = getTag(updateBits.fetchPC)
    val updateMeta       = updateBits.meta.btb
    val maxOffset        = Cat(0.B, Fill(offsetWidth - 1, 1.B)).asSInt
    val minOffset        = Cat(1.B, Fill(offsetWidth - 1, 0.B)).asSInt
    val newOffset        = (updateBits.target.asSInt - (updateBits.fetchPC + (updateBits.cfiIdx << log2Ceil(instBytes))).asSInt)
    val offsetIsExtended = (newOffset > maxOffset || newOffset < minOffset)
    val updateBtbData    = Wire(new BTBEntry)
    updateBtbData.offset   := newOffset
    updateBtbData.extended := offsetIsExtended
    val updateBtbMask  = UIntToOH(updateBits.cfiIdx) & Fill(fetchWidth, io.update.valid && updateBits.cfiTaken)
    val updateMetaData = Wire(Vec(fetchWidth, new BTBMeta))
    val updateMetaMask = UIntToOH(updateBits.cfiIdx) & Fill(fetchWidth, io.update.valid && updateBits.cfiTaken)
    for (i <- 0 until fetchWidth) {
      updateMetaData(i).isBr := updateBits.cfiIsBr
      updateMetaData(i).tag  := updateTag
    }

    for (w <- 0 until nWays) {
      when(doingReset || updateMeta === w.U) {
        val resetBtbData = Wire(new BTBEntry)
        resetBtbData.offset   := 0.S
        resetBtbData.extended := false.B
        btb(w).write(
          Mux(doingReset, resetIdx, updateSet),
          Mux(doingReset, VecInit(Seq.fill(fetchWidth)(resetBtbData)), VecInit(Seq.fill(fetchWidth)(updateBtbData))),
          Mux(doingReset, Fill(fetchWidth, true.B), updateBtbMask.asUInt).asBools,
        )
        val resetMetaData = Wire(new BTBMeta)
        resetMetaData.isBr := false.B
        resetMetaData.tag  := 0.U
        meta(w).write(
          Mux(doingReset, resetIdx, updateSet),
          Mux(doingReset, VecInit(Seq.fill(fetchWidth)(resetMetaData)), updateMetaData),
          Mux(doingReset, Fill(fetchWidth, true.B), updateMetaMask.asUInt).asBools,
        )
      }
    }
    when(updateBtbMask =/= 0.U && offsetIsExtended) {
      ebtb.write(updateSet, updateBits.target)
    }

    io.resp.bits.meta := rmeta
    io.resp.bits.btb  := rbtb
    io.resp.bits.ebtb := rebtb
  }

  class BTBStage1 extends Module {
    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new Bundle {
        val vaddr = UInt(vaddrWidth.W)
        val meta  = Vec(nWays, Vec(fetchWidth, new BTBMeta))
        val btb   = Vec(nWays, Vec(fetchWidth, new BTBEntry))
        val ebtb  = UInt(vaddrWidth.W)
      }))
      val resp = Decoupled(new Bundle {
        val pred = Vec(fetchWidth, new BranchPrediction)
        val meta = new BTBPredictMeta
      })
    })

    io.req.ready  := io.resp.ready
    io.resp.valid := io.req.valid

    val tag = getTag(io.req.bits.vaddr)

    val hitOHs = VecInit((0 until fetchWidth).map { i =>
      VecInit((0 until nWays).map { w =>
        io.req.bits.meta(w)(i).tag === tag
      })
    })
    val hits    = hitOHs.map(_.reduce(_ || _))
    val hitWays = hitOHs.map(PriorityEncoder(_))

    for (i <- 0 until fetchWidth) {
      val hitWay = hitWays(i)
      val meta   = io.req.bits.meta(hitWay)(i)
      val btb    = io.req.bits.btb(hitWay)(i)
      io.resp.bits.pred(i).target.valid := hits(i)
      io.resp.bits.pred(i).target.bits  := Mux(btb.extended, io.req.bits.ebtb, (io.req.bits.vaddr.asSInt + (i << log2Ceil(instBytes)).S + btb.offset).asUInt)
      io.resp.bits.pred(i).isBr         := hits(i) && meta.isBr
      io.resp.bits.pred(i).isJmp        := hits(i) && !meta.isBr
      io.resp.bits.pred(i).taken        := DontCare
    }

    val allocWay = if (nWays > 1) {
      val metas   = Cat(VecInit(io.req.bits.meta.map(w => VecInit(w.map(_.tag)))).asUInt, tag)
      val l       = log2Ceil(nWays)
      val nChunks = (metas.getWidth + l - 1) / l
      val chunks = (0 until nChunks) map { i =>
        metas(math.min((i + 1) * l, metas.getWidth) - 1, i * l)
      }
      chunks.reduce(_ ^ _)
    } else {
      0.U
    }

    io.resp.bits.meta.hits     := hits
    io.resp.bits.meta.writeWay := Mux(hits.reduce(_ || _), PriorityEncoder(hitOHs.map(_.asUInt).reduce(_ | _)), allocWay)
  }

  val io = IO(new Bundle {
    val flush = Input(Bool())
    val req = new Bundle {
      val stage0 = Flipped(Decoupled(UInt(vaddrWidth.W)))
      val stage1 = Flipped(Valid(UInt(vaddrWidth.W)))
    }
    val resp = Decoupled(new Bundle {
      val pred = Vec(fetchWidth, new BranchPrediction)
      val meta = new BTBPredictMeta
    })
    val update = Flipped(Valid(new BranchPredictionUpdate))
  })

  val stage0to1 = Module(new BTBStage0to1)
  val stage1    = Module(new BTBStage1)

  stage0to1.io.flush := io.flush

  io.req.stage0         <> stage0to1.io.req
  stage0to1.io.update   := io.update
  stage0to1.io.keepRead := io.req.stage1.bits

  stage1.io.req.valid      := stage0to1.io.resp.valid && io.req.stage1.valid
  stage0to1.io.resp.ready  := stage1.io.req.ready
  stage1.io.req.bits.vaddr := io.req.stage1.bits
  stage1.io.req.bits.meta  := stage0to1.io.resp.bits.meta
  stage1.io.req.bits.btb   := stage0to1.io.resp.bits.btb
  stage1.io.req.bits.ebtb  := stage0to1.io.resp.bits.ebtb

  stage1.io.resp <> io.resp
}
