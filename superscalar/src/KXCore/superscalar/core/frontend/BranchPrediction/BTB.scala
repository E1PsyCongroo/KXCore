package KXCore.superscalar.core.frontend

import java.io._
import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.util.experimental._
import firrtl.annotations.MemoryLoadFileType
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._

object BTB {
  def getSet(fetchPC: UInt)(implicit params: CoreParameters): UInt = {
    import params.frontendParams.btbParams._
    params.fetchIdx(fetchPC)(log2Ceil(nSets) - 1, 0)
  }

  def getTag(fetchPC: UInt)(implicit params: CoreParameters): UInt = {
    import params.frontendParams.btbParams._
    params.fetchIdx(fetchPC) >> log2Ceil(nSets)
  }

  class BTBEntry(implicit params: CoreParameters) extends Bundle {
    val offset   = UInt(params.frontendParams.btbParams.offsetWidth.W)
    val extended = Bool()
  }

  class BTBMeta(implicit params: CoreParameters) extends Bundle {
    import params.{fetchBytes, commonParams, frontendParams}
    import commonParams.{vaddrWidth}
    import frontendParams._
    import btbParams._
    private val tagWidth = vaddrWidth - log2Ceil(nSets) - log2Ceil(fetchBytes)

    val isBr = Bool()
    val tag  = UInt(tagWidth.W)
  }

  def generateMetaHexFile(implicit params: CoreParameters): String = {
    import params.{frontendParams}
    import frontendParams.{fetchWidth, btbParams}
    import btbParams.{nSets}

    val fileName = s"btb_meta_${nSets}_${fetchWidth}.mem"
    val hexFile  = new File("build", fileName)

    if (!hexFile.exists()) {
      val writer = new PrintWriter(hexFile)
      try {
        val entryBits = "0" * (new BTBMeta).getWidth * fetchWidth
        for (_ <- 0 until nSets) {
          writer.println(entryBits)
        }
      } finally {
        writer.close()
      }
    }
    fileName
  }

  def generateEntryHexFile(implicit params: CoreParameters): String = {
    import params.{frontendParams}
    import frontendParams.{fetchWidth, btbParams}
    import btbParams.{nSets}

    val fileName = s"btb_entry_${nSets}_${fetchWidth}.mem"
    val hexFile  = new File("build", fileName)

    if (!hexFile.exists()) {
      val writer = new PrintWriter(hexFile)
      try {
        val entryBits = "0" * (new BTBEntry).getWidth * fetchWidth
        for (_ <- 0 until nSets) {
          writer.println(entryBits)
        }
      } finally {
        writer.close()
      }
    }
    fileName
  }

  class BTBStorage(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import commonParams.{vaddrWidth, instBytes}
    import frontendParams._
    import btbParams._

    private val metaInitFile  = generateMetaHexFile
    private val entryInitFile = generateEntryHexFile

    val io = IO(new Bundle {
      val read = new Bundle {
        val en   = Input(Bool())
        val set  = Input(UInt(log2Ceil(nSets).W))
        val meta = Output(Vec(nWays, Vec(fetchWidth, new BTBMeta)))
        val btb  = Output(Vec(nWays, Vec(fetchWidth, new BTBEntry)))
        val ebtb = Output(UInt(vaddrWidth.W))
      }
      val update = Flipped(new BranchPredictionUpdate)
    })

    val metas = Seq.fill(nWays) { SyncReadMem(nSets, Vec(fetchWidth, UInt((new BTBMeta).getWidth.W))) }
    metas.foreach(meta => loadMemoryFromFileInline(meta, metaInitFile, MemoryLoadFileType.Binary))
    val btbs = Seq.fill(nWays) { SyncReadMem(nSets, Vec(fetchWidth, UInt((new BTBEntry).getWidth.W))) }
    btbs.foreach(btb => loadMemoryFromFileInline(btb, entryInitFile, MemoryLoadFileType.Binary))
    val ebtbs = SyncReadMem(extendedNSets, UInt(vaddrWidth.W))

    io.read.meta := VecInit(metas.map(meta => VecInit(meta.read(io.read.set, io.read.en).map(_.asTypeOf(new BTBMeta)))))
    io.read.btb  := VecInit(btbs.map(btb => VecInit(btb.read(io.read.set, io.read.en).map(_.asTypeOf(new BTBEntry)))))
    io.read.ebtb := ebtbs.read(io.read.set, io.read.en)

    val updateSet = getSet(io.update.fetchPC)
    val updateTag = getTag(io.update.fetchPC)
    val updateWay = io.update.meta.btb

    val maxOffset = Cat(0.B, Fill(offsetWidth - 1, 1.B)).asSInt
    val minOffset = Cat(1.B, Fill(offsetWidth - 1, 0.B)).asSInt
    val newOffset = io.update.target.asSInt -
      (params.fetchAlign(io.update.fetchPC) + (io.update.cfiIdx.bits << log2Ceil(instBytes))).asSInt
    val offsetIsExtended = (newOffset > maxOffset || newOffset < minOffset)
    val updateBtbData    = Wire(new BTBEntry)
    updateBtbData.offset   := newOffset.asUInt
    updateBtbData.extended := offsetIsExtended
    val updateBtbMask = UIntToOH(io.update.cfiIdx.valid) & Fill(fetchWidth, io.update.cfiIdx.valid)

    val updateMetaData = Wire(Vec(fetchWidth, new BTBMeta))
    for (i <- 0 until fetchWidth) {
      updateMetaData(i).isBr := io.update.brMask(i)
      updateMetaData(i).tag  := updateTag
    }
    val updateMetaMask = updateBtbMask | io.update.brMask

    for (w <- 0 until nWays) {
      when(io.update.valid && updateWay === w.U && !(io.read.en && io.read.set === updateSet)) {
        btbs(w).write(
          updateSet,
          VecInit.fill(fetchWidth)(updateBtbData.asUInt),
          updateBtbMask.asBools,
        )
        metas(w).write(
          updateSet,
          VecInit(updateMetaData.map(_.asUInt)),
          updateMetaMask.asBools,
        )
      }
    }
    when(io.update.valid && updateBtbMask =/= 0.U && !(io.read.en && io.read.set === updateSet) && offsetIsExtended) {
      ebtbs.write(updateSet, io.update.target)
    }

    if (params.debug) {
      dontTouch(updateSet)
      dontTouch(updateWay)
      dontTouch(updateTag)
      dontTouch(newOffset)
      dontTouch(updateBtbData)
      dontTouch(updateBtbMask)
      dontTouch(updateMetaData)
      dontTouch(updateMetaMask)
    }
  }

  class BTBStage0to1(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import commonParams.{vaddrWidth, instBytes}
    import frontendParams._
    import btbParams._

    val io = IO(new Bundle {
      val flush = Input(Bool())
      val infoRead = new Bundle {
        val en   = Output(Bool())
        val set  = Output(UInt(log2Ceil(nSets).W))
        val meta = Input(Vec(nWays, Vec(fetchWidth, new BTBMeta)))
        val btb  = Input(Vec(nWays, Vec(fetchWidth, new BTBEntry)))
        val ebtb = Input(UInt(vaddrWidth.W))
      }
      val holdRead = Input(UInt(vaddrWidth.W))
      val req      = Flipped(Decoupled(UInt(vaddrWidth.W)))
      val resp = Decoupled(new Bundle {
        val meta = Vec(nWays, Vec(fetchWidth, new BTBMeta))
        val btb  = Vec(nWays, Vec(fetchWidth, new BTBEntry))
        val ebtb = UInt(vaddrWidth.W)
      })
    })

    val en     = RegInit(false.B)
    val nextEn = WireDefault(en & ~io.resp.ready)
    en            := Mux(io.req.ready, io.req.valid, nextEn)
    io.req.ready  := (nextEn === 0.U) || io.flush
    io.resp.valid := en && !io.flush

    io.infoRead.en    := io.req.fire || en
    io.infoRead.set   := getSet(Mux(io.req.ready, io.req.bits, io.holdRead))
    io.resp.bits.meta := io.infoRead.meta
    io.resp.bits.btb  := io.infoRead.btb
    io.resp.bits.ebtb := io.infoRead.ebtb
  }

  class BTBStage1(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import commonParams.{vaddrWidth, instBytes, instWidth}
    import frontendParams._
    import btbParams._

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new Bundle {
        val fetchPC = UInt(vaddrWidth.W)
        val meta    = Vec(nWays, Vec(fetchWidth, new BTBMeta))
        val btb     = Vec(nWays, Vec(fetchWidth, new BTBEntry))
        val ebtb    = UInt(vaddrWidth.W)
      }))
      val resp = Decoupled(new Bundle {
        val pred = Vec(fetchWidth, new BranchPrediction)
        val meta = UInt(log2Ceil(nWays).W)
      })
    })

    io.req.ready  := io.resp.ready
    io.resp.valid := io.req.valid

    val tag = getTag(io.req.bits.fetchPC)
    val hitOHs = VecInit((0 until fetchWidth).map { i =>
      VecInit((0 until nWays).map { w =>
        io.req.bits.meta(w)(i).tag === tag
      })
    })
    val hits    = VecInit(hitOHs.map(_.reduce(_ || _)))
    val hitWays = VecInit(hitOHs.map(PriorityEncoder(_)))

    for (i <- 0 until fetchWidth) {
      val hitWay = hitWays(i)
      val meta   = io.req.bits.meta(hitWay)(i)
      val btb    = io.req.bits.btb(hitWay)(i)
      io.resp.bits.pred(i).target.valid := hits(i)
      io.resp.bits.pred(i).target.bits := Mux(
        btb.extended,
        io.req.bits.ebtb,
        params.fetchAlign(io.req.bits.fetchPC) + (i << log2Ceil(instBytes)).U +
          Sext(btb.offset, instWidth),
      )
      io.resp.bits.pred(i).isBr  := hits(i) && meta.isBr
      io.resp.bits.pred(i).isJmp := hits(i) && !meta.isBr
      io.resp.bits.pred(i).taken := DontCare
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

    io.resp.bits.meta := Mux(hits.reduce(_ || _), PriorityEncoder(hitOHs.map(_.asUInt).reduce(_ | _)), allocWay)

    dontTouch(tag)
    dontTouch(hitOHs)
    dontTouch(hits)
    dontTouch(hitWays)
  }

}
