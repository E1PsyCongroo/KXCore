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

object BIM {
  def getSet(fetchPC: UInt)(implicit params: CoreParameters): UInt = {
    params.fetchIdx(fetchPC)(log2Ceil(params.frontendParams.bimParams.nSets) - 1, 0)
  }

  def bimWrite(v: UInt, taken: Bool): UInt = {
    val old_bim_sat_taken  = v === 3.U
    val old_bim_sat_ntaken = v === 0.U
    Mux(old_bim_sat_taken && taken, 3.U, Mux(old_bim_sat_ntaken && !taken, 0.U, Mux(taken, v + 1.U, v - 1.U)))
  }

  class BIMMeta(implicit params: CoreParameters) extends Bundle {
    val bims = Vec(params.frontendParams.fetchWidth, UInt(2.W))
  }

  def generateMetaHexFile(implicit params: CoreParameters): String = {
    import params.{frontendParams}
    import frontendParams.{fetchWidth, bimParams}
    import bimParams.{nSets}

    val fileName = s"bim_meta_${nSets}_${fetchWidth}.mem"
    val hexFile  = new File("build", fileName)

    if (!hexFile.exists()) {
      val writer = new PrintWriter(hexFile)
      try {
        val entryBits = "10" * fetchWidth
        for (_ <- 0 until nSets) {
          writer.println(entryBits)
        }
      } finally {
        writer.close()
      }
    }
    fileName
  }

  class BIMStorage(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import commonParams.{vaddrWidth}
    import frontendParams._
    import bimParams._
    private val metaInitFile = generateMetaHexFile

    val io = IO(new Bundle {
      val read = new Bundle {
        val en   = Input(Bool())
        val set  = Input(UInt(log2Ceil(nSets).W))
        val data = Output(new BIMMeta)
      }
      val update = Flipped(new BranchPredictionUpdate)
    })
    val metas = SyncReadMem(nSets, Vec(fetchWidth, UInt(2.W)))
    loadMemoryFromFileInline(metas, metaInitFile, MemoryLoadFileType.Binary)

    val updateSet   = getSet(io.update.fetchPC)
    val updateWdata = Wire(Vec(fetchWidth, UInt(2.W)))
    val updateWmask = Wire(Vec(fetchWidth, Bool()))
    val updateMeta  = io.update.meta.bim
    val bypass      = Reg(Vec(fetchWidth, Bool()))

    for (w <- 0 until fetchWidth) {
      val isTaken = io.update.cfiIdx.valid && io.update.cfiIdx.bits === w.U
      updateWmask(w) := io.update.brMask(w) || isTaken
      updateWdata(w) := bimWrite(updateMeta(w), isTaken)
      when(updateWmask(w)) {
        bypass(w) := updateWdata(w)
      }.otherwise {
        bypass(w) := updateMeta(w)
      }
    }

    when(io.update.valid) {
      metas.write(updateSet, updateWdata, updateWmask)
    }

    io.read.data.bims := metas.read(io.read.set, io.read.en)
  }

  class BIMStage0to1(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import commonParams.{vaddrWidth}
    import frontendParams._
    import bimParams._

    val io = IO(new Bundle {
      val flush = Input(Bool())
      val metaRead = new Bundle {
        val en   = Output(Bool())
        val set  = Output(UInt(log2Ceil(nSets).W))
        val data = Input(new BIMMeta)
      }
      val holdRead = Input(UInt(vaddrWidth.W))
      val req      = Flipped(Decoupled(UInt(vaddrWidth.W)))
      val resp     = Decoupled(new BIMMeta)
    })

    val en     = RegInit(false.B)
    val nextEn = WireDefault(en & ~io.resp.ready)
    en            := Mux(io.req.ready, io.req.valid, nextEn)
    io.req.ready  := (nextEn === 0.U) || io.flush
    io.resp.valid := en && !io.flush

    io.metaRead.en    := io.req.fire || en
    io.metaRead.set   := getSet(Mux(io.req.ready, io.req.bits, io.holdRead))
    io.resp.bits.bims := io.metaRead.data.bims
  }

  class BIMStage1(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import commonParams.{vaddrWidth}
    import frontendParams._
    import bimParams._

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
}
