package KXCore.superscalar.core.frontend

import java.io._
import chisel3._
import chisel3.experimental.dataview._
import chisel3.util._
import chisel3.util.random._
import chisel3.util.experimental._
import firrtl.annotations.MemoryLoadFileType
import KXCore.common._
import KXCore.common.peripheral._
import KXCore.common.Privilege._
import KXCore.common.Privilege.CACOPType._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.backend._

object ICache {
  def getSet(vaddr: UInt)(implicit params: CoreParameters): UInt = {
    import params.frontendParams.icacheParams.{setWidth, blockWidth}
    vaddr(setWidth + blockWidth - 1, blockWidth)
  }

  def getTag(paddr: UInt)(implicit params: CoreParameters): UInt = {
    import params.{commonParams, frontendParams}
    import params.commonParams.{paddrWidth}
    import frontendParams.icacheParams.{setWidth, blockWidth}
    paddr.head(paddrWidth - setWidth - blockWidth)
  }

  class ICacheMeta(implicit params: CoreParameters) extends Bundle {
    import params.{commonParams, frontendParams}
    import frontendParams.{icacheParams}
    import commonParams.{paddrWidth}
    import icacheParams.{setWidth, blockWidth}
    private val tagWidth = paddrWidth - setWidth - blockWidth

    val valid = Bool()
    val tag   = UInt(tagWidth.W)
  }

  def generateMetaTagHexFile(implicit params: CoreParameters): String = {
    import params.{frontendParams}
    import frontendParams.{icacheParams}
    import icacheParams.{nSets, nWays}

    val fileName = s"icache_meta_tag_${nSets}_${nWays}.mem"
    val hexFile  = new File("build", fileName)

    if (!hexFile.exists()) {
      val writer = new PrintWriter(hexFile)
      try {
        val entryBits = "0" * (new ICacheMeta).tag.getWidth * nWays
        for (_ <- 0 until nSets) {
          writer.println(entryBits)
        }
      } finally {
        writer.close()
      }
    }
    fileName
  }

  class ICacheStorage(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import frontendParams.{icacheParams}
    import commonParams.{vaddrWidth, paddrWidth}
    import icacheParams._
    private val metaTagInitFile = generateMetaTagHexFile

    val io = IO(new Bundle {
      val clear = Input(Bool())
      val metaPort = new Bundle {
        val read = new Bundle {
          val en   = Input(Bool())
          val set  = Input(UInt(setWidth.W))
          val data = Output(Vec(nWays, new ICacheMeta))
        }
        val write = new Bundle {
          val en   = Input(Bool())
          val set  = Input(UInt(setWidth.W))
          val way  = Input(UInt(wayWidth.W))
          val data = Input(new ICacheMeta)
        }
      }
      val dataPort = new Bundle {
        val read = new Bundle {
          val en   = Input(Bool())
          val set  = Input(UInt(setWidth.W))
          val way  = Input(UInt(wayWidth.W))
          val data = Output(UInt(blockBits.W))
        }
        val write = new Bundle {
          val en   = Input(Bool())
          val set  = Input(UInt(setWidth.W))
          val way  = Input(UInt(wayWidth.W))
          val data = Input(UInt(blockBits.W))
        }
      }
    })

    val valids = RegInit(VecInit.fill(nSets, nWays)(false.B))
    val tags   = SyncReadMem(nSets, Vec(nWays, UInt((new ICacheMeta).tag.getWidth.W)))
    loadMemoryFromFileInline(tags, metaTagInitFile, MemoryLoadFileType.Binary)
    val data = Seq.tabulate(nBanks) { _ =>
      (0 until nWays).map { _ =>
        SyncReadMem(nSets, UInt(bankBits.W))
      }
    }

    assert(!(io.metaPort.read.en && io.metaPort.write.en))
    assert(!(io.dataPort.read.en && io.dataPort.write.en))

    val rvalids = RegEnable(valids(io.metaPort.read.set), io.metaPort.read.en)
    val rtags   = Wire(Vec(nWays, UInt((new ICacheMeta).tag.getWidth.W)))
    val rdata   = Wire(UInt(blockBits.W))

    val rtagsReg = RegEnable(rtags, RegNext(io.metaPort.read.en))
    val rdataReg = RegEnable(rdata, RegNext(io.dataPort.read.en))

    for (w <- 0 until nWays) {
      io.metaPort.read.data(w).valid := rvalids(w)
      io.metaPort.read.data(w).tag   := Mux(RegNext(io.metaPort.read.en), rtags(w), rtagsReg(w))
    }
    io.dataPort.read.data := Mux(RegNext(io.dataPort.read.en), rdata, rdataReg)

    when(io.clear) {
      valids.foreach(_.foreach(_ := false.B))
    }.elsewhen(io.metaPort.write.en) {
      valids(io.metaPort.write.set)(io.metaPort.write.way) := io.metaPort.write.data.valid
    }

    if (singlePorted) {
      rtags := tags.readWrite(
        Mux(io.metaPort.write.en, io.metaPort.write.set, io.metaPort.read.set),
        VecInit.fill(nWays)(io.metaPort.write.data.tag),
        UIntToOH(io.metaPort.write.way).asBools,
        io.metaPort.read.en || io.metaPort.write.en,
        io.metaPort.write.en,
      )
      rdata := VecInit(
        data.zipWithIndex.map { case (wayData, i) =>
          val writeData = io.dataPort.write.data(bankBits * (i + 1) - 1, bankBits * i)
          VecInit(wayData.zipWithIndex.map { case (data, w) =>
            val ren = io.dataPort.read.en && (io.dataPort.read.way === w.U)
            val wen = io.dataPort.write.en && (io.dataPort.write.way === w.U)
            data.readWrite(
              Mux(wen, io.dataPort.write.set, io.dataPort.read.set),
              writeData,
              ren || wen,
              wen,
            )
          })(RegNext(io.dataPort.read.way))
        },
      ).asUInt
    } else {
      rtags := tags.read(io.metaPort.read.set, io.metaPort.read.en)
      tags.write(
        io.metaPort.write.set,
        VecInit.fill(nWays)(io.metaPort.write.data.tag),
        (UIntToOH(io.metaPort.write.way) & Fill(nWays, io.metaPort.write.en)).asBools,
      )

      rdata := VecInit(
        data.map { wayData =>
          VecInit(wayData.zipWithIndex.map { case (data, w) =>
            val ren = io.dataPort.read.en && (io.dataPort.read.way === w.U)
            data.read(io.dataPort.read.set, ren)
          })(RegNext(io.dataPort.read.way))
        },
      ).asUInt
      data.zipWithIndex.foreach { case (wayData, i) =>
        val writeData = io.dataPort.write.data(bankBits * (i + 1) - 1, bankBits * i)
        wayData.zipWithIndex.foreach { case (data, w) =>
          val wen = io.dataPort.write.en && (io.dataPort.write.way === w.U)
          when(wen) { data.write(io.dataPort.write.set, writeData) }
        }
      }
    }
  }

  class ICacheStage0to1(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import frontendParams.{icacheParams}
    import commonParams.{vaddrWidth, paddrWidth}
    import icacheParams._

    val io = IO(new Bundle {
      val flush = Input(Bool())
      val readMeta = new Bundle {
        val en   = Output(Bool())
        val set  = Output(UInt(setWidth.W))
        val data = Input(Vec(nWays, new ICacheMeta))
      }
      val req  = Flipped(Decoupled(UInt(vaddrWidth.W)))
      val resp = Decoupled(Vec(nWays, new ICacheMeta))
    })

    val en     = RegInit(false.B)
    val nextEn = WireDefault(en & ~io.resp.ready)
    en            := Mux(io.req.ready, io.req.valid, nextEn)
    io.req.ready  := (nextEn === 0.U) || io.flush
    io.resp.valid := en && !io.flush

    io.readMeta.en  := io.req.fire
    io.readMeta.set := getSet(io.req.bits)
    io.resp.bits    := io.readMeta.data
  }

  class ICacheStage1(implicit params: CoreParameters) extends Module {
    import params.{commonParams, axiParams, frontendParams}
    import frontendParams.{fetchWidth, icacheParams}
    import commonParams.{instBytes, instWidth, vaddrWidth, paddrWidth}
    import icacheParams._
    import axiParams.{dataBits}
    private val tagWidth = paddrWidth - setWidth - blockWidth
    private val burstLen = blockBits / dataBits
    require(dataBits == instWidth)
    require(fetchWidth == fetchBytes / instBytes)

    val io = IO(new Bundle {
      val axi = new AXIBundle(axiParams)
      val metaWrite = new Bundle {
        val en   = Output(Bool())
        val set  = Output(UInt(setWidth.W))
        val way  = Output(UInt(wayWidth.W))
        val data = Output(new ICacheMeta)
      }
      val dataWrite = new Bundle {
        val en   = Output(Bool())
        val set  = Output(UInt(setWidth.W))
        val way  = Output(UInt(wayWidth.W))
        val data = Output(UInt(blockBits.W))
      }
      val req = Flipped(Decoupled(new Bundle {
        val vaddr  = UInt(vaddrWidth.W)
        val paddr  = UInt(paddrWidth.W)
        val cached = Bool()
        val cacop  = UInt(CACOPType.getWidth.W)
        val meta   = Vec(nWays, new ICacheMeta)
      }))
      val resp = Decoupled(new Bundle {
        val cached       = Bool()
        val set          = UInt(setWidth.W)
        val way          = UInt(wayWidth.W)
        val uncachedRead = UInt(blockBits.W)
      })
    })

    val bits      = io.req.bits
    val vaddr     = bits.vaddr
    val paddr     = bits.paddr
    val cached    = bits.cached
    val set       = getSet(paddr)
    val tag       = getTag(paddr)
    val cacop     = bits.cacop
    val wayMeta   = bits.meta
    val isRead    = cacop === CACOP_HIT_READ.asUInt
    val isIdxInit = cacop === CACOP_IDX_INIT.asUInt
    val isIdxInv  = cacop === CACOP_IDX_INV.asUInt
    val isHitInv  = cacop === CACOP_HIT_INV.asUInt

    val matches = wayMeta.map(meta => meta.valid && meta.tag === tag)
    val matched = PriorityEncoder(matches)
    val hit     = matches.reduce(_ || _)
    assert(PopCount(matches) < 2.U)

    val invalids    = wayMeta.map(!_.valid)
    val random      = if (nWays == 1) 0.U else GaloisLFSR.maxPeriod(wayWidth)
    val replacedSel = RegEnable(Mux(invalids.reduce(_ || _), PriorityEncoder(invalids), random), io.axi.ar.fire)
    val lineData    = Reg(Vec(burstLen, UInt(dataBits.W)))
    val (burstCnt, _) =
      Counter(
        0 until burstLen,
        io.axi.r.valid && io.axi.r.bits.id === id.U,
        io.axi.ar.fire,
      )
    lineData(burstCnt) := Mux(
      io.axi.r.valid && io.axi.r.bits.id === id.U,
      io.axi.r.bits.data,
      lineData(burstCnt),
    )

    val sHandleReq :: sSendBusReq :: sReadBusResp :: sIgnoreBusResp :: sWriteBack :: sSendReadResp :: Nil = Enum(6)

    val state = RegInit(sHandleReq)
    state := MuxLookup(state, sHandleReq)(
      Seq(
        sHandleReq -> Mux(io.req.valid && isRead && (!hit || !cached), sSendBusReq, sHandleReq),
        sSendBusReq -> Mux(
          io.req.valid,
          Mux(io.axi.ar.ready, sReadBusResp, sSendBusReq),
          Mux(io.axi.ar.ready, sIgnoreBusResp, sHandleReq),
        ),
        sReadBusResp -> Mux(
          io.req.valid,
          Mux(
            io.axi.r.valid && io.axi.r.bits.last.asBool && io.axi.r.bits.id === id.U,
            Mux(cached, sWriteBack, sSendReadResp),
            sReadBusResp,
          ),
          Mux(
            io.axi.r.valid && io.axi.r.bits.last.asBool && io.axi.r.bits.id === id.U,
            sHandleReq,
            sIgnoreBusResp,
          ),
        ),
        sIgnoreBusResp -> Mux(
          io.axi.r.valid && io.axi.r.bits.last.asBool && io.axi.r.bits.id === id.U,
          sHandleReq,
          sIgnoreBusResp,
        ),
        sWriteBack    -> Mux(io.req.valid, sSendReadResp, sHandleReq),
        sSendReadResp -> Mux(!io.req.valid || io.resp.ready, sHandleReq, sSendReadResp),
      ),
    )

    io.req.ready := MuxLookup(state, false.B)(
      Seq(
        sHandleReq    -> ((isRead && hit && cached && io.resp.ready) || isIdxInit || isIdxInv || isHitInv),
        sSendReadResp -> io.resp.ready,
      ),
    )

    io.resp.valid := MuxLookup(state, false.B)(
      Seq(
        sHandleReq    -> (io.req.valid && isRead && hit && cached),
        sSendReadResp -> io.req.valid,
      ),
    )
    io.resp.bits.cached       := cached
    io.resp.bits.set          := set
    io.resp.bits.way          := Mux(hit, matched, replacedSel)
    io.resp.bits.uncachedRead := lineData.asUInt

    val idxSet = vaddr(blockWidth + setWidth - 1, blockWidth)
    val idxWay = vaddr(wayWidth - 1, 0)
    io.metaWrite.en := MuxLookup(state, false.B)(
      Seq(
        sHandleReq -> (io.req.valid && ((isHitInv && hit) || isIdxInit || isIdxInv)),
        sWriteBack -> (io.req.valid),
      ),
    )
    io.metaWrite.set        := Mux(isIdxInit || isIdxInv, idxSet, set)
    io.metaWrite.way        := Mux(isIdxInit || isIdxInv, idxWay, replacedSel)
    io.metaWrite.data.valid := isRead
    io.metaWrite.data.tag   := Mux(isIdxInit, 0.U, tag)

    io.dataWrite.en   := io.req.valid && (state === sWriteBack)
    io.dataWrite.set  := set
    io.dataWrite.way  := replacedSel
    io.dataWrite.data := lineData.asUInt

    io.axi.ar.valid     := state === sSendBusReq
    io.axi.ar.bits.addr := paddr & ~(blockBytes - 1).U(paddrWidth.W)
    io.axi.ar.bits.id   := id.U
    io.axi.ar.bits.len  := (burstLen - 1).U

    io.axi.ar.bits.size  := log2Ceil(axiParams.dataBits / 8).U
    io.axi.ar.bits.burst := AXIParameters.BURST_INCR
    io.axi.ar.bits.lock  := 0.U
    io.axi.ar.bits.cache := 0.U
    io.axi.ar.bits.prot  := 0.U

    io.axi.r.ready := (state === sReadBusResp) || (state === sIgnoreBusResp)

    io.axi.aw.valid := false.B
    io.axi.aw.bits  := DontCare

    io.axi.w.valid := false.B
    io.axi.w.bits  := DontCare

    io.axi.b.ready := false.B
  }

  class ICacheStage1to2(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import frontendParams.{icacheParams}
    import commonParams.{vaddrWidth, paddrWidth}
    import icacheParams._
    private val tagWidth = paddrWidth - setWidth - blockWidth

    val io = IO(new Bundle {
      val flush = Input(Bool())
      val readData = new Bundle {
        val en   = Output(Bool())
        val set  = Output(UInt(setWidth.W))
        val way  = Output(UInt(wayWidth.W))
        val data = Input(UInt(blockBits.W))
      }
      val req = Flipped(Decoupled(new Bundle {
        val cached       = Bool()
        val set          = UInt(setWidth.W)
        val way          = UInt(wayWidth.W)
        val uncachedRead = UInt(blockBits.W)
      }))
      val resp = Decoupled(UInt(blockBits.W))
    })

    val en     = RegInit(false.B)
    val nextEn = WireDefault(en & ~io.resp.ready)
    en            := Mux(io.req.ready, io.req.valid, nextEn)
    io.req.ready  := (nextEn === 0.U) || io.flush
    io.resp.valid := en && !io.flush

    io.readData.en  := io.req.fire
    io.readData.set := io.req.bits.set
    io.readData.way := io.req.bits.way
    io.resp.bits := Mux(
      RegEnable(io.req.bits.cached, io.req.fire),
      io.readData.data,
      RegEnable(io.req.bits.uncachedRead, io.req.fire),
    )
  }
}
