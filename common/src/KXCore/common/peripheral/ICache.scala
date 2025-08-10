package KXCore.common.peripheral

import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.experimental.dataview._
import KXCore.common._
import KXCore.common.peripheral._
import KXCore.common.Privilege._
import KXCore.common.Privilege.CACOPType._
import firtoolresolver.shaded.coursier.cache.Cache

object ICache {
  def getSet(vaddr: UInt)(implicit cacheParams: CacheParameters): UInt = {
    vaddr(cacheParams.setWidth + cacheParams.blockWidth - 1, cacheParams.blockWidth)
  }

  def getTag(paddr: UInt)(implicit commonParams: CommonParameters, cacheParams: CacheParameters): UInt = {
    paddr.head(commonParams.paddrWidth - cacheParams.setWidth - cacheParams.blockWidth)
  }
  class ICacheValidWrite(implicit commonParams: CommonParameters, cacheParams: CacheParameters) extends Bundle {
    import cacheParams.{setWidth, wayWidth}
    val all  = Bool()
    val set  = UInt(setWidth.W)
    val way  = UInt(wayWidth.W)
    val data = Bool()
  }

  class ICacheTagWrite(implicit commonParams: CommonParameters, cacheParams: CacheParameters) extends Bundle {
    import commonParams.{paddrWidth}
    import cacheParams.{setWidth, wayWidth, blockWidth}
    private val tagWidth = paddrWidth - setWidth - blockWidth

    val set  = UInt(setWidth.W)
    val way  = UInt(wayWidth.W)
    val data = UInt(tagWidth.W)
  }

  class ICacheDataWrite(implicit commonParams: CommonParameters, cacheParams: CacheParameters) extends Bundle {
    import cacheParams.{setWidth, wayWidth, blockBits}
    val set  = UInt(setWidth.W)
    val way  = UInt(wayWidth.W)
    val data = UInt(blockBits.W)
  }
}

import ICache._

class ICacheStage0to1(implicit
    commonParams: CommonParameters,
    cacheParams: CacheParameters,
    axiParams: AXIBundleParameters,
) extends Module {
  import commonParams.{vaddrWidth, paddrWidth}
  import cacheParams._
  private val tagWidth = paddrWidth - setWidth - blockWidth

  val io = IO(new Bundle {
    val flush = Input(Bool())
    val req   = Flipped(Decoupled(UInt(vaddrWidth.W)))
    val resp = Decoupled(new Bundle {
      val cacheValid = Vec(nSets, Vec(nWays, Bool()))
      val wayTag     = Vec(nWays, UInt(tagWidth.W))
    })
    val validWrite = Flipped(Valid(new ICacheValidWrite))
    val tagWrite   = Flipped(Valid(new ICacheTagWrite))
  })

  val cacheValid = RegInit(VecInit.fill(nSets, nWays)(false.B))
  val cacheTag   = SyncReadMem(nSets, Vec(nWays, UInt(tagWidth.W)))
  val en         = RegInit(false.B)
  val nextEn     = WireDefault(en & ~io.resp.ready)
  en            := Mux(io.req.ready, io.req.valid, nextEn)
  io.req.ready  := (nextEn === 0.U) || io.flush
  io.resp.valid := en && !io.flush

  val holdReqReg = RegEnable(io.req.bits, io.req.ready)
  val readSet    = getSet(Mux(io.req.ready, io.req.bits, holdReqReg))

  val validWriteEn = io.validWrite.valid
  val validWrite   = io.validWrite.bits
  for (set <- 0 until nSets) {
    for (way <- 0 until nWays) {
      val v  = cacheValid(set)(way)
      val we = validWriteEn && ((validWrite.set === set.U && validWrite.way === way.U) || validWrite.all)
      cacheValid(set)(way) := Mux(we, validWrite.data, v)
    }
  }

  if (!singlePorted) {
    val tagWriteEn   = io.tagWrite.valid
    val tagWrite     = io.tagWrite.bits
    val tagWriteData = WireDefault(VecInit.fill(nWays)(tagWrite.data))
    cacheTag.write(tagWrite.set, tagWriteData, (UIntToOH(tagWrite.way) & Fill(wayWidth, tagWriteEn)).asBools)
  }

  val wayTag = if (singlePorted) {
    val tagWriteEn   = io.tagWrite.valid
    val tagWrite     = io.tagWrite.bits
    val tagWriteData = WireDefault(VecInit.fill(nWays)(tagWrite.data))
    cacheTag.readWrite(
      Mux(tagWriteEn, tagWrite.set, readSet),
      tagWriteData,
      (UIntToOH(tagWrite.way) & Fill(wayWidth, tagWriteEn)).asBools,
      true.B,
      tagWriteEn,
    )
  } else {
    cacheTag.read(readSet)
  }

  io.resp.bits.cacheValid := cacheValid
  io.resp.bits.wayTag     := wayTag
}

class ICacheStage1(implicit
    commonParams: CommonParameters,
    cacheParams: CacheParameters,
    axiParams: AXIBundleParameters,
) extends Module {
  import commonParams.{vaddrWidth, paddrWidth}
  import cacheParams._
  import axiParams.{dataBits}
  private val tagWidth = paddrWidth - setWidth - blockWidth
  private val burstLen = blockBits / axiParams.dataBits

  val io = IO(new Bundle {
    val axi        = new AXIBundle(axiParams)
    val validWrite = Valid(new ICacheValidWrite)
    val tagWrite   = Valid(new ICacheTagWrite)
    val dataWrite  = Valid(new ICacheDataWrite)
    val req = Flipped(Decoupled(new Bundle {
      val vaddr      = UInt(vaddrWidth.W)
      val paddr      = UInt(paddrWidth.W)
      val cached     = Bool()
      val cacop      = UInt(CACOPType.getWidth.W)
      val cacheValid = Vec(nSets, Vec(nWays, Bool()))
      val wayTag     = Vec(nWays, UInt(tagWidth.W))
    }))
    val resp = Decoupled(new Bundle {
      val cached       = Bool()
      val set          = UInt(setWidth.W)
      val way          = UInt(wayWidth.W)
      val uncachedRead = UInt(blockBits.W)
    })
  })

  val bits     = io.req.bits
  val vaddr    = bits.vaddr
  val paddr    = bits.paddr
  val cached   = bits.cached
  val set      = getSet(paddr)
  val tag      = getTag(paddr)
  val cacop    = bits.cacop
  val wayValid = bits.cacheValid(set)
  val wayTag   = bits.wayTag
  val isRead   = cacop === CACOP_HIT_READ.asUInt
  val isFlush  = cacop === CACOP_FLUSH.asUInt
  val isIdxInv = cacop === CACOP_IDX_INV.asUInt
  val isHitInv = cacop === CACOP_HIT_INV.asUInt

  val sHandleReq :: sSendBusReq :: sReadBusResp :: sIgnoreBusResp :: sWriteBack :: sSendReadResp :: Nil = Enum(6)

  val state     = RegInit(sHandleReq)
  val nextState = WireDefault(sHandleReq)
  val hit       = Wire(Bool())

  nextState := MuxLookup(state, sHandleReq)(
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
          io.axi.r.valid && io.axi.r.bits.last.asBool && io.axi.r.bits.id === cacheParams.id.U,
          Mux(cached, sWriteBack, sSendReadResp),
          sReadBusResp,
        ),
        Mux(
          io.axi.r.valid && io.axi.r.bits.last.asBool && io.axi.r.bits.id === cacheParams.id.U,
          sHandleReq,
          sIgnoreBusResp,
        ),
      ),
      sIgnoreBusResp -> Mux(
        io.axi.r.valid && io.axi.r.bits.last.asBool && io.axi.r.bits.id === cacheParams.id.U,
        sHandleReq,
        sIgnoreBusResp,
      ),
      sWriteBack    -> Mux(io.req.valid, sSendReadResp, sHandleReq),
      sSendReadResp -> Mux(!io.req.valid || io.resp.ready, sHandleReq, sSendBusReq),
    ),
  )
  state := nextState

  val matches = wayTag.map(tag === _)
  val matched = PriorityEncoder(matches)
  hit := matches.reduce(_ || _) && wayValid(matched)

  val random      = if (nWays == 1) 0.U else GaloisLFSR.maxPeriod(wayWidth)
  val replacedSel = RegEnable(Mux(wayValid.contains(false.B), PriorityEncoder(wayValid.map(!_)), random), io.axi.ar.fire)
  val lineData    = Reg(Vec(burstLen, UInt(axiParams.dataBits.W)))
  val (burstCnt, _) =
    Counter(
      0 until burstLen,
      io.axi.r.valid && io.axi.r.bits.id === cacheParams.id.U,
      io.axi.ar.fire && io.axi.r.bits.id === cacheParams.id.U,
    )
  lineData(burstCnt) := Mux(
    io.axi.r.valid && io.axi.r.bits.id === cacheParams.id.U,
    io.axi.r.bits.data,
    lineData(burstCnt),
  )

  io.req.ready := MuxLookup(state, false.B)(
    Seq(
      sHandleReq    -> ((isRead && hit && !cached && io.resp.ready) || isFlush || isIdxInv || isHitInv),
      sSendReadResp -> io.resp.ready,
    ),
  )

  io.resp.valid := MuxLookup(state, false.B)(
    Seq(
      sHandleReq    -> (io.req.valid && isRead && hit && !cached),
      sSendReadResp -> io.req.valid,
    ),
  )
  io.resp.bits.cached       := cached
  io.resp.bits.set          := set
  io.resp.bits.way          := Mux(state === sSendReadResp, replacedSel, matched)
  io.resp.bits.uncachedRead := lineData.asUInt

  val idxSet = vaddr(blockWidth + setWidth - 1, blockWidth)
  val idxWay = vaddr(wayWidth - 1, 0)
  io.validWrite.valid := MuxLookup(state, false.B)(
    Seq(
      sHandleReq -> (io.req.valid && ((isHitInv && hit) || isFlush || isIdxInv)),
      sWriteBack -> (io.req.valid),
    ),
  )
  io.validWrite.bits.all  := isFlush
  io.validWrite.bits.set  := Mux(isIdxInv, idxSet, set)
  io.validWrite.bits.way  := Mux(isIdxInv, idxWay, replacedSel)
  io.validWrite.bits.data := isRead

  io.tagWrite.valid     := io.req.valid && (state === sWriteBack)
  io.tagWrite.bits.set  := set
  io.tagWrite.bits.way  := replacedSel
  io.tagWrite.bits.data := tag

  io.dataWrite.valid     := io.req.valid && (state === sWriteBack)
  io.dataWrite.bits.set  := set
  io.dataWrite.bits.way  := replacedSel
  io.dataWrite.bits.data := lineData.asUInt

  io.axi.ar.valid     := state === sSendBusReq
  io.axi.ar.bits.addr := paddr & ~(cacheParams.blockBytes - 1).U(commonParams.paddrWidth.W)
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

class ICacheStage1to2(implicit
    commonParams: CommonParameters,
    cacheParams: CacheParameters,
    axiParams: AXIBundleParameters,
) extends Module {
  import commonParams.{vaddrWidth, paddrWidth}
  import cacheParams._
  private val tagWidth = paddrWidth - setWidth - blockWidth

  val io = IO(new Bundle {
    val flush = Input(Bool())
    val req = Flipped(Decoupled(new Bundle {
      val cached       = Bool()
      val set          = UInt(setWidth.W)
      val way          = UInt(wayWidth.W)
      val uncachedRead = UInt(blockBits.W)
    }))
    val resp = Decoupled(new Bundle {
      val cached = Bool()
      val data   = UInt(blockBits.W)
    })

    val dataWrite = Flipped(Valid(new ICacheDataWrite))
  })

  val cacheData = Seq.tabulate(nBanks) { _ =>
    (0 until nWays).map { _ =>
      SyncReadMem(nSets, UInt(bankBits.W))
    }
  }
  val en     = RegInit(false.B)
  val nextEn = WireDefault(en & ~io.resp.ready)
  en            := Mux(io.req.ready, io.req.valid, nextEn)
  io.req.ready  := (nextEn === 0.U) || io.flush
  io.resp.valid := en && !io.flush

  val holdReqReg = RegEnable(io.req.bits, io.req.ready)
  val readSet    = Mux(io.req.ready, io.req.bits.set, holdReqReg.set)
  val readWay    = holdReqReg.way

  if (!singlePorted) {
    val dataWrite = io.dataWrite.bits
    for (i <- 0 until nBanks) {
      for (w <- 0 until nWays) {
        val dataWriteEn   = io.dataWrite.valid && (dataWrite.way === w.U)
        val dataWriteData = WireDefault(dataWrite.data(bankBits * (i + 1) - 1, bankBits * i))
        when(dataWriteEn) {
          cacheData(i)(w).write(dataWrite.set, dataWriteData)
        }
      }
    }
  }

  val data = if (singlePorted) {
    val dataWrite = io.dataWrite.bits
    VecInit(
      cacheData.zipWithIndex.map { case (cacheDataWays, i) =>
        val dataWriteData = WireDefault(dataWrite.data(bankBits * (i + 1) - 1, bankBits * i))
        VecInit(cacheDataWays.zipWithIndex.map { case (cacheData, w) =>
          val dataWriteEn = io.dataWrite.valid && (dataWrite.way === w.U)
          cacheData.readWrite(
            Mux(dataWriteEn, dataWrite.set, readSet),
            dataWriteData,
            true.B,
            dataWriteEn,
          )
        })(readWay)
      },
    ).asUInt
  } else {
    VecInit(
      cacheData.map { cacheDataBanks =>
        VecInit(cacheDataBanks.map(_.read(readSet)))(readWay)
      },
    ).asUInt
  }

  io.resp.bits.cached := holdReqReg.cached
  io.resp.bits.data   := Mux(holdReqReg.cached, data, holdReqReg.uncachedRead)
}

class ICache(implicit commonParams: CommonParameters, cacheParams: CacheParameters, axiParams: AXIBundleParameters) extends Module {
  import commonParams.{vaddrWidth, paddrWidth}
  import cacheParams._
  private val tagWidth = paddrWidth - setWidth - blockWidth

  val io = IO(new Bundle {
    val flush = Input(new Bundle {
      val stage1 = Bool()
      val stage2 = Bool()
    })
    val axi = new AXIBundle(axiParams)
    val req = new Bundle {
      val stage0 = Flipped(Decoupled(new Bundle {
        val vaddr = UInt(vaddrWidth.W)
      }))
      val stage1 = Flipped(Decoupled(new Bundle {
        val vaddr  = UInt(vaddrWidth.W)
        val paddr  = UInt(paddrWidth.W)
        val cached = Bool()
        val cacop  = UInt(CACOPType.getWidth.W)
      }))
    }
    val resp = new Bundle {
      val stage2 = Decoupled(new Bundle {
        val cached = Bool()
        val data   = UInt(blockBits.W)
      })
    }
  })

  val stage0to1 = Module(new ICacheStage0to1)
  val stage1    = Module(new ICacheStage1)
  val stage1to2 = Module(new ICacheStage1to2)

  stage0to1.io.flush := io.flush.stage1
  stage1to2.io.flush := io.flush.stage2

  // stage0to1
  stage0to1.io.req.valid := io.req.stage0.valid
  stage0to1.io.req.bits  := io.req.stage0.bits.vaddr
  io.req.stage0.ready    := stage0to1.io.req.ready

  // stage1
  io.axi                        <> stage1.io.axi
  stage1.io.req.valid           := stage0to1.io.resp.valid && io.req.stage1.valid
  stage0to1.io.resp.ready       := stage1.io.req.ready
  io.req.stage1.ready           := stage1.io.req.ready
  stage1.io.req.bits.vaddr      := io.req.stage1.bits.vaddr
  stage1.io.req.bits.paddr      := io.req.stage1.bits.paddr
  stage1.io.req.bits.cached     := io.req.stage1.bits.cached
  stage1.io.req.bits.cacop      := io.req.stage1.bits.cacop
  stage1.io.req.bits.cacheValid := stage0to1.io.resp.bits.cacheValid
  stage1.io.req.bits.wayTag     := stage0to1.io.resp.bits.wayTag
  stage0to1.io.validWrite       := stage1.io.validWrite
  stage0to1.io.tagWrite         := stage1.io.tagWrite

  // stage1to2
  stage1to2.io.dataWrite := stage1.io.dataWrite
  stage1to2.io.req       <> stage1.io.resp

  io.resp.stage2 <> stage1to2.io.resp
}
