package KXCore.superscalar.core.backend
import java.io._
import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.util.experimental._
import chisel3.experimental.dataview._
import firrtl.annotations.MemoryLoadFileType
import KXCore.common._
import KXCore.common.peripheral._
import KXCore.common.Privilege._
import KXCore.common.Privilege.CACOPType._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.backend._

object DCache {
  def getSet(vaddr: UInt)(implicit params: CoreParameters): UInt = {
    import params.backendParams.dcacheParams.{setWidth, blockWidth}
    vaddr(setWidth + blockWidth - 1, blockWidth)
  }

  def getTag(paddr: UInt)(implicit params: CoreParameters): UInt = {
    import params.{commonParams, backendParams}
    import commonParams.{paddrWidth}
    import backendParams.{dcacheParams}
    import dcacheParams.{setWidth, blockWidth}
    paddr.head(paddrWidth - setWidth - blockWidth)
  }

  def getOffset(addr: UInt)(implicit params: CoreParameters): UInt = {
    import params.{backendParams}
    import backendParams.{dcacheParams}
    import dcacheParams.{blockWidth}
    addr(blockWidth - 1, 0)
  }

  class DCacheMeta(implicit params: CoreParameters) extends Bundle {

    import params.{commonParams, backendParams}
    import backendParams.{dcacheParams}
    import commonParams.{paddrWidth}
    import dcacheParams.{setWidth, blockWidth}
    private val tagWidth = paddrWidth - setWidth - blockWidth

    val valid = Bool()
    val dirty = Bool()
    val tag   = UInt(tagWidth.W)
  }

  def generateMetaTagHexFile(implicit params: CoreParameters): String = {
    import params.{backendParams}
    import backendParams.{dcacheParams}
    import dcacheParams.{nSets, nWays}

    val fileName = s"dcache_meta_tag_${nSets}_${nWays}.mem"
    val hexFile  = new File("build", fileName)

    if (!hexFile.exists()) {
      val writer = new PrintWriter(hexFile)
      try {
        val entryBits = "0" * (new DCacheMeta).tag.getWidth * nWays
        for (_ <- 0 until nSets) {
          writer.println(entryBits)
        }
      } finally {
        writer.close()
      }
    }
    fileName
  }

  class DCacheAXIAdapter(implicit params: CoreParameters) extends Module {
    import params.{commonParams, backendParams, axiParams}
    import commonParams.{paddrWidth}
    import backendParams.{dcacheParams}
    import dcacheParams._
    require(paddrWidth == axiParams.addrBits)
    require(dcacheParams.blockBits % axiParams.dataBits == 0)
    require(dcacheParams.blockBits % commonParams.dataWidth == 0)

    private val maxBurstLen = dcacheParams.blockBits / axiParams.dataBits

    val io = IO(new Bundle {
      val axi   = new AXIBundle(axiParams)
      val flush = Input(Bool())
      val read = new Bundle {
        val req = Flipped(Decoupled(new Bundle {
          val paddr = UInt(paddrWidth.W)
          val len   = UInt(axiParams.lenBits.W)
        }))
        val resp = Decoupled(new Bundle {
          val data = UInt(blockBits.W)
        })
      }
      val write = new Bundle {
        val req = Flipped(Decoupled(new Bundle {
          val paddr = UInt(paddrWidth.W)
          val data  = UInt(blockBits.W)
          val wmask = UInt(axiParams.wstrBits.W)
          val len   = UInt(axiParams.lenBits.W)
        }))
        val resp = Decoupled(new Bundle {})
      }
    })

    val sHandleReadReq :: sBusRead :: sBusReadIgnore :: Nil = Enum(3)

    val read_state          = RegInit(sHandleReadReq)
    val arfire              = io.axi.ar.fire
    val rfire               = io.axi.r.fire && io.axi.r.bits.id === id.U
    val read_burst_len      = RegEnable(io.read.req.bits.len, io.read.req.fire)
    val (read_burst_cnt, _) = Counter(0 to maxBurstLen, rfire, arfire)
    val read_burst_fin      = read_burst_len === (read_burst_cnt - 1.U)
    val read_data_vec       = Reg(Vec(maxBurstLen, UInt(commonParams.dataWidth.W)))
    read_data_vec(read_burst_cnt) := Mux(
      rfire,
      io.axi.r.bits.data,
      read_data_vec(read_burst_cnt),
    )

    read_state := MuxLookup(read_state, sHandleReadReq)(
      Seq(
        sHandleReadReq -> Mux(io.read.req.fire, sBusRead, sHandleReadReq),
        sBusRead -> MuxCase(
          sBusRead,
          Seq(
            io.flush            -> sBusReadIgnore,
            (io.read.resp.fire) -> sHandleReadReq,
          ),
        ),
        sBusReadIgnore -> Mux(read_burst_fin, sHandleReadReq, sBusReadIgnore),
      ),
    )

    io.axi.ar.valid      := (read_state === sHandleReadReq && io.read.req.valid)
    io.axi.ar.bits.addr  := io.read.req.bits.paddr & ~(blockBytes - 1).U(paddrWidth.W)
    io.axi.ar.bits.id    := id.U
    io.axi.ar.bits.len   := io.read.req.bits.len
    io.axi.ar.bits.size  := log2Ceil(axiParams.dataBits / 8).U
    io.axi.ar.bits.burst := AXIParameters.BURST_INCR
    io.axi.ar.bits.lock  := 0.U
    io.axi.ar.bits.cache := 0.U
    io.axi.ar.bits.prot  := 0.U

    io.read.req.ready := (read_state === sHandleReadReq && io.axi.ar.ready)

    io.axi.r.ready := ((read_state === sBusRead) || (read_state === sBusReadIgnore)) && !read_burst_fin

    io.read.resp.valid     := !io.flush && (read_state === sBusRead) && read_burst_fin
    io.read.resp.bits.data := read_data_vec.asUInt

    val sHandleWriteReq :: sBusWrite :: sBusWriteIgnore :: Nil = Enum(3)

    val write_state          = RegInit(sHandleReadReq)
    val awfire               = io.axi.w.fire
    val wfire                = io.axi.w.fire
    val bfire                = io.axi.b.fire && io.axi.b.bits.id === id.U
    val write_burst_len      = RegEnable(io.write.req.bits.len, io.write.req.fire)
    val (write_burst_cnt, _) = Counter(0 to maxBurstLen, wfire, awfire)
    val write_burst_last     = write_burst_len === write_burst_cnt
    val write_burst_fin      = write_burst_len === (write_burst_cnt - 1.U)
    val write_data_vec = RegEnable(
      VecInit((0 until maxBurstLen).map { i =>
        io.write.req.bits.data(commonParams.dataWidth * (i + 1) - 1, commonParams.dataWidth * i)
      }),
      io.write.req.fire,
    )
    val write_data = write_data_vec(write_burst_cnt)
    val write_mask = RegEnable(io.write.req.bits.wmask, io.write.req.fire)

    write_state := MuxLookup(write_state, sHandleWriteReq)(
      Seq(
        sHandleWriteReq -> Mux(io.write.req.fire, sBusWrite, sHandleWriteReq),
        sBusWrite -> MuxCase(
          sBusWrite,
          Seq(
            io.flush             -> sBusWriteIgnore,
            (io.write.resp.fire) -> sHandleWriteReq,
          ),
        ),
        sBusWriteIgnore -> Mux(bfire, sHandleWriteReq, sBusWriteIgnore),
      ),
    )

    io.axi.aw.valid      := (write_state === sHandleWriteReq && io.write.req.valid)
    io.axi.aw.bits.addr  := io.write.req.bits.paddr & ~(blockBytes - 1).U(paddrWidth.W)
    io.axi.aw.bits.id    := id.U
    io.axi.aw.bits.len   := io.write.req.bits.len
    io.axi.aw.bits.size  := log2Ceil(axiParams.dataBits / 8).U
    io.axi.aw.bits.burst := AXIParameters.BURST_INCR
    io.axi.aw.bits.lock  := 0.U
    io.axi.aw.bits.cache := 0.U
    io.axi.aw.bits.prot  := 0.U

    io.write.req.ready := (write_state === sHandleWriteReq && io.axi.aw.ready)

    io.axi.w.valid     := (write_state === sBusWrite || write_state === sBusWriteIgnore) && !write_burst_fin
    io.axi.w.bits.id   := id.U
    io.axi.w.bits.last := write_burst_last
    io.axi.w.bits.data := write_data
    io.axi.w.bits.strb := write_mask

    io.axi.b.ready := ((!io.flush && write_state === sBusWrite && io.write.resp.ready) ||
      (write_state === sBusWriteIgnore)) && write_burst_fin

    io.write.resp.valid := !io.flush && (write_state === sBusWrite) && io.axi.b.valid

    if (params.debug) {
      dontTouch(read_state)
      dontTouch(arfire)
      dontTouch(rfire)
      dontTouch(read_burst_len)
      dontTouch(read_burst_cnt)
      dontTouch(read_burst_fin)
      dontTouch(read_data_vec)

      dontTouch(write_state)
      dontTouch(awfire)
      dontTouch(wfire)
      dontTouch(bfire)
      dontTouch(write_burst_len)
      dontTouch(write_burst_cnt)
      dontTouch(write_burst_last)
      dontTouch(write_burst_fin)
      dontTouch(write_data_vec)
    }
  }

  class DCacheStorage(implicit params: CoreParameters) extends Module {
    import params.{commonParams, backendParams}
    import commonParams.{vaddrWidth, paddrWidth}
    import backendParams.{dcacheParams}
    import dcacheParams._
    private val metaTagInitFile = generateMetaTagHexFile

    val io = IO(new Bundle {
      val metaPort = new Bundle {
        val read = new Bundle {
          val en   = Input(Bool())
          val set  = Input(UInt(setWidth.W))
          val data = Output(Vec(nWays, new DCacheMeta))
        }
        val write = new Bundle {
          val en   = Input(Bool())
          val set  = Input(UInt(setWidth.W))
          val way  = Input(UInt(wayWidth.W))
          val data = Input(new DCacheMeta)
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
          val mask = Input(UInt((blockBytes).W))
          val data = Input(UInt(blockBits.W))
        }
      }
    })

    val valids = RegInit(VecInit.fill(nSets, nWays)((false.B)))
    val dirtys = Reg(Vec(nSets, Vec(nWays, Bool())))
    val tags   = SyncReadMem(nSets, Vec(nWays, UInt((new DCacheMeta).tag.getWidth.W)))
    loadMemoryFromFileInline(tags, metaTagInitFile, MemoryLoadFileType.Binary)
    val data = Seq.tabulate(nBanks) { _ =>
      (0 until nWays).map { _ =>
        SyncReadMem(nSets, Vec(bankBytes, UInt(8.W)))
      }
    }

    assert(!(io.metaPort.read.en && io.metaPort.write.en))
    assert(!(io.dataPort.read.en && io.dataPort.write.en))

    val rvalids = RegEnable(valids(io.metaPort.read.set), io.metaPort.read.en)
    val rdirtys = RegEnable(dirtys(io.metaPort.read.set), io.metaPort.read.en)
    val rtags   = Wire(Vec(nWays, UInt((new DCacheMeta).tag.getWidth.W)))

    io.metaPort.read.data zip rvalids zip rdirtys zip rtags foreach { case (((port, valid), dirty), tag) =>
      port.valid := valid
      port.dirty := dirty
      port.tag   := tag
    }

    when(io.metaPort.write.en) {
      valids(io.metaPort.write.set)(io.metaPort.write.way) := io.metaPort.write.data.valid
      dirtys(io.metaPort.write.set)(io.metaPort.write.way) := io.metaPort.write.data.dirty
    }

    if (singlePorted) {
      rtags := tags.readWrite(
        Mux(io.metaPort.write.en, io.metaPort.write.set, io.metaPort.read.set),
        VecInit.fill(nWays)(io.metaPort.write.data.tag),
        UIntToOH(io.metaPort.write.way).asBools,
        io.metaPort.read.en || io.metaPort.write.en,
        io.metaPort.write.en,
      )
      io.dataPort.read.data := VecInit(
        data.zipWithIndex.map { case (wayData, i) =>
          val writeData = io.dataPort.write.data(bankBits * (i + 1) - 1, bankBits * i)
          val writeMask = io.dataPort.write.mask(bankBytes * (i + 1) - 1, bankBytes * i)
          VecInit(wayData.zipWithIndex.map { case (data, w) =>
            val ren = io.dataPort.read.en && (io.dataPort.read.way === w.U)
            val wen = io.dataPort.write.en && (io.dataPort.write.way === w.U)
            data.readWrite(
              Mux(wen, io.dataPort.write.set, io.dataPort.read.set),
              VecInit((0 until bankBytes).map(i => writeData(8 * (i + 1) - 1, 8 * i))),
              writeMask.asBools,
              ren || wen,
              wen,
            )
          })(io.dataPort.read.way)
        },
      ).asUInt
    } else {
      rtags := tags.read(io.metaPort.read.set, io.metaPort.read.en)
      tags.write(
        io.metaPort.write.set,
        VecInit.fill(nWays)(io.metaPort.write.data.tag),
        (UIntToOH(io.metaPort.write.way) & Fill(nWays, io.metaPort.write.en)).asBools,
      )

      io.dataPort.read.data := VecInit(
        data.map { wayData =>
          VecInit(wayData.zipWithIndex.map { case (data, w) =>
            val ren = io.dataPort.read.en && (io.dataPort.read.way === w.U)
            data.read(io.dataPort.read.set, ren)
          })(io.dataPort.read.way)
        },
      ).asUInt
      data.zipWithIndex.foreach { case (wayData, i) =>
        val writeData = io.dataPort.write.data(bankBits * (i + 1) - 1, bankBits * i)
        val writeMask = io.dataPort.write.mask(bankBytes * (i + 1) - 1, bankBytes * i)
        wayData.zipWithIndex.foreach { case (data, w) =>
          val wen = io.dataPort.write.en && (io.dataPort.write.way === w.U)
          when(wen) {
            data.write(
              io.dataPort.write.set,
              VecInit((0 until bankBytes).map(i => writeData(8 * (i + 1) - 1, 8 * i))),
            )
          }
        }
      }
    }
  }

  class DCache(implicit params: CoreParameters) extends Module {
    import params.{commonParams, axiParams, backendParams}
    import commonParams.{dataBytes, dataWidth, vaddrWidth, paddrWidth}
    import axiParams.{dataBits}
    import backendParams.{dcacheParams}
    import dcacheParams._
    private val tagWidth = paddrWidth - setWidth - blockWidth
    private val burstLen = blockBits / dataBits
    require(dataBits == dataWidth)

    val io = IO(new Bundle {
      val flush = Input(Bool())
      val axi   = new AXIBundle(axiParams)
      val req = Flipped(Decoupled(new Bundle {
        val cached = Bool()
        val cacop  = UInt(CACOPType.getWidth.W)
        val vaddr  = UInt(vaddrWidth.W)
        val paddr  = UInt(paddrWidth.W)
        val wen    = Bool()
        val wmask  = UInt(dataBytes.W)
        val wdata  = UInt(dataWidth.W)
      }))
      val resp = Decoupled(new Bundle {
        val data = UInt(dataWidth.W)
      })

      val sIdle :: sLoopup :: sMiss :: sRefill :: sWriteCache :: Nil = Enum(5)
    })
  }

  // class DCacheStage0to1(implicit paramsV」: CoreParameters) extends Module {
  //   import params.{commonParams, backendParams}
  //   import backendParams.{dcacheParams}
  //   import commonParams.{vaddrWidth, paddrWidth}
  //   import dcacheParams._
  //   private val tagWidth = paddrWidth - setWidth - blockWidth

  //   val io = IO(new Bundle {
  //     val flush = Input(Bool())
  //     val readMeta = new Bundle {
  //       val en   = Output(Bool())
  //       val set  = Output(UInt(setWidth.W))
  //       val data = Input(Vec(nWays, new DCacheMeta))
  //     }
  //     val holdRead = Input(UInt(vaddrWidth.W))
  //     val req      = Flipped(Decoupled(UInt(vaddrWidth.W)))
  //     val resp     = Decoupled(Vec(nWays, new DCacheMeta))
  //   })

  //   val en     = RegInit(false.B)
  //   val nextEn = WireDefault(en & ~io.resp.ready)
  //   en            := Mux(io.req.ready, io.req.valid, nextEn)
  //   io.req.ready  := (nextEn === 0.U) || io.flush
  //   io.resp.valid := en && !io.flush

  //   io.readMeta.en  := io.req.fire || en
  //   io.readMeta.set := getSet(Mux(io.req.ready, io.req.bits, io.holdRead))
  //   io.resp.bits    := io.readMeta.data
  // }

  //  class DCacheStage1(implicit params: CoreParameters) extends Module {
  //   import params.{commonParams, axiParams, backendParams}
  //   import commonParams.{dataWidth, vaddrWidth, paddrWidth}
  //   import axiParams.{dataBits}
  //   import backendParams.{dcacheParams}
  //   import dcacheParams._
  //   private val tagWidth = paddrWidth - setWidth - blockWidth
  //   private val burstLen = blockBits / dataBits
  //   require(dataBits == dataWidth)

  //   val io = IO(new Bundle {
  //     val axi = new AXIBundle(axiParams)
  //     val metaWrite = new Bundle {
  //       val en   = Output(Bool())
  //       val set  = Output(UInt(setWidth.W))
  //       val way  = Output(UInt(wayWidth.W))
  //       val data = Output(new DCacheMeta)
  //     }
  //     val dataWrite = new Bundle {
  //       val en   = Output(Bool())
  //       val set  = Output(UInt(setWidth.W))
  //       val way  = Output(UInt(wayWidth.W))
  //       val data = Output(UInt(blockBits.W))
  //     }
  //     val req = Flipped(Decoupled(new Bundle {
  //       val vaddr     = UInt(vaddrWidth.W)
  //       val paddr     = UInt(paddrWidth.W)
  //       val cached    = Bool()
  //       val cacop     = UInt(CACOPType.getWidth.W)
  //       val isWrite   = Bool()
  //       val writeData = UInt(dataBits.W)
  //       val writeMask = UInt((dataBits / 8).W)
  //       val meta      = Vec(nWays, (new DCacheMeta))
  //     }))
  //     val resp = Decoupled(new Bundle {
  //       val cached       = Bool()
  //       val set          = UInt(setWidth.W)
  //       val way          = UInt(wayWidth.W)
  //       val uncachedRead = UInt(axiParams.dataBits.W)
  //     })
  //   })

  //   val bits      = io.req.bits
  //   val vaddr     = bits.vaddr
  //   val paddr     = bits.paddr
  //   val cached    = bits.cached
  //   val set       = getSet(paddr)
  //   val tag       = getTag(paddr)
  //   val offset    = getOffset(paddr)
  //   val idxSet    = vaddr(blockWidth + setWidth - 1, blockWidth)
  //   val idxWay    = vaddr(wayWidth - 1, 0)
  //   val cacop     = bits.cacop
  //   val isWrite   = bits.isWrite
  //   val writeData = bits.writeData
  //   val writeMask = bits.writeMask
  //   val wayMeta   = bits.meta
  //   val isRead    = cacop === CACOP_HIT_READ.asUInt
  //   val isIdxInit = cacop === CACOP_IDX_INIT.asUInt
  //   val isIdxInv  = cacop === CACOP_IDX_INV.asUInt
  //   val isHitInv  = cacop === CACOP_HIT_INV.asUInt

  //   val matches = wayMeta.map(meta => meta.valid && meta.tag === tag)
  //   val matched = PriorityEncoder(matches)
  //   val hit     = matches.reduce(_ || _)
  //   assert(PopCount(matches) < 2.U)

  //   val invalids      = wayMeta.map(!_.valid)
  //   val random        = if (nWays == 1) 0.U else GaloisLFSR.maxPeriod(wayWidth)
  //   val replacedSel   = RegEnable(Mux(invalids.reduce(_ || _), PriorityEncoder(invalids), random), io.axi.ar.fire)
  //   val needWriteBack = wayMeta(replacedSel).valid && wayMeta(replacedSel).dirty
  //   val writeBackAddr = Cat(wayMeta(replacedSel).tag, set, 0.U(blockWidth.W))

  //   val lineData      = Reg(Vec(burstLen, UInt(dataBits.W)))
  //   val dirtyLineData = Reg(Vec(burstLen, UInt(dataBits.W)))

  //   val (burstCnt, _) =
  //     Counter(
  //       0 until burstLen,
  //       io.axi.r.valid && io.axi.r.bits.id === cacheParams.id.U,
  //       io.axi.ar.fire,
  //     )
  //   val (writeBackCnt, _) = Counter(0 until burstLen, io.axi.w.ready, io.axi.aw.fire)
  //   lineData(burstCnt) := Mux(
  //     io.axi.r.valid && io.axi.r.bits.id === cacheParams.id.U,
  //     io.axi.r.bits.data,
  //     lineData(burstCnt),
  //   )
  //   val cacopNeedWriteBack = (isHitInv && hit && wayMeta(matched).dirty) ||
  //     (isIdxInv && wayMeta(idxWay).valid && wayMeta(idxWay).dirty)

  //   // assume store never flush
  //   val sHandleReq :: sCacheMiss :: sSendBusReadReq :: sReadBusResp :: sSendBusWriteReq :: sWriteBusReq :: sWriteBusResp :: sFlushBusReq :: sFlushBusResp :: sWriteBack :: sSendReadResp :: Nil =
  //     Enum(11)

  //   val state = RegInit(sHandleReq)

  //   state := MuxLookup(state, sHandleReq)(
  //     Seq(
  //       sHandleReq -> Mux(
  //         io.req.valid,
  //         Mux(
  //           (isRead || isWrite) && (!hit || !cached),
  //           Mux(needWriteBack, sReadDirtyData, sSendBusReadReq),
  //           Mux(
  //             cacopNeedWriteBack,
  //             sReadDirtyData,
  //             sHandleReq,
  //           ),
  //         ),
  //         sHandleReq,
  //       ),
  //       sReadDirtyData -> sSendBusWriteReq,
  //       sSendBusReadReq -> Mux(
  //         io.req.valid,
  //         Mux(io.axi.ar.ready, sReadBusResp, sSendBusReadReq),
  //         Mux(io.axi.ar.ready, sFlushBusResp, sFlushBusReq),
  //       ),
  //       sReadBusResp -> Mux(
  //         io.req.valid,
  //         Mux(io.axi.r.valid && io.axi.r.bits.last.asBool, Mux(cached, sWriteBack, sSendReadResp), sReadBusResp),
  //         Mux(io.axi.r.valid && io.axi.r.bits.last.asBool, sHandleReq, sFlushBusResp),
  //       ),
  //       sSendBusWriteReq -> Mux(io.axi.aw.ready, sWriteBusReq, sSendBusWriteReq),
  //       sWriteBusReq     -> Mux(io.axi.w.ready && writeBackCnt === (burstLen - 1).U, sWriteBusResp, sWriteBusReq),
  //       sWriteBusResp    -> Mux(io.axi.b.valid, sSendBusReadReq, sWriteBusResp),
  //       sFlushBusReq     -> Mux(io.axi.ar.ready, sFlushBusResp, sFlushBusReq),
  //       sFlushBusResp    -> Mux(io.axi.r.valid && io.axi.r.bits.last.asBool, sHandleReq, sFlushBusResp),
  //       sWriteBack       -> Mux(io.req.valid, sSendReadResp, sHandleReq),
  //       sSendReadResp    -> Mux(io.req.valid, Mux(io.resp.ready, sHandleReq, sSendBusReadReq), sHandleReq),
  //     ),
  //   )

  //   when(state === sReadDirtyData) {
  //     val dirtyWay  = Mux(cacopNeedWriteBack, Mux(isIdxInv, idxWay, matched), replacedSel)
  //     val dirtyData = wayData(dirtyWay)
  //     for (i <- 0 until burstLen) {
  //       dirtyLineData(i) := dirtyData(axiParams.dataBits * (i + 1) - 1, axiParams.dataBits * i)
  //     }
  //   }

  //   io.req.ready := MuxLookup(state, false.B)(
  //     Seq(
  //       sHandleReq    -> (((isRead || isWrite) && hit && cached && io.resp.ready) || isIdxInit || isIdxInv || isHitInv),
  //       sSendReadResp -> io.resp.ready,
  //     ),
  //   )

  //   io.resp.valid := MuxLookup(state, false.B)(
  //     Seq(
  //       sHandleReq    -> (io.req.valid && (isRead || isWrite) && hit && cached),
  //       sSendReadResp -> io.req.valid,
  //     ),
  //   )

  //   io.resp.bits.cached       := cached
  //   io.resp.bits.set          := set
  //   io.resp.bits.way          := Mux(state === sSendReadResp, replacedSel, matched)
  //   io.resp.bits.uncachedRead := lineData(0)
  //   io.resp.bits.exception    := false.B

  //   io.validWrite.valid := MuxLookup(state, false.B)(
  //     Seq(
  //       sHandleReq -> (io.req.valid && ((isHitInv && hit) || isIdxInit || isIdxInv)),
  //       sWriteBack -> (io.req.valid),
  //     ),
  //   )
  //   io.validWrite.bits.all  := isIdxInit
  //   io.validWrite.bits.set  := Mux(isIdxInv, idxSet, set)
  //   io.validWrite.bits.way  := Mux(isIdxInv, idxWay, Mux(isHitInv, matched, replacedSel))
  //   io.validWrite.bits.data := (isRead || isWrite)

  //   io.dirtyWrite.valid := io.req.valid && (((state === sWriteBack) && isWrite) ||
  //     (isWrite && hit && (state === sHandleReq)) ||
  //     (state === sWriteBusResp && io.axi.b.valid) ||
  //     ((isHitInv && hit) || isIdxInv))
  //   io.dirtyWrite.bits.set := set
  //   io.dirtyWrite.bits.way := MuxCase(
  //     matched,
  //     Seq(
  //       (state === sWriteBack)    -> replacedSel,
  //       (state === sWriteBusResp) -> replacedSel,
  //       (isIdxInv)                -> idxWay,
  //     ),
  //   )
  //   io.dirtyWrite.bits.data := Mux((state === sWriteBusResp) || ((isHitInv && hit) || isIdxInv), false.B, true.B)

  //   io.tagWrite.valid     := io.req.valid && (state === sWriteBack)
  //   io.tagWrite.bits.set  := set
  //   io.tagWrite.bits.way  := replacedSel
  //   io.tagWrite.bits.data := tag

  //   val blockMask        = WireDefault(0.U((blockBits / 8).W))
  //   val offsetBytes      = offset >> log2Ceil(commonParams.dataWidth / 8)
  //   val writeMaskShifted = writeMask << offsetBytes
  //   when(isWrite && hit) {
  //     blockMask := writeMaskShifted
  //   }.elsewhen(state === sWriteBack) {
  //     blockMask := Fill(blockBits / 8, 1.U)
  //   }

  //   val mergedData = Wire(UInt(blockBits.W))
  //   mergedData := Mux(
  //     state === sWriteBack && isWrite,
  //     lineData.asUInt & ~(Fill(commonParams.dataWidth, 1.U) << (offset * 8.U)) | (writeData << (offset * 8.U)),
  //     lineData.asUInt,
  //   )

  //   io.dataWrite.valid    := io.req.valid && ((state === sWriteBack) || (isWrite && hit))
  //   io.dataWrite.bits.set := set
  //   io.dataWrite.bits.way := Mux(state === sWriteBack, replacedSel, matched)
  //   io.dataWrite.bits.data := Mux(
  //     state === sWriteBack,
  //     mergedData,
  //     Mux(isWrite && hit, (wayData(matched) & ~(Fill(commonParams.dataWidth / 8, 1.U) << (offset * 8.U))) | (writeData << (offset * 8.U)), wayData(matched)),
  //   )
  //   io.dataWrite.bits.mask := blockMask

  //   io.axi.ar.valid      := (state === sSendBusReadReq) || (state === sFlushBusReq)
  //   io.axi.ar.bits.addr  := paddr & ~(cacheParams.blockBytes - 1).U(commonParams.paddrWidth.W)
  //   io.axi.ar.bits.id    := id.U
  //   io.axi.ar.bits.len   := Mux(cached, (burstLen - 1).U, 0.U)
  //   io.axi.ar.bits.size  := log2Ceil(axiParams.dataBits / 8).U
  //   io.axi.ar.bits.burst := AXIParameters.BURST_INCR
  //   io.axi.ar.bits.lock  := 0.U
  //   io.axi.ar.bits.cache := 0.U
  //   io.axi.ar.bits.prot  := 0.U

  //   io.axi.r.ready := (state === sReadBusResp) || (state === sFlushBusResp)

  //   io.axi.aw.valid      := (state === sSendBusWriteReq)
  //   io.axi.aw.bits.addr  := writeBackAddr
  //   io.axi.aw.bits.id    := id.U
  //   io.axi.aw.bits.len   := (burstLen - 1).U
  //   io.axi.aw.bits.size  := log2Ceil(axiParams.dataBits / 8).U
  //   io.axi.aw.bits.burst := AXIParameters.BURST_INCR
  //   io.axi.aw.bits.lock  := 0.U
  //   io.axi.aw.bits.cache := 0.U
  //   io.axi.aw.bits.prot  := 0.U

  //   io.axi.w.valid     := (state === sWriteBusReq)
  //   io.axi.w.bits.data := dirtyLineData(writeBackCnt)
  //   io.axi.w.bits.strb := Fill(axiParams.dataBits / 8, 1.U)
  //   io.axi.w.bits.last := writeBackCnt === (burstLen - 1).U
  //   io.axi.w.bits.id   := id.U

  //   io.axi.b.ready := (state === sWriteBusResp)
  // }
}

// class DCacheStage0to1(implicit commonParams: CommonParameters, cacheParams: CacheParameters) extends Module {
//   import commonParams.{vaddrWidth, paddrWidth}
//   import cacheParams._
//   private val tagWidth = paddrWidth - setWidth - blockWidth

//   val io = IO(new Bundle {
//     val flush = Input(Bool())
//     val req   = Flipped(Decoupled(UInt(vaddrWidth.W)))
//     val resp = Decoupled(new Bundle {
//       val cacheValid = Vec(nSets, Vec(nWays, Bool()))
//       val cacheDirty = Vec(nSets, Vec(nWays, Bool()))
//       val wayTag     = Vec(nWays, UInt(tagWidth.W))
//       val wayData    = Vec(nWays, UInt(blockBits.W))
//     })
//     val keepRead   = Input(UInt(vaddrWidth.W))
//     val validWrite = Flipped(Valid(new DCacheValidWrite))
//     val dirtyWrite = Flipped(Valid(new DCacheDirtyWrite))
//     val tagWrite   = Flipped(Valid(new DCacheTagWrite))
//     val dataWrite  = Flipped(Valid(new DCacheDataWrite))
//   })

//   val cacheValid = RegInit(VecInit.fill(nSets, nWays)(false.B))
//   val cacheDirty = RegInit(VecInit.fill(nSets, nWays)(false.B))
//   val cacheTag   = SyncReadMem(nSets, Vec(nWays, UInt(tagWidth.W)))

//   val cacheData = Seq.tabulate(nBanks) { _ =>
//     (0 until nWays).map { _ =>
//       SyncReadMem(nSets, UInt(bankBits.W))
//     }
//   }

//   val en     = RegInit(false.B)
//   val nextEn = WireDefault(en & ~io.resp.ready)
//   en            := Mux(io.req.ready, io.req.valid, nextEn)
//   io.req.ready  := (nextEn === 0.U) || io.flush
//   io.resp.valid := en && !io.flush

//   val readEn  = io.req.fire || nextEn
//   val readSet = getSet(Mux(io.req.fire, io.req.bits, io.keepRead))

//   val validWriteEn = io.validWrite.valid
//   val validWrite   = io.validWrite.bits
//   val dirtyWriteEn = io.dirtyWrite.valid
//   val dirtyWrite   = io.dirtyWrite.bits
//   for (set <- 0 until nSets) {
//     for (way <- 0 until nWays) {
//       val v       = cacheValid(set)(way)
//       val validWe = validWriteEn && ((validWrite.set === set.U && validWrite.way === way.U) || validWrite.all)
//       cacheValid(set)(way) := Mux(validWe, validWrite.data, v)

//       val d       = cacheDirty(set)(way)
//       val dirtyWe = dirtyWriteEn && (dirtyWrite.set === set.U && dirtyWrite.way === way.U)
//       cacheDirty(set)(way) := Mux(dirtyWe, dirtyWrite.data, d)
//     }
//   }

//   if (!singlePorted) {
//     val tagWriteEn   = io.tagWrite.valid
//     val tagWrite     = io.tagWrite.bits
//     val tagWriteData = WireDefault(VecInit.fill(nWays)(tagWrite.data))
//     cacheTag.write(tagWrite.set, tagWriteData, (UIntToOH(tagWrite.way) & Fill(wayWidth, tagWriteEn)).asBools)

//     val dataWrite = io.dataWrite.bits
//     for (i <- 0 until nBanks) {
//       for (w <- 0 until nWays) {
//         val dataWriteEn   = io.dataWrite.valid && (dataWrite.way === w.U)
//         val dataWriteData = WireDefault(dataWrite.data(bankBits * (i + 1) - 1, bankBits * i))
//         val dataWriteMask = dataWrite.mask(bankBits / 8 * (i + 1) - 1, bankBits / 8 * i)
//         when(dataWriteEn && dataWriteMask.orR) {
//           cacheData(i)(w).write(dataWrite.set, dataWriteData)
//         }
//       }
//     }
//   }

//   val wayTag = if (singlePorted) {
//     val tagWriteEn   = io.tagWrite.valid
//     val tagWrite     = io.tagWrite.bits
//     val tagWriteData = WireDefault(VecInit.fill(nWays)(tagWrite.data))
//     cacheTag.readWrite(
//       Mux(tagWriteEn, tagWrite.set, readSet),
//       tagWriteData,
//       (UIntToOH(tagWrite.way) & Fill(wayWidth, tagWriteEn)).asBools,
//       readEn || tagWriteEn,
//       tagWriteEn,
//     )
//   } else {
//     cacheTag.read(readSet, io.req.fire)
//   }

//   val wayData = if (singlePorted) {
//     val dataWrite = io.dataWrite.bits
//     VecInit((0 until nWays).map { w =>
//       VecInit(cacheData.zipWithIndex.map { case (cacheDataWays, i) =>
//         val dataWriteData = WireDefault(dataWrite.data(bankBits * (i + 1) - 1, bankBits * i))
//         val dataWriteMask = dataWrite.mask(bankBits / 8 * (i + 1) - 1, bankBits / 8 * i)
//         val dataWriteEn   = io.dataWrite.valid && (dataWrite.way === w.U) && dataWriteMask.orR
//         cacheDataWays(w).readWrite(
//           Mux(dataWriteEn, dataWrite.set, readSet),
//           dataWriteData,
//           readEn || dataWriteEn,
//           dataWriteEn,
//         )
//       }).asUInt
//     })
//   } else {
//     VecInit((0 until nWays).map { w =>
//       VecInit(cacheData.map(_(w).read(readSet, readEn))).asUInt
//     })
//   }

//   io.resp.bits.cacheValid := cacheValid
//   io.resp.bits.cacheDirty := cacheDirty
//   io.resp.bits.wayTag     := wayTag
//   io.resp.bits.wayData    := wayData
// }

// class DCacheStage1(implicit commonParams: CommonParameters, cacheParams: CacheParameters, axiParams: AXIBundleParameters) extends Module {
//   import commonParams.{vaddrWidth, paddrWidth}
//   import cacheParams._
//   private val tagWidth = paddrWidth - setWidth - blockWidth
//   private val burstLen = blockBits / axiParams.dataBits

//   val io = IO(new Bundle {
//     val axi        = new AXIBundle(axiParams)
//     val validWrite = Valid(new DCacheValidWrite)
//     val dirtyWrite = Valid(new DCacheDirtyWrite)
//     val tagWrite   = Valid(new DCacheTagWrite)
//     val dataWrite  = Valid(new DCacheDataWrite)
//     val req = Flipped(Decoupled(new Bundle {
//       val vaddr      = UInt(vaddrWidth.W)
//       val paddr      = UInt(paddrWidth.W)
//       val cacop      = UInt(CACOPType.getWidth.W)
//       val isWrite    = Bool()
//       val writeData  = UInt(commonParams.dataWidth.W)
//       val writeMask  = UInt((commonParams.dataWidth / 8).W)
//       val cached     = Bool()
//       val cacheValid = Vec(nSets, Vec(nWays, Bool()))
//       val cacheDirty = Vec(nSets, Vec(nWays, Bool()))
//       val wayTag     = Vec(nWays, UInt(tagWidth.W))
//       val wayData    = Vec(nWays, UInt(blockBits.W))
//     }))
//     val resp = Decoupled(new Bundle {
//       val cached       = Bool()
//       val set          = UInt(setWidth.W)
//       val way          = UInt(wayWidth.W)
//       val uncachedRead = UInt(axiParams.dataBits.W)
//       val exception    = Bool()
//     })
//   })

//   val bits      = io.req.bits
//   val vaddr     = bits.vaddr
//   val paddr     = bits.paddr
//   val cached    = bits.cached
//   val set       = getSet(paddr)
//   val tag       = getTag(paddr)
//   val offset    = getOffset(paddr)
//   val cacop     = bits.cacop
//   val isWrite   = bits.isWrite
//   val writeData = bits.writeData
//   val writeMask = bits.writeMask
//   val wayValid  = bits.cacheValid(set)
//   val wayDirty  = bits.cacheDirty(set)
//   val wayTag    = bits.wayTag
//   val isRead    = cacop === CACOP_HIT_READ.asUInt
//   val isIdxInit = cacop === CACOP_IDX_INIT.asUInt
//   val isIdxInv  = cacop === CACOP_IDX_INV.asUInt
//   val isHitInv  = cacop === CACOP_HIT_INV.asUInt

//   val wayData = bits.wayData

//   val sHandleReq :: sReadDirtyData :: sSendBusReadReq :: sReadBusResp :: sSendBusWriteReq :: sWriteBusReq :: sWriteBusResp :: sFlushBusReq :: sFlushBusResp :: sWriteBack :: sSendReadResp :: Nil =
//     Enum(11)

//   val state     = RegInit(sHandleReq)
//   val nextState = WireDefault(sHandleReq)
//   val hit       = Wire(Bool())

//   val matches = wayTag.map(tag === _)
//   val matched = PriorityEncoder(matches)
//   hit := matches.reduce(_ || _) && wayValid(matched)

//   val idxSet = vaddr(blockWidth + setWidth - 1, blockWidth)
//   val idxWay = vaddr.tail(wayWidth)

//   val random        = if (nWays == 1) 0.U else GaloisLFSR.maxPeriod(wayWidth)
//   val replacedSel   = RegEnable(Mux(wayValid.contains(false.B), PriorityEncoder(wayValid.map(!_)), random), io.axi.ar.fire || io.axi.aw.fire)
//   val needWriteBack = wayValid(replacedSel) && wayDirty(replacedSel)
//   val writeBackAddr = Cat(wayTag(replacedSel), set, 0.U(blockWidth.W))

//   val lineData      = Reg(Vec(burstLen, UInt(axiParams.dataBits.W)))
//   val dirtyLineData = Reg(Vec(burstLen, UInt(axiParams.dataBits.W)))

//   val (burstCnt, _)     = Counter(0 until burstLen, io.axi.r.valid, io.axi.ar.fire)
//   val (writeBackCnt, _) = Counter(0 until burstLen, io.axi.w.ready, io.axi.aw.fire)

//   val cacopNeedWriteBack = (isHitInv && hit && wayDirty(matched)) || (isIdxInv && wayValid(idxWay) && wayDirty(idxWay))

//   nextState := MuxLookup(state, sHandleReq)(
//     Seq(
//       sHandleReq -> Mux(
//         io.req.valid,
//         Mux(
//           (isRead || isWrite) && (!hit || !cached),
//           Mux(needWriteBack, sReadDirtyData, sSendBusReadReq),
//           Mux(
//             cacopNeedWriteBack,
//             sReadDirtyData,
//             sHandleReq,
//           ),
//         ),
//         sHandleReq,
//       ),
//       sReadDirtyData -> sSendBusWriteReq,
//       sSendBusReadReq -> Mux(
//         io.req.valid,
//         Mux(io.axi.ar.ready, sReadBusResp, sSendBusReadReq),
//         Mux(io.axi.ar.ready, sFlushBusResp, sFlushBusReq),
//       ),
//       sReadBusResp -> Mux(
//         io.req.valid,
//         Mux(io.axi.r.valid && io.axi.r.bits.last.asBool, Mux(cached, sWriteBack, sSendReadResp), sReadBusResp),
//         Mux(io.axi.r.valid && io.axi.r.bits.last.asBool, sHandleReq, sFlushBusResp),
//       ),
//       sSendBusWriteReq -> Mux(io.axi.aw.ready, sWriteBusReq, sSendBusWriteReq),
//       sWriteBusReq     -> Mux(io.axi.w.ready && writeBackCnt === (burstLen - 1).U, sWriteBusResp, sWriteBusReq),
//       sWriteBusResp    -> Mux(io.axi.b.valid, sSendBusReadReq, sWriteBusResp),
//       sFlushBusReq     -> Mux(io.axi.ar.ready, sFlushBusResp, sFlushBusReq),
//       sFlushBusResp    -> Mux(io.axi.r.valid && io.axi.r.bits.last.asBool, sHandleReq, sFlushBusResp),
//       sWriteBack       -> Mux(io.req.valid, sSendReadResp, sHandleReq),
//       sSendReadResp    -> Mux(io.req.valid, Mux(io.resp.ready, sHandleReq, sSendBusReadReq), sHandleReq),
//     ),
//   )
//   state := nextState

//   lineData(burstCnt) := Mux(io.axi.r.valid, io.axi.r.bits.data, lineData(burstCnt))

//   when(state === sReadDirtyData) {
//     val dirtyWay  = Mux(cacopNeedWriteBack, Mux(isIdxInv, idxWay, matched), replacedSel)
//     val dirtyData = wayData(dirtyWay)
//     for (i <- 0 until burstLen) {
//       dirtyLineData(i) := dirtyData(axiParams.dataBits * (i + 1) - 1, axiParams.dataBits * i)
//     }
//   }

//   io.req.ready := MuxLookup(state, false.B)(
//     Seq(
//       sHandleReq    -> (((isRead || isWrite) && hit && cached && io.resp.ready) || isIdxInit || isIdxInv || isHitInv),
//       sSendReadResp -> io.resp.ready,
//     ),
//   )

//   io.resp.valid := MuxLookup(state, false.B)(
//     Seq(
//       sHandleReq    -> (io.req.valid && (isRead || isWrite) && hit && cached),
//       sSendReadResp -> io.req.valid,
//     ),
//   )

//   io.resp.bits.cached       := cached
//   io.resp.bits.set          := set
//   io.resp.bits.way          := Mux(state === sSendReadResp, replacedSel, matched)
//   io.resp.bits.uncachedRead := lineData(0)
//   io.resp.bits.exception    := false.B

//   io.validWrite.valid := MuxLookup(state, false.B)(
//     Seq(
//       sHandleReq -> (io.req.valid && ((isHitInv && hit) || isIdxInit || isIdxInv)),
//       sWriteBack -> (io.req.valid),
//     ),
//   )
//   io.validWrite.bits.all  := isIdxInit
//   io.validWrite.bits.set  := Mux(isIdxInv, idxSet, set)
//   io.validWrite.bits.way  := Mux(isIdxInv, idxWay, Mux(isHitInv, matched, replacedSel))
//   io.validWrite.bits.data := (isRead || isWrite)

//   io.dirtyWrite.valid := io.req.valid && (((state === sWriteBack) && isWrite) ||
//     (isWrite && hit && (state === sHandleReq)) ||
//     (state === sWriteBusResp && io.axi.b.valid) ||
//     ((isHitInv && hit) || isIdxInv))
//   io.dirtyWrite.bits.set := set
//   io.dirtyWrite.bits.way := MuxCase(
//     matched,
//     Seq(
//       (state === sWriteBack)    -> replacedSel,
//       (state === sWriteBusResp) -> replacedSel,
//       (isIdxInv)                -> idxWay,
//     ),
//   )
//   io.dirtyWrite.bits.data := Mux((state === sWriteBusResp) || ((isHitInv && hit) || isIdxInv), false.B, true.B)

//   io.tagWrite.valid     := io.req.valid && (state === sWriteBack)
//   io.tagWrite.bits.set  := set
//   io.tagWrite.bits.way  := replacedSel
//   io.tagWrite.bits.data := tag

//   val blockMask        = WireDefault(0.U((blockBits / 8).W))
//   val offsetBytes      = offset >> log2Ceil(commonParams.dataWidth / 8)
//   val writeMaskShifted = writeMask << offsetBytes
//   when(isWrite && hit) {
//     blockMask := writeMaskShifted
//   }.elsewhen(state === sWriteBack) {
//     blockMask := Fill(blockBits / 8, 1.U)
//   }

//   val mergedData = Wire(UInt(blockBits.W))
//   mergedData := Mux(
//     state === sWriteBack && isWrite,
//     lineData.asUInt & ~(Fill(commonParams.dataWidth, 1.U) << (offset * 8.U)) | (writeData << (offset * 8.U)),
//     lineData.asUInt,
//   )

//   io.dataWrite.valid    := io.req.valid && ((state === sWriteBack) || (isWrite && hit))
//   io.dataWrite.bits.set := set
//   io.dataWrite.bits.way := Mux(state === sWriteBack, replacedSel, matched)
//   io.dataWrite.bits.data := Mux(
//     state === sWriteBack,
//     mergedData,
//     Mux(isWrite && hit, (wayData(matched) & ~(Fill(commonParams.dataWidth / 8, 1.U) << (offset * 8.U))) | (writeData << (offset * 8.U)), wayData(matched)),
//   )
//   io.dataWrite.bits.mask := blockMask

//   io.axi.ar.valid      := (state === sSendBusReadReq) || (state === sFlushBusReq)
//   io.axi.ar.bits.addr  := paddr & ~(cacheParams.blockBytes - 1).U(commonParams.paddrWidth.W)
//   io.axi.ar.bits.id    := id.U
//   io.axi.ar.bits.len   := Mux(cached, (burstLen - 1).U, 0.U)
//   io.axi.ar.bits.size  := log2Ceil(axiParams.dataBits / 8).U
//   io.axi.ar.bits.burst := AXIParameters.BURST_INCR
//   io.axi.ar.bits.lock  := 0.U
//   io.axi.ar.bits.cache := 0.U
//   io.axi.ar.bits.prot  := 0.U

//   io.axi.r.ready := (state === sReadBusResp) || (state === sFlushBusResp)

//   io.axi.aw.valid      := (state === sSendBusWriteReq)
//   io.axi.aw.bits.addr  := writeBackAddr
//   io.axi.aw.bits.id    := id.U
//   io.axi.aw.bits.len   := (burstLen - 1).U
//   io.axi.aw.bits.size  := log2Ceil(axiParams.dataBits / 8).U
//   io.axi.aw.bits.burst := AXIParameters.BURST_INCR
//   io.axi.aw.bits.lock  := 0.U
//   io.axi.aw.bits.cache := 0.U
//   io.axi.aw.bits.prot  := 0.U

//   io.axi.w.valid     := (state === sWriteBusReq)
//   io.axi.w.bits.data := dirtyLineData(writeBackCnt)
//   io.axi.w.bits.strb := Fill(axiParams.dataBits / 8, 1.U)
//   io.axi.w.bits.last := writeBackCnt === (burstLen - 1).U
//   io.axi.w.bits.id   := id.U

//   io.axi.b.ready := (state === sWriteBusResp)
// }

// class DCacheStage1to2(implicit commonParams: CommonParameters, cacheParams: CacheParameters, axiParams: AXIBundleParameters) extends Module {
//   import commonParams.{vaddrWidth, paddrWidth}
//   import cacheParams._
//   private val tagWidth = paddrWidth - setWidth - blockWidth

//   val io = IO(new Bundle {
//     val flush = Input(Bool())
//     val req = Flipped(Decoupled(new Bundle {
//       val cached       = Bool()
//       val set          = UInt(setWidth.W)
//       val way          = UInt(wayWidth.W)
//       val offset       = UInt(blockWidth.W)
//       val exception    = Bool()
//       val data         = UInt(blockBits.W)
//       val uncachedRead = UInt(axiParams.dataBits.W)
//     }))
//     val resp = Decoupled(new Bundle {
//       val cached    = Bool()
//       val data      = UInt(commonParams.dataWidth.W)
//       val exception = Bool()
//     })
//     val keepRead = Input(new Bundle {
//       val set    = UInt(setWidth.W)
//       val way    = UInt(wayWidth.W)
//       val offset = UInt(blockWidth.W)
//     })
//     val dataWrite = Flipped(Valid(new DCacheDataWrite))
//   })

//   val en     = RegInit(false.B)
//   val nextEn = WireDefault(en & ~io.resp.ready)
//   en            := Mux(io.req.ready, io.req.valid, nextEn)
//   io.req.ready  := (nextEn === 0.U) || io.flush
//   io.resp.valid := en && !io.flush

//   val readOffset = Mux(io.req.fire, io.req.bits.offset, io.keepRead.offset)
//   val data       = RegEnable(io.req.bits.data, io.req.fire)

//   val cached       = RegEnable(io.req.bits.cached, io.req.fire)
//   val uncachedRead = RegEnable(io.req.bits.uncachedRead, io.req.fire)

//   val wordOffset            = readOffset >> log2Ceil(commonParams.dataWidth / 8)
//   val extractedData         = (data >> (wordOffset * commonParams.dataWidth.U))(commonParams.dataWidth - 1, 0)
//   val extractedUncachedData = uncachedRead(commonParams.dataWidth - 1, 0)

//   io.resp.bits.cached    := cached
//   io.resp.bits.exception := RegEnable(io.req.bits.exception, io.req.fire)
//   io.resp.bits.data      := Mux(cached, extractedData, extractedUncachedData)
// }

// class DCache(implicit commonParams: CommonParameters, cacheParams: CacheParameters, axiParams: AXIBundleParameters) extends Module {
//   import commonParams.{vaddrWidth, paddrWidth}
//   import cacheParams._
//   private val tagWidth = paddrWidth - setWidth - blockWidth

//   val io = IO(new Bundle {
//     val flush = Input(new Bundle {
//       val stage1 = Bool()
//       val stage2 = Bool()
//     })
//     val axi = new AXIBundle(axiParams)
//     val req = new Bundle {
//       val stage0 = Flipped(Decoupled(new Bundle {
//         val vaddr = UInt(vaddrWidth.W)
//       }))
//       val stage1 = Flipped(Decoupled(new Bundle {
//         val vaddr     = UInt(vaddrWidth.W)
//         val paddr     = UInt(paddrWidth.W)
//         val cached    = Bool()
//         val cacop     = UInt(CACOPType.getWidth.W)
//         val isWrite   = Bool()
//         val writeData = UInt(commonParams.dataWidth.W)
//         val writeMask = UInt((commonParams.dataWidth / 8).W)
//       }))
//     }
//     val resp = new Bundle {
//       val stage2 = Decoupled(new Bundle {
//         val cached    = Bool()
//         val data      = UInt(commonParams.dataWidth.W)
//         val exception = Bool()
//       })
//     }
//   })

//   val stage0to1 = Module(new DCacheStage0to1)
//   val stage1    = Module(new DCacheStage1)
//   val stage1to2 = Module(new DCacheStage1to2)

//   stage0to1.io.flush := io.flush.stage1
//   stage1to2.io.flush := io.flush.stage2

//   stage0to1.io.req.valid := io.req.stage0.valid
//   stage0to1.io.req.bits  := io.req.stage0.bits.vaddr
//   io.req.stage0.ready    := stage0to1.io.req.ready
//   stage0to1.io.keepRead  := io.req.stage1.bits.vaddr

//   io.axi                        <> stage1.io.axi
//   stage1.io.req.valid           := stage0to1.io.resp.valid && io.req.stage1.valid
//   stage0to1.io.resp.ready       := stage1.io.req.ready
//   io.req.stage1.ready           := stage1.io.req.ready
//   stage1.io.req.bits.vaddr      := io.req.stage1.bits.vaddr
//   stage1.io.req.bits.paddr      := io.req.stage1.bits.paddr
//   stage1.io.req.bits.cached     := io.req.stage1.bits.cached
//   stage1.io.req.bits.cacop      := io.req.stage1.bits.cacop
//   stage1.io.req.bits.isWrite    := io.req.stage1.bits.isWrite
//   stage1.io.req.bits.writeData  := io.req.stage1.bits.writeData
//   stage1.io.req.bits.writeMask  := io.req.stage1.bits.writeMask
//   stage1.io.req.bits.cacheValid := stage0to1.io.resp.bits.cacheValid
//   stage1.io.req.bits.cacheDirty := stage0to1.io.resp.bits.cacheDirty
//   stage1.io.req.bits.wayTag     := stage0to1.io.resp.bits.wayTag
//   stage1.io.req.bits.wayData    := stage0to1.io.resp.bits.wayData
//   stage0to1.io.validWrite       := stage1.io.validWrite
//   stage0to1.io.dirtyWrite       := stage1.io.dirtyWrite
//   stage0to1.io.tagWrite         := stage1.io.tagWrite
//   stage0to1.io.dataWrite        := stage1.io.dataWrite
//   stage1to2.io.dataWrite        := stage1.io.dataWrite

//   val stage1to2Keep = RegEnable(
//     Cat(stage1.io.resp.bits.set, stage1.io.resp.bits.way, getOffset(io.req.stage1.bits.paddr)),
//     stage1.io.resp.fire,
//   )
//   stage1to2.io.req.valid             := stage1.io.resp.valid
//   stage1to2.io.req.bits.set          := stage1.io.resp.bits.set
//   stage1to2.io.req.bits.way          := stage1.io.resp.bits.way
//   stage1to2.io.req.bits.offset       := getOffset(io.req.stage1.bits.paddr)
//   stage1to2.io.req.bits.cached       := stage1.io.resp.bits.cached
//   stage1to2.io.req.bits.uncachedRead := stage1.io.resp.bits.uncachedRead
//   stage1to2.io.req.bits.exception    := stage1.io.resp.bits.exception
//   stage1to2.io.req.bits.data         := stage0to1.io.resp.bits.wayData(stage1.io.resp.bits.way)
//   stage1.io.resp.ready               := stage1to2.io.req.ready

//   stage1to2.io.keepRead.set    := stage1to2Keep(setWidth + wayWidth + blockWidth - 1, wayWidth + blockWidth)
//   stage1to2.io.keepRead.way    := stage1to2Keep(wayWidth + blockWidth - 1, blockWidth)
//   stage1to2.io.keepRead.offset := stage1to2Keep(blockWidth - 1, 0)

//   io.resp.stage2 <> stage1to2.io.resp
// }
