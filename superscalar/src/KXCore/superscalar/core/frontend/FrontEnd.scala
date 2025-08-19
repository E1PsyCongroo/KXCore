package KXCore.superscalar.core.frontend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.Privilege._
import KXCore.common.peripheral._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.backend._

class FrontEndIO(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, axiParams, frontendParams}
  import commonParams.{vaddrWidth, paddrWidth}
  import frontendParams.{ftqIdxWidth}

  val axi      = new AXIBundle(axiParams)
  val itlbReq  = Output(new TLBReq)
  val itlbResp = Input(new TLBResp)

  val fetchPacket = Decoupled(new FetchBufferResp())

  val icacheClear = Input(Bool())
  val icacheCacop = Flipped(Decoupled(new CacheCACOPIO))
  val ftqReqs     = Input(Vec(3, UInt(ftqIdxWidth.W)))
  val ftqResps    = Output(Vec(3, new FTQInfo))

  val commit = Flipped(Valid(UInt(ftqIdxWidth.W)))
  val redirect = Input(new Bundle {
    val valid      = Bool()
    val target     = UInt(vaddrWidth.W)
    val ftqIdx     = UInt(ftqIdxWidth.W)
    val brRecovery = new BrRecoveryInfo
  })
}

class FrontEnd(implicit params: CoreParameters) extends Module {
  import params._
  import commonParams.{vaddrWidth, paddrWidth, instWidth, instBytes, pcReset}
  import frontendParams._
  import CACOPType._

  val io = IO(new FrontEndIO)

  val icache = Module(new ICache.ICacheStorage)
  val bpu    = Module(new BranchPredictor.BranchPredictorStorage)

  val flush = Wire(new Bundle {
    val stage1 = Bool()
    val stage2 = Bool()
  })

  val stage1Redirect  = Wire(Valid(UInt(vaddrWidth.W)))
  val stage2Redirect  = Wire(Valid(UInt(vaddrWidth.W)))
  val backendRedirect = Wire(Valid(UInt(vaddrWidth.W)))
  backendRedirect.valid := io.redirect.valid
  backendRedirect.bits  := io.redirect.target

  flush.stage1 := backendRedirect.valid || stage2Redirect.valid
  flush.stage2 := backendRedirect.valid

  // stage0: pre-fetch
  val icacheArb       = Module(new Arbiter(new CacheCACOPIO, 2))
  val icacheStage0to1 = Module(new ICache.ICacheStage0to1)
  val bpuStage0to1    = Module(new BranchPredictor.BranchPredictorStage0to1)
  val pipeStage0to1 = Module(
    new PipeStageReg(
      new Bundle {
        val fetchPC   = UInt(vaddrWidth.W)
        val cacop     = UInt(CACOPType.getWidth.W)
        val cached    = Bool()
        val paddr     = UInt(paddrWidth.W)
        val exception = Valid(UInt(ECODE.getWidth.W))
      },
      true,
    ),
  )

  val s0_fetchPC   = Wire(UInt(vaddrWidth.W))
  val s0_npc       = RegInit(pcReset.U)
  val s0_cacop     = WireInit(icacheArb.io.out.bits.cacop)
  val s0_cached    = io.itlbResp.mat(0)
  val s0_vaddr     = WireInit(icacheArb.io.out.bits.vaddr)
  val s0_paddr     = WireInit(icacheArb.io.out.bits.paddr)
  val s0_isAdef    = Wire(Bool())
  val s0_exception = Wire(Valid(UInt(ECODE.getWidth.W)))
  val s0_fire      = pipeStage0to1.io.in.ready && icacheStage0to1.io.req.ready && bpuStage0to1.io.req.ready
  s0_fetchPC := MuxCase(
    s0_npc,
    Seq(
      backendRedirect.valid -> backendRedirect.bits,
      stage2Redirect.valid  -> stage2Redirect.bits,
      stage1Redirect.valid  -> stage1Redirect.bits,
    ),
  )
  s0_npc    := s0_fetchPC
  s0_isAdef := s0_vaddr(1, 0) =/= 0.U
  s0_exception.valid := (s0_cacop === CACOP_NONE.asUInt) && (s0_isAdef ||
    io.itlbResp.exception.valid)
  s0_exception.bits := Mux(s0_isAdef, ECODE.ADEF.asUInt, io.itlbResp.exception.bits)

  icacheStage0to1.io.flush   := flush.stage1
  bpuStage0to1.io.flush      := flush.stage1
  pipeStage0to1.io.flush.get := flush.stage1

  icacheStage0to1.io.readMeta <> icache.io.metaPort.read
  bpuStage0to1.io.bimRead     <> bpu.io.bim
  bpuStage0to1.io.btbRead     <> bpu.io.btb

  icacheArb.io.in(0).valid      := io.icacheCacop.valid
  icacheArb.io.in(0).bits.cacop := io.icacheCacop.bits.cacop
  icacheArb.io.in(0).bits.vaddr := io.icacheCacop.bits.vaddr
  icacheArb.io.in(0).bits.paddr := io.icacheCacop.bits.paddr
  io.icacheCacop.ready          := icacheArb.io.in(0).ready

  icacheArb.io.in(1).valid      := true.B
  icacheArb.io.in(1).bits.cacop := CACOP_NONE.asUInt
  icacheArb.io.in(1).bits.vaddr := s0_fetchPC
  icacheArb.io.in(1).bits.paddr := io.itlbResp.paddr

  icacheArb.io.out.ready := s0_fire

  io.itlbReq.isWrite := false.B
  io.itlbReq.vaddr   := icacheArb.io.out.bits.vaddr

  pipeStage0to1.io.in.valid          := s0_fire
  pipeStage0to1.io.in.bits.cacop     := s0_cacop
  pipeStage0to1.io.in.bits.cached    := s0_cached
  pipeStage0to1.io.in.bits.fetchPC   := s0_vaddr
  pipeStage0to1.io.in.bits.paddr     := s0_paddr
  pipeStage0to1.io.in.bits.exception := s0_exception

  icacheStage0to1.io.req.valid := s0_fire && !s0_exception.valid
  icacheStage0to1.io.req.bits  := s0_vaddr

  bpuStage0to1.io.req.valid := s0_fire && !s0_exception.valid &&
    s0_cacop === CACOPType.CACOP_NONE.asUInt
  bpuStage0to1.io.req.bits := s0_fetchPC
  bpuStage0to1.io.holdRead := pipeStage0to1.io.out.bits.fetchPC

  // stage1: fetch & branch prediction
  val icacheStage1    = Module(new ICache.ICacheStage1)
  val bpuStage1       = Module(new BranchPredictor.BranchPredictorStage1)
  val icacheStage1to2 = Module(new ICache.ICacheStage1to2)
  val pipeStage1to2 = Module(
    new PipeStageReg(
      new Bundle {
        val fetchPC   = UInt(vaddrWidth.W)
        val exception = Valid(UInt(ECODE.getWidth.W))

        val icache = new Bundle {
          val cached       = Bool()
          val set          = UInt(icacheParams.setWidth.W)
          val way          = UInt(icacheParams.wayWidth.W)
          val uncachedRead = UInt(icacheParams.blockBits.W)
        }
        val bpu = new Bundle {
          val pred = Vec(fetchWidth, new BranchPrediction)
          val meta = new Bundle {
            val bim = Vec(fetchWidth, UInt(2.W))
            val btb = UInt(log2Ceil(btbParams.nWays).W)
          }
        }
        val redirect = Bool()
        val npc      = UInt(vaddrWidth.W)
      },
      true,
    ),
  )

  val s1_fetchPC    = WireInit(pipeStage0to1.io.out.bits.fetchPC)
  val s1_cacop      = WireInit(pipeStage0to1.io.out.bits.cacop)
  val s1_cached     = WireInit(pipeStage0to1.io.out.bits.cached)
  val s1_paddr      = WireInit(pipeStage0to1.io.out.bits.paddr)
  val s1_exception  = WireInit(pipeStage0to1.io.out.bits.exception)
  val s1_icacheResp = WireInit(icacheStage1.io.resp.bits)
  val s1_bpuResp    = WireInit(bpuStage1.io.resp.bits)
  val s1_fire = pipeStage0to1.io.out.valid && Mux(
    pipeStage0to1.io.out.bits.exception.valid,
    pipeStage1to2.io.in.ready,
    Mux(
      pipeStage0to1.io.out.bits.cacop =/= CACOP_NONE.asUInt,
      icacheStage0to1.io.resp.valid && icacheStage1.io.req.ready,
      icacheStage1.io.resp.valid && bpuStage1.io.resp.valid && icacheStage1to2.io.req.ready && pipeStage1to2.io.in.ready,
    ),
  )

  pipeStage0to1.io.out.ready    := s1_fire
  icacheStage0to1.io.resp.ready := s1_fire
  bpuStage0to1.io.resp.ready    := s1_fire

  icacheStage1to2.io.flush   := flush.stage2
  pipeStage1to2.io.flush.get := flush.stage2

  icacheStage1to2.io.readData <> icache.io.dataPort.read

  icacheStage1.io.axi       <> io.axi
  icacheStage1.io.metaWrite <> icache.io.metaPort.write
  icacheStage1.io.dataWrite <> icache.io.dataPort.write

  icacheStage1.io.req.valid       := pipeStage0to1.io.out.valid && icacheStage0to1.io.resp.valid
  icacheStage1.io.req.bits.cacop  := s1_cacop
  icacheStage1.io.req.bits.cached := s1_cached
  icacheStage1.io.req.bits.vaddr  := s1_fetchPC
  icacheStage1.io.req.bits.paddr  := s1_paddr
  icacheStage1.io.req.bits.meta   := icacheStage0to1.io.resp.bits

  bpuStage1.io.req.valid            := pipeStage0to1.io.out.valid && bpuStage0to1.io.resp.valid
  bpuStage1.io.req.bits.bim         := bpuStage0to1.io.resp.bits.bim
  bpuStage1.io.req.bits.btb.fetchPC := s1_fetchPC
  bpuStage1.io.req.bits.btb.meta    := bpuStage0to1.io.resp.bits.btb.meta
  bpuStage1.io.req.bits.btb.btb     := bpuStage0to1.io.resp.bits.btb.btb
  bpuStage1.io.req.bits.btb.ebtb    := bpuStage0to1.io.resp.bits.btb.ebtb

  val s1_fetchMask = fetchMask(s1_fetchPC)
  val s1_redirects = (0 until fetchWidth).map { i =>
    s1_fetchMask(i) && s1_bpuResp.pred(i).target.valid &&
    (s1_bpuResp.pred(i).isJmp ||
      (s1_bpuResp.pred(i).isBr && s1_bpuResp.pred(i).taken))
  }
  stage1Redirect.valid := bpuStage1.io.resp.valid
  stage1Redirect.bits := Mux(
    s1_redirects.reduce(_ || _),
    s1_bpuResp.pred(PriorityEncoder(s1_redirects)).target.bits,
    nextFetch(s1_fetchPC),
  )

  icacheStage1.io.resp.ready := icacheStage1to2.io.req.ready && pipeStage1to2.io.in.ready
  bpuStage1.io.resp.ready    := icacheStage1to2.io.req.ready && pipeStage1to2.io.in.ready

  pipeStage1to2.io.in.valid          := s1_fire && s1_cacop === CACOP_NONE.asUInt
  pipeStage1to2.io.in.bits.fetchPC   := s1_fetchPC
  pipeStage1to2.io.in.bits.exception := s1_exception
  pipeStage1to2.io.in.bits.icache    := s1_icacheResp
  pipeStage1to2.io.in.bits.bpu       := s1_bpuResp
  pipeStage1to2.io.in.bits.redirect  := s1_redirects.reduce(_ || _)
  pipeStage1to2.io.in.bits.npc       := stage1Redirect.bits

  icacheStage1to2.io.req.valid := s1_fire && !s1_exception.valid && s1_cacop === CACOP_NONE.asUInt
  icacheStage1to2.io.req.bits  := s1_icacheResp

  // stage2: pre-decode
  def isBr(inst: UInt): Bool = {
    import Instruction._
    Seq(BEQ, BNE, BLT, BGE, BLTU, BGEU).map(_.inst === inst).reduce(_ || _)
  }
  def isB(inst: UInt): Bool = {
    import Instruction._
    Seq(B, BL, IDLE).map(_.inst === inst).reduce(_ || _)
  }
  def isJIRL(inst: UInt): Bool = {
    inst === Instruction.JIRL.inst
  }
  def isIDLE(inst: UInt): Bool = {
    inst === Instruction.IDLE.inst
  }
  def isJmp(inst: UInt): Bool = {
    import Instruction._
    isB(inst) || isJIRL(inst)
  }
  def isCall(inst: UInt): Bool = {
    import Instruction._
    (inst === BL.inst) || (inst === JIRL.inst && inst(4, 0) === 1.U && inst(25, 10) === 0.U)
  }
  def isRet(inst: UInt): Bool = {
    inst === Instruction.JIRL.inst && inst(4, 0) === 0.U && inst(9, 5) === 1.U && inst(25, 10) === 0.U
  }

  val fb         = Module(new FetchBuffer)
  val ftq        = Module(new FetchTargetQueue)
  val ras        = Module(new RAS)
  val rasIdx     = RegInit(0.U(log2Ceil(rasNum).W))
  val nextRasIdx = Wire(UInt(log2Ceil(rasNum).W))

  val s2_fetchPC        = WireInit(pipeStage1to2.io.out.bits.fetchPC)
  val s2_exception      = WireInit(pipeStage1to2.io.out.bits.exception)
  val s2_icacheResp     = WireInit(icacheStage1to2.io.resp.bits)
  val s2_bpuResp        = WireInit(pipeStage1to2.io.out.bits.bpu)
  val s2_stage1Redirect = WireInit(pipeStage1to2.io.out.bits.redirect)
  val s2_stage1Npc      = WireInit(pipeStage1to2.io.out.bits.npc)
  val s2_fetchMask      = fetchMask(s2_fetchPC)
  val s2_fetchBundle    = Wire(new FetchBundle)
  val s2_pcs            = VecInit((0 to fetchWidth).map(i => fetchAlign(s2_fetchPC) + (i * instBytes).U))
  val s2_brMask         = VecInit(s2_fetchBundle.insts.map(isBr(_))).asUInt
  val s2_bMask          = VecInit(s2_fetchBundle.insts.map(isB(_))).asUInt
  val s2_jirlMask       = VecInit(s2_fetchBundle.insts.map(isJIRL(_))).asUInt
  val s2_idleMask       = VecInit(s2_fetchBundle.insts.map(isIDLE(_))).asUInt
  val s2_jmpMask        = VecInit(s2_fetchBundle.insts.map(isJmp(_))).asUInt
  val s2_callMask       = VecInit(s2_fetchBundle.insts.map(isCall(_))).asUInt
  val s2_retMask        = VecInit(s2_fetchBundle.insts.map(isRet(_))).asUInt
  val s2_cfiMask = PriorityEncoderOH(VecInit((0 until fetchWidth).map { i =>
    s2_fetchMask(i) && (s2_jmpMask(i) || (s2_brMask(i) && s2_bpuResp.pred(i).taken))
  }).asUInt)
  val s2_cfiIdx = OHToUInt(s2_cfiMask)
  val s2_fire = pipeStage1to2.io.out.valid && (icacheStage1to2.io.resp.valid || s2_exception.valid) &&
    fb.io.enq.ready && ftq.io.enq.ready

  pipeStage1to2.io.out.ready    := s2_fire
  icacheStage1to2.io.resp.ready := s2_fire

  fb.io.flush := flush.stage2

  ras.io.read.idx    := WrapDec(rasIdx, rasNum)
  ras.io.write.valid := s2_fire && !s2_exception.valid && s2_fetchBundle.cfiIdx.valid && s2_callMask(s2_cfiIdx)
  ras.io.write.idx   := rasIdx
  ras.io.write.addr  := s2_pcs(s2_cfiIdx +& 1.U)
  nextRasIdx := MuxCase(
    rasIdx,
    Seq(
      ftq.io.rasUpdate.valid                                                                    -> ftq.io.rasUpdate.bits,
      (s2_fire && !s2_exception.valid && s2_fetchBundle.cfiIdx.valid && s2_retMask(s2_cfiIdx))  -> WrapDec(rasIdx, rasNum),
      (s2_fire && !s2_exception.valid && s2_fetchBundle.cfiIdx.valid && s2_callMask(s2_cfiIdx)) -> WrapInc(rasIdx, rasNum),
    ),
  )
  rasIdx := nextRasIdx

  s2_fetchBundle.pc  := s2_fetchPC
  s2_fetchBundle.npc := Mux(stage2Redirect.valid, stage2Redirect.bits, s2_stage1Npc)
  s2_fetchBundle.pcs := VecInit(s2_pcs.init)
  s2_fetchBundle.insts := VecInit((0 until fetchWidth).map { i =>
    s2_icacheResp((i + 1) * instWidth - 1, i * instWidth)
  })
  s2_fetchBundle.mask := Mux(
    s2_exception.valid,
    UIntToOH(s2_fetchPC(log2Ceil(fetchBytes) - 1, log2Ceil(instBytes))),
    s2_fetchMask & ~(MaskUpper(s2_cfiMask) << 1.U),
  )
  s2_fetchBundle.brMask       := s2_brMask
  s2_fetchBundle.bMask        := s2_bMask
  s2_fetchBundle.jirlMask     := s2_jirlMask
  s2_fetchBundle.cfiIdx.valid := s2_cfiMask.orR
  s2_fetchBundle.cfiIdx.bits  := s2_cfiIdx
  s2_fetchBundle.rasIdx       := nextRasIdx
  s2_fetchBundle.ftqIdx       := ftq.io.enqIdx
  s2_fetchBundle.exception    := s2_exception
  s2_fetchBundle.bpuMeta      := s2_bpuResp.meta

  stage2Redirect.valid := pipeStage1to2.io.out.valid && icacheStage1to2.io.resp.valid &&
    (stage2Redirect.bits =/= s2_stage1Npc)
  stage2Redirect.bits := MuxCase(
    s2_pcs(fetchWidth),
    Seq(
      !s2_cfiMask.orR        -> s2_pcs(fetchWidth),
      s2_retMask(s2_cfiIdx)  -> ras.io.read.addr,
      s2_idleMask(s2_cfiIdx) -> s2_pcs(s2_cfiIdx),
      s2_bMask(s2_cfiIdx) -> (s2_pcs(s2_cfiIdx) + Sext(
        Cat(s2_fetchBundle.insts(s2_cfiIdx)(9, 0), s2_fetchBundle.insts(s2_cfiIdx)(25, 10), 0.U(2.W)),
        32,
      )),
      s2_brMask(s2_cfiIdx)   -> (s2_pcs(s2_cfiIdx) + Sext(Cat(s2_fetchBundle.insts(s2_cfiIdx)(25, 10), 0.U(2.W)), 32)),
      s2_jirlMask(s2_cfiIdx) -> s2_bpuResp.pred(s2_cfiIdx).target.bits,
    ),
  )

  fb.io.enq.valid  := s2_fire
  fb.io.enq.bits   := s2_fetchBundle
  ftq.io.enq.valid := s2_fire
  ftq.io.enq.bits  := s2_fetchBundle

  // to bankend
  icache.io.clear            := io.icacheClear
  bpu.io.update              := ftq.io.bpuUpdate
  io.fetchPacket             <> fb.io.deq
  ftq.io.deq                 := io.commit
  ftq.io.redirect.valid      := io.redirect.valid
  ftq.io.redirect.idx        := io.redirect.ftqIdx
  ftq.io.redirect.brRecovery := io.redirect.brRecovery
  ftq.io.reqs                := io.ftqReqs
  io.ftqResps                := ftq.io.resps

  if (params.debug) {
    dontTouch(stage1Redirect)
    dontTouch(stage2Redirect)
    dontTouch(s2_fetchBundle)
  }
}
