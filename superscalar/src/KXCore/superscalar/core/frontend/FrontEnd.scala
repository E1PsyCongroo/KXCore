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

  val bpu = Module(new BranchPredictor.BranchPredictorStorage)

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
  val icacheArb    = Module(new Arbiter(new CacheCACOPIO, 2))
  val icache       = Module(new ICache.ICache)
  val bpuStage0to1 = Module(new BranchPredictor.BranchPredictorStage0to1)
  val pipeStage0to1 = Module(
    new PipeStageReg(
      new Bundle {
        val fetchPC   = UInt(vaddrWidth.W)
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
  val s0_fire      = pipeStage0to1.io.in.ready && bpuStage0to1.io.req.ready && icacheArb.io.in(1).ready
  s0_fetchPC := MuxCase(
    s0_npc,
    Seq(
      backendRedirect.valid -> backendRedirect.bits,
      stage2Redirect.valid  -> stage2Redirect.bits,
      stage1Redirect.valid  -> stage1Redirect.bits,
    ),
  )
  s0_npc             := s0_fetchPC
  s0_isAdef          := s0_fetchPC(1, 0) =/= 0.U
  s0_exception.valid := s0_isAdef || io.itlbResp.exception.valid
  s0_exception.bits  := Mux(s0_isAdef, ECODE.ADEF.asUInt, io.itlbResp.exception.bits)

  icache.io.flush            := flush.stage1
  bpuStage0to1.io.flush      := flush.stage1
  pipeStage0to1.io.flush.get := flush.stage1

  icache.io.clear := io.icacheClear
  icache.io.axi   <> io.axi

  bpuStage0to1.io.bimRead <> bpu.io.bim
  bpuStage0to1.io.btbRead <> bpu.io.btb

  icacheArb.io.in(0).valid      := io.icacheCacop.valid
  icacheArb.io.in(0).bits.cacop := io.icacheCacop.bits.cacop
  icacheArb.io.in(0).bits.vaddr := io.icacheCacop.bits.vaddr
  icacheArb.io.in(0).bits.paddr := io.icacheCacop.bits.paddr
  io.icacheCacop.ready          := icacheArb.io.in(0).ready

  icacheArb.io.in(1).valid      := s0_fire
  icacheArb.io.in(1).bits.cacop := CACOP_NONE.asUInt
  icacheArb.io.in(1).bits.vaddr := s0_fetchPC
  icacheArb.io.in(1).bits.paddr := io.itlbResp.paddr

  icacheArb.io.out.ready := icache.io.req.ready

  io.itlbReq.isWrite := false.B
  io.itlbReq.vaddr   := icacheArb.io.out.bits.vaddr

  pipeStage0to1.io.in.valid          := s0_fire
  pipeStage0to1.io.in.bits.fetchPC   := s0_fetchPC
  pipeStage0to1.io.in.bits.exception := s0_exception

  icache.io.req.valid := (icacheArb.io.out.valid && icacheArb.io.chosen === 0.U) ||
    (s0_fire && !s0_exception.valid)
  icache.io.req.bits.cacop  := s0_cacop
  icache.io.req.bits.cached := s0_cached
  icache.io.req.bits.vaddr  := s0_vaddr
  icache.io.req.bits.paddr  := s0_paddr

  bpuStage0to1.io.req.valid := s0_fire && !s0_exception.valid
  bpuStage0to1.io.req.bits  := s0_fetchPC
  bpuStage0to1.io.holdRead  := pipeStage0to1.io.out.bits.fetchPC

  // stage1: fetch & branch prediction
  val bpuStage1 = Module(new BranchPredictor.BranchPredictorStage1)
  val pipeStage1to2 = Module(
    new PipeStageReg(
      new Bundle {
        val fetchPC   = UInt(vaddrWidth.W)
        val exception = Valid(UInt(ECODE.getWidth.W))
        val insts     = Vec(fetchWidth, UInt(instWidth.W))
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

  val s1_fetchPC   = WireInit(pipeStage0to1.io.out.bits.fetchPC)
  val s1_exception = WireInit(pipeStage0to1.io.out.bits.exception)
  val s1_insts = WireInit(VecInit((0 until fetchWidth).map { i =>
    icache.io.resp.bits(instWidth * (i + 1) - 1, instWidth * i)
  }))
  val s1_bpuResp = WireInit(bpuStage1.io.resp.bits)
  val s1_fire = pipeStage0to1.io.out.valid && Mux(
    pipeStage0to1.io.out.bits.exception.valid,
    pipeStage1to2.io.in.ready,
    icache.io.resp.valid && bpuStage1.io.resp.valid && pipeStage1to2.io.in.ready,
  )

  pipeStage0to1.io.out.ready := s1_fire
  bpuStage0to1.io.resp.ready := s1_fire

  pipeStage1to2.io.flush.get := flush.stage2

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

  bpuStage1.io.resp.ready := s1_fire
  icache.io.resp.ready    := s1_fire

  pipeStage1to2.io.in.valid          := s1_fire
  pipeStage1to2.io.in.bits.fetchPC   := s1_fetchPC
  pipeStage1to2.io.in.bits.exception := s1_exception
  pipeStage1to2.io.in.bits.insts     := s1_insts
  pipeStage1to2.io.in.bits.bpu       := s1_bpuResp
  pipeStage1to2.io.in.bits.redirect  := s1_redirects.reduce(_ || _)
  pipeStage1to2.io.in.bits.npc       := stage1Redirect.bits

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
  val s2_insts          = WireInit(pipeStage1to2.io.out.bits.insts)
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
  val s2_fire   = pipeStage1to2.io.out.valid && fb.io.enq.ready && ftq.io.enq.ready

  pipeStage1to2.io.out.ready := s2_fire

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

  s2_fetchBundle.pc    := s2_fetchPC
  s2_fetchBundle.npc   := Mux(stage2Redirect.valid, stage2Redirect.bits, s2_stage1Npc)
  s2_fetchBundle.pcs   := VecInit(s2_pcs.init)
  s2_fetchBundle.insts := s2_insts
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

  stage2Redirect.valid := pipeStage1to2.io.out.valid && (stage2Redirect.bits =/= s2_stage1Npc)
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
