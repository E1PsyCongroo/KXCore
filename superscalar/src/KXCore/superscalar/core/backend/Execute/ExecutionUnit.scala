package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.Privilege._
import KXCore.common.utils._
import KXCore.common.peripheral._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._
import EXUType._
import CACOPType._
import firtoolresolver.shaded.coursier.cache.Cache
import KXCore.common.Instruction.SLL_W

abstract class ExecutionUnit(implicit params: CoreParameters) extends Module {
  def fu_types: UInt = 0.U(FUType.getWidth.W)
  def nReaders: Int  = 1
  require(nReaders > 0 && nReaders <= 2)

  val io_kill       = IO(Input(Bool()))
  val io_fu_types   = IO(Output(UInt(FUType.getWidth.W)))
  val io_iss_uop    = IO(Flipped(Decoupled(new MicroOp)))
  val io_read_reqs  = IO(Vec(nReaders, Decoupled(UInt(params.backendParams.pregWidth.W))))
  val io_read_resps = IO(Vec(nReaders, Input(UInt(params.commonParams.dataWidth.W))))

  val iss_fire = Wire(Bool())

  io_iss_uop.ready := iss_fire

  io_read_reqs(0).valid := io_iss_uop.valid && io_iss_uop.bits.lrs1 =/= 0.U
  io_read_reqs(0).bits  := io_iss_uop.bits.prs1
  if (nReaders == 2) {
    io_read_reqs(1).valid := io_iss_uop.valid && io_iss_uop.bits.lrs2 =/= 0.U
    io_read_reqs(1).bits  := io_iss_uop.bits.prs2
  }

  val s0_regs = Wire(Valid(Vec(nReaders, UInt(params.commonParams.dataWidth.W))))
  s0_regs.valid := io_iss_uop.valid &&
    io_read_reqs.map(req => (!req.valid || req.ready)).reduce(_ && _)
  s0_regs.bits(0) := Mux(io_iss_uop.bits.lrs1 =/= 0.U, io_read_resps(0), 0.U)
  if (nReaders == 2) {
    s0_regs.bits(1) := Mux(io_iss_uop.bits.lrs2 =/= 0.U, io_read_resps(1), 0.U)
  }

  val s0_uop = Wire(Valid(new MicroOp))
  s0_uop.valid := io_iss_uop.valid
  s0_uop.bits  := io_iss_uop.bits

  s0_uop.bits.debug      := 0.U.asTypeOf(s0_uop.bits.debug.cloneType)
  s0_uop.bits.debug.pc   := io_iss_uop.bits.debug.pc
  s0_uop.bits.debug.inst := io_iss_uop.bits.debug.inst

  val regs0to1 = Module(new PipeStageReg(Vec(nReaders, UInt(params.commonParams.dataWidth.W)), true))
  regs0to1.io.flush.get := io_kill
  regs0to1.io.in.valid  := iss_fire
  regs0to1.io.in.bits   := s0_regs.bits

  val uop0to1 = Module(new PipeStageReg(new MicroOp, true))
  uop0to1.io.flush.get := io_kill
  uop0to1.io.in.valid  := iss_fire
  uop0to1.io.in.bits   := s0_uop.bits

  iss_fire := s0_uop.valid && s0_regs.valid && regs0to1.io.in.ready && uop0to1.io.in.ready

  val s1_regs = regs0to1.io.out
  val s1_uop  = uop0to1.io.out

  io_fu_types := fu_types

  if (params.debug) {
    dontTouch(s0_uop)
    dontTouch(s0_regs)
    dontTouch(s1_uop)
    dontTouch(s1_regs)
  }
}

class CacheCACOPIO(implicit params: CoreParameters) extends Bundle {
  val valid     = Output(Bool())
  val cacop     = Output(UInt(CACOPType.getWidth.W))
  val vaddr     = Output(UInt(params.commonParams.vaddrWidth.W))
  val ready     = Input(Bool())
  val exception = Input(Valid(UInt(ECODE.getWidth.W)))
}

class MemExeUnit(implicit params: CoreParameters) extends ExecutionUnit {
  import params.{commonParams, axiParams}
  import commonParams.{dataWidth, vaddrWidth, paddrWidth}
  override def fu_types: UInt = FUType.FUT_MEM.asUInt
  override def nReaders       = 2

  val io_axi       = IO(new AXIBundle(params.axiParams))
  val io_dtlb_req  = IO(Output(new TLBReq))
  val io_dtlb_resp = IO(Input(new TLBResp))
  val io_csr_llbit = IO(new Bundle {
    val bit = Input(Bool())
    val set = Output(Bool())
    val clr = Output(Bool())
  })
  val io_dcache_cacop = IO(Flipped(new CacheCACOPIO))
  val io_mem_resp     = IO(Output(Valid(new ExeUnitResp)))
  val io_mem_xcep     = IO(Output(Valid(new MicroOp)))

  val dcacheArb = Module(
    new Arbiter(
      new Bundle {
        val cacop = UInt(CACOPType.getWidth.W)
        val vaddr = UInt(vaddrWidth.W)
      },
      2,
    ),
  )
  val lsu = Module(new LoadStoreUnit)
  lsu.io.axi   <> io_axi
  lsu.io.llbit <> io_csr_llbit

  val s1_cacop   = WireInit(dcacheArb.io.out.bits.cacop)
  val s1_cached  = io_dtlb_resp.mat(0)
  val s1_vaddr   = WireInit(dcacheArb.io.out.bits.vaddr)
  val s1_paddr   = io_dtlb_resp.paddr
  val s1_isWrite = EXUType.isSotre(s1_uop.bits.exuCmd) && s1_cacop === CACOP_HIT_READ.asUInt
  val s1_wmask = MuxLookup(s1_uop.bits.exuCmd, 0.U)(
    Seq(
      EXU_STB.asUInt -> ("b0001".U << s1_vaddr(1, 0)),
      EXU_STH.asUInt -> ("b0011".U << s1_vaddr(1, 0)),
      EXU_STW.asUInt -> "b1111".U,
      EXU_SCW.asUInt -> "b1111".U,
    ),
  )
  val s1_writeData = s1_regs.bits(1) << (s1_vaddr(1, 0) ## 0.U(3.W))
  val s1_isAle = (s1_vaddr &
    MuxLookup(s1_uop.bits.exuCmd, 0.U)(
      Seq(
        EXU_STB.asUInt  -> 0.U,
        EXU_STH.asUInt  -> 1.U,
        EXU_STW.asUInt  -> 3.U,
        EXU_SCW.asUInt  -> 3.U,
        EXU_LDB.asUInt  -> 0.U,
        EXU_LDBU.asUInt -> 0.U,
        EXU_LDH.asUInt  -> 1.U,
        EXU_LDHU.asUInt -> 1.U,
        EXU_LDW.asUInt  -> 3.U,
        EXU_LLW.asUInt  -> 3.U,
      ),
    )) =/= 0.U
  val s1_exception = Wire(Valid(UInt(ECODE.getWidth.W)))

  dcacheArb.io.in(0).valid        := io_dcache_cacop.valid
  dcacheArb.io.in(0).bits.cacop   := io_dcache_cacop.cacop
  dcacheArb.io.in(0).bits.vaddr   := io_dcache_cacop.vaddr
  io_dcache_cacop.ready           := dcacheArb.io.in(0).ready
  io_dcache_cacop.exception.valid := io_dcache_cacop.cacop === CACOP_HIT_INV.asUInt && io_dtlb_resp.exception.valid
  io_dcache_cacop.exception.bits  := io_dtlb_resp.exception.bits

  dcacheArb.io.in(1).valid      := s1_uop.valid && s1_regs.valid
  dcacheArb.io.in(1).bits.cacop := CACOP_HIT_READ.asUInt
  dcacheArb.io.in(1).bits.vaddr := s1_regs.bits(0) + s1_uop.bits.imm

  dcacheArb.io.out.ready := true.B

  io_dtlb_req.isWrite := s1_isWrite
  io_dtlb_req.vaddr   := s1_vaddr

  s1_exception.valid := (s1_cacop === CACOP_HIT_READ.asUInt) && (s1_isAle || io_dtlb_resp.exception.valid)
  s1_exception.bits  := Mux(s1_isAle, ECODE.ALE.asUInt, io_dtlb_resp.exception.bits)

  val s2_reg = Module(
    new PipeStageReg(
      new Bundle {
        val uop       = new MicroOp
        val cacop     = UInt(CACOPType.getWidth.W)
        val cached    = Bool()
        val vaddr     = UInt(vaddrWidth.W)
        val paddr     = UInt(paddrWidth.W)
        val isWrite   = Bool()
        val wmask     = UInt((dataWidth / 8).W)
        val writeData = UInt(dataWidth.W)
      },
      true,
    ),
  )
  s2_reg.io.flush.get             := io_kill
  s2_reg.io.in.valid              := s1_uop.valid && s1_regs.valid
  s1_uop.ready                    := s2_reg.io.in.fire
  s1_regs.ready                   := s2_reg.io.in.fire
  s2_reg.io.in.bits.uop           := s1_uop.bits
  s2_reg.io.in.bits.cacop         := s1_cacop
  s2_reg.io.in.bits.cached        := s1_cached
  s2_reg.io.in.bits.vaddr         := s1_vaddr
  s2_reg.io.in.bits.paddr         := s1_paddr
  s2_reg.io.in.bits.isWrite       := s1_isWrite
  s2_reg.io.in.bits.wmask         := s1_wmask
  s2_reg.io.in.bits.writeData     := s1_writeData
  s2_reg.io.in.bits.uop.exception := s1_exception.valid
  s2_reg.io.in.bits.uop.ecode     := s1_exception.bits
  s2_reg.io.in.bits.uop.badv      := s1_vaddr

  s2_reg.io.in.bits.uop.debug.load      := VecInit(Seq(EXU_LDB, EXU_LDBU, EXU_LDH, EXU_LDHU, EXU_LDW, EXU_LLW).map(_.asUInt === s1_uop.bits.exuCmd)).asUInt
  s2_reg.io.in.bits.uop.debug.loadVaddr := s1_vaddr
  s2_reg.io.in.bits.uop.debug.loadPaddr := s1_paddr

  s2_reg.io.in.bits.uop.debug.store := VecInit(
    Seq(EXU_STB, EXU_STH, EXU_STW, EXU_SCW).map(_.asUInt === s1_uop.bits.exuCmd),
  ).asUInt & (io_csr_llbit.bit ## "b111".U(3.W))
  s2_reg.io.in.bits.uop.debug.storeVaddr := s1_vaddr
  s2_reg.io.in.bits.uop.debug.storePaddr := s1_paddr
  s2_reg.io.in.bits.uop.debug.storeData := s1_writeData &
    (VecInit((0 until 4).map(i => Fill(8, s1_wmask(i)) << (i * 8))).reduce(_ | _))

  val s2_data = s2_reg.io.out

  lsu.io.req.valid := s2_data.valid && !s2_data.bits.uop.exception
  lsu.io.req.bits  := s2_data.bits

  io_mem_resp.valid := lsu.io.resp.valid
  io_mem_resp.bits  := lsu.io.resp.bits
  lsu.io.resp.ready := true.B

  io_mem_resp.bits.uop.debug.wen      := io_mem_resp.bits.uop.ldst =/= 0.U
  io_mem_resp.bits.uop.debug.wdest    := io_mem_resp.bits.uop.ldst
  io_mem_resp.bits.uop.debug.wdata    := io_mem_resp.bits.data
  io_mem_resp.bits.uop.debug.loadData := io_mem_resp.bits.data

  io_mem_xcep.valid := s2_data.valid && s2_data.bits.uop.exception
  io_mem_xcep.bits  := s2_data.bits.uop

  s2_data.ready := io_mem_resp.valid || io_mem_xcep.valid

  if (params.debug) {
    dontTouch(s2_data)
  }
}

/*
class MemExeUnitWithCache(implicit params: CoreParameters) extends ExecutionUnit {
  import params._
  import commonParams.{dataWidth, vaddrWidth, paddrWidth}
  import backendParams._
  import LSUType._
  override def fu_types: UInt = FUType.FUT_MEM.asUInt
  override def nReaders       = 2

  val io_dtlb_req  = IO(Output(new TLBReq()(commonParams)))
  val io_dtlb_resp = IO(Input(new TLBResp()(commonParams)))
  val io_axi       = IO(new AXIBundle(params.axiParams))
  val io_dcache_flush = IO(Input(new Bundle {
    val stage1 = Bool()
    val stage2 = Bool()
  }))

  val dcache = Module(new DCache()(commonParams, dcacheParams, axiParams))

  io_axi <> dcache.io.axi
  dcache.io.flush := io_dcache_flush

  val isWrite = Seq(LSU_STB, LSU_STH, LSU_STW).map(_.asUInt === stage1Uop.bits.lsuCmd).reduce(_ || _)
  io_dtlb_req.asid    := 0.U
  io_dtlb_req.isWrite := isWrite
  io_dtlb_req.plv     := 0.U
  io_dtlb_req.vaddr   := Mux(isWrite, stage1Regs.bits(1), stage1Regs.bits(0)) + stage1Uop.bits.imm

  val stage1Data = Wire(DecoupledIO(new Bundle {
    val uop       = new MicroOp
    val isWrite   = Bool()
    val writeData = UInt(dataWidth.W)
    val paddr     = UInt(paddrWidth.W)
    val vaddr     = UInt(vaddrWidth.W)
    val wmask     = UInt(4.W)
  }))
  stage1Data.valid          := stage1Uop.valid && stage1Regs.valid
  stage1Uop.ready           := stage1Data.ready
  stage1Regs.ready          := stage1Data.ready
  stage1Data.bits.uop       := stage1Uop.bits
  stage1Data.bits.isWrite   := isWrite
  stage1Data.bits.writeData := stage1Regs.bits(0)
  stage1Data.bits.paddr     := io_dtlb_resp.paddr
  stage1Data.bits.vaddr     := io_dtlb_req.vaddr
  stage1Data.bits.wmask := MuxLookup(stage1Uop.bits.lsuCmd, 0.U)(
    Seq(
      LSU_STB.asUInt -> ("b0001".U << stage1Data.bits.paddr(1, 0)),
      LSU_STH.asUInt -> ("b0011".U << stage1Data.bits.paddr(1)),
      LSU_STW.asUInt -> "b1111".U,
    ),
  )

  val sIdle :: sWaitDCacheStage0 :: sWaitDCacheStage1 :: sWaitDCacheStage2 :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val nextState = WireDefault(sIdle)

  nextState := MuxLookup(state, sIdle)(
    Seq(
      sIdle -> Mux(
        stage1Data.valid && !io_kill,
        Mux(dcache.io.req.stage0.ready, sWaitDCacheStage0, sIdle),
        sIdle
      ),
      sWaitDCacheStage0 -> Mux(
        io_kill,
        sIdle,
        Mux(dcache.io.req.stage1.ready, sWaitDCacheStage1, sWaitDCacheStage0)
      ),
      sWaitDCacheStage1 -> Mux(
        io_kill,
        sIdle,
        Mux(dcache.io.resp.stage2.valid, sWaitDCacheStage2, sWaitDCacheStage1)
      ),
      sWaitDCacheStage2 -> Mux(
        dcache.io.resp.stage2.fire,
        sIdle,
        sWaitDCacheStage2
      )
    )
  )
  state := nextState

  stage1Data.ready := (state === sIdle) && dcache.io.req.stage0.ready

  dcache.io.req.stage0.valid := (state === sIdle) && stage1Data.valid && !io_kill
  dcache.io.req.stage0.bits.vaddr := stage1Data.bits.vaddr

  dcache.io.req.stage1.valid := (state === sWaitDCacheStage0) && !io_kill
  dcache.io.req.stage1.bits.vaddr := stage1Data.bits.vaddr
  dcache.io.req.stage1.bits.paddr := stage1Data.bits.paddr
  dcache.io.req.stage1.bits.cached := true.B
  dcache.io.req.stage1.bits.cacop := 0.U
  dcache.io.req.stage1.bits.isWrite := stage1Data.bits.isWrite
  dcache.io.req.stage1.bits.writeData := stage1Data.bits.writeData
  dcache.io.req.stage1.bits.writeMask := stage1Data.bits.wmask

  val stage2Data = RegEnable(stage1Data.bits, state === sWaitDCacheStage0 && dcache.io.req.stage1.fire)
  val io_mem_resp = IO(Output(Valid(new ExeUnitResp)))

  dcache.io.resp.stage2.ready := true.B

  val dcache_rdata = dcache.io.resp.stage2.bits.data
  val loffset = WireDefault(stage2Data.paddr(1, 0) << 3.U)
  val lshift = dcache_rdata >> loffset
  val rdata = MuxCase(
    lshift,
    Seq(
      LSU_LDB  -> Fill(24, lshift(7)) ## lshift(7, 0),
      LSU_LDH  -> Fill(16, lshift(15)) ## lshift(15, 0),
      LSU_LDHU -> Fill(16, 0.U(1.W)) ## lshift(15, 0),
      LSU_LDBU -> Fill(24, 0.U(1.W)) ## lshift(7, 0),
    ).map { case (key, data) => (stage2Data.uop.lsuCmd === key.asUInt, data) },
  )

  io_mem_resp.valid := !io_kill && (state === sWaitDCacheStage2) && dcache.io.resp.stage2.valid
  io_mem_resp.bits.brInfo.valid         := false.B
  io_mem_resp.bits.brInfo.bits          := DontCare
  io_mem_resp.bits.uop                  := stage2Data.uop
  io_mem_resp.bits.uop.debug.load       := VecInit(Seq(LSU_LDB, LSU_LDBU, LSU_LDH, LSU_LDHU, LSU_LDW).map(_.asUInt === stage2Data.uop.lsuCmd)).asUInt
  io_mem_resp.bits.uop.debug.loadVaddr  := stage2Data.vaddr
  io_mem_resp.bits.uop.debug.loadPaddr  := stage2Data.paddr
  io_mem_resp.bits.uop.debug.loadData   := rdata
  io_mem_resp.bits.uop.debug.store      := VecInit(Seq(LSU_STB, LSU_STH, LSU_STW).map(_.asUInt === stage2Data.uop.lsuCmd)).asUInt
  io_mem_resp.bits.uop.debug.storeVaddr := stage2Data.vaddr
  io_mem_resp.bits.uop.debug.storePaddr := stage2Data.paddr
  io_mem_resp.bits.uop.debug.storeData  := stage2Data.writeData
  io_mem_resp.bits.data                 := rdata

  dontTouch(state)
  dontTouch(nextState)
  dontTouch(stage1Uop)
  dontTouch(stage1Regs)
  dontTouch(stage1Data)
  dontTouch(stage2Data)
  dontTouch(dcache_rdata)
}
 */

class UniqueExeUnit(
    val hasCSR: Boolean = true,
    val hasMul: Boolean = true,
    val hasDiv: Boolean = true,
)(implicit params: CoreParameters)
    extends ExecutionUnit {

  import params.{commonParams}
  import commonParams.{dataWidth}

  override def fu_types: UInt =
    (if (hasCSR) FUType.FUT_CSR.asUInt else 0.U) |
      (if (hasMul) FUType.FUT_MUL.asUInt else 0.U) |
      (if (hasDiv) FUType.FUT_DIV.asUInt else 0.U)
  override def nReaders = 2

  s1_regs.ready := false.B
  s1_uop.ready  := false.B

  val mul_div_arbiter = Module(new Arbiter(new ExeUnitResp, 2))
  mul_div_arbiter.io.in(0).valid := false.B
  mul_div_arbiter.io.in(0).bits  := DontCare
  mul_div_arbiter.io.in(1).valid := false.B
  mul_div_arbiter.io.in(1).bits  := DontCare
  mul_div_arbiter.io.out.ready   := true.B

  val io_mul_div_resp = if (hasMul || hasDiv) Some(IO(Valid(new ExeUnitResp))) else None
  io_mul_div_resp.get.valid := mul_div_arbiter.io.out.valid
  io_mul_div_resp.get.bits  := mul_div_arbiter.io.out.bits

  if (hasMul) {
    val mul_resp = Wire(Decoupled(new ExeUnitResp))
    val mulUnit  = Module(new MultiplyUnit)
    mulUnit.io.kill := io_kill

    mulUnit.io.req.bits.rs1_data := s1_regs.bits(0)
    mulUnit.io.req.bits.rs2_data := s1_regs.bits(1)
    mulUnit.io.req.bits.uop      := s1_uop.bits
    mulUnit.io.req.bits.ftq_info := DontCare
    mulUnit.io.req.valid         := false.B
    when(s1_uop.valid && s1_regs.valid && s1_uop.bits.fuType === FUType.FUT_MUL.asUInt) {
      mulUnit.io.req.valid := true.B
      s1_uop.ready         := mulUnit.io.req.ready
      s1_regs.ready        := mulUnit.io.req.ready
    }
    mul_resp.valid        := mulUnit.io.resp.valid
    mulUnit.io.resp.ready := mul_resp.ready
    mul_resp.bits         := mulUnit.io.resp.bits

    mul_resp.bits.uop.debug.wen   := mul_resp.bits.uop.ldst =/= 0.U
    mul_resp.bits.uop.debug.wdest := mul_resp.bits.uop.ldst
    mul_resp.bits.uop.debug.wdata := mul_resp.bits.data

    mul_div_arbiter.io.in(0).valid := mul_resp.valid
    mul_div_arbiter.io.in(0).bits  := mul_resp.bits
    mul_resp.ready                 := mul_div_arbiter.io.in(0).ready
  }

  if (hasDiv) {
    val div_resp = Wire((DecoupledIO(new ExeUnitResp)))
    val divUnit  = Module(new DivUnit)
    divUnit.io.kill := io_kill

    divUnit.io.req.bits.rs1_data := s1_regs.bits(0)
    divUnit.io.req.bits.rs2_data := s1_regs.bits(1)
    divUnit.io.req.bits.uop      := s1_uop.bits
    divUnit.io.req.bits.ftq_info := DontCare
    divUnit.io.req.valid         := false.B
    when(s1_uop.valid && s1_regs.valid && s1_uop.bits.fuType === FUType.FUT_DIV.asUInt) {
      divUnit.io.req.valid := true.B
      s1_regs.ready        := divUnit.io.req.ready
      s1_uop.ready         := divUnit.io.req.ready
    }
    div_resp.valid        := divUnit.io.resp.valid
    divUnit.io.resp.ready := div_resp.ready
    div_resp.bits         := divUnit.io.resp.bits

    div_resp.bits.uop.debug.wen   := div_resp.bits.uop.ldst =/= 0.U
    div_resp.bits.uop.debug.wdest := div_resp.bits.uop.ldst
    div_resp.bits.uop.debug.wdata := div_resp.bits.data

    mul_div_arbiter.io.in(1).valid := div_resp.valid
    mul_div_arbiter.io.in(1).bits  := div_resp.bits
    div_resp.ready                 := mul_div_arbiter.io.in(1).ready
  }

  val (io_csr_access, io_icache_cacop, io_dcache_cacop, io_tlb_cmd, io_csr_resp, io_csr_xcep) = if (hasCSR) {
    val cpuinfo = Module(new CPUInfo)

    val io_csr_access = IO(new Bundle {
      val raddr = Output(UInt(14.W))       // CSR address to read
      val rdata = Input(UInt(dataWidth.W)) // CSR read data

      val we    = Output(Bool())            // Write enable
      val waddr = Output(UInt(14.W))        // CSR address to write
      val wdata = Output(UInt(dataWidth.W)) // CSR write data
      val wmask = Output(UInt(dataWidth.W)) // CSR write mask

      val crmd      = Input(new CSR.CRMD)
      val counterID = Input(UInt(dataWidth.W))
      val cntvh     = Input(UInt(dataWidth.W))
      val cntvl     = Input(UInt(dataWidth.W))
    })
    val io_tlb_cmd      = IO(Output(new TLBCmdIO))
    val io_icache_cacop = IO(new CacheCACOPIO)
    val io_dcache_cacop = IO(new CacheCACOPIO)
    val io_csr_resp     = IO(Output(Valid(new ExeUnitResp)))
    val io_csr_xcep     = IO(Output(Valid(new MicroOp)))

    cpuinfo.io.idx := s1_regs.bits(0)

    io_csr_access.raddr := s1_uop.bits.imm
    io_csr_access.waddr := s1_uop.bits.imm
    io_csr_access.wmask := Mux(
      s1_uop.bits.exuCmd === EXU_CSRXCHG.asUInt,
      s1_regs.bits(0),
      Fill(dataWidth, 1.B),
    )
    io_csr_access.wdata := s1_regs.bits(1)
    io_csr_access.we    := false.B

    io_tlb_cmd.cmd        := EXU_TLBNONE.asUInt
    io_tlb_cmd.inv_op     := s1_uop.bits.imm(4, 0)
    io_tlb_cmd.inv_asid   := s1_regs.bits(0)(9, 0)
    io_tlb_cmd.inv_vaddr  := s1_regs.bits(1)
    io_icache_cacop       := DontCare
    io_dcache_cacop       := DontCare
    io_icache_cacop.valid := false.B
    io_dcache_cacop.valid := false.B
    io_csr_resp.valid     := false.B
    io_csr_resp.bits      := DontCare
    io_csr_xcep           := DontCare
    io_csr_xcep.valid     := false.B
    when(s1_uop.valid && s1_regs.valid && s1_uop.bits.fuType === FUType.FUT_CSR.asUInt) {
      s1_regs.ready := Mux(
        s1_uop.bits.exuCmd === EXU_CACOP.asUInt,
        Mux(s1_uop.bits.cacopCode(2, 0) === 0.U, io_icache_cacop.ready, io_dcache_cacop.ready),
        true.B,
      )
      s1_uop.ready := Mux(
        s1_uop.bits.exuCmd === EXU_CACOP.asUInt,
        Mux(s1_uop.bits.cacopCode(2, 0) === 0.U, io_icache_cacop.ready, io_dcache_cacop.ready),
        true.B,
      )

      val isIPE = EXUType.unqPriv(s1_uop.bits.exuCmd) && (io_csr_access.crmd.plv =/= 0.U) &&
        !(s1_uop.bits.exuCmd === EXU_CACOP.asUInt && s1_uop.bits.cacopCode(4, 3) === CACOP_HIT_INV.asUInt)

      io_csr_access.we := EXUType.csrWen(s1_uop.bits.exuCmd)

      io_tlb_cmd.cmd := Mux(isIPE, EXU_TLBNONE.asUInt, s1_uop.bits.exuCmd)

      io_icache_cacop.valid := s1_uop.bits.exuCmd === EXU_CACOP.asUInt && s1_uop.bits.cacopCode(2, 0) === 0.U && !isIPE
      io_icache_cacop.cacop := s1_uop.bits.cacopCode(4, 3)
      io_icache_cacop.vaddr := s1_regs.bits(0) + s1_uop.bits.imm
      io_dcache_cacop.valid := s1_uop.bits.exuCmd === EXU_CACOP.asUInt && s1_uop.bits.cacopCode(2, 0) === 1.U && !isIPE
      io_dcache_cacop.cacop := s1_uop.bits.cacopCode(4, 3)
      io_dcache_cacop.vaddr := s1_regs.bits(0) + s1_uop.bits.imm

      io_csr_resp.valid := !io_csr_xcep.valid && (!io_icache_cacop.valid || io_icache_cacop.ready) &&
        (!io_dcache_cacop.valid || io_dcache_cacop.ready)
      io_csr_resp.bits.uop := s1_uop.bits
      io_csr_resp.bits.data := MuxLookup(s1_uop.bits.exuCmd, io_csr_access.rdata)(
        Seq(
          EXU_CPUCFG.asUInt  -> cpuinfo.io.info,
          EXU_RDCNTID.asUInt -> io_csr_access.counterID,
          EXU_RDCNTVL.asUInt -> io_csr_access.cntvl,
          EXU_RDCNTVH.asUInt -> io_csr_access.cntvh,
        ),
      )

      io_csr_xcep.valid := MuxCase(
        false.B,
        Seq(
          isIPE                 -> true.B,
          io_icache_cacop.valid -> (io_icache_cacop.ready && io_icache_cacop.exception.valid),
          io_dcache_cacop.valid -> (io_dcache_cacop.ready && io_dcache_cacop.exception.valid),
        ),
      )
      io_csr_xcep.bits           := s1_uop.bits
      io_csr_xcep.bits.exception := io_csr_xcep.valid
      io_csr_xcep.bits.ecode := MuxCase(
        DontCare,
        Seq(
          isIPE                 -> ECODE.IPE.asUInt,
          io_icache_cacop.valid -> io_icache_cacop.exception.bits,
          io_dcache_cacop.valid -> io_dcache_cacop.exception.bits,
        ),
      )
      io_csr_xcep.bits.badv := s1_regs.bits(0) + s1_uop.bits.imm
    }

    io_csr_resp.bits.uop.debug.is_CNTinst     := Seq(EXU_RDCNTID, EXU_RDCNTVL, EXU_RDCNTVH).map(_.asUInt === io_csr_resp.bits.uop.exuCmd).reduce(_ || _)
    io_csr_resp.bits.uop.debug.timer_64_value := io_csr_access.cntvh ## io_csr_access.cntvl
    io_csr_resp.bits.uop.debug.wen            := io_csr_resp.bits.uop.ldst =/= 0.U
    io_csr_resp.bits.uop.debug.wdest          := io_csr_resp.bits.uop.ldst
    io_csr_resp.bits.uop.debug.wdata          := io_csr_resp.bits.data
    io_csr_resp.bits.uop.debug.csr_rstat := Seq(EXU_CSRRD, EXU_CSRWR, EXU_CSRXCHG)
      .map(_.asUInt === io_csr_resp.bits.uop.exuCmd)
      .reduce(_ || _) && io_csr_resp.bits.uop.imm === CSR.CSRAddr.ESTAT.U
    io_csr_resp.bits.uop.debug.csr_data := io_csr_resp.bits.data

    (Some(io_csr_access), Some(io_icache_cacop), Some(io_dcache_cacop), Some(io_tlb_cmd), Some(io_csr_resp), Some(io_csr_xcep))
  } else {
    (None, None, None, None, None, None)
  }
}

class ALUExeUnit(implicit params: CoreParameters) extends ExecutionUnit {
  override def fu_types: UInt = FUType.FUT_ALU.asUInt | FUType.FUT_CFI.asUInt
  override def nReaders       = 2

  val io_ftq_req  = IO(Vec(2, Decoupled(UInt(params.frontendParams.ftqIdxWidth.W))))
  val io_ftq_resp = IO(Input(Vec(2, new FTQInfo)))

  io_ftq_req(0).valid := io_iss_uop.valid &&
    (io_iss_uop.bits.op1Sel === OP1Type.OP1_PC.asUInt ||
      io_iss_uop.bits.isB || io_iss_uop.bits.isBr || io_iss_uop.bits.isJirl)
  io_ftq_req(0).bits  := io_iss_uop.bits.ftqIdx
  io_ftq_req(1).valid := io_iss_uop.valid && io_iss_uop.bits.isJirl
  io_ftq_req(1).bits  := WrapInc(io_iss_uop.bits.ftqIdx, params.frontendParams.ftqNum)
  val s0_ftq = Wire(Valid(io_ftq_resp.cloneType))
  s0_ftq.valid := io_iss_uop.valid &&
    (!io_ftq_req(0).valid || io_ftq_req(0).ready) && (!io_ftq_req(1).valid || io_ftq_req(1).ready)
  s0_ftq.bits := io_ftq_resp

  val ftq0to1 = Module(new PipeStageReg(Vec(2, new FTQInfo), true))
  ftq0to1.io.flush.get := io_kill
  ftq0to1.io.in.valid  := iss_fire
  ftq0to1.io.in.bits   := s0_ftq.bits

  iss_fire := s0_uop.valid && s0_regs.valid && s0_ftq.valid &&
    regs0to1.io.in.ready && uop0to1.io.in.ready && ftq0to1.io.in.ready

  val s1_ftq = ftq0to1.io.out

  val alu = Module(new ALUUnit)
  alu.io.req.valid         := s1_uop.valid && s1_regs.valid && s1_ftq.valid
  s1_uop.ready             := alu.io.req.fire
  s1_regs.ready            := alu.io.req.fire
  s1_ftq.ready             := alu.io.req.fire
  alu.io.req.bits.uop      := s1_uop.bits
  alu.io.req.bits.rs1_data := s1_regs.bits(0)
  alu.io.req.bits.rs2_data := s1_regs.bits(1)
  alu.io.req.bits.ftq_info := s1_ftq.bits
  alu.io.kill              := io_kill

  val io_alu_resp = IO(Output(Valid(new ExeUnitResp)))
  io_alu_resp.valid := alu.io.resp.valid
  alu.io.resp.ready := true.B
  io_alu_resp.bits  := alu.io.resp.bits

  io_alu_resp.bits.uop.debug.wen   := alu.io.resp.bits.uop.ldst =/= 0.U
  io_alu_resp.bits.uop.debug.wdest := alu.io.resp.bits.uop.ldst
  io_alu_resp.bits.uop.debug.wdata := alu.io.resp.bits.data

  val io_alu_brInfo = IO(Output(new BrRecoveryInfo))
  io_alu_brInfo := alu.io.brInfo

  if (params.debug) {
    dontTouch(io_ftq_req)
    dontTouch(s0_ftq)
    dontTouch(s1_ftq)
  }
}
