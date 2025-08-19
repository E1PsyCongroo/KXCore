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
  val cacop = UInt(CACOPType.getWidth.W)
  val vaddr = UInt(params.commonParams.vaddrWidth.W)
  val paddr = UInt(params.commonParams.paddrWidth.W)
}

class MemExeUnit(implicit params: CoreParameters) extends ExecutionUnit {
  import params.{commonParams, axiParams}
  import commonParams.{dataWidth, vaddrWidth, paddrWidth}
  override def fu_types: UInt = FUType.FUT_MEM.asUInt
  override def nReaders       = 2

  val io_axi       = IO(new AXIBundle(params.axiParams))
  val io_dtlb_req  = IO(Output(new TLBReq))
  val io_dtlb_resp = IO(Input(new TLBResp))
  val io_csr_access = IO(new Bundle {
    val crmd      = Input(new CSR.CRMD)
    val llbit     = Input(Bool())
    val set_llbit = Output(Bool())
    val clr_llbit = Output(Bool())
  })
  val io_mem_resp = IO(Output(Valid(new ExeUnitResp)))
  val io_mem_xcep = IO(Output(Valid(new MicroOp)))

  val s1_is_ll      = s1_uop.bits.exuCmd === EXU_LLW.asUInt
  val s1_is_sc      = s1_uop.bits.exuCmd === EXU_SCW.asUInt
  val s1_is_cacop   = s1_uop.bits.exuCmd === EXU_CACOP.asUInt
  val s1_is_icacop  = s1_is_cacop && s1_uop.bits.cacopCode(2, 0) === 0.U
  val s1_is_dcacop  = s1_is_cacop && s1_uop.bits.cacopCode(2, 0) === 1.U
  val s1_cacop_code = s1_uop.bits.cacopCode(4, 3)
  val s1_cached     = io_dtlb_resp.mat(0)
  val s1_vaddr      = s1_regs.bits(0) + s1_uop.bits.imm
  val s1_paddr      = io_dtlb_resp.paddr
  val s1_isWrite    = EXUType.isSotre(s1_uop.bits.exuCmd)
  val s1_wmask = MuxLookup(s1_uop.bits.exuCmd, 0.U)(
    Seq(
      EXU_STB.asUInt -> ("b0001".U << s1_vaddr(1, 0)),
      EXU_STH.asUInt -> ("b0011".U << s1_vaddr(1, 0)),
      EXU_STW.asUInt -> "b1111".U,
      EXU_SCW.asUInt -> "b1111".U,
    ),
  )
  val s1_writeData = s1_regs.bits(1) << (s1_vaddr(1, 0) ## 0.U(3.W))
  val s1_isIPE     = s1_is_cacop && s1_cacop_code =/= CACOP_HIT_INV.asUInt && io_csr_access.crmd.plv =/= 0.U
  val s1_isALE = !s1_is_cacop && !(s1_is_sc && !io_csr_access.llbit) && (s1_vaddr &
    MuxLookup(s1_uop.bits.exuCmd, 0.U)(
      Seq(
        EXU_LDB.asUInt   -> 0.U,
        EXU_LDBU.asUInt  -> 0.U,
        EXU_LDH.asUInt   -> 1.U,
        EXU_LDHU.asUInt  -> 1.U,
        EXU_LDW.asUInt   -> 3.U,
        EXU_LLW.asUInt   -> 3.U,
        EXU_CACOP.asUInt -> 0.U,
        EXU_STB.asUInt   -> 0.U,
        EXU_STH.asUInt   -> 1.U,
        EXU_STW.asUInt   -> 3.U,
        EXU_SCW.asUInt   -> 3.U,
      ),
    )) =/= 0.U
  val s1_tlb_xcep = io_dtlb_resp.exception.valid &&
    !(s1_is_cacop && s1_cacop_code =/= CACOP_HIT_INV.asUInt) &&
    !(s1_is_sc && !io_csr_access.llbit)
  val s1_exception = Wire(Valid(UInt(ECODE.getWidth.W)))

  io_dtlb_req.isWrite := s1_isWrite
  io_dtlb_req.vaddr   := s1_vaddr

  s1_exception.valid := s1_isALE || s1_isIPE || s1_tlb_xcep
  s1_exception.bits  := Mux(s1_isIPE, ECODE.IPE.asUInt, Mux(s1_isALE, ECODE.ALE.asUInt, io_dtlb_resp.exception.bits))

  val io_icache_cacop = IO(Decoupled(new CacheCACOPIO))
  val dcache          = Module(new DCache.DCache)
  val uop1to2         = Module(new PipeStageReg(new MicroOp, true))

  val s1_use_icache = s1_is_icacop && !s1_exception.valid
  val s1_use_dcache = !(s1_uop.bits.exuCmd === EXU_CACOP.asUInt && !s1_is_dcacop) &&
    !(s1_is_sc && io_csr_access.llbit) && !s1_exception.valid
  val s1_use_uop1to2 = WireInit(true.B)

  val s1_fire = s1_uop.valid && s1_regs.valid &&
    (!s1_use_icache || io_icache_cacop.ready) &&
    (!s1_use_dcache || dcache.io.req.ready) &&
    (!s1_use_uop1to2 || uop1to2.io.in.ready)

  dcache.io.flush      := io_kill
  dcache.io.axi        <> io_axi
  uop1to2.io.flush.get := io_kill

  s1_uop.ready  := s1_fire
  s1_regs.ready := s1_fire

  io_icache_cacop.valid      := s1_fire && s1_use_icache
  io_icache_cacop.bits.cacop := s1_cacop_code
  io_icache_cacop.bits.vaddr := s1_vaddr
  io_icache_cacop.bits.paddr := s1_paddr

  dcache.io.req.valid       := s1_fire && s1_use_dcache
  dcache.io.req.bits.cached := s1_cached
  dcache.io.req.bits.cacop  := Mux(s1_is_dcacop, s1_cacop_code, CACOP_NONE.asUInt)
  dcache.io.req.bits.vaddr  := s1_vaddr
  dcache.io.req.bits.paddr  := s1_paddr
  dcache.io.req.bits.wen    := s1_isWrite
  dcache.io.req.bits.wmask  := s1_wmask
  dcache.io.req.bits.wdata  := s1_writeData

  uop1to2.io.in.valid          := s1_fire && s1_use_uop1to2
  uop1to2.io.in.bits           := s1_uop.bits
  uop1to2.io.in.bits.exception := s1_exception.valid
  uop1to2.io.in.bits.ecode     := s1_exception.bits
  uop1to2.io.in.bits.badv      := s1_vaddr

  uop1to2.io.in.bits.debug.load      := VecInit(Seq(EXU_LDB, EXU_LDBU, EXU_LDH, EXU_LDHU, EXU_LDW, EXU_LLW).map(_.asUInt === s1_uop.bits.exuCmd)).asUInt
  uop1to2.io.in.bits.debug.loadVaddr := s1_vaddr
  uop1to2.io.in.bits.debug.loadPaddr := s1_paddr

  uop1to2.io.in.bits.debug.store := VecInit(
    Seq(EXU_STB, EXU_STH, EXU_STW, EXU_SCW).map(_.asUInt === s1_uop.bits.exuCmd),
  ).asUInt & (io_csr_access.llbit ## "b111".U(3.W))
  uop1to2.io.in.bits.debug.storeVaddr := s1_vaddr
  uop1to2.io.in.bits.debug.storePaddr := s1_paddr
  uop1to2.io.in.bits.debug.storeData := s1_writeData &
    (VecInit((0 until 4).map(i => Fill(8, s1_wmask(i)) << (i * 8))).reduce(_ | _))

  val s2_uop     = uop1to2.io.out
  val s2_loffset = RegEnable(s1_paddr(1, 0) << 3.U, s1_fire)
  val s2_lshift  = dcache.io.resp.bits >> s2_loffset
  val s2_rdata = MuxCase(
    s2_lshift,
    Seq(
      EXU_LDB  -> Fill(24, s2_lshift(7)) ## s2_lshift(7, 0),
      EXU_LDH  -> Fill(16, s2_lshift(15)) ## s2_lshift(15, 0),
      EXU_LDHU -> Fill(16, 0.U(1.W)) ## s2_lshift(15, 0),
      EXU_LDBU -> Fill(24, 0.U(1.W)) ## s2_lshift(7, 0),
    ).map { case (key, data) => (RegEnable(s1_uop.bits.exuCmd, s1_fire) === key.asUInt, data) },
  )
  val s2_use_dcache = RegEnable(s1_use_dcache, s1_fire)
  val s2_is_ll      = RegEnable(s1_is_ll, s1_fire)
  val s2_is_sc      = RegEnable(s1_is_sc, s1_fire)
  val s2_fire       = s2_uop.valid && (!s2_use_dcache || dcache.io.resp.valid)

  s2_uop.ready := s2_fire

  io_csr_access.set_llbit := io_mem_resp.valid && s2_is_ll
  io_csr_access.clr_llbit := io_mem_resp.valid && s2_is_sc

  io_mem_resp.valid     := s2_fire && !s2_uop.bits.exception
  io_mem_resp.bits.uop  := s2_uop.bits
  io_mem_resp.bits.data := Mux(s2_is_sc, io_csr_access.llbit.asUInt, s2_rdata)

  io_mem_resp.bits.uop.debug.wen      := io_mem_resp.bits.uop.ldst =/= 0.U
  io_mem_resp.bits.uop.debug.wdest    := io_mem_resp.bits.uop.ldst
  io_mem_resp.bits.uop.debug.wdata    := io_mem_resp.bits.data
  io_mem_resp.bits.uop.debug.loadData := io_mem_resp.bits.data

  io_mem_xcep.valid := s2_fire && s2_uop.bits.exception
  io_mem_xcep.bits  := s2_uop.bits

  if (params.debug) {
    dontTouch(s1_is_ll)
    dontTouch(s1_is_sc)
    dontTouch(s1_is_cacop)
    dontTouch(s1_is_icacop)
    dontTouch(s1_is_dcacop)
    dontTouch(s1_cacop_code)
    dontTouch(s1_cached)
    dontTouch(s1_vaddr)
    dontTouch(s1_paddr)
    dontTouch(s1_isWrite)
    dontTouch(s1_wmask)
    dontTouch(s1_writeData)
    dontTouch(s1_isIPE)
    dontTouch(s1_isALE)
    dontTouch(s1_tlb_xcep)
    dontTouch(s1_exception)
    dontTouch(s1_use_icache)
    dontTouch(s1_use_dcache)
    dontTouch(s1_is_sc)
    dontTouch(s1_use_uop1to2)
    dontTouch(s1_fire)
  }
}

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

  val (io_csr_access, io_tlb_cmd, io_csr_resp, io_csr_xcep) = if (hasCSR) {
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
    val io_tlb_cmd  = IO(Output(new TLBCmdIO))
    val io_csr_resp = IO(Output(Valid(new ExeUnitResp)))
    val io_csr_xcep = IO(Output(Valid(new MicroOp)))

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

    io_tlb_cmd.cmd       := EXU_TLBNONE.asUInt
    io_tlb_cmd.inv_op    := s1_uop.bits.imm(4, 0)
    io_tlb_cmd.inv_asid  := s1_regs.bits(0)(9, 0)
    io_tlb_cmd.inv_vaddr := s1_regs.bits(1)
    io_csr_resp.valid    := false.B
    io_csr_resp.bits     := DontCare
    io_csr_xcep          := DontCare
    io_csr_xcep.valid    := false.B
    when(s1_uop.valid && s1_regs.valid && s1_uop.bits.fuType === FUType.FUT_CSR.asUInt) {
      s1_regs.ready := true.B
      s1_uop.ready  := true.B

      val isIPE = EXUType.unqPriv(s1_uop.bits.exuCmd) && (io_csr_access.crmd.plv =/= 0.U)

      io_csr_access.we := EXUType.csrWen(s1_uop.bits.exuCmd)

      io_tlb_cmd.cmd := Mux(isIPE, EXU_TLBNONE.asUInt, s1_uop.bits.exuCmd)

      io_csr_resp.valid    := !io_csr_xcep.valid
      io_csr_resp.bits.uop := s1_uop.bits
      io_csr_resp.bits.data := MuxLookup(s1_uop.bits.exuCmd, io_csr_access.rdata)(
        Seq(
          EXU_CPUCFG.asUInt  -> cpuinfo.io.info,
          EXU_RDCNTID.asUInt -> io_csr_access.counterID,
          EXU_RDCNTVL.asUInt -> io_csr_access.cntvl,
          EXU_RDCNTVH.asUInt -> io_csr_access.cntvh,
        ),
      )

      io_csr_xcep.valid          := isIPE
      io_csr_xcep.bits           := s1_uop.bits
      io_csr_xcep.bits.exception := io_csr_xcep.valid
      io_csr_xcep.bits.ecode     := ECODE.IPE.asUInt
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

    (Some(io_csr_access), Some(io_tlb_cmd), Some(io_csr_resp), Some(io_csr_xcep))
  } else {
    (None, None, None, None)
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
