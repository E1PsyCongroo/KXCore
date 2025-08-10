package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.utils._
import KXCore.common.peripheral._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._
import dataclass.data
import KXCore.superscalar.EXUType.isCSR

abstract class ExecutionUnit(implicit params: CoreParameters) extends Module {
  def fu_types: UInt = 0.U(FUType.getWidth.W)
  def nReaders: Int  = 1
  require(nReaders > 0 && nReaders <= 2)

  val io_kill       = IO(Input(Bool()))
  val io_fu_types   = IO(Output(UInt(FUType.getWidth.W)))
  val io_iss_uop    = IO(Flipped(Decoupled(new MicroOp)))
  val io_read_reqs  = IO(Vec(nReaders, Decoupled(UInt(params.backendParams.pregWidth.W))))
  val io_read_resps = IO(Vec(nReaders, Input(UInt(params.commonParams.dataWidth.W))))

  val iss_uop_ext = ReadyValidIOExpand(io_iss_uop, 3)

  io_read_reqs(0).valid := iss_uop_ext.valid(0) && iss_uop_ext.bits.busy && iss_uop_ext.bits.lrs1 =/= 0.U
  io_read_reqs(0).bits  := iss_uop_ext.bits.prs1
  if (nReaders == 2) {
    io_read_reqs(1).valid := iss_uop_ext.valid(0) && iss_uop_ext.bits.busy && iss_uop_ext.bits.lrs2 =/= 0.U
    io_read_reqs(1).bits  := iss_uop_ext.bits.prs2
  }

  val stage0Regs = Wire(Decoupled(Vec(nReaders, UInt(params.commonParams.dataWidth.W))))
  stage0Regs.valid := iss_uop_ext.valid(0) && iss_uop_ext.bits.busy &&
    io_read_reqs.map(req => (!req.valid || req.ready)).reduce(_ && _)
  stage0Regs.bits(0) := io_read_resps(0)
  if (nReaders == 2) {
    stage0Regs.bits(1) := io_read_resps(1)
  }
  iss_uop_ext.ready(0) := stage0Regs.ready

  val stage0Uop = Wire(Decoupled(new MicroOp))
  stage0Uop.valid      := iss_uop_ext.valid(1) && iss_uop_ext.bits.busy
  stage0Uop.bits       := iss_uop_ext.bits
  iss_uop_ext.ready(1) := stage0Uop.ready

  val stage0to1Regs = Wire(Decoupled(Vec(nReaders, UInt(params.commonParams.dataWidth.W))))
  PipeConnect(Some(io_kill), stage0Regs, stage0to1Regs)

  val stage0to1Uop = Wire(Decoupled(new MicroOp))
  PipeConnect(Some(io_kill), stage0Uop, stage0to1Uop)

  val stage1Regs = Wire(Decoupled(Vec(nReaders, UInt(params.commonParams.dataWidth.W))))
  stage1Regs.valid    := stage0to1Regs.valid
  stage1Regs.bits     := stage0to1Regs.bits
  stage0to1Regs.ready := stage1Regs.ready

  val stage1Uop = Wire(Decoupled(new MicroOp))
  stage1Uop.valid    := stage0to1Uop.valid
  stage1Uop.bits     := stage0to1Uop.bits
  stage0to1Uop.ready := stage1Uop.ready

  io_fu_types := fu_types
}

class MemExeUnit(implicit params: CoreParameters) extends ExecutionUnit {
  import params.{commonParams, axiParams}
  import commonParams.{dataWidth, vaddrWidth, paddrWidth}
  import LSUType._
  override def fu_types: UInt = FUType.FUT_MEM.asUInt
  override def nReaders       = 2

  iss_uop_ext.ready(2) := true.B

  val io_dtlb_req  = IO(Output(new TLBReq()(commonParams)))
  val io_dtlb_resp = IO(Input(new TLBResp()(commonParams)))
  val io_axi       = IO(new AXIBundle(params.axiParams))

  val isWrite = Seq(LSU_STB, LSU_STH, LSU_STW).map(_.asUInt === stage1Uop.bits.lsuCmd).reduce(_ || _)
  // io_dtlb_req.asid    := 0.U
  io_dtlb_req.isWrite := isWrite
  // io_dtlb_req.plv     := 0.U
  io_dtlb_req.vaddr := stage1Regs.bits(0) + stage1Uop.bits.imm
  io_dtlb_req.size := Mux(
    isWrite,
    MuxLookup(stage1Uop.bits.lsuCmd, 0.U)(
      Seq(
        LSU_STB.asUInt -> 0.U,
        LSU_STH.asUInt -> 1.U,
        LSU_STW.asUInt -> 3.U,
      ),
    ),
    MuxLookup(stage1Uop.bits.lsuCmd, 0.U)(
      Seq(
        LSU_LDB.asUInt  -> 0.U,
        LSU_LDBU.asUInt -> 0.U,
        LSU_LDH.asUInt  -> 1.U,
        LSU_LDHU.asUInt -> 1.U,
        LSU_LDW.asUInt  -> 3.U,
      ),
    ),
  )

  val stage1Data = Wire(DecoupledIO(new Bundle {
    val uop       = new MicroOp
    val isWrite   = Bool()
    val writeData = UInt(dataWidth.W)
    val paddr     = UInt(paddrWidth.W)
    val vaddr     = UInt(vaddrWidth.W)
    val wmask     = UInt(4.W)
  }))
  stage1Data.valid              := stage1Uop.valid && stage1Regs.valid
  stage1Uop.ready               := stage1Data.ready
  stage1Regs.ready              := stage1Data.ready
  stage1Data.bits.uop           := stage1Uop.bits
  stage1Data.bits.uop.exception := stage1Uop.bits.exception | io_dtlb_resp.exception.valid
  stage1Data.bits.uop.ecode     := Mux(stage1Uop.bits.exception, stage1Uop.bits.ecode, io_dtlb_resp.exception.bits)
  stage1Data.bits.isWrite       := isWrite
  stage1Data.bits.writeData     := stage1Regs.bits(1) << (stage1Data.bits.vaddr(1, 0) ## 0.U(3.W))
  stage1Data.bits.paddr         := io_dtlb_resp.paddr
  stage1Data.bits.vaddr         := io_dtlb_req.vaddr
  stage1Data.bits.wmask := MuxLookup(stage1Uop.bits.lsuCmd, 0.U)(
    Seq(
      LSU_STB.asUInt -> ("b0001".U << stage1Data.bits.paddr(1, 0)),
      LSU_STH.asUInt -> ("b0011".U << stage1Data.bits.paddr(1, 0)),
      LSU_STW.asUInt -> "b1111".U,
    ),
  )

  // TODO: Fix this state machine make it Irrevocable
  val sSendReq :: sWaitAWfire :: sWaitWfire :: sWaitResp :: sWaitAWfireNextIgnore :: sWaitWfireNextIgnore :: sIgnoreResp :: sSendExcp :: Nil = Enum(8)

  val state     = RegInit(sSendReq)
  val nextState = WireDefault(sSendReq)
  nextState := MuxLookup(state, sSendReq)(
    Seq(
      sSendReq -> Mux(
        stage1Data.valid && !stage1Data.bits.uop.exception,
        MuxCase(
          sSendReq,
          Seq(
            (io_axi.ar.fire || (io_axi.aw.fire && io_axi.w.fire)) -> sWaitResp,
            (io_axi.aw.fire)                                      -> sWaitWfire,
            (io_axi.w.fire)                                       -> sWaitAWfire,
          ),
        ),
        Mux(stage1Data.valid && stage1Data.bits.uop.exception, sSendExcp, sSendReq),
      ),
      sWaitAWfire -> Mux(
        io_kill,
        Mux(io_axi.aw.fire, sIgnoreResp, sWaitAWfireNextIgnore),
        Mux(io_axi.aw.fire, sWaitResp, sWaitAWfire),
      ),
      sWaitWfire -> Mux(
        io_kill,
        Mux(io_axi.w.fire, sIgnoreResp, sWaitWfireNextIgnore),
        Mux(io_axi.w.fire, sWaitResp, sWaitWfire),
      ),
      sWaitResp -> Mux(
        (io_axi.r.fire && io_axi.r.bits.id === 1.U) || (io_axi.b.fire && io_axi.b.bits.id === 1.U),
        sSendReq,
        Mux(io_kill, sIgnoreResp, sWaitResp),
      ),
      sWaitAWfireNextIgnore -> Mux(io_axi.aw.fire, sIgnoreResp, sWaitWfireNextIgnore),
      sWaitWfireNextIgnore  -> Mux(io_axi.w.fire, sIgnoreResp, sWaitWfireNextIgnore),
      sIgnoreResp -> Mux(
        (io_axi.r.fire && io_axi.r.bits.id === 1.U) || (io_axi.b.fire && io_axi.b.bits.id === 1.U),
        sSendReq,
        sIgnoreResp,
      ),
      sSendExcp -> sSendReq,
    ),
  )
  state := nextState

  stage1Data.ready := (nextState === sWaitResp) || (nextState === sSendExcp)

  io_axi.ar.valid     := state === sSendReq && stage1Data.valid && !stage1Data.bits.isWrite && !stage1Data.bits.uop.exception
  io_axi.ar.bits.addr := stage1Data.bits.paddr
  io_axi.ar.bits.id   := 1.U
  io_axi.ar.bits.len  := 0.U

  io_axi.ar.bits.size  := log2Ceil(dataWidth / 8).U
  io_axi.ar.bits.burst := AXIParameters.BURST_INCR
  io_axi.ar.bits.lock  := 0.U
  io_axi.ar.bits.cache := 0.U
  io_axi.ar.bits.prot  := 0.U

  io_axi.r.ready := (state === sWaitResp) || (state === sIgnoreResp)

  io_axi.aw.valid := state === sWaitAWfire || state === sWaitAWfireNextIgnore ||
    (state === sSendReq && stage1Data.valid && stage1Data.bits.isWrite && !stage1Data.bits.uop.exception)
  io_axi.aw.bits.addr := stage1Data.bits.paddr
  io_axi.aw.bits.id   := 1.U
  io_axi.aw.bits.len  := 0.U

  io_axi.aw.bits.size  := log2Ceil(dataWidth / 8).U
  io_axi.aw.bits.burst := AXIParameters.BURST_INCR
  io_axi.aw.bits.lock  := 0.U
  io_axi.aw.bits.cache := 0.U
  io_axi.aw.bits.prot  := 0.U

  io_axi.w.valid := state === sWaitWfire || state === sWaitWfireNextIgnore ||
    (state === sSendReq && stage1Data.valid && stage1Data.bits.isWrite && !stage1Data.bits.uop.exception)

  io_axi.w.bits.id   := 1.U
  io_axi.w.bits.last := 1.U
  io_axi.w.bits.data := stage1Data.bits.writeData
  io_axi.w.bits.strb := Mux(
    state === sWaitWfireNextIgnore || (state === sWaitWfire && io_kill),
    0.U,
    stage1Data.bits.wmask,
  )

  io_axi.b.ready := (state === sWaitResp) || (state === sIgnoreResp)

  // For Now
  when(io_axi.r.fire) {
    assert(
      io_axi.r.bits.last.asBool && io_axi.r.bits.id === 1.U &&
        io_axi.r.bits.resp === AXIParameters.RESP_OKAY,
    )
  }
  when(io_axi.b.fire) { assert(io_axi.b.bits.id === 1.U && io_axi.b.bits.resp === AXIParameters.RESP_OKAY) }

  val stage2Data  = RegEnable(stage1Data.bits, stage1Data.fire)
  val io_mem_resp = IO(Output(Valid(new ExeUnitResp)))

  val loffset = WireDefault(stage2Data.paddr(1, 0) << 3.U)
  val lshift  = io_axi.r.bits.data >> loffset
  val rdata = MuxCase(
    lshift,
    Seq(
      LSU_LDB  -> Fill(24, lshift(7)) ## lshift(7, 0),
      LSU_LDH  -> Fill(16, lshift(15)) ## lshift(15, 0),
      LSU_LDHU -> Fill(16, 0.U(1.W)) ## lshift(15, 0),
      LSU_LDBU -> Fill(24, 0.U(1.W)) ## lshift(7, 0),
    ).map { case (key, data) => (stage2Data.uop.lsuCmd === key.asUInt, data) },
  )

  io_mem_resp.valid := !io_kill && ((state === sSendExcp) || ((state === sWaitResp) &&
    ((io_axi.r.fire && io_axi.r.bits.id === 1.U) || (io_axi.b.fire && io_axi.b.bits.id === 1.U))))
  io_mem_resp.bits.brInfo.valid         := false.B
  io_mem_resp.bits.brInfo.bits          := DontCare
  io_mem_resp.bits.uop                  := stage2Data.uop
  io_mem_resp.bits.uop.badv             := stage2Data.vaddr
  io_mem_resp.bits.uop.debug.load       := VecInit(Seq(LSU_LDB, LSU_LDBU, LSU_LDH, LSU_LDHU, LSU_LDW).map(_.asUInt === stage2Data.uop.lsuCmd)).asUInt
  io_mem_resp.bits.uop.debug.loadVaddr  := stage2Data.vaddr
  io_mem_resp.bits.uop.debug.loadPaddr  := stage2Data.paddr
  io_mem_resp.bits.uop.debug.loadData   := rdata
  io_mem_resp.bits.uop.debug.store      := VecInit(Seq(LSU_STB, LSU_STH, LSU_STW).map(_.asUInt === stage2Data.uop.lsuCmd)).asUInt
  io_mem_resp.bits.uop.debug.storeVaddr := stage2Data.vaddr
  io_mem_resp.bits.uop.debug.storePaddr := stage2Data.paddr
  io_mem_resp.bits.uop.debug.storeData := stage2Data.writeData & (VecInit(
    (0 until 4).map { i =>
      val bit = stage2Data.wmask(i)
      Fill(8, bit) << (i * 8)
    },
  ).reduce(_ | _))

  io_mem_resp.bits.data := rdata

  dontTouch(state)
  dontTouch(nextState)
  dontTouch(stage1Uop)
  dontTouch(stage1Regs)
  dontTouch(stage1Data)
  dontTouch(stage2Data)
}

/*
class MemExeUnitWithCache(implicit params: CoreParameters) extends ExecutionUnit {
  import params._
  import commonParams.{dataWidth, vaddrWidth, paddrWidth}
  import backendParams._
  import LSUType._
  override def fu_types: UInt = FUType.FUT_MEM.asUInt
  override def nReaders       = 2

  iss_uop_ext.ready(2) := true.B

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

  iss_uop_ext.ready(2) := true.B

  stage1Regs.ready := false.B
  stage1Uop.ready  := false.B

  val io_unq_resp = IO(Output(Valid(new ExeUnitResp)))

  io_unq_resp.valid             := false.B
  io_unq_resp.bits              := DontCare
  io_unq_resp.bits.brInfo.valid := false.B

  if (hasMul) {
    val mulUnit = Module(new MultiplyUnit)
    mulUnit.io.kill := io_kill

    mulUnit.io.req.bits.rs1_data := stage1Regs.bits(0)
    mulUnit.io.req.bits.rs2_data := stage1Regs.bits(1)
    mulUnit.io.req.bits.uop      := stage1Uop.bits
    mulUnit.io.req.bits.ftq_info := DontCare
    mulUnit.io.req.valid         := false.B
    when(stage1Uop.valid && stage1Regs.valid && EXUType.isMul(stage1Uop.bits.exuCmd)) {
      mulUnit.io.req.valid := true.B
      stage1Uop.ready      := mulUnit.io.resp.valid
      stage1Regs.ready     := mulUnit.io.resp.valid
      io_unq_resp.valid    := mulUnit.io.resp.valid
      io_unq_resp.bits     := mulUnit.io.resp.bits
    }
    mulUnit.io.resp.ready := true.B
  }

  if (hasDiv) {
    val divUnit = Module(new DivUnit)
    divUnit.io.kill := io_kill

    divUnit.io.req.bits.rs1_data := stage1Regs.bits(0)
    divUnit.io.req.bits.rs2_data := stage1Regs.bits(1)
    divUnit.io.req.bits.uop      := stage1Uop.bits
    divUnit.io.req.bits.ftq_info := DontCare
    divUnit.io.req.valid         := false.B
    when(stage1Uop.valid && stage1Regs.valid && EXUType.isDiv(stage1Uop.bits.exuCmd)) {
      divUnit.io.req.valid := true.B
      stage1Uop.ready      := divUnit.io.resp.valid
      stage1Regs.ready     := divUnit.io.resp.valid
      io_unq_resp.valid    := divUnit.io.resp.valid
      io_unq_resp.bits     := divUnit.io.resp.bits
    }
    divUnit.io.resp.ready := true.B
  }

  val io_csr_access =
    if (hasCSR) Some(IO(new Bundle {
      val raddr = Output(UInt(14.W))       // CSR address to read
      val rdata = Input(UInt(dataWidth.W)) // CSR read data

      val we    = Output(Bool())            // Write enable
      val waddr = Output(UInt(14.W))        // CSR address to write
      val wdata = Output(UInt(dataWidth.W)) // CSR write data
      val wmask = Output(UInt(dataWidth.W)) // CSR write mask

      val counterID = Input(UInt(dataWidth.W))
      val cntvh     = Input(UInt(dataWidth.W))
      val cntvl     = Input(UInt(dataWidth.W))
    }))
    else None

  if (hasCSR) {

    io_csr_access.get.raddr := stage1Uop.bits.imm
    io_csr_access.get.waddr := stage1Uop.bits.imm
    io_csr_access.get.wmask := Mux(
      stage1Uop.bits.csrCmd === CSRType.XCHG.asUInt,
      stage1Regs.bits(0),
      Fill(dataWidth, 1.B),
    )
    io_csr_access.get.wdata := stage1Regs.bits(1)
    io_csr_access.get.we    := false.B

    when(stage1Uop.valid && stage1Regs.valid && isCSR(stage1Uop.bits.exuCmd)) {
      stage1Regs.ready     := true.B
      stage1Uop.ready      := true.B
      io_csr_access.get.we := CSRType.isWrite(stage1Uop.bits.csrCmd)
      io_unq_resp.valid    := true.B
      io_unq_resp.bits.uop := stage1Uop.bits
      io_unq_resp.bits.data := MuxLookup(stage1Uop.bits.csrCmd, io_csr_access.get.rdata)(
        Seq(
          CSRType.RDCNTID.asUInt -> io_csr_access.get.counterID,
          CSRType.RDCNTVL.asUInt -> io_csr_access.get.cntvl,
          CSRType.RDCNTVH.asUInt -> io_csr_access.get.cntvh,
        ),
      )
    }
  }

  dontTouch(stage1Uop)
  dontTouch(stage1Regs)
}

class ALUExeUnit(implicit params: CoreParameters) extends ExecutionUnit {
  override def fu_types: UInt = FUType.FUT_ALU.asUInt | FUType.FUT_CFI.asUInt
  override def nReaders       = 2

  val io_ftq_req  = IO(Vec(2, Decoupled(UInt(params.frontendParams.ftqIdxWidth.W))))
  val io_ftq_resp = IO(Input(Vec(2, new FTQInfo)))

  io_ftq_req(0).valid := iss_uop_ext.valid(2) && iss_uop_ext.bits.busy &&
    (iss_uop_ext.bits.op1Sel === OP1Type.OP1_PC.asUInt || iss_uop_ext.bits.cfiType =/= CFIType.CFI_NONE.asUInt)
  io_ftq_req(0).bits := iss_uop_ext.bits.ftqIdx
  io_ftq_req(1).valid := iss_uop_ext.valid(2) && iss_uop_ext.bits.busy &&
    iss_uop_ext.bits.cfiType === CFIType.CFI_JIRL.asUInt
  io_ftq_req(1).bits := WrapInc(iss_uop_ext.bits.ftqIdx, params.frontendParams.ftqNum)
  val stage0Ftq = Wire(Decoupled(io_ftq_resp.cloneType))
  stage0Ftq.valid := iss_uop_ext.valid(2) && iss_uop_ext.bits.busy
  (!io_ftq_req(0).valid || io_ftq_req(0).ready) && (!io_ftq_req(1).valid || io_ftq_req(1).ready)
  stage0Ftq.bits       := io_ftq_resp
  iss_uop_ext.ready(2) := stage0Ftq.ready

  val stage0to1Ftq = Wire(Decoupled(io_ftq_resp.cloneType))
  PipeConnect(Some(io_kill), stage0Ftq, stage0to1Ftq)

  val stage1Ftq = Wire(Decoupled(io_ftq_resp.cloneType))
  stage1Ftq.valid    := stage0to1Ftq.valid
  stage1Ftq.bits     := stage0to1Ftq.bits
  stage0to1Ftq.ready := stage1Ftq.ready

  val exe_req = Wire(new FuncUnitReq)
  exe_req.uop      := stage1Uop.bits
  exe_req.rs1_data := stage1Regs.bits(0)
  exe_req.rs2_data := stage1Regs.bits(1)
  exe_req.ftq_info := stage1Ftq.bits

  val alu = Module(new ALUUnit)
  alu.io.req.valid := stage1Uop.valid && stage1Regs.valid && stage1Ftq.valid
  stage1Uop.ready  := alu.io.req.fire
  stage1Regs.ready := alu.io.req.fire
  stage1Ftq.ready  := alu.io.req.fire
  alu.io.req.bits  := exe_req
  alu.io.kill      := io_kill

  val io_alu_resp = IO(Output(Valid(new ExeUnitResp)))
  io_alu_resp.valid := alu.io.resp.valid
  alu.io.resp.ready := true.B
  io_alu_resp.bits  := alu.io.resp.bits

  dontTouch(stage0Uop)
  dontTouch(stage0Ftq)
  dontTouch(stage0Regs)
  dontTouch(stage1Uop)
  dontTouch(stage1Ftq)
  dontTouch(stage1Regs)
  dontTouch(io_read_reqs)
}
