package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.Privilege._
import KXCore.common.peripheral._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._
import scala.annotation.varargs

class BackEndIO(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, axiParams, frontendParams, backendParams}
  import commonParams.{dataWidth, vaddrWidth}
  import frontendParams.{ftqIdxWidth}
  import backendParams.{coreWidth, lregNum}

  val axi         = new AXIBundle(axiParams)
  val dtlbReq     = Output(new TLBReq)
  val dtlbResp    = Input(new TLBResp)
  val fetchPacket = Flipped(Decoupled(new FetchBufferResp()))
  val ftqReqs     = Output(Vec(3, UInt(ftqIdxWidth.W)))
  val ftqResps    = Input(Vec(3, new FTQInfo))
  val commit      = Valid(UInt(ftqIdxWidth.W))
  val redirect    = Output(new RoBRedirectIO)
  val csr_access = new Bundle {
    val raddr = Output(UInt(14.W))       // CSR address to read
    val rdata = Input(UInt(dataWidth.W)) // CSR read data

    val we    = Output(Bool())            // Write enable
    val waddr = Output(UInt(14.W))        // CSR address to write
    val wdata = Output(UInt(dataWidth.W)) // CSR write data
    val wmask = Output(UInt(dataWidth.W)) // CSR write mask

    val counterID = Input(UInt(dataWidth.W))
    val cntvh     = Input(UInt(dataWidth.W))
    val cntvl     = Input(UInt(dataWidth.W))

    val epc       = Output(UInt(vaddrWidth.W)) // Program counter for exception handling
    val ecode     = Output(UInt(6.W))          // Exception code
    val ecode_sub = Output(UInt(9.W))
    val badv      = Output(UInt(vaddrWidth.W)) // Bad virtual address for exception
    val excp_en   = Output(Bool())             // Exception enable
    val eentry    = Input(UInt(32.W))          // Exception entry address

    val eret_en = Output(Bool())            // Exception return enable
    val era     = Input(UInt(vaddrWidth.W)) // Exception return address

    val intr_pending = Input(Bool())
  }
  val debug = Output(new Bundle {
    val regs          = Vec(lregNum, UInt(dataWidth.W))
    val rob_commit    = new RoBCommitIO
    val rob_exception = new RoBExceptionIO
  })
}

class BackEnd(implicit params: CoreParameters) extends Module {
  import params.{commonParams, frontendParams, backendParams}
  import backendParams.{coreWidth, issueParams, pregNum, memIQParams, unqIQParams, intIQParams}

  require(memIQParams.issueWidth == 1)
  require(unqIQParams.issueWidth == 1)

  val io = IO(new BackEndIO)

  val decoder         = Module(new Decoder)
  val renameMapTable  = Module(new RenameMapTable(true))
  val renameFreeList  = Module(new RenameFreeList(coreWidth, coreWidth))
  val renameBusyTable = Module(new RenameBusyTable(true))
  val rob             = Module(new ReorderBuffer)
  val dispatcher      = Module(new BasicDispatcher)
  val memIssUnit      = Module(new IssueUnitCollapsing(memIQParams))
  val unqIssUnit      = Module(new IssueUnitCollapsing(unqIQParams))
  val intIssUnit      = Module(new IssueUnitCollapsing(intIQParams))
  val memExeUnit      = Module(new MemExeUnit)
  val unqExeUnit      = Module(new UniqueExeUnit(true, true, true))
  val aluExeUnits     = Seq.fill(intIQParams.issueWidth)(Module(new ALUExeUnit))
  val regFile = Module(new FullyPortedRF(pregNum, aluExeUnits.map(_.nReaders).sum + memExeUnit.nReaders + unqExeUnit.nReaders, aluExeUnits.length + 1 + 2))

  val flush = Wire(Bool())

  renameMapTable.io.rollback := flush
  renameFreeList.io.rollback := flush
  memIssUnit.io.flush        := flush
  unqIssUnit.io.flush        := flush
  intIssUnit.io.flush        := flush
  memExeUnit.io_kill         := flush
  unqExeUnit.io_kill         := flush
  aluExeUnits.foreach(unit => unit.io_kill := flush)

  memIssUnit.io.fu_types := VecInit(Seq(memExeUnit.io_fu_types))
  unqIssUnit.io.fu_types := VecInit(Seq(unqExeUnit.io_fu_types))
  intIssUnit.io.fu_types := VecInit(aluExeUnits.map(_.io_fu_types))

  // decode & rename
  val decData       = Wire(Decoupled(Vec(coreWidth, Valid(new MicroOp))))
  val decUopFire    = Wire(UInt(coreWidth.W))
  val decUopFireReg = RegInit(0.U(coreWidth.W))
  decData.valid        := io.fetchPacket.valid
  io.fetchPacket.ready := decData.ready
  decUopFire           := VecInit(io.fetchPacket.bits.uops.map(_.valid)).asUInt
  decUopFireReg        := Mux(flush, 0.U, Mux(decData.fire, Mux(decUopFire.andR, 0.U, decUopFire), decUopFireReg))
  for (i <- 0 until coreWidth) {
    decoder.io.req(i)     := io.fetchPacket.bits.uops(i).bits
    decData.bits(i).valid := io.fetchPacket.bits.uops(i).valid && !decUopFireReg(i)
    decData.bits(i).bits  := decoder.io.resp(i)

    renameMapTable.io.mapReqs(i).ldst := decData.bits(i).bits.ldst
    renameMapTable.io.mapReqs(i).lrs1 := decData.bits(i).bits.lrs1
    renameMapTable.io.mapReqs(i).lrs2 := decData.bits(i).bits.lrs2
    decData.bits(i).bits.stalePdst    := renameMapTable.io.mapResps(i).stalePdst
    decData.bits(i).bits.prs1         := renameMapTable.io.mapResps(i).prs1
    decData.bits(i).bits.prs2         := renameMapTable.io.mapResps(i).prs2
  }

  val decToRen = Wire(Decoupled(Vec(coreWidth, Valid(new MicroOp))))
  PipeConnect(Some(flush), decData, decToRen)

  // rename & dispatch
  val disData       = Wire(Decoupled(Vec(coreWidth, Valid(new MicroOp))))
  val disUopFire    = Wire(UInt(coreWidth.W))
  val disUopFireReg = RegInit(0.U(coreWidth.W))
  disData.valid  := decToRen.valid
  decToRen.ready := disData.ready
  disUopFireReg  := Mux(decToRen.ready || flush, 0.U, disUopFireReg | disUopFire)
  disData.ready  := VecInit(decToRen.bits.map(_.valid)).asUInt === disUopFire

  var dis_not_fire    = false.B
  var dis_first_valid = true.B
  val dis_alloc_regs  = Reg(Vec(coreWidth, UInt(backendParams.pregWidth.W)))
  for (i <- 0 until coreWidth) {
    renameFreeList.io.allocPregs(i).ready := disUopFire(i) && disData.bits(i).bits.ldst =/= 0.U
    dis_alloc_regs(i) := Mux(
      renameFreeList.io.allocPregs(i).ready,
      renameFreeList.io.allocPregs(i).bits,
      dis_alloc_regs(i),
    )

    renameMapTable.io.renRemapReqs(i).valid := disUopFire(i) && disData.bits(i).bits.ldst =/= 0.U
    renameMapTable.io.renRemapReqs(i).ldst  := disData.bits(i).bits.ldst
    renameMapTable.io.renRemapReqs(i).pdst  := disData.bits(i).bits.pdst

    renameBusyTable.io.uopReqs(i)    := disData.bits(i).bits
    renameBusyTable.io.rebusyReqs(i) := disData.bits(i).valid

    rob.io.alloc(i).valid := disUopFire(i)
    rob.io.alloc(i).uop   := disData.bits(i).bits

    val dis_first_valid_yet = WireInit(dis_first_valid)
    val dis_not_fire_yet    = WireInit(dis_not_fire)

    disData.bits(i).valid := decToRen.valid && decToRen.bits(i).valid && !disUopFireReg(i) &&
      (renameFreeList.io.allocPregs(i).valid || disData.bits(i).bits.ldst === 0.U) &&
      (!disData.bits(i).bits.isUnique || (dis_first_valid_yet && rob.io.empty)) && !dis_not_fire_yet
    disData.bits(i).bits        := decToRen.bits(i).bits
    disData.bits(i).bits.pdst   := Mux(disData.bits(i).bits.ldst =/= 0.U, renameFreeList.io.allocPregs(i).bits, 0.U)
    disData.bits(i).bits.robIdx := rob.io.alloc(i).idx
    for (j <- 0 until i) {
      when(decToRen.bits(j).valid) {
        when(disData.bits(j).bits.ldst === disData.bits(i).bits.ldst) {
          disData.bits(i).bits.stalePdst := Mux(disUopFireReg(j), dis_alloc_regs(j), disData.bits(j).bits.pdst)
        }
        when(disData.bits(j).bits.ldst === disData.bits(i).bits.lrs1) {
          disData.bits(i).bits.prs1 := Mux(disUopFireReg(j), dis_alloc_regs(j), disData.bits(j).bits.pdst)
        }
        when(disData.bits(j).bits.ldst === disData.bits(i).bits.lrs2) {
          disData.bits(i).bits.prs2 := Mux(disUopFireReg(j), dis_alloc_regs(j), disData.bits(j).bits.pdst)
        }
      }
    }
    disData.bits(i).bits.prs1Busy := renameBusyTable.io.busyResps(i).prs1Busy
    disData.bits(i).bits.prs2Busy := renameBusyTable.io.busyResps(i).prs2Busy

    dis_first_valid = dis_first_valid && !disData.bits(i).valid
    dis_not_fire = dis_not_fire || (decToRen.bits(i).valid && !disUopFireReg(i) &&
      (!disData.bits(i).valid || !dispatcher.io.ren_uops(i).ready || !rob.io.alloc(i).ready))
    dispatcher.io.ren_uops(i).valid := disData.bits(i).valid
    dispatcher.io.ren_uops(i).bits  := disData.bits(i).bits
  }
  disUopFire := VecInit(disData.bits.map(_.valid)).asUInt & VecInit(dispatcher.io.ren_uops.map(_.ready)).asUInt & VecInit(rob.io.alloc.map(_.ready)).asUInt

  // issue
  dispatcher.io.dis_uops(0) <> memIssUnit.io.dis_uops
  dispatcher.io.dis_uops(1) <> unqIssUnit.io.dis_uops
  dispatcher.io.dis_uops(2) <> intIssUnit.io.dis_uops

  // execute
  memIssUnit.io.iss_uops(0) <> memExeUnit.io_iss_uop
  io.axi                    <> memExeUnit.io_axi
  io.dtlbReq                := memExeUnit.io_dtlb_req
  memExeUnit.io_dtlb_resp   := io.dtlbResp

  io.csr_access.raddr                := unqExeUnit.io_csr_access.get.raddr
  unqExeUnit.io_csr_access.get.rdata := io.csr_access.rdata

  io.csr_access.we    := unqExeUnit.io_csr_access.get.we
  io.csr_access.waddr := unqExeUnit.io_csr_access.get.waddr
  io.csr_access.wdata := unqExeUnit.io_csr_access.get.wdata
  io.csr_access.wmask := unqExeUnit.io_csr_access.get.wmask

  unqExeUnit.io_csr_access.get.counterID := io.csr_access.counterID
  unqExeUnit.io_csr_access.get.cntvh     := io.csr_access.cntvh
  unqExeUnit.io_csr_access.get.cntvl     := io.csr_access.cntvl

  /* use memExeUnitWithCache
  memExeUnit.io_dcache_flush.stage1 := flush
  memExeUnit.io_dcache_flush.stage2 := flush
   */
  unqIssUnit.io.iss_uops(0) <> unqExeUnit.io_iss_uop
  intIssUnit.io.iss_uops zip aluExeUnits map { case (iss_uop, exu) => iss_uop <> exu.io_iss_uop }

  memExeUnit.io_read_reqs(0)  <> regFile.io.read_reqs(0)
  memExeUnit.io_read_reqs(1)  <> regFile.io.read_reqs(1)
  memExeUnit.io_read_resps(0) <> regFile.io.read_resps(0)
  memExeUnit.io_read_resps(1) <> regFile.io.read_resps(1)
  unqExeUnit.io_read_reqs(0)  <> regFile.io.read_reqs(2)
  unqExeUnit.io_read_reqs(1)  <> regFile.io.read_reqs(3)
  unqExeUnit.io_read_resps(0) <> regFile.io.read_resps(2)
  unqExeUnit.io_read_resps(1) <> regFile.io.read_resps(3)

  val ftq_port_issued = Array.fill(2) { false.B }
  val ftq_port_addrs  = Array.fill(2) { 0.U(frontendParams.ftqIdxWidth.W) }
  for (i <- 0 until aluExeUnits.length) {
    aluExeUnits(i).io_read_reqs(0)  <> regFile.io.read_reqs(2 * (i + 2))
    aluExeUnits(i).io_read_reqs(1)  <> regFile.io.read_reqs(2 * (i + 2) + 1)
    aluExeUnits(i).io_read_resps(0) <> regFile.io.read_resps(2 * (i + 2))
    aluExeUnits(i).io_read_resps(1) <> regFile.io.read_resps(2 * (i + 2) + 1)
    for (w <- 0 until 2) {
      val req         = aluExeUnits(i).io_ftq_req(w)
      var read_issued = false.B
      val data_sel    = WireInit(0.U(2.W))
      for (j <- 0 until 2) {
        val issue_read = WireInit(false.B)
        val use_port   = WireInit(false.B)
        when(!read_issued && !ftq_port_issued(j) && req.valid) {
          issue_read := true.B
          use_port   := true.B
          data_sel   := UIntToOH(j.U)
        }
        val was_port_issued_yet = ftq_port_issued(j)
        ftq_port_issued(j) = use_port || ftq_port_issued(j)
        ftq_port_addrs(j) = ftq_port_addrs(j) | Mux(was_port_issued_yet || !use_port, 0.U, req.bits)
        read_issued = issue_read || read_issued
      }
      req.ready                     := read_issued
      aluExeUnits(i).io_ftq_resp(w) := Mux(data_sel(0), io.ftqResps(1), io.ftqResps(2))
    }
  }
  for (j <- 0 until 2) {
    io.ftqReqs(j + 1) := ftq_port_addrs(j)
  }

  // write back
  val mem_csr_wb = Seq(memExeUnit.io_mem_resp, unqExeUnit.io_csr_resp.get, unqExeUnit.io_mul_div_resp.get)
  mem_csr_wb.zipWithIndex.foreach { case (resp, i) =>
    renameBusyTable.io.wbValids(i) := resp.valid
    renameBusyTable.io.wbPdsts(i)  := resp.bits.uop.pdst

    memIssUnit.io.wakeup_ports(i).valid := resp.valid
    memIssUnit.io.wakeup_ports(i).bits  := resp.bits.uop.pdst
    unqIssUnit.io.wakeup_ports(i).valid := resp.valid
    unqIssUnit.io.wakeup_ports(i).bits  := resp.bits.uop.pdst
    intIssUnit.io.wakeup_ports(i).valid := resp.valid
    intIssUnit.io.wakeup_ports(i).bits  := resp.bits.uop.pdst

    regFile.io.write_ports(i).valid     := resp.valid && resp.bits.uop.ldst =/= 0.U
    regFile.io.write_ports(i).bits.addr := resp.bits.uop.pdst
    regFile.io.write_ports(i).bits.data := resp.bits.data

    rob.io.write(i)                               := resp
    rob.io.write(i).bits.uop.debug.timer_64_value := io.csr_access.cntvh ## io.csr_access.cntvl
  }
  rob.io.xcepInfo(0) := memExeUnit.io_mem_xcep
  rob.io.xcepInfo(1) := unqExeUnit.io_csr_xcep.get

  val aluStart = mem_csr_wb.length
  for (i <- 0 until aluExeUnits.length) {
    renameBusyTable.io.wbValids(i + aluStart) := aluExeUnits(i).io_alu_resp.valid
    renameBusyTable.io.wbPdsts(i + aluStart)  := aluExeUnits(i).io_alu_resp.bits.uop.pdst

    memIssUnit.io.wakeup_ports(i + aluStart).valid := aluExeUnits(i).io_alu_resp.valid
    memIssUnit.io.wakeup_ports(i + aluStart).bits  := aluExeUnits(i).io_alu_resp.bits.uop.pdst
    unqIssUnit.io.wakeup_ports(i + aluStart).valid := aluExeUnits(i).io_alu_resp.valid
    unqIssUnit.io.wakeup_ports(i + aluStart).bits  := aluExeUnits(i).io_alu_resp.bits.uop.pdst
    intIssUnit.io.wakeup_ports(i + aluStart).valid := aluExeUnits(i).io_alu_resp.valid
    intIssUnit.io.wakeup_ports(i + aluStart).bits  := aluExeUnits(i).io_alu_resp.bits.uop.pdst

    regFile.io.write_ports(i + aluStart).valid := aluExeUnits(i).io_alu_resp.valid &&
      aluExeUnits(i).io_alu_resp.bits.uop.ldst =/= 0.U
    regFile.io.write_ports(i + aluStart).bits.addr := aluExeUnits(i).io_alu_resp.bits.uop.pdst
    regFile.io.write_ports(i + aluStart).bits.data := aluExeUnits(i).io_alu_resp.bits.data

    rob.io.write(i + aluStart)                               := aluExeUnits(i).io_alu_resp
    rob.io.write(i + aluStart).bits.uop.debug.timer_64_value := io.csr_access.cntvh ## io.csr_access.cntvl

    rob.io.brInfo(i).uop            := aluExeUnits(i).io_alu_resp.bits.uop
    rob.io.brInfo(i).brRecoveryInfo := aluExeUnits(i).io_alu_brInfo
  }

  // commit
  for (i <- 0 until coreWidth) {
    renameMapTable.io.comRemapReqs(i).valid := rob.io.commit.valids(i) && rob.io.commit.uop(i).ldst =/= 0.U
    renameMapTable.io.comRemapReqs(i).ldst  := rob.io.commit.uop(i).ldst
    renameMapTable.io.comRemapReqs(i).pdst  := rob.io.commit.uop(i).pdst

    renameFreeList.io.dealloc(i).valid := rob.io.commit.valids(i) && rob.io.commit.uop(i).ldst =/= 0.U
    renameFreeList.io.dealloc(i).bits  := rob.io.commit.uop(i).stalePdst

    renameFreeList.io.despec(i).valid := rob.io.commit.valids(i) && rob.io.commit.uop(i).ldst =/= 0.U
    renameFreeList.io.despec(i).bits  := rob.io.commit.uop(i).pdst
  }

  io.ftqReqs(0)  := rob.io.ftqReq
  rob.io.ftqResp := io.ftqResps(0)

  val youngest_com_idx = (coreWidth - 1).U - PriorityEncoder(rob.io.commit.valids.reverse)
  io.commit.valid := rob.io.commit.valids.reduce(_ || _)
  io.commit.bits  := rob.io.commit.uop(youngest_com_idx).ftqIdx

  io.redirect := rob.io.redirect

  io.csr_access.epc       := rob.io.exception.epc
  io.csr_access.ecode     := rob.io.exception.ecode
  io.csr_access.ecode_sub := rob.io.exception.ecode_sub
  io.csr_access.badv      := rob.io.exception.badv
  io.csr_access.excp_en   := rob.io.exception.valid
  rob.io.intr_pending     := io.csr_access.intr_pending
  rob.io.eentry           := io.csr_access.eentry

  io.csr_access.eret_en := rob.io.ertn
  rob.io.era            := io.csr_access.era

  flush := rob.io.redirect.valid

  for (i <- 0 until backendParams.lregNum) {
    io.debug.regs(i) := regFile.io.debug(renameMapTable.io.debug(i))
  }
  io.debug.rob_commit    := rob.io.commit
  io.debug.rob_exception := rob.io.exception

  if (params.debug) {
    dontTouch(decData)
    dontTouch(decToRen)
    dontTouch(disData)
    dontTouch(rob.io)
    dontTouch(dispatcher.io)
    dontTouch(memIssUnit.io)
    dontTouch(unqIssUnit.io)
    dontTouch(intIssUnit.io)
    dontTouch(memExeUnit.io_mem_resp)
    dontTouch(memExeUnit.io_mem_xcep)
    dontTouch(unqExeUnit.io_mul_div_resp.get)
    dontTouch(unqExeUnit.io_csr_access.get)
    dontTouch(unqExeUnit.io_csr_resp.get)
    dontTouch(unqExeUnit.io_csr_xcep.get)
    aluExeUnits.foreach(unit => dontTouch(unit.io_alu_resp))
  }
}
