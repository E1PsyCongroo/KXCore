package KXCore.superscalar.core

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.Privilege._
import KXCore.superscalar._
import ECODE._
import CSR._

class TLBReq(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams}
  import commonParams.{vaddrWidth}

  /** request address from CPU. */
  val vaddr = UInt(vaddrWidth.W)

  // /* address space identifier */
  // val asid = UInt(10.W)

  // /* privilege level */
  // val plv = UInt(2.W)

  /* memory access type */
  val isWrite = Bool()
}

class TLBResp(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams}
  import commonParams.{paddrWidth}

  /* memory access type */
  val mat = UInt(2.W)

  /** physical address */
  val paddr = UInt(paddrWidth.W)

  val exception = Valid(UInt(ECODE.getWidth.W))
}

class TLBTranslateItem(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams}
  import commonParams.{paddrWidth}

  val ppn   = UInt((paddrWidth - 12).W) // physical page number
  val plv   = UInt(2.W)                 // privilege level
  val mat   = UInt(2.W)                 // memory access type
  val dirty = Bool()                    // dirty bit
  val valid = Bool()                    // valid bit
}

class TLBEntry(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams}
  import commonParams.{vaddrWidth, paddrWidth}
  private val vppnWidth = vaddrWidth - 13
  private val ppnWidth  = paddrWidth - 12

  val vppn = UInt(vppnWidth.W) // virtual page number
  // val ps     = UInt(6.W) // page size
  val ps     = Bool()     // page size, 0 for 4K, 1 for 2M
  val global = Bool()     // global bit
  val asid   = UInt(10.W) // address space identifier
  val item   = Vec(2, new TLBTranslateItem)
  val e      = Bool()

  def page_size(): UInt = {
    Mux(ps, 21.U, 12.U) // 2M or 4K
  }
}

class TLBCmdIO(implicit params: CoreParameters) extends Bundle {
  val cmd   = UInt(EXUType.getWidth.W)
  val invOp = UInt(5.W) // TLB invalidate operation
}

class TLB(implicit params: CoreParameters) extends Module {
  import params.{tlbNum, commonParams}
  import commonParams.{vaddrWidth, paddrWidth, instBytes, dataWidth}
  private val vppnWidth = vaddrWidth - 13
  private val ppnWidth  = paddrWidth - 12

  val io = IO(new Bundle {
    val mode = Input(new Bundle {
      val crmd    = new CRMD // Current privilege level
      val estat   = new ESTAT
      val asid    = new ASID
      val dmw     = Vec(2, new DMW)
      val tlbidx  = new TLBIDX(log2Ceil(params.tlbNum))
      val tlbehi  = new TLBEHI
      val tlbelo0 = new TLBELO(params.commonParams.paddrWidth)
      val tlbelo1 = new TLBELO(params.commonParams.paddrWidth)
    })

    val tlbCmd = Input(new TLBCmdIO)

    val tlbUpdate = Output(new Bundle {
      val cmd         = UInt(EXUType.getWidth.W) // TLB command
      val new_asid    = UInt(dataWidth.W)
      val new_tlbidx  = UInt(dataWidth.W)
      val new_tlbehi  = UInt(dataWidth.W)
      val new_tlbelo0 = UInt(dataWidth.W)
      val new_tlbelo1 = UInt(dataWidth.W)
    })

    val transReq0  = Input(new TLBReq) // MUST BE USED FOR FETCH
    val transReq1  = Input(new TLBReq) // MUST BE USED FOR LOAD/STORE
    val transResp0 = Output(new TLBResp)
    val transResp1 = Output(new TLBResp)
  })

  val tlbEntry = Reg(Vec(tlbNum, new TLBEntry))

  def tlb_translate(req: TLBReq, is_fetch: Boolean): TLBResp = {
    val vaddr = req.vaddr

    val dmw_hits = Wire(Vec(2, Bool()))
    for (i <- 0 until 2) {
      val dmw = io.mode.dmw(i)
      dmw_hits(i) := (dmw.plv0 && io.mode.crmd.plv === 0.U) || (dmw.plv3 && io.mode.crmd.plv === 3.U) ||
        (dmw.vseg === req.vaddr(31, 29))
    }
    val dmw_hit = dmw_hits.asUInt.orR
    val dmw_paddr = MuxLookup(dmw_hits.asUInt, 0.U)(
      Seq(
        1.U -> Cat(io.mode.dmw(0).pseg, vaddr(28, 0)), // DMW0
        3.U -> Cat(io.mode.dmw(1).pseg, vaddr(28, 0)), // DMW1
      ),
    )
    val dmw_mat = MuxLookup(dmw_hits.asUInt, 0.U)(
      Seq(
        1.U -> io.mode.dmw(0).mat, // DMW0
        3.U -> io.mode.dmw(1).mat, // DMW1
      ),
    )

    val tlb_hits = Wire(Vec(tlbNum, Bool()))
    for (i <- 0 until tlbNum) {
      val entry = tlbEntry(i)
      tlb_hits(i) := (entry.vppn === req.vaddr(vaddrWidth - 1, 13)) &&
        ((entry.asid === io.mode.asid.asid) || entry.global) &&
        (entry.e)
    }
    val isHit = tlb_hits.asUInt.orR

    val hitIndex = PriorityEncoder(tlb_hits)
    val hitEntry = tlbEntry(hitIndex)

    val found = Wire(new TLBTranslateItem)
    found := Mux(vaddr(hitEntry.page_size()) === 0.U, hitEntry.item(0), hitEntry.item(1))
    val found_ps = hitEntry.ps
    val tlb_paddr = Mux(
      found_ps,
      Cat(found.ppn(paddrWidth - 13, 9), vaddr(8, 0)), // 2M page
      Cat(found.ppn(paddrWidth - 13, 0), vaddr(11, 0)),// 4K page
    )

    val tlb_exception_valid = !isHit || !found.valid || io.mode.crmd.plv > found.plv ||
      (req.isWrite && found.dirty === 0.U) || (is_fetch.B && vaddr(log2Ceil(instBytes), 0) =/= 0.U)
    val tlb_exception_ecode =
      Mux(
        !isHit,
        ECODE.TLBR,
        Mux(
          !found.valid,
          if (is_fetch) ECODE.PIF else Mux(req.isWrite, ECODE.PIS, ECODE.PIL),
          Mux(io.mode.crmd.plv > found.plv, ECODE.PPI, Mux(req.isWrite && found.dirty === 0.U, ECODE.PME, DontCare)),
        ),
      ).asUInt

    val resp = Wire(new TLBResp)
    resp.exception.valid := Mux(dmw_hit, false.B, !isHit)
    resp.exception.bits  := Mux(dmw_hit, 0.U, tlb_exception_ecode)
    resp.mat             := Mux(dmw_hit, dmw_mat, found.mat)
    resp.paddr           := Mux(dmw_hit, dmw_paddr, tlb_paddr)

    resp
  }

  def tlb_translate_direct(req: TLBReq, mat: UInt, is_fetch: Boolean): TLBResp = {
    val resp = Wire(new TLBResp)
    resp.exception.valid := 0.U
    resp.exception.bits  := DontCare
    resp.mat             := mat
    resp.paddr           := req.vaddr // passthrough vaddr as paddr for now
    resp
  }

  dontTouch(io.transReq0.vaddr)

  io.transResp0 := Mux(io.mode.crmd.da && !io.mode.crmd.pg, tlb_translate_direct(io.transReq0, io.mode.crmd.datf, true), tlb_translate(io.transReq0, true))
  io.transResp1 := Mux(io.mode.crmd.da && !io.mode.crmd.pg, tlb_translate_direct(io.transReq1, io.mode.crmd.datm, false), tlb_translate(io.transReq1, false))

  /* ------ TLBSRCH ------ */
  val srch_vppn = io.mode.tlbehi.vppn
  val srch_hits = Wire(Vec(tlbNum, Bool()))
  for (i <- 0 until tlbNum) {
    val entry = tlbEntry(i)
    srch_hits(i) := (entry.vppn === srch_vppn) &&
      (entry.asid === io.mode.asid.asid)
  }
  val tlbsrch_hit    = srch_hits.reduce(_ || _)
  val tlbsrch_index  = PriorityEncoder(srch_hits)
  val tlbsrch_tlbidx = !tlbsrch_hit ## io.mode.tlbidx.value(30, log2Ceil(tlbNum)) ## Mux(tlbsrch_hit, io.mode.tlbidx.index, tlbsrch_index)
  /* ------ TLBSRCH ------ */

  /* ------ TLBRD ------ */
  val tlbrd_entry  = tlbEntry(io.mode.tlbidx.index)
  val tlbrd_tlbehi = Mux(tlbrd_entry.e, 0.U, Cat(tlbrd_entry.vppn, 0.U(12.W)))
  val tlbrd_tlbrlo0 = Mux(
    tlbrd_entry.e,
    0.U,
    Cat(
      0.U((36 - paddrWidth).W),
      tlbrd_entry.item(0).ppn,
      0.U(1.W),
      tlbrd_entry.global,
      tlbrd_entry.item(0).mat,
      tlbrd_entry.item(0).plv,
      tlbrd_entry.item(0).dirty,
      tlbrd_entry.item(0).valid,
    ),
  )
  val tlbrd_tlbrlo1 = Mux(
    tlbrd_entry.e,
    0.U,
    Cat(
      0.U((36 - paddrWidth).W),
      tlbrd_entry.item(1).ppn,
      0.U(1.W),
      tlbrd_entry.global,
      tlbrd_entry.item(1).mat,
      tlbrd_entry.item(1).plv,
      tlbrd_entry.item(1).dirty,
      tlbrd_entry.item(1).valid,
    ),
  )
  val tlbrd_tlbidx = Mux(
    tlbrd_entry.e,
    0.B ## io.mode.tlbidx.value(30, 0),
    1.B ## io.mode.tlbidx.value(30, 0),
  )
  val tlbrd_asid = Mux(tlbrd_entry.e, 0.U, tlbrd_entry.asid)
  /* ------ TLBRD ------ */

  /* ------ TLBWR ------ */
  val tlbwr_entry_e = io.mode.estat.ecode === ECODE.getEcode(ECODE.TLBR.asUInt) || io.mode.tlbidx.ne
  val tlbwr_index   = io.mode.tlbidx.index
  val cmd_is_tlbwr  = io.tlbCmd.cmd === EXUType.EXU_TLBWR.asUInt
  /* ------ TLBWR ------ */

  /* ------ TLBINV ------ */

  /* ------ TLBINV ------ */

  /* ------ WRITE TLB ------ */
  val cmd_inv  = io.tlbCmd.cmd === EXUType.EXU_INVTLB.asUInt
  val cmd_fill = io.tlbCmd.cmd === EXUType.EXU_TLBFILL.asUInt
  val cmd_wr   = io.tlbCmd.cmd === EXUType.EXU_TLBWR.asUInt

  val fill_entry = Wire(new TLBEntry)
  fill_entry.e             := true.B
  fill_entry.vppn          := io.mode.tlbehi.vppn
  fill_entry.ps            := io.mode.tlbidx.ps === 21.U // 2M page
  fill_entry.global        := io.mode.tlbelo0.g && io.mode.tlbelo1.g
  fill_entry.asid          := io.mode.asid.asid
  fill_entry.item(0).ppn   := io.mode.tlbelo0.ppn
  fill_entry.item(0).plv   := io.mode.tlbelo0.plv
  fill_entry.item(0).mat   := io.mode.tlbelo0.mat
  fill_entry.item(0).dirty := io.mode.tlbelo0.d
  fill_entry.item(0).valid := io.mode.tlbelo0.v
  fill_entry.item(1).ppn   := io.mode.tlbelo1.ppn
  fill_entry.item(1).plv   := io.mode.tlbelo1.plv
  fill_entry.item(1).mat   := io.mode.tlbelo1.mat
  fill_entry.item(1).dirty := io.mode.tlbelo1.d
  fill_entry.item(1).valid := io.mode.tlbelo1.v

  when(cmd_inv) {
    for (i <- 0 until tlbNum) {
      val en = MuxLookup(io.tlbCmd.invOp, false.B)(
        Seq(
          0.U -> true.B, // invalidate all entries
          1.U -> true.B,
          2.U -> tlbEntry(i).global,
          3.U -> !tlbEntry(i).global,
          4.U -> (!tlbEntry(i).global && (tlbEntry(i).asid === io.mode.asid.asid)),
          5.U -> (!tlbEntry(i).global && (tlbEntry(i).asid === io.mode.asid.asid) && (tlbEntry(i).vppn === io.mode.tlbehi.vppn)),
          6.U -> ((tlbEntry(i).global || (tlbEntry(i).asid === io.mode.asid.asid)) && (tlbEntry(i).vppn === io.mode.tlbehi.vppn)),
        ),
      )
      tlbEntry(i).e := false.B
    }
  }.elsewhen(cmd_fill) {
    tlbEntry(0) := fill_entry // for now, only fill the first entry
  }.elsewhen(cmd_wr) {
    tlbEntry(tlbwr_index)   := fill_entry
    tlbEntry(tlbwr_index).e := tlbwr_entry_e
  }
  // for (i <- 0 until params.tlbNum) {
  // val en = tlbwr_index === i.U && cmd_is_tlbwr
  // tlbEntry(i).asid := Mux(en, io.cmd_in.tlb_asid(9, 0), tlbEntry(i).asid)
  // tlbEntry(i).vppn := Mux(en, io.cmd_in.tlb_ehi(31, 13), tlbEntry(i).vppn)
  // tlbEntry(i).ps := Mux(en, io.cmd_in.tlb_idx(29, 24) === 21.U, tlbEntry(i).ps)
  // tlbEntry(i).global := Mux(en, io.cmd_in.tlb_elo0(6) && io.cmd_in.tlb_elo1(6), tlbEntry(i).global)
  // tlbEntry(i).item(0).ppn := Mux(en, io.cmd_in.tlb_elo0(params.paddrWidth - 5, 8), tlbEntry(i).item(0).ppn)
  // tlbEntry(i).item(0).plv := Mux(en, io.cmd_in.tlb_elo0(3, 2), tlbEntry(i).item(0).plv)
  // tlbEntry(i).item(0).mat := Mux(en, io.cmd_in.tlb_elo0(5, 4), tlbEntry(i).item(0).mat)
  // tlbEntry(i).item(0).dirty := Mux(en, io.cmd_in.tlb_elo0(1), tlbEntry(i).item(0).dirty)
  // tlbEntry(i).item(0).valid := Mux(en, io.cmd_in.tlb_elo0(0), tlbEntry(i).item(0).valid)
  // tlbEntry(i).item(1).ppn := Mux(en, io.cmd_in.tlb_elo1(params.paddrWidth - 5, 8), tlbEntry(i).item(1).ppn)
  // tlbEntry(i).item(1).plv := Mux(en, io.cmd_in.tlb_elo1(3, 2), tlbEntry(i).item(1).plv)
  // tlbEntry(i).item(1).mat := Mux(en, io.cmd_in.tlb_elo1(5, 4), tlbEntry(i).item(1).mat)
  // tlbEntry(i).item(1).dirty := Mux(en, io.cmd_in.tlb_elo1(1), tlbEntry(i).item(1).dirty)
  // tlbEntry(i).item(1).valid := Mux(en, io.cmd_in.tlb_elo1(0), tlbEntry(i).item(1).valid)
  // tlbEntry(i).e := Mux(en, tlbwr_entry_e, tlbEntry(i).e)
  // }

  /* ------ OUTPUT CSR ------ */

  io.tlbUpdate.cmd := io.tlbCmd.cmd
  io.tlbUpdate.new_asid := MuxLookup(io.tlbCmd.cmd, io.mode.asid.value)(
    Seq(
      EXUType.EXU_TLBRD.asUInt -> tlbrd_asid,
    ),
  );
  io.tlbUpdate.new_tlbidx := MuxLookup(io.tlbCmd.cmd, io.mode.tlbidx.value)(
    Seq(
      EXUType.EXU_TLBSRCH.asUInt -> tlbsrch_tlbidx,
      EXUType.EXU_TLBRD.asUInt   -> tlbrd_tlbidx,
    ),
  );

  io.tlbUpdate.new_tlbehi := MuxLookup(io.tlbCmd.cmd, io.mode.tlbehi.value)(
    Seq(
      EXUType.EXU_TLBSRCH.asUInt -> Cat(srch_vppn, 0.U(12.W)),
      EXUType.EXU_TLBRD.asUInt   -> tlbrd_tlbehi,
    ),
  );

  io.tlbUpdate.new_tlbelo0 := MuxLookup(io.tlbCmd.cmd, io.mode.tlbelo0.value)(
    Seq(
      EXUType.EXU_TLBSRCH.asUInt -> tlbrd_tlbrlo0,
    ),
  );

  io.tlbUpdate.new_tlbelo1 := MuxLookup(io.tlbCmd.cmd, io.mode.tlbelo1.value)(
    Seq(
      EXUType.EXU_TLBRD.asUInt -> tlbrd_tlbrlo1,
    ),
  );

  /* ------ OUTPUT CSR ------ */

  // io.transResp.miss  := false.B           // TLB miss, always false for now
  // io.transResp.paddr := io.transReq.vaddr // passthrough vaddr as paddr for now

  // assert(io.mode.da && !io.mode.pg, "TLB mode must be da for now")
}
