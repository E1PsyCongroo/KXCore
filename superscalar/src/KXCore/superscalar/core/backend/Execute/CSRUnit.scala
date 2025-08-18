package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.Privilege._
import KXCore.superscalar._
import KXCore.superscalar.core._
import ECODE._
import CSR._
import dataclass.data

class CSRIO(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams}
  import commonParams.{dataWidth, vaddrWidth}
  /* ------ CSR RW ------- */
  val raddr = Input(UInt(14.W))         // CSR address to read
  val rdata = Output(UInt(dataWidth.W)) // CSR read data

  val we    = Input(Bool())            // Write enable
  val waddr = Input(UInt(14.W))        // CSR address to write
  val wdata = Input(UInt(dataWidth.W)) // CSR write data
  val wmask = Input(UInt(dataWidth.W)) // CSR write mask
  /* ------ CSR RW ------- */

  /* ------ Global State ------ */
  val crmd  = Output(new CRMD) // Current privilege level
  val estat = Output(new ESTAT)
  val tlb = Output(new Bundle {
    val asid    = new ASID
    val dmw     = Vec(2, new DMW)
    val tlbidx  = new TLBIDX(log2Ceil(params.tlbNum))
    val tlbehi  = new TLBEHI
    val tlbelo0 = new TLBELO(params.commonParams.paddrWidth)
    val tlbelo1 = new TLBELO(params.commonParams.paddrWidth)
  })
  val counterID = Output(UInt(dataWidth.W))
  val cntvh     = Output(UInt(dataWidth.W))
  val cntvl     = Output(UInt(dataWidth.W))
  /* ------ Global State ------ */

  /* ------ TLB Command ------ */
  val tlbUpdate = Input(new Bundle {
    val cmd         = UInt(EXUType.getWidth.W)
    val new_asid    = UInt(dataWidth.W)
    val new_tlbidx  = UInt(dataWidth.W)
    val new_tlbehi  = UInt(dataWidth.W)
    val new_tlbelo0 = UInt(dataWidth.W)
    val new_tlbelo1 = UInt(dataWidth.W)
  })
  /* ------ TLB Command ------ */

  /* ------ ATOMIC Command ------ */
  val atomic = new Bundle {
    val llbit     = Output(Bool())
    val set_llbit = Input(Bool())
    val clr_llbit = Input(Bool())
  }
  /* ------ ATOMIC Command ------ */

  /* ------ Exception Enter ------ */
  val epc     = Input(UInt(vaddrWidth.W))     // Program counter for exception handling
  val ecode   = Input(UInt(ECODE.getWidth.W)) // Exception code
  val badv    = Input(UInt(vaddrWidth.W))     // Bad virtual address for exception
  val excp_en = Input(Bool())                 // Exception enable
  val eentry  = Output(UInt(32.W))            // Exception entry address
  /* ------ Exception Enter ------ */

  /* ------ Exception Return ------ */
  val eret_en = Input(Bool())              // Exception return enable
  val era     = Output(UInt(vaddrWidth.W)) // Exception return address
  /* ------ Exception Return ------ */

  /* ------ Interrupt Pending ------ */
  val interrupt = new Bundle {
    // val enable  = Output(Bool())     // Interrupt enable
    // val pending = Output(UInt(13.W)) // Interrupt pending
    val externel_sample = Input(UInt(8.W)) // External interrupt sample
    val pending         = Output(Bool())   // Interrupt pending. Is there any pending interrupt?
  }
  /* ------ Interrupt Pending ------ */

  /* ------ Debug Info ------ */
  /* ------ Debug Info ------ */
  val debug = Output(new Bundle {
    val crmd      = UInt(dataWidth.W)
    val prmd      = UInt(dataWidth.W)
    val euen      = UInt(dataWidth.W)
    val ecfg      = UInt(dataWidth.W)
    val estat     = UInt(dataWidth.W)
    val era       = UInt(dataWidth.W)
    val badv      = UInt(dataWidth.W)
    val eentry    = UInt(dataWidth.W)
    val tlbidx    = UInt(dataWidth.W)
    val tlbehi    = UInt(dataWidth.W)
    val tlbelo0   = UInt(dataWidth.W)
    val tlbelo1   = UInt(dataWidth.W)
    val asid      = UInt(dataWidth.W)
    val pgdl      = UInt(dataWidth.W)
    val pgdh      = UInt(dataWidth.W)
    val pgd       = UInt(dataWidth.W)
    val cpuid     = UInt(dataWidth.W)
    val saved0    = UInt(dataWidth.W)
    val saved1    = UInt(dataWidth.W)
    val saved2    = UInt(dataWidth.W)
    val saved3    = UInt(dataWidth.W)
    val tid       = UInt(dataWidth.W)
    val tcfg      = UInt(dataWidth.W)
    val tval      = UInt(dataWidth.W)
    val ticlr     = UInt(dataWidth.W)
    val llbctl    = UInt(dataWidth.W)
    val tlbrentry = UInt(dataWidth.W)
    val ctag      = UInt(dataWidth.W)
    val dmw0      = UInt(dataWidth.W)
    val dmw1      = UInt(dataWidth.W)
  })
}

class CSRUnit(implicit params: CoreParameters) extends Module {
  val io = IO(new CSRIO)

  val crmd      = RegInit(("b1000").U.asTypeOf(new CRMD)) // da=1
  val prmd      = RegInit(0.U.asTypeOf(new PRMD))
  val euen      = WireInit(0.U(32.W))
  val ecfg      = RegInit(0.U.asTypeOf(new ECFG))
  val estat     = RegInit(0.U.asTypeOf(new ESTAT))
  val era       = Reg(UInt(32.W))
  val badv      = Reg(UInt(32.W))
  val eentry    = Reg(new EENTRY)
  val tlbidx    = Reg(new TLBIDX(log2Ceil(params.tlbNum)))
  val tlbehi    = Reg(new TLBEHI)
  val tlbelo0   = Reg(new TLBELO(params.commonParams.paddrWidth))
  val tlbelo1   = Reg(new TLBELO(params.commonParams.paddrWidth))
  val asid      = RegInit(0xa0000.U.asTypeOf(new ASID))
  val pgdl      = RegInit(0.U(32.W))
  val pgdh      = RegInit(0.U(32.W))
  val pgd       = RegInit(0.U(32.W))
  val cpuid     = WireInit(0.U(32.W))
  val saved     = Reg(Vec(4, UInt(32.W)))
  val tid       = Reg(UInt(32.W))
  val tlbrentry = Reg(new TLBRENTRY)
  val ctag      = WireInit(0.U(32.W))
  val dmw       = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf((new DMW)))))

  val llbit = RegInit(false.B)
  val klo   = RegInit(false.B)

  /* ------ Global State ------ */
  io.crmd        := crmd
  io.estat       := estat
  io.tlb.asid    := asid // Dont care for now, can be set by TLB
  io.tlb.dmw     := dmw  // DMW0 register
  io.tlb.tlbehi  := tlbehi
  io.tlb.tlbidx  := tlbidx
  io.tlb.tlbelo0 := tlbelo0
  io.tlb.tlbelo1 := tlbelo1
  /* ------ Global State ------ */

  /* ------ Timer ------ */
  val timer = Module(new Timer)
  timer.io.waddr := io.waddr
  timer.io.wdata := io.wdata
  timer.io.wen   := io.we
  val timer_interrupt_pending = timer.io.pending
  /* ------ Timer ------ */

  /* ------ Stable Counter ------ */
  val stable_counter = Module(new StableCounter)
  // val cntid = stable_counter.io.id
  val cntvh = stable_counter.io.high
  val cntvl = stable_counter.io.low
  /* ------ Stable Counter ------ */

  /* ------ Exception Enter ------ */
  val excp_crmd = Mux(
    io.ecode === ECODE.TLBR.asUInt,
    crmd.set_ie(0.B).set_plv(0.U(2.W)).set_da(1.B).set_pg(0.B),
    crmd.set_ie(0.B).set_plv(0.U(2.W)),
  )
  val excp_prmd  = prmd.set_pie(crmd.ie).set_pplv(crmd.plv)
  val excp_estat = estat.set_ecode(ECODE.getEcode(io.ecode)).set_sub_ecode(ECODE.getEsubCode(io.ecode))

  io.eentry := Mux(io.ecode === ECODE.TLBR.asUInt, tlbrentry.value, eentry.value)
  /* ------ Exception Enter ------ */

  /* ------ Exception Return ------ */
  val eret_crmd = Mux(
    (estat.sub_ecode ## estat.ecode) === ECODE.TLBR.asUInt,
    crmd.set_ie(prmd.pie).set_plv(prmd.pplv).set_da(0.B).set_pg(1.B),
    crmd.set_ie(prmd.pie).set_plv(prmd.pplv),
  )

  io.era := era
  /* ------ Exception Return ------ */

  /* ------ Interrupt Pending ------ */
  io.interrupt.pending := (estat.is & ecfg.lie).orR & crmd.ie
  /* ------ Interrupt Pending ------ */

  /* ------ atomic ------ */
  io.atomic.llbit := llbit
  when(io.atomic.set_llbit) {
    llbit := true.B
  }.elsewhen(io.atomic.clr_llbit) {
    llbit := false.B
  }
  /* ------ atomic ------ */

  /* ------ Write Logic ------ */
  when(io.excp_en) {
    crmd         := excp_crmd
    prmd         := excp_prmd
    estat        := excp_estat
    badv         := Mux(Seq(TLBR, ADEF, ALE, PIL, PIS, PIF, PME, PPI).map(e => ECODE.getEcode(e.asUInt) === io.ecode).reduce(_ || _), io.badv, badv)
    era          := io.epc
    tlbehi.value := Mux(Seq(PIL, PIS, PIF, PME, PPI, TLBR).map(e => ECODE.getEcode(e.asUInt) === io.ecode).reduce(_ || _), tlbehi.write(io.badv), tlbehi.value)
  }.elsewhen(io.eret_en) {
    crmd  := eret_crmd
    klo   := false.B
    llbit := Mux(klo, llbit, false.B)
    // prmd := eret_prmd
  }.elsewhen(io.tlbUpdate.cmd === EXUType.EXU_TLBSRCH.asUInt) {
    tlbidx.value := tlbidx.write(io.tlbUpdate.new_tlbidx)
  }.elsewhen(io.tlbUpdate.cmd === EXUType.EXU_TLBRD.asUInt) {
    asid.value    := asid.write(io.tlbUpdate.new_asid)
    tlbidx.value  := tlbidx.write(io.tlbUpdate.new_tlbidx)
    tlbehi.value  := tlbehi.write(io.tlbUpdate.new_tlbehi)
    tlbelo0.value := tlbelo0.write(io.tlbUpdate.new_tlbelo0)
    tlbelo1.value := tlbelo1.write(io.tlbUpdate.new_tlbelo1)
  }.elsewhen(io.we) {
    val wdata = io.wdata & io.wmask

    for (i <- 0 until 4) {
      saved(i) := Mux(io.waddr === (CSRAddr.SAVED0 + i).U, wdata | (saved(i) & ~io.wmask), saved(i))
    }

    for (i <- 0 until 2) {
      dmw(i).value := Mux(io.waddr === (CSRAddr.DMW0 + i).U, dmw(i).write(wdata | (dmw(i).value & ~io.wmask)), dmw(i).value)
    }

    crmd.value := Mux(io.waddr === CSRAddr.CRMD.U, crmd.write(wdata | (crmd.value & ~io.wmask)), crmd.value)
    prmd.value := Mux(io.waddr === CSRAddr.PRMD.U, prmd.write(wdata | (prmd.value & ~io.wmask)), prmd.value)

    ecfg.value := Mux(io.waddr === CSRAddr.ECFG.U, ecfg.write(wdata | (ecfg.value & !io.wmask)), ecfg.value)
    estat := Mux(
      io.waddr === CSRAddr.ESTAT.U,
      estat.write(wdata | (estat.value & !io.wmask)),
      estat.value,
    ).asTypeOf(new ESTAT).set_sample(io.interrupt.externel_sample).set_tis(timer_interrupt_pending)
    era             := Mux(io.waddr === CSRAddr.ERA.U, wdata | (era & !io.wmask), era)
    badv            := Mux(io.waddr === CSRAddr.BADV.U, wdata | (badv & !io.wmask), badv)
    eentry.value    := Mux(io.waddr === CSRAddr.EENTRY.U, eentry.write(wdata | (eentry.value & !io.wmask)), eentry.value)
    tlbrentry.value := Mux(io.waddr === CSRAddr.TLBRENTRY.U, tlbrentry.write(wdata | (tlbrentry.value & !io.wmask)), tlbrentry.value)

    asid.value    := Mux(io.waddr === CSRAddr.ASID.U, asid.write(wdata | (asid.value & ~io.wmask)), asid.value)
    tlbidx.value  := Mux(io.waddr === CSRAddr.TLBIDX.U, tlbidx.write(wdata | (tlbidx.value & ~io.wmask)), tlbidx.value)
    tlbehi.value  := Mux(io.waddr === CSRAddr.TLBEHI.U, tlbehi.write(wdata | (tlbehi.value & ~io.wmask)), tlbehi.value)
    tlbelo0.value := Mux(io.waddr === CSRAddr.TLBELO0.U, tlbelo0.write(wdata | (tlbelo0.value & ~io.wmask)), tlbelo0.value)
    tlbelo1.value := Mux(io.waddr === CSRAddr.TLBELO1.U, tlbelo1.write(wdata | (tlbelo1.value & ~io.wmask)), tlbelo1.value)

    tid := Mux(io.waddr === CSRAddr.TID.U, wdata | (tid & ~io.wmask), tid)

    klo   := Mux(io.waddr === CSRAddr.LLBCTL.U, wdata(2) && io.wmask(2), klo)
    llbit := Mux(io.waddr === CSRAddr.LLBCTL.U && wdata(1) && io.wmask(1), false.B, llbit)

    pgdh := Mux(io.waddr === CSRAddr.PGDH.U, wdata | (tlbelo0.value & ~io.wmask) & ~0xfff.U, pgdh)
    pgdl := Mux(io.waddr === CSRAddr.PGDL.U, wdata | (tlbelo0.value & ~io.wmask) & ~0xfff.U, pgdl)
    pgd  := Mux(io.waddr === CSRAddr.PGD.U, wdata | (tlbelo0.value & ~io.wmask) & ~0xfff.U, pgd)
  }.otherwise {
    estat := estat.set_sample(io.interrupt.externel_sample).set_tis(timer_interrupt_pending)
  }
  /* ------ Write Logic ------ */

  /* ------ Read Logic ------ */
  io.rdata := MuxLookup(io.raddr, 0.U(32.W))(
    Seq(
      CSRAddr.CRMD.U      -> crmd.value,
      CSRAddr.PRMD.U      -> prmd.value,
      CSRAddr.EUEN.U      -> euen,
      CSRAddr.ECFG.U      -> ecfg.value,
      CSRAddr.ESTAT.U     -> estat.value,
      CSRAddr.ERA.U       -> era,
      CSRAddr.BADV.U      -> badv,
      CSRAddr.EENTRY.U    -> eentry.value,
      CSRAddr.TLBIDX.U    -> tlbidx.value,
      CSRAddr.TLBEHI.U    -> tlbehi.value,
      CSRAddr.TLBELO0.U   -> tlbelo0.value,
      CSRAddr.TLBELO1.U   -> tlbelo1.value,
      CSRAddr.ASID.U      -> asid.value,
      CSRAddr.PGDL.U      -> pgdl,
      CSRAddr.PGDH.U      -> pgdh,
      CSRAddr.CPUID.U     -> cpuid,
      CSRAddr.SAVED0.U    -> saved(0),
      CSRAddr.SAVED1.U    -> saved(1),
      CSRAddr.SAVED2.U    -> saved(2),
      CSRAddr.SAVED3.U    -> saved(3),
      CSRAddr.TID.U       -> tid,
      CSRAddr.TCFG.U      -> timer.io.tcfg,
      CSRAddr.TVAL.U      -> timer.io.tval,
      CSRAddr.TICLR.U     -> 0.U,
      CSRAddr.TLBRENTRY.U -> tlbrentry.value,
      CSRAddr.CTAG.U      -> ctag,
      CSRAddr.LLBCTL.U    -> (klo ## 0.B ## llbit),
      CSRAddr.DMW0.U      -> dmw(0).value,
      CSRAddr.DMW1.U      -> dmw(1).value,
    ),
  )
  io.counterID := tid
  io.cntvh     := cntvh
  io.cntvl     := cntvl
  /* ------ Read Logic ------ */

  /* ------ Debug Info ------ */
  io.debug.crmd      := crmd.value
  io.debug.prmd      := prmd.value
  io.debug.euen      := euen
  io.debug.ecfg      := ecfg.value
  io.debug.estat     := estat.value
  io.debug.era       := era
  io.debug.badv      := badv
  io.debug.eentry    := eentry.value
  io.debug.tlbidx    := tlbidx.value
  io.debug.tlbehi    := tlbehi.value
  io.debug.tlbelo0   := tlbelo0.value
  io.debug.tlbelo1   := tlbelo1.value
  io.debug.asid      := asid.value
  io.debug.pgdl      := pgdl
  io.debug.pgdh      := pgdh
  io.debug.pgd       := pgd
  io.debug.cpuid     := cpuid
  io.debug.saved0    := saved(0)
  io.debug.saved1    := saved(1)
  io.debug.saved2    := saved(2)
  io.debug.saved3    := saved(3)
  io.debug.tid       := tid
  io.debug.tcfg      := timer.io.tcfg
  io.debug.tval      := timer.io.tval
  io.debug.ticlr     := 0.U
  io.debug.llbctl    := (klo ## 0.B ## llbit)
  io.debug.tlbrentry := tlbrentry.value
  io.debug.ctag      := ctag
  io.debug.dmw0      := dmw(0).value
  io.debug.dmw1      := dmw(1).value
  /* ------ Debug Info ------ */
}
