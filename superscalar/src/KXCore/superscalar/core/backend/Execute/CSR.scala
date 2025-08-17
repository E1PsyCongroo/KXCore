package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.peripheral._
import KXCore.common.Privilege._
import KXCore.common.Privilege.ECODE._
import KXCore.superscalar._

class CRMD extends Bundle {
  val value = UInt(32.W)

  def plv()  = value(1, 0) // Privilege level
  def ie()   = value(2)    // Interrupt enable
  def da()   = value(3)    // Debug active
  def pg()   = value(4)    // Modify privilege
  def datf() = value(6, 5) // Data address translation fault
  def datm() = value(8, 7) // Data address translation mode

  def write(value: UInt): UInt = {
    value & 0x1ff.U(32.W)
  }

  def set_da(da: Bool): CRMD = {
    val write_mask = 0x8.U(32.W)
    ((value & ~write_mask) | (da.asUInt << 3)).asTypeOf(this)
  }

  def set_plv(plv: UInt): CRMD = {
    val write_mask = 0x3.U(32.W)
    ((value & ~write_mask) | (plv & write_mask)).asTypeOf(this)
  }

  def set_ie(ie: Bool): CRMD = {
    val write_mask = 0x4.U(32.W)
    ((value & ~write_mask) | (ie.asUInt << 2)).asTypeOf(this)
  }
}

class PRMD extends Bundle {
  val value = UInt(32.W)

  def pplv() = value(1, 0) // Privilege level
  def pie()  = value(2)    // Interrupt enable

  def write(value: UInt): UInt = {
    value & 0x3.U(32.W)
  }

  def set_pplv(pplv: UInt): PRMD = {
    val write_mask = 0x3.U(32.W)
    ((value & ~write_mask) | (pplv & write_mask)).asTypeOf(this)
  }

  def set_pie(pie: Bool): PRMD = {
    val write_mask = 0x4.U(32.W)
    ((value & ~write_mask) | (pie.asUInt << 2)).asTypeOf(this)
  }
}

class ESTAT extends Bundle {
  val value = UInt(32.W)

  def is() = value(12, 0)

  def ecode()     = value(21, 16)
  def sub_ecode() = value(30, 22)

  def write(value: UInt): UInt = {
    val write_mask = 0x3.U(32.W)
    (value & write_mask) | (this.value & ~write_mask)
  }

  def set_sample(sample: UInt): ESTAT = {
    val write_mask = 0xff.U(32.W) << 2
    ((value & ~write_mask) | (sample << 2)).asTypeOf(this)
  }

  def set_tis(tis: Bool): ESTAT = {
    val write_mask = 0x1.U(32.W) << 11
    ((value & ~write_mask) | (tis.asUInt << 11)).asTypeOf(this)
  }

  def set_ecode(ecode: UInt): ESTAT = {
    val write_mask = 0x3f.U(32.W) << 16
    ((value & ~write_mask) | (ecode << 16)).asTypeOf(this)
  }

  def set_sub_ecode(sub_ecode: UInt): ESTAT = {
    val write_mask = 0x1ff.U(32.W) << 22
    ((value & ~write_mask) | (sub_ecode << 22)).asTypeOf(this)
  }
}

class EENTRY extends Bundle {
  val value = UInt(32.W)

  def eentry() = value(31, 0) // Exception entry address

  def write(value: UInt): UInt = {
    value & ~0x1f.U(32.W)
  }
}

class ECFG extends Bundle {
  val value = UInt(32.W)

  def lie() = value(12, 0)

  def write(value: UInt): UInt = {
    val write_mask = 0x3ff.U | (0x3.U << 11)
    value & write_mask
  }
}

class TLBRENTRY extends Bundle {
  val value = UInt(32.W)

  def write(value: UInt): UInt = {
    value & ~0x1f.U(32.W)
  }
}

class Timer extends Module {
  val io = IO(new Bundle {
    val waddr = Input(UInt(12.W))
    val wdata = Input(UInt(32.W))
    val wen   = Input(Bool())

    val tcfg = Output(UInt(32.W))
    val tval = Output(UInt(32.W))

    val pending = Output(Bool())
  })

  val en        = RegInit(false.B)
  val periodic  = RegInit(false.B)
  val initvalue = RegInit(0.U(32.W))
  val pending   = RegInit(false.B)
  val tval      = RegInit(0.U(32.W))

  val write_tfcg = io.waddr === CSRAddr.TCFG.U && io.wen

  en       := Mux(write_tfcg, io.wdata(0), en)
  periodic := Mux(write_tfcg, io.wdata(1), periodic)

  val tcfg_initvalue = Cat(io.wdata(31, 2), 0.U(2.W))
  initvalue := Mux(write_tfcg, tcfg_initvalue, initvalue)

  tval := Mux(
    write_tfcg,
    tcfg_initvalue,
    Mux(
      en,
      Mux(tval === 0.U, Mux(periodic, initvalue, tval), tval - 1.U),
      tval,
    ),
  )

  val write_ticlr = io.waddr === CSRAddr.TICLR.U && io.wen
  // pending := Mux(
  //   write_ticlr && io.wdata(0),
  //   false.B,
  //   Mux(en && (tval === 1.U || initvalue === 0.U), true.B, pending),
  // )
  pending := MuxCase(
    false.B,
    Seq(
      (write_tfcg && io.wdata(0) && tcfg_initvalue === 0.U)     -> true.B,
      (en && (tval === 1.U || (initvalue === 0.U && periodic))) -> true.B,
      (pending && !(write_ticlr && io.wdata(0)))                -> true.B,
    ),
  )

  io.pending := pending
  io.tval    := tval
  io.tcfg    := Cat(initvalue(31, 2), periodic, en)
}

object CSRAddr {
  val CRMD    = 0x000
  val PRMD    = 0x001
  val ECFG    = 0x004
  val ESTAT   = 0x005
  val ERA     = 0x006
  val BADV    = 0x007
  val EENTRY  = 0x00c
  val TLBIDX  = 0x010
  val TLBEHI  = 0x011
  val TLBELO0 = 0x012
  val TLBELO1 = 0x013
  val ASID    = 0x018
  val SAVED0  = 0x030
  val TID     = 0x040
  val TCFG    = 0x041
  val TVAL    = 0x042
  val TICLR   = 0x044
  val LLBTL   = 0x060
  val TLBRE   = 0x088
  val DMW0    = 0x180
  val DMW1    = 0x181
}

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
  val priv = Output(UInt(PLV.getWidth.W)) // Current privilege level
  val tlb = new Bundle {
    val da   = Output(Bool())
    val pg   = Output(Bool())
    val matf = Output(UInt(2.W))       // Memory access type for fetch
    val matd = Output(UInt(2.W))       // Memory access type for data
    val asid = Output(UInt(10.W))      // Address space ID
    val dmw  = Output(Vec(2, new DMW)) // DMW register
  }
  val counterID = Output(UInt(dataWidth.W))
  val cntvh     = Output(UInt(dataWidth.W))
  val cntvl     = Output(UInt(dataWidth.W))
  /* ------ Global State ------ */

  /* ------ TLB Command ------ */
  val tlb_cmd = Input(new TLBCmd)
  /* ------ TLB Command ------ */

  /* ------ Exception Enter ------ */
  val epc       = Input(UInt(vaddrWidth.W)) // Program counter for exception handling
  val ecode     = Input(UInt(6.W))          // Exception code
  val ecode_sub = Input(UInt(9.W))
  val badv      = Input(UInt(vaddrWidth.W)) // Bad virtual address for exception
  val excp_en   = Input(Bool())             // Exception enable
  val eentry    = Output(UInt(32.W))        // Exception entry address
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
    val ecfg      = UInt(dataWidth.W)
    val estat     = UInt(dataWidth.W)
    val era       = UInt(dataWidth.W)
    val badv      = UInt(dataWidth.W)
    val eentry    = UInt(dataWidth.W)
    val saved0    = UInt(dataWidth.W)
    val saved1    = UInt(dataWidth.W)
    val saved2    = UInt(dataWidth.W)
    val saved3    = UInt(dataWidth.W)
    val tid       = UInt(dataWidth.W)
    val tcfg      = UInt(dataWidth.W)
    val tval      = UInt(dataWidth.W)
    val tlbrentry = UInt(dataWidth.W)
    val tlbidx    = UInt(dataWidth.W)
    val tlbehi    = UInt(dataWidth.W)
    val tlbelo0   = UInt(dataWidth.W)
    val tlbelo1   = UInt(dataWidth.W)
    val asid      = UInt(dataWidth.W)
    val dmw0      = UInt(dataWidth.W)
    val dmw1      = UInt(dataWidth.W)
  })
}

class CSR(implicit params: CoreParameters) extends Module {
  val io = IO(new CSRIO)

  val crmd      = RegInit(("b1000").U.asTypeOf(new CRMD)) // da=1
  val prmd      = RegInit(0.U.asTypeOf(new PRMD))
  val ecfg      = RegInit(0.U.asTypeOf(new ECFG))
  val estat     = RegInit(0.U.asTypeOf(new ESTAT))
  val era       = Reg(UInt(32.W))
  val badv      = Reg(UInt(32.W))
  val eentry    = Reg(new EENTRY)
  val saved     = Reg(Vec(4, UInt(32.W)))
  val tid       = Reg(UInt(32.W))
  val tlbrentry = RegInit(0.U.asTypeOf(new TLBRENTRY))
  val tlbidx    = Reg(UInt(32.W))
  val tlbehi    = Reg(UInt(32.W))
  val tlbelo0   = Reg(UInt(32.W))
  val tlbelo1   = Reg(UInt(32.W))
  val asid      = Reg(UInt(32.W))
  val dmw       = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf((new DMW)))))

  val llbit = RegInit(false.B)
  val klo   = RegInit(false.B)

  /* ------ Global State ------ */
  io.priv     := crmd.plv()
  io.tlb.da   := crmd.da()
  io.tlb.pg   := crmd.pg()
  io.tlb.matf := crmd.datf() // Memory access type for fetch
  io.tlb.matd := crmd.datm() // Memory access type for data
  io.tlb.asid := 0.U         // Dont care for now, can be set by TLB
  io.tlb.dmw  := dmw         // DMW0 register
  // io.tlb.asid := prmd.value(31, 22) // Address space ID
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
  val excp_crmd  = crmd.set_ie(false.B).set_plv(0.U(2.W))
  val excp_prmd  = prmd.set_pie(crmd.ie()).set_pplv(crmd.plv())
  val excp_estat = estat.set_ecode(io.ecode).set_sub_ecode(io.ecode_sub)

  io.eentry := Mux(io.ecode === ECODE.TLBR.asUInt, tlbrentry.value, eentry.value)
  /* ------ Exception Enter ------ */

  /* ------ Exception Return ------ */
  val eret_crmd = crmd.set_ie(prmd.pie()).set_plv(prmd.pplv())
  val eret_prmd = prmd.set_pie(false.B).set_pplv(0.U(2.W))

  io.era := era
  /* ------ Exception Return ------ */

  /* ------ Interrupt Pending ------ */
  io.interrupt.pending := (estat.is() & ecfg.lie()).orR & crmd.ie()
  /* ------ Interrupt Pending ------ */

  /* ------ Write Logic ------ */
  when(io.excp_en) {
    crmd  := excp_crmd
    prmd  := excp_prmd
    estat := excp_estat
    badv  := Mux(Seq(TLBR, ADEF, ALE, PIL, PIS, PIF, PME, PPI).map(e => ECODE.getEcode(e.asUInt) === io.ecode).reduce(_ || _), io.badv, badv)
    era   := io.epc
  }.elsewhen(io.eret_en) {
    crmd := eret_crmd
    klo  := false.B
    llbit := Mux(klo, llbit, false.B)
    // prmd := eret_prmd
  }.elsewhen(io.tlb_cmd.cmd === TLBCmd.CMD_SRCH.U) {
    tlbidx := io.tlb_cmd.tlb_idx
  }.elsewhen(io.tlb_cmd.cmd === TLBCmd.CMD_RD.U) {
    tlbidx  := io.tlb_cmd.tlb_idx
    tlbehi  := io.tlb_cmd.tlb_ehi
    tlbelo0 := io.tlb_cmd.tlb_elo0
    tlbelo1 := io.tlb_cmd.tlb_elo1
  }.elsewhen(io.we) {
    val wdata = io.wdata & io.wmask

    for (i <- 0 until 4) {
      saved(i) := Mux(io.waddr === (CSRAddr.SAVED0 + i).U, wdata | (saved(i) & ~io.wmask), saved(i))
    }

    for (i <- 0 until 2) {
      dmw(i).value := Mux(io.waddr === (CSRAddr.DMW0 + i).U, dmw(i).write(wdata | (dmw(i).value & ~io.wmask)), dmw(i).value)
    }

    asid    := Mux(io.waddr === CSRAddr.ASID.U, wdata | (asid & ~io.wmask), asid)
    tlbidx  := Mux(io.waddr === CSRAddr.TLBIDX.U, wdata | (tlbidx & ~io.wmask), tlbidx)
    tlbehi  := Mux(io.waddr === CSRAddr.TLBEHI.U, wdata | (tlbehi & ~io.wmask), tlbehi)
    tlbelo0 := Mux(io.waddr === CSRAddr.TLBELO0.U, wdata | (tlbelo0 & ~io.wmask), tlbelo0)
    tlbelo1 := Mux(io.waddr === CSRAddr.TLBELO1.U, wdata | (tlbelo1 & ~io.wmask), tlbelo1)

    crmd.value := Mux(io.waddr === CSRAddr.CRMD.U, crmd.write(wdata | (crmd.value & ~io.wmask)), crmd.value)
    prmd.value := Mux(io.waddr === CSRAddr.PRMD.U, prmd.write(wdata | (prmd.value & ~io.wmask)), prmd.value)

    ecfg.value      := Mux(io.waddr === CSRAddr.ECFG.U, ecfg.write(wdata | (ecfg.value & !io.wmask)), ecfg.value)
    estat.value     := Mux(io.waddr === CSRAddr.ESTAT.U, estat.write(wdata | (estat.value & !io.wmask)), estat.value)
    era             := Mux(io.waddr === CSRAddr.ERA.U, wdata | (era & !io.wmask), era)
    eentry.value    := Mux(io.waddr === CSRAddr.EENTRY.U, eentry.write(wdata | (eentry.value & !io.wmask)), eentry.value)
    tlbrentry.value := Mux(io.waddr === CSRAddr.TLBRE.U, tlbrentry.write(wdata | (tlbrentry.value & !io.wmask)), tlbrentry.value)

    tid := Mux(io.waddr === CSRAddr.TID.U, wdata | (tid & ~io.wmask), tid)

    llbit := Mux(io.waddr === CSRAddr.LLBTL.U && ((wdata & 0b10.U) === 0b10.U), false.B, llbit)
  }.otherwise {
    estat := estat.set_sample(io.interrupt.externel_sample).set_tis(timer_interrupt_pending)
  }
  /* ------ Write Logic ------ */

  /* ------ Read Logic ------ */
  io.rdata := MuxLookup(io.raddr, 0.U(32.W))(
    Seq(
      CSRAddr.CRMD.U    -> crmd.value,
      CSRAddr.PRMD.U    -> prmd.value,
      CSRAddr.ECFG.U    -> ecfg.value,
      CSRAddr.BADV.U    -> badv,
      CSRAddr.ESTAT.U   -> estat.value,
      CSRAddr.ERA.U     -> era,
      CSRAddr.EENTRY.U  -> eentry.value,
      CSRAddr.TLBIDX.U  -> tlbidx,
      CSRAddr.TLBEHI.U  -> tlbehi,
      CSRAddr.TLBELO0.U -> tlbelo0,
      CSRAddr.TLBELO1.U -> tlbelo1,
      CSRAddr.ASID.U    -> asid,
      CSRAddr.LLBTL.U   -> llbit.asUInt,
      CSRAddr.TID.U     -> tid,
      CSRAddr.TCFG.U    -> timer.io.tcfg,
      CSRAddr.TVAL.U    -> timer.io.tval,
      CSRAddr.TICLR.U   -> 0.U,// TICLR is always read as 0
    ) ++ (0 until 4).map(i => (CSRAddr.SAVED0 + i).U -> saved(i)),
  )
  io.counterID := tid
  io.cntvh     := cntvh
  io.cntvl     := cntvl
  /* ------ Read Logic ------ */

  /* ------ Debug Info ------ */
  io.debug.crmd      := crmd.value
  io.debug.prmd      := prmd.value
  io.debug.ecfg      := ecfg.value
  io.debug.estat     := estat.value
  io.debug.era       := era
  io.debug.badv      := badv
  io.debug.eentry    := eentry.value
  io.debug.saved0    := saved(0)
  io.debug.saved1    := saved(1)
  io.debug.saved2    := saved(2)
  io.debug.saved3    := saved(3)
  io.debug.tid       := tid
  io.debug.tcfg      := timer.io.tcfg
  io.debug.tval      := timer.io.tval
  io.debug.tlbrentry := tlbrentry.value
  io.debug.tlbidx    := tlbidx
  io.debug.tlbehi    := tlbehi
  io.debug.tlbelo0   := tlbelo0
  io.debug.tlbelo1   := tlbelo1
  io.debug.asid      := asid
  io.debug.dmw0      := dmw(0).value
  io.debug.dmw1      := dmw(1).value
  /* ------ Debug Info ------ */
}
