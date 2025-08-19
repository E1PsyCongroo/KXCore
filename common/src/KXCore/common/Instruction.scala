package KXCore.common

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

case class Instruction(
    val name: String,
    val inst: BitPat,
) extends DecodePattern {
  require(
    inst.getWidth == 32,
    s"Instruction pattern must be 32 bits long, got: ${inst.getWidth} bits",
  )
  def bitPat: BitPat    = inst
  def rd: BitPat        = inst(4, 0)
  def rj: BitPat        = inst(9, 5)
  def rk: BitPat        = inst(14, 10)
  def ui5: BitPat       = inst(14, 10)
  def si12: BitPat      = inst(21, 12)
  def ui12: BitPat      = inst(21, 12)
  def csr: BitPat       = inst(23, 12)
  def cacopCode: BitPat = inst(4, 0)
  def si20: BitPat      = inst(24, 5)
  def si14: BitPat      = inst(23, 12)
  def si16: BitPat      = inst(25, 10)
  def si26: BitPat      = inst(0, 9) ## inst(25, 10)
}

object Instruction {
  /* Arithmetic Operations Instructions */
  val ADD_W     = Instruction("ADD.W", BitPat("b0000 0000 0001 00000 ????? ????? ?????"))
  val SUB_W     = Instruction("SUB.W", BitPat("b0000 0000 0001 00010 ????? ????? ?????"))
  val SLT       = Instruction("SLT", BitPat("b0000 0000 0001 00100 ????? ????? ?????"))
  val SLTU      = Instruction("SLTU", BitPat("b0000 0000 0001 00101 ????? ????? ?????"))
  val NOR       = Instruction("NOR", BitPat("b0000 0000 0001 01000 ????? ????? ?????"))
  val AND       = Instruction("AND", BitPat("b0000 0000 0001 01001 ????? ????? ?????"))
  val OR        = Instruction("OR", BitPat("b0000 0000 0001 01010 ????? ????? ?????"))
  val XOR       = Instruction("XOR", BitPat("b0000 0000 0001 01011 ????? ????? ?????"))
  val MUL_W     = Instruction("MUL.W", BitPat("b0000 0000 0001 11000 ????? ????? ?????"))
  val MULH_W    = Instruction("MULH.W", BitPat("b0000 0000 0001 11001 ????? ????? ?????"))
  val MULH_WU   = Instruction("MULH.WU", BitPat("b0000 0000 0001 11010 ????? ????? ?????"))
  val DIV_W     = Instruction("DIV.W", BitPat("b0000 0000 0010 00000 ????? ????? ?????"))
  val MOD_W     = Instruction("MOD.W", BitPat("b0000 0000 0010 00001 ????? ????? ?????"))
  val DIV_WU    = Instruction("DIV.WU", BitPat("b0000 0000 0010 00010 ????? ????? ?????"))
  val MOD_WU    = Instruction("MOD.WU", BitPat("b0000 0000 0010 00011 ????? ????? ?????"))
  val SLTI      = Instruction("SLTI", BitPat("b0000 001000 ???????????? ????? ?????"))
  val SLTUI     = Instruction("SLTUI", BitPat("b0000 001001 ???????????? ????? ?????"))
  val ADDI_W    = Instruction("ADDI.W", BitPat("b0000 001010 ???????????? ????? ?????"))
  val ANDI      = Instruction("ANDI.W", BitPat("b0000 001101 ???????????? ????? ?????"))
  val ORI       = Instruction("ORI.W", BitPat("b0000 001110 ???????????? ????? ?????"))
  val XORI      = Instruction("XORI.W", BitPat("b0000 001111 ???????????? ????? ?????"))
  val LU12I_W   = Instruction("LU12I.W", BitPat("b0001010 ???????????????????? ?????"))
  val PCADDU12I = Instruction("PCADDU12I", BitPat("b0001110 ????? ????? ????? ????? ?????"))

  /* Shift Arithmetic Instructions*/
  val SLL_W  = Instruction("SLL.W", BitPat("b0000 0000 0001 01110 ????? ????? ?????"))
  val SRL_W  = Instruction("SRL.W", BitPat("b0000 0000 0001 01111 ????? ????? ?????"))
  val SRA_W  = Instruction("SRA.W", BitPat("b0000 0000 0001 10000 ????? ????? ?????"))
  val SLLI_W = Instruction("SLLI.W", BitPat("b0000 0000 0100 00001 ????? ????? ?????"))
  val SRLI_W = Instruction("SRLI.W", BitPat("b0000 0000 0100 01001 ????? ????? ?????"))
  val SRAI_W = Instruction("SRAI.W", BitPat("b0000 0000 0100 10001 ????? ????? ?????"))

  /* Transfer Instructions */
  val JIRL = Instruction("JIRL", BitPat("b010011 ???????????????? ????? ?????"))
  val B    = Instruction("B", BitPat("b010100 ???????????????? ????? ?????"))
  val BL   = Instruction("BL", BitPat("b010101 ???????????????? ????? ?????"))
  val BEQ  = Instruction("BEQ", BitPat("b010110 ???????????????? ????? ?????"))
  val BNE  = Instruction("BNE", BitPat("b010111 ???????????????? ????? ?????"))
  val BLT  = Instruction("BLT", BitPat("b011000 ???????????????? ????? ?????"))
  val BGE  = Instruction("BGE", BitPat("b011001 ???????????????? ????? ?????"))
  val BLTU = Instruction("BLTU", BitPat("b011010 ???????????????? ????? ?????"))
  val BGEU = Instruction("BGEU", BitPat("b011011 ???????????????? ????? ?????"))

  /* Common Access Memory Instructions */
  val LD_B  = Instruction("LD.B", BitPat("b00101 00000 ???????????? ????? ?????"))
  val LD_H  = Instruction("LD.H", BitPat("b00101 00001 ???????????? ????? ?????"))
  val LD_W  = Instruction("LD.W", BitPat("b00101 00010 ???????????? ????? ?????"))
  val ST_B  = Instruction("ST.B", BitPat("b00101 00100 ???????????? ????? ?????"))
  val ST_H  = Instruction("ST.H", BitPat("b00101 00101 ???????????? ????? ?????"))
  val ST_W  = Instruction("ST.W", BitPat("b00101 00110 ???????????? ????? ?????"))
  val LD_BU = Instruction("LD.BU", BitPat("b00101 01000 ???????????? ????? ?????"))
  val LD_HU = Instruction("LD.HU", BitPat("b00101 01001 ???????????? ????? ?????"))
  val PRELD = Instruction("PRELD", BitPat("b00101 01011 ???????????? ????? ?????"))

  /* Atomic Access Memory Instructions */
  val LL_W = Instruction("LL.W", BitPat("b0010 0000 ?????????????? ????? ?????"))
  val SC_W = Instruction("SC.W", BitPat("b0010 0001 ?????????????? ????? ?????"))

  /* Fence Instructions */
  val DBAR = Instruction("DBAR", BitPat("b0011 1000 0111 00100 ????? ????? ?????"))
  val IBAR = Instruction("IBAR", BitPat("b0011 1000 0111 00101 ????? ????? ?????"))

  /* Other Instructions */
  val BREAK       = Instruction("BREAK", BitPat("b0000 0000 0010 10100 ????? ????? ?????"))
  val SYSCALL     = Instruction("SYSCALL", BitPat("b0000 0000 0010 10110 ????? ????? ?????"))
  val RDCNTID_W_0 = Instruction("RDCNTID.W", BitPat("b0000 0000 0000 00000 11000 1???? 00000"))
  val RDCNTID_W_1 = Instruction("RDCNTID.W", BitPat("b0000 0000 0000 00000 11000 ?1??? 00000"))
  val RDCNTID_W_2 = Instruction("RDCNTID.W", BitPat("b0000 0000 0000 00000 11000 ??1?? 00000"))
  val RDCNTID_W_3 = Instruction("RDCNTID.W", BitPat("b0000 0000 0000 00000 11000 ???1? 00000"))
  val RDCNTID_W_4 = Instruction("RDCNTID.W", BitPat("b0000 0000 0000 00000 11000 ????1 00000"))
  val RDCNTVL_W   = Instruction("RDCNTVL.W", BitPat("b0000 0000 0000 00000 11000 00000 ?????"))
  val RDCNTVH_W   = Instruction("RDCNTVH.W", BitPat("b0000 0000 0000 00000 11001 00000 ?????"))
  val RDTIMEL_W   = Instruction("RDTIMEL.W", BitPat("b0000 0000 0000 00000 11000 ????? ?????"))
  val RDTIMEH_W   = Instruction("RDTIMEL.W", BitPat("b0000 0000 0000 00000 11001 ????? ?????"))

  /* CSR Access Instructions */
  val CSRRD     = Instruction("CSRRD", BitPat("b00000100 ?????????????? 00000 ?????"))
  val CSRWR     = Instruction("CSRWR", BitPat("b00000100 ?????????????? 00001 ?????"))
  val CSRXCHG_0 = Instruction("CSRXCHG", BitPat("b00000100 ?????????????? 1???? ?????"))
  val CSRXCHG_1 = Instruction("CSRXCHG", BitPat("b00000100 ?????????????? ?1??? ?????"))
  val CSRXCHG_2 = Instruction("CSRXCHG", BitPat("b00000100 ?????????????? ??1?? ?????"))
  val CSRXCHG_3 = Instruction("CSRXCHG", BitPat("b00000100 ?????????????? ???1? ?????"))

  /* Cache Maintenance Instructions */
  val CACOP = Instruction("CACOP", BitPat("b0000011000 ???????????? ????? ?????"))

  /* TLB Maintenance Instructions */
  val TLBSRCH = Instruction("TLBSRCH", BitPat("b0000 0110 0100 10000 01010 00000 00000"))
  val TLBRD   = Instruction("TLBRD", BitPat("b0000 0110 0100 10000 01011 00000 00000"))
  val TLBWR   = Instruction("TLBWR", BitPat("b0000 0110 0100 10000 01100 00000 00000"))
  val TLBFILL = Instruction("TLBFILL", BitPat("b0000 0110 0100 10000 01101 00000 00000"))
  val INVTLB  = Instruction("INVTLB", BitPat("b0000 0110 0100 10011 ????? ????? ?????"))

  /* Other miscellaneous directives */
  val ERTN   = Instruction("ERTN", BitPat("b0000 0110 0100 10000 01110 00000 00000"))
  val IDLE   = Instruction("IDLE", BitPat("b0000 0110 0100 10001 ????? ????? ?????"))
  val CPUCFG = Instruction("CPUCFG", BitPat("b0000 0000 0000 00000 11011 ????? ?????"))
}

object Privilege {
  object PLV extends ChiselEnum {
    val PLV_0 = Value(0.U(2.W)) // User mode
    val PLV_3 = Value(3.U(2.W)) // Machine mode
  }

  object CACOPType extends ChiselEnum {
    val CACOP_IDX_INIT = Value(0.U(2.W)) // init cache line
    val CACOP_IDX_INV  = Value(1.U(2.W)) // invalidate and writeback indexed cache line
    val CACOP_HIT_INV  = Value(2.U(2.W)) // invalidate and writeback cache line if hit
    val CACOP_NONE     = Value(3.U(2.W))
  }

  object ECODE extends ChiselEnum {
    def getEcode(e: UInt)    = e(5, 0)
    def getEsubCode(e: UInt) = e(6)

    val INT  = Value(0x00.U)
    val PIL  = Value(0x01.U)
    val PIS  = Value(0x02.U)
    val PIF  = Value(0x03.U)
    val PME  = Value(0x04.U)
    val PPI  = Value(0x07.U)
    val ADEF = Value(0x08.U)
    val ALE  = Value(0x09.U)
    val SYS  = Value(0x0b.U)
    val BRK  = Value(0x0c.U)
    val INE  = Value(0x0d.U)
    val IPE  = Value(0x0e.U)
    val FPD  = Value(0x0f.U)
    val FPE  = Value(0x12.U)
    val TLBR = Value(0x3f.U)
    val ADEM = Value(0x7f.U)
  }

  object CSR {
    object CSRAddr {
      val CRMD      = 0x000
      val PRMD      = 0x001
      val EUEN      = 0x002
      val ECFG      = 0x004
      val ESTAT     = 0x005
      val ERA       = 0x006
      val BADV      = 0x007
      val EENTRY    = 0x00c
      val TLBIDX    = 0x010
      val TLBEHI    = 0x011
      val TLBELO0   = 0x012
      val TLBELO1   = 0x013
      val ASID      = 0x018
      val PGDL      = 0x019
      val PGDH      = 0x01a
      val PGD       = 0x01b
      val CPUID     = 0x020
      val SAVED0    = 0x030
      val SAVED1    = 0x031
      val SAVED2    = 0x032
      val SAVED3    = 0x033
      val TID       = 0x040
      val TCFG      = 0x041
      val TVAL      = 0x042
      val TICLR     = 0x044
      val LLBCTL    = 0x060
      val TLBRENTRY = 0x088
      val CTAG      = 0x098
      val DMW0      = 0x180
      val DMW1      = 0x181
    }

    class CRMD extends Bundle {
      val value = UInt(32.W)

      def plv  = value(1, 0) // Privilege level
      def ie   = value(2)    // Interrupt enable
      def da   = value(3)    // Debug active
      def pg   = value(4)    // Modify privilege
      def datf = value(6, 5) // Data address translation fault
      def datm = value(8, 7) // Data address translation mode

      def write(value: UInt): UInt = {
        value & 0x1ff.U(32.W)
      }

      def set_da(da: Bool): CRMD = {
        val write_mask = 0x8.U(32.W)
        ((value & ~write_mask) | (da.asUInt << 3)).asTypeOf(this)
      }

      def set_pg(pg: Bool): CRMD = {
        val write_mask = 0x10.U(32.W)
        ((value & ~write_mask) | (pg.asUInt << 4)).asTypeOf(this)
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

      def pplv = value(1, 0) // Privilege level
      def pie  = value(2)    // Interrupt enable

      def write(value: UInt): UInt = {
        value & 0x7.U(32.W)
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

    class EUEN extends Bundle {
      val value = UInt(32.W)

      def write(value: UInt): UInt = {
        value & 1.U(32.W)
      }
    }

    class ECFG extends Bundle {
      val value = UInt(32.W)

      def lie = value(12, 0)

      def write(value: UInt): UInt = {
        val write_mask = 0x3ff.U | (0x3.U << 11)
        value & write_mask
      }
    }

    class ESTAT extends Bundle {
      val value = UInt(32.W)

      def is = value(12, 0)

      def ecode     = value(21, 16)
      def sub_ecode = value(30, 22)

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

      def write(value: UInt): UInt = {
        value & ~0x3f.U(32.W)
      }
    }

    class TLBIDX(n: Int) extends Bundle {
      val value = UInt(32.W)

      require(n <= 16)

      def index = value(n - 1, 0)
      def ps    = value(29, 24)
      def ne    = value(31)

      def write(value: UInt): UInt = {
        value & ("b1011_1111_0000_0000".U(16.W) ## Fill(16 - n, 0.B) ## Fill(n, 1.B))
      }
    }

    class TLBEHI extends Bundle {
      val value = UInt(32.W)

      def vppn = value(31, 13)

      def write(value: UInt): UInt = {
        value & ~0x1fff.U(32.W)
      }
    }

    class TLBELO(paddrWidth: Int) extends Bundle {
      val value = UInt(32.W)

      def v   = value(0)
      def d   = value(1)
      def plv = value(3, 2)
      def mat = value(5, 4)
      def g   = value(6)
      def ppn = value(paddrWidth - 5, 8)

      def write(value: UInt): UInt = {
        value & (Fill(paddrWidth - 12, 1.B) ## "b0111_1111".U(8.W))
      }
    }

    class ASID extends Bundle {
      val value = UInt(32.W)

      def asid     = value(9, 0)
      def asidbits = 10.U(8.W)

      def write(value: UInt): UInt = {
        (value & 0x3ff.U(32.W)) | 0xa0000.U(32.W)
      }
    }

    class PGD extends Bundle {
      val value = UInt(32.W)

      def write(value: UInt): UInt = {
        (value & ~0xfff.U(32.W))
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
  }

  class TLBRENTRY extends Bundle {
    val value = UInt(32.W)

    def write(value: UInt): UInt = {
      value & ~0x3f.U(32.W)
    }
  }

  class DMW extends Bundle {
    val value = UInt(32.W)

    def plv0 = value(0)
    def plv3 = value(3)
    def mat  = value(5, 4)
    def pseg = value(27, 25)
    def vseg = value(31, 29)

    def write(value: UInt): UInt = {
      val write_mask = "hee000039".U
      value & write_mask
    }
  }

}
