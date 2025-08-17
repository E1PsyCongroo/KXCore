package KXCore.common.utils

import chisel3._
import chisel3.util._
import scala.math._

class MultiplierIn(dwidth: Int) extends Bundle {
  val signed       = UInt(2.W)
  val multiplicand = UInt(dwidth.W)
  val multiplier   = UInt(dwidth.W)
}

class MultiplierOut(dwidth: Int) extends Bundle {
  val result_lo = UInt(dwidth.W)
  val result_hi = UInt(dwidth.W)
}

class MultiplierIO(dwidth: Int) extends Bundle {
  val flush = Input(Bool())
  val in    = Flipped(DecoupledIO(new MultiplierIn(dwidth)))
  val out   = DecoupledIO(new MultiplierOut(dwidth))
}

abstract class Multiplier(dwidth: Int) extends Module {
  val io = IO(new MultiplierIO(dwidth))
}

class ShiftMultiplier(dwidth: Int) extends Multiplier(dwidth) {
  val multiplicand_r   = Reg(UInt((dwidth * 2).W))
  val nextMultiplicand = WireDefault(multiplicand_r << 1.U)
  val multiplier_r     = Reg(UInt((dwidth).W))
  val nextMultiplier   = WireDefault(multiplier_r >> 1.U)
  val partialProduct   = Wire(UInt((dwidth * 2).W))
  val result           = Reg(UInt((dwidth * 2).W))
  val lastAddSub       = RegEnable(Mux(io.in.bits.signed(0), io.in.bits.multiplier(dwidth - 1), 0.U), io.in.fire)
  val last             = WireDefault(nextMultiplier === 0.U)

  val sIdle :: sMul :: sSend :: Nil = Enum(3)

  val state  = RegInit(sIdle)
  val isIdle = state === sIdle
  val isMul  = state === sMul
  val isSend = state === sSend
  state := Mux(
    io.flush,
    sIdle,
    MuxLookup(state, sIdle)(
      Seq(
        sIdle -> Mux(io.in.fire, sMul, sIdle),
        sMul  -> Mux(last, sSend, sMul),
        sSend -> Mux(io.out.fire, sIdle, sSend),
      ),
    ),
  )

  multiplicand_r := Mux(
    io.in.fire,
    Fill(dwidth, io.in.bits.signed(1) & io.in.bits.multiplicand(dwidth - 1)) ## io.in.bits.multiplicand,
    nextMultiplicand,
  )
  multiplier_r := Mux(io.in.fire, io.in.bits.multiplier, nextMultiplier)
  partialProduct := Mux(last, (Fill(dwidth * 2, lastAddSub) ^ multiplicand_r) + lastAddSub, multiplicand_r) & Fill(
    dwidth * 2,
    multiplier_r(0),
  )
  result := MuxCase(
    DontCare,
    Seq(
      io.in.fire -> 0.U,
      isMul      -> (result + partialProduct),
      isSend     -> result,
    ),
  )

  io.in.ready           := isIdle
  io.out.valid          := isSend && !io.flush
  io.out.bits.result_lo := result(dwidth - 1, 0)
  io.out.bits.result_hi := result(dwidth * 2 - 1, dwidth)
}

class Booth2Multiplier(dwidth: Int) extends Multiplier(dwidth) {
  require(dwidth % 2 == 0)

  val in = io.in.bits
  def GenPartialProduct(y_add: Bool, y: Bool, y_sub: Bool, multiplicand: UInt): UInt = {
    val width               = multiplicand.getWidth
    val sel_negative        = WireDefault(y_add & (y & ~y_sub | ~y & y_sub))
    val sel_positive        = WireDefault(~y_add & (y & ~y_sub | ~y & y_sub))
    val sel_double_negative = WireDefault(y_add & ~y & ~y_sub)
    val sel_double_positive = WireDefault(~y_add & y & y_sub)
    val x                   = multiplicand(width - 1) ## multiplicand ## 0.U
    val partialProduct = for (i <- 1 to width + 1) yield {
      WireDefault(
        (sel_negative & ~x(i)) | (sel_double_negative & ~x(i - 1)) |
          (sel_positive & x(i)) | (sel_double_positive & x(i - 1)),
      )
    }
    val result = Cat(partialProduct.toSeq.reverse) + (sel_negative || sel_double_negative)
    require(result.getWidth == width + 1)
    result
  }
  val multiplicandSigned = RegEnable(in.signed(1), io.in.fire)
  val multiplierWidth    = dwidth + 2
  // 乘数双符号/零扩展 和 最低位补0 需要 3 bits
  val result     = Reg(UInt((dwidth + multiplierWidth + 1).W))
  val nextResult = Wire(result.cloneType)
  // 被乘数符号/零扩展 需要 1 bit
  val multiplicand_r = RegEnable(Mux(in.signed(1), in.multiplicand(dwidth - 1), 0.U) ## in.multiplicand, io.in.fire)
  // 部分积 宽度为 dwidth + 2(可能的左移与符号扩展)
  val partialProduct = WireDefault(GenPartialProduct(result(2), result(1), result(0), multiplicand_r))
  val cycleCounter   = Reg(UInt(log2Ceil(multiplierWidth / 2).W))
  val last           = WireDefault(cycleCounter === (multiplierWidth / 2 - 1).U)

  val sIdle :: sMul :: sSend :: Nil = Enum(3)

  val state  = RegInit(sIdle)
  val isIdle = state === sIdle
  val isMul  = state === sMul
  val isSend = state === sSend
  state := Mux(
    io.flush,
    sIdle,
    MuxLookup(state, sIdle)(
      Seq(
        sIdle -> Mux(io.in.fire, sMul, sIdle),
        sMul  -> Mux(last, sSend, sMul),
        sSend -> Mux(io.out.fire, sIdle, sSend),
      ),
    ),
  )

  result := MuxCase(
    DontCare,
    Seq(
      io.in.fire -> Fill(2, Mux(in.signed(0), in.multiplier(dwidth - 1), 0.U(1.W))) ## in.multiplier ## 0.U(1.W),
      isMul      -> nextResult,
      isSend     -> result,
    ),
  )
  nextResult := (partialProduct + (Fill(2, result(dwidth + multiplierWidth)) ## result(
    dwidth + multiplierWidth,
    multiplierWidth + 1,
  ))) ## result(multiplierWidth, 2)
  cycleCounter := MuxCase(
    DontCare,
    Seq(
      io.in.fire -> 0.U,
      isMul      -> (cycleCounter + 1.U),
    ),
  )

  io.in.ready           := isIdle
  io.out.valid          := isSend && !io.flush
  io.out.bits.result_lo := result(dwidth, 1)
  io.out.bits.result_hi := result(dwidth * 2, dwidth + 1)
}

class WallaceMultiplier(dwidth: Int, pipeDepth: Int) extends Multiplier(dwidth) {
  require(dwidth % 2 == 0)

  val sIdle :: sMul :: sSend :: Nil = Enum(3)

  val state  = RegInit(sIdle)
  val isIdle = state === sIdle
  val isMul  = state === sMul
  val isSend = state === sSend
  val in     = io.in.bits
  def GenPartialProduct(y_add: Bool, y: Bool, y_sub: Bool, multiplicand: UInt): UInt = {
    val width               = multiplicand.getWidth
    val sel_negative        = y_add & (y & ~y_sub | ~y & y_sub)
    val sel_positive        = ~y_add & (y & ~y_sub | ~y & y_sub)
    val sel_double_negative = y_add & ~y & ~y_sub
    val sel_double_positive = ~y_add & y & y_sub
    val x                   = multiplicand(width - 1) ## multiplicand ## 0.U
    val partialProduct = for (i <- 1 to width + 1) yield {
      (sel_negative & ~x(i)) | (sel_double_negative & ~x(i - 1)) |
        (sel_positive & x(i)) | (sel_double_positive & x(i - 1))
    }
    val result = Cat(partialProduct.toSeq.reverse) + (sel_negative || sel_double_negative)
    require(result.getWidth == width + 1)
    result
  }
  // 被乘数符号/零扩展 需要 dwidth + 1 bits
  val multiplicand = Mux(in.signed(1), in.multiplicand(dwidth - 1), 0.U) ## in.multiplicand
  // 乘数两位符号/零扩展 需要 dwidth + 2 bits
  val multiplier = Fill(2, Mux(in.signed(0), in.multiplier(dwidth - 1), 0.U)) ## in.multiplier ## 0.U(1.W)
  // 部分积
  val partialProducts: Seq[Seq[Bool]] = (1 until (dwidth + 2, 2)).map { i =>
    GenPartialProduct(multiplier(i + 1), multiplier(i), multiplier(i - 1), multiplicand).asBools
  }

  /*
   *    sssss*****               n*****            n*****
   *(1) sss*****       (2)     n*****      (3)   n*****
   *    s*****               n*****            n*****
   *                       - 1 1 1           + 0101011111
   *                         n = ~s          +          1
   *
   *          n*****           nss*****          nss*****
   *(4)     n*****       (5)  1n*****     (6*)  1n***** s
   *      n*****             n*****            n***** s
   *    + 01011
   */
  def switch(rows: Seq[Seq[Bool]]): Seq[Seq[Bool]] = {
    if (rows.isEmpty) return Seq.empty[Seq[Bool]]
    val rowCount    = rows.length
    val rowLength   = rows(0).length
    val maxColCount = rowLength + (rowCount - 1) * 2
    val switchRows = rows.zipWithIndex.map { case (row, i) =>
      val sign    = row(rowLength - 1)
      val notSign = ~sign
      if (i == 0) {
        row.dropRight(1).appendedAll(Seq(sign, sign, notSign))
      } else if (i == rowLength - 1) {
        row.dropRight(1).appended(notSign)
      } else {
        row.dropRight(1).appendedAll(Seq(notSign, true.B))
      }
    }
    val cols = Seq.tabulate(maxColCount) { colIndex =>
      switchRows.zipWithIndex.flatMap { case (row, i) =>
        if (colIndex >= 2 * i && colIndex < row.length + 2 * i) Some(row(colIndex - 2 * i))
        else None
      }
    }
    cols
  }

  def addOneColumn(col: Seq[Bool], cin: Seq[Bool]): (Seq[Bool], Seq[Bool], Seq[Bool]) = {
    var sum   = Seq[Bool]()
    var cout1 = Seq[Bool]()
    var cout2 = Seq[Bool]()
    col.size match {
      case 1 =>
        sum = col ++ cin
      case 2 =>
        val (s, c) = HalfAdder(col(0), col(1))
        sum = s.asBool +: cin
        cout2 = Seq(c.asBool)
      case 3 =>
        val (s, c) = FullAdder(col(0), col(1), col(2))
        sum = s.asBool +: cin
        cout2 = Seq(c.asBool)
      case n =>
        val cin_1               = if (cin.nonEmpty) Seq(cin.head) else Nil
        val cin_2               = if (cin.nonEmpty) cin.drop(1) else Nil
        val (s_1, c_1_1, c_1_2) = addOneColumn(col.take(3), cin_1)
        val (s_2, c_2_1, c_2_2) = addOneColumn(col.drop(3), cin_2)
        sum = s_1 ++ s_2
        cout1 = c_1_1 ++ c_2_1
        cout2 = c_1_2 ++ c_2_2
    }
    (sum, cout1, cout2)
  }

  def addAll(cols: Seq[Seq[Bool]], depth: Int): (UInt, UInt, Int) = {
    if (cols.map(_.length).max <= 2) {
      val sum   = Cat(cols.map(_(0)).reverse)
      val carry = Cat(cols.map(_.applyOrElse(1, (_: Int) => false.B)).reverse)
      (sum, carry, depth)
    } else {
      val columns_next = Array.fill((dwidth + 1) * 2)(Seq[Bool]())
      var cout1, cout2 = Seq[Bool]()
      for (i <- cols.indices) {
        val (s, c1, c2) = addOneColumn(cols(i), cout1)
        columns_next(i) = s ++ cout2
        cout1 = c1
        cout2 = c2
      }

      val needReg = (depth != 0) && (depth % pipeDepth == 0)
      val toNextLayer: Seq[Seq[Bool]] =
        if (needReg)
          columns_next.toSeq.map(_.map(x => RegNext(x)))
        else
          columns_next.toSeq

      addAll(toNextLayer, depth + 1)
    }
  }

  val columns                   = switch(partialProducts)
  val columns_r: Seq[Seq[Bool]] = columns.map(c => c.map(b => RegEnable(b, io.in.fire)))
  val (sum, carry, depth)       = addAll(columns_r, 0)
  val cycleCounter              = Reg(UInt(log2Ceil(depth / pipeDepth + 1).W))
  val last                      = WireDefault(cycleCounter === (depth / pipeDepth).U)
  val result                    = sum + carry

  state := Mux(
    io.flush,
    sIdle,
    MuxLookup(state, sIdle)(
      Seq(
        sIdle -> Mux(io.in.fire, sMul, sIdle),
        sMul  -> Mux(last, sSend, sMul),
        sSend -> Mux(io.out.fire, sIdle, sSend),
      ),
    ),
  )

  cycleCounter := MuxCase(
    DontCare,
    Seq(
      io.in.fire -> 0.U,
      isMul      -> (cycleCounter + 1.U),
    ),
  )

  io.in.ready           := isIdle
  io.out.valid          := isSend && !io.flush
  io.out.bits.result_lo := result(dwidth - 1, 0)
  io.out.bits.result_hi := result(dwidth * 2 - 1, dwidth)
}

class VivadoIPMultiplier(dwidth: Int, pipeDepth: Int) extends Multiplier(dwidth) {

  // TODO: now only support this, modify in vivado?
  require(dwidth == 32)
  require(pipeDepth == 3)

  val sIdle :: sMul :: sSend :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val isIdle = state === sIdle
  val isMul = state === sMul
  val isSend = state === sSend

  val latency = 3
  val cycleCounter = RegInit(0.U(log2Ceil(latency + 1).W))
  val last = (cycleCounter === (latency - 1).U)

  state := Mux(
    io.flush,
    sIdle,
    MuxLookup(state, sIdle)(
      Seq(
        sIdle -> Mux(io.in.fire, sMul, sIdle),
        sMul  -> Mux(last, sSend, sMul),
        sSend -> Mux(io.out.fire, sIdle, sSend),
      ),
    ),
  )
  cycleCounter := MuxCase(
    0.U,
    Seq(
      io.in.fire -> 0.U,
      isMul      -> (cycleCounter + 1.U),
    ),
  )

  val multiplicand_signed = io.in.bits.signed(1)
  val multiplier_signed = io.in.bits.signed(0)

  val multiplicand_abs_comb = Mux(
    multiplicand_signed && io.in.bits.multiplicand(dwidth - 1),
    (~io.in.bits.multiplicand + 1.U),
    io.in.bits.multiplicand
  )

  val multiplier_abs_comb = Mux(
    multiplier_signed && io.in.bits.multiplier(dwidth - 1),
    (~io.in.bits.multiplier + 1.U),
    io.in.bits.multiplier
  )

  val multiplicand_abs = RegEnable(multiplicand_abs_comb, io.in.fire)
  val multiplier_abs = RegEnable(multiplier_abs_comb, io.in.fire)

  val result_negative_comb = (multiplicand_signed && io.in.bits.multiplicand(dwidth - 1)) ^ (multiplier_signed && io.in.bits.multiplier(dwidth - 1))
  val result_negative = RegEnable(result_negative_comb, io.in.fire)

  val multiplier = Module(new VivadoMultiplierIP(dwidth, pipeDepth))

  multiplier.io.CLK := clock
  multiplier.io.SCLR := reset.asBool || io.flush
  multiplier.io.CE := true.B

  multiplier.io.A := multiplicand_abs
  multiplier.io.B := multiplier_abs

  val result_data = multiplier.io.P
  val final_result = Mux(
    result_negative,
    (~result_data + 1.U),
    result_data
  )

  io.in.ready           := isIdle
  io.out.valid          := isSend && !io.flush

  io.out.bits.result_lo := final_result(dwidth - 1, 0)
  io.out.bits.result_hi := final_result(dwidth * 2 - 1, dwidth)
}
