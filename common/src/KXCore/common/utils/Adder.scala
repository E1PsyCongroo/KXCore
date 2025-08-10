package KXCore.common.utils

import chisel3._
import chisel3.util._

class AdderOut(width: Int) extends Bundle {
  val carry    = Output(Bool())
  val zero     = Output(Bool())
  val overflow = Output(Bool())
  val result   = Output(UInt(width.W))
}

object AdderOut {
  def apply(width: Int) = new AdderOut(width)
}

object HalfAdder {
  def apply(ina: UInt, inb: UInt): (UInt, UInt) = {
    require(ina.getWidth == 1)
    require(inb.getWidth == 1)
    val sum   = ina ^ inb
    val carry = ina & inb
    (sum, carry)
  }
}

object FullAdder {
  def apply(ina: UInt, inb: UInt, inc: UInt): (UInt, UInt) = {
    require(ina.getWidth == 1)
    require(inb.getWidth == 1)
    require(inc.getWidth == 1)
    val sum   = (ina & inb & inc) | (ina & ~inb & ~inc) | (~ina & inb & ~inc) | (~ina & ~inb & inc)
    val carry = (ina & inb) | (ina & inc) | (inb & inc)
    (sum, carry)
  }
}

object RippleCarryAdder {
  def apply(ina: UInt, inb: UInt, addSub: Bool): AdderOut = {
    require(ina.getWidth == inb.getWidth)
    val width  = ina.getWidth
    val out    = Wire(AdderOut(width))
    val tnoCin = (Fill(width, addSub) ^ inb)
    val s      = Seq.fill(width)(Wire(UInt(1.W)))
    val c      = Seq.fill(width + 1)(Wire(UInt(1.W)))
    c(0) := addSub.asUInt
    for (i <- 0 until width) {
      val (_s, _c) = FullAdder(ina(i), tnoCin(i), c(i))
      s(i)     := _s
      c(i + 1) := _c
    }

    val sum = Cat(s.reverse)
    out.result   := sum
    out.carry    := c(width)
    out.zero     := ~(sum.orR)
    out.overflow := (ina(width - 1) === tnoCin(width - 1)) && (sum(width - 1) =/= ina(width - 1))
    out
  }
}

object Adder {
  def apply(ina: UInt, inb: UInt, addSub: Bool): AdderOut = {
    require(ina.getWidth == inb.getWidth)
    val width  = ina.getWidth
    val out    = Wire(AdderOut(width))
    val tA     = ina
    val tB     = inb
    val tnoCin = (Fill(width, addSub) ^ tB)
    val result = Wire(UInt((width + 1).W))
    result       := (tA +& tnoCin) + addSub
    out.result   := result(width - 1, 0)
    out.carry    := result(width).asBool
    out.zero     := ~(out.result.orR)
    out.overflow := (tA(width - 1) === tnoCin(width - 1)) && (result(width - 1) =/= tA(width - 1))
    out
  }
}
