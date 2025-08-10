package KXCore.common.utils

import chisel3._
import chisel3.util._
import scala.math._

class DividerIn(dwidth: Int) extends Bundle {
  val signed   = Bool()
  val dividend = UInt(dwidth.W)
  val divisor  = UInt(dwidth.W)
}

class DividerOut(dwidth: Int) extends Bundle {
  val quotient  = UInt(dwidth.W)
  val remainder = UInt(dwidth.W)
}

class DividerIO(dwidth: Int) extends Bundle {
  val flush = Input(Bool())
  val in    = Flipped(DecoupledIO(new DividerIn(dwidth)))
  val out   = DecoupledIO(new DividerOut(dwidth))
}

abstract class Divider(dwidth: Int) extends Module {
  val io = IO(new DividerIO(dwidth))
}

class BoothDivider(dwidth: Int) extends Divider(dwidth) {
  val in      = io.in.bits
  val divZero = in.divisor === 0.U
  val needNeg = RegEnable(
    Fill(2, in.signed && !divZero) & Cat(in.dividend(dwidth - 1), in.dividend(dwidth - 1) ^ in.divisor(dwidth - 1)),
    io.in.fire,
  )
  val negDiviDend  = in.signed && in.dividend(dwidth - 1)
  val result       = Reg(UInt((dwidth * 2).W))
  val negDivisor   = in.signed && in.divisor(dwidth - 1)
  val divisor_r    = RegEnable((Fill(dwidth, negDivisor) ^ in.divisor) + negDivisor, io.in.fire)
  val divisor      = WireDefault(0.U(1.W) ## divisor_r)
  val nextResult   = Wire(result.cloneType)
  val cycleCounter = Reg(UInt(log2Ceil(dwidth).W))
  val last         = WireDefault(cycleCounter === (dwidth - 1).U)

  val sIdle :: sDiv :: sSend :: sDivZeroSend :: Nil = Enum(4)

  val state         = RegInit(sIdle)
  val isIdle        = state === sIdle
  val isDiv         = state === sDiv
  val isSend        = state === sSend
  val isDivZeroSend = state === sDivZeroSend
  state := Mux(
    io.flush,
    sIdle,
    MuxLookup(state, sIdle)(
      Seq(
        sIdle        -> Mux(io.in.fire, Mux(divZero, sDivZeroSend, sDiv), sIdle),
        sDiv         -> Mux(last, sSend, sDiv),
        sSend        -> Mux(io.out.fire, sIdle, sSend),
        sDivZeroSend -> Mux(io.out.fire, sIdle, sSend),
      ),
    ),
  )

  result := MuxCase(
    DontCare,
    Seq(
      io.in.fire -> Mux(
        divZero,
        in.dividend ## Fill(dwidth, 1.U(1.W)),
        ((Fill(dwidth, negDiviDend) ^ in.dividend) + negDiviDend),
      ),
      isDiv         -> nextResult,
      isSend        -> result,
      isDivZeroSend -> result,
    ),
  )
  val partialRemainder = result(dwidth * 2 - 1, dwidth - 1) + ~divisor + 1.U
  val partialQuotient  = ~partialRemainder(dwidth)
  nextResult := Mux(
    partialRemainder(dwidth),
    result(dwidth * 2 - 2, 0) ## partialQuotient,
    partialRemainder(dwidth - 1, 0) ## result(dwidth - 2, 0) ## partialQuotient,
  )
  cycleCounter := MuxCase(
    DontCare,
    Seq(
      io.in.fire -> 0.U,
      isDiv      -> (cycleCounter + 1.U),
    ),
  )

  io.in.ready           := isIdle
  io.out.valid          := (isSend || isDivZeroSend) && !io.flush
  io.out.bits.quotient  := (Fill(dwidth, needNeg(0)) ^ result(dwidth - 1, 0)) + needNeg(0)
  io.out.bits.remainder := (Fill(dwidth, needNeg(1)) ^ result(dwidth * 2 - 1, dwidth)) + needNeg(1)
}
