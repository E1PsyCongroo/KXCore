package KXCore.superscalar.core.backend

import chisel3._
import KXCore.superscalar._
import KXCore.superscalar.core._

class StableCounter(implicit params: CoreParameters) extends Module {
  val io = IO(new Bundle {
    val low  = Output(UInt(32.W))
    val high = Output(UInt(32.W))
  })

  val counter_high = RegInit(0.U(32.W))
  val counter_low  = RegInit(0.U(32.W))

  counter_high := Mux(counter_low.andR, counter_high + 1.U, counter_high)
  counter_low  := counter_low + 1.U

  io.low  := counter_low
  io.high := counter_high
}
