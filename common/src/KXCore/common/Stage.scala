package KXCore.common

import chisel3._
import chisel3.util._

object ReadyValidIOExpand {
  def apply[T <: Data](io: ReadyValidIO[T], num: Int) = {
    val ioExt = Wire(new Bundle {
      val valid = Vec(num, Bool())
      val bits  = io.bits.cloneType
      val ready = Vec(num, Bool())
    })
    val en     = RegInit(Fill(num, 1.B))
    val nextEn = WireDefault(en & ~ioExt.ready.asUInt)
    en         := Mux(io.valid, Mux(nextEn === 0.U, Fill(num, 1.B), nextEn), Fill(num, 1.B))
    io.ready   := nextEn === 0.U
    ioExt.bits := io.bits
    for (i <- 0 until num) {
      ioExt.valid(i) := io.valid && en(i)
    }
    ioExt
  }
}

class PipeStageReg[T <: Data](gen: T, hasFlush: Boolean) extends Module {
  val io = IO(new Bundle {
    val flush = if (hasFlush) Some(Input(Bool())) else None
    val in    = Flipped(Decoupled(gen))
    val out   = Decoupled(gen)
  })
  val write  = io.in.valid
  val finish = io.out.ready
  val bits   = RegEnable(io.in.bits, io.in.ready)
  val en     = RegInit(false.B)
  val nextEn = WireDefault(en & ~finish)
  en           := Mux(io.in.ready, write, nextEn)
  io.in.ready  := (nextEn === 0.U) || io.flush.getOrElse(false.B)
  io.out.bits  := bits
  io.out.valid := !io.flush.getOrElse(false.B) && en
}

object PipeConnect {
  def apply[T <: Data](
      flush: Option[Bool],
      fanIn: DecoupledIO[T],
      fanOut: DecoupledIO[T],
  ) = {
    val pipeReg = Module(new PipeStageReg(fanIn.bits.cloneType, flush.isDefined))
    if (flush.isDefined) pipeReg.io.flush.get := flush.get

    fanIn          <> pipeReg.io.in
    pipeReg.io.out <> fanOut
    pipeReg
  }
}
