package KXCore.common

import chisel3._
import chisel3.util._

class StageBundle[In <: Bundle, Out <: Bundle](inNum: Int, outNum: Int, genIn: => In, genOut: => Out)(implicit params: CommonParameters) extends Bundle {
  val in  = Vec(inNum, Flipped(Decoupled(genIn)))
  val out = Vec(outNum, Decoupled(genOut))
}

object PipeConnect {
  def apply[T <: Data](
      flush: Option[Bool],
      fanIn: DecoupledIO[T],
      fanOut: DecoupledIO[T]*,
  ): (T, UInt) = {
    require(!fanOut.isEmpty)
    val write  = fanIn.valid
    val finish = Cat(fanOut.map(_.ready).reverse)
    val bits   = RegEnable(fanIn.bits, fanIn.fire)
    val en     = RegInit(Fill(fanOut.length, false.B))
    val nextEn = WireDefault(en & ~finish)
    en          := Mux(fanIn.ready, Fill(fanOut.length, write), nextEn)
    fanIn.ready := (nextEn === 0.U) || flush.getOrElse(false.B)
    for ((right, idx) <- fanOut.zipWithIndex) {
      right.bits  := bits
      right.valid := !flush.getOrElse(false.B) && en(idx)
    }
    (bits, en)
  }
}

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
