package KXCore.common.utils

import chisel3._
import chisel3.util._

/** N-wide one-hot priority encoder.
  */
object SelectFirstN {
  def apply(in: UInt, n: Int) = {
    val sels = Wire(Vec(n, UInt(in.getWidth.W)))
    var mask = in

    for (i <- 0 until n) {
      sels(i) := PriorityEncoderOH(mask)
      mask = mask & ~sels(i)
    }

    sels
  }
}

/** Object to rotate a signal left by one
  */
object RotateL1 {
  def apply(signal: UInt): UInt = {
    val w   = signal.getWidth
    val out = Cat(signal(w - 2, 0), signal(w - 1))

    return out
  }
}

/** Object to sext a value to a particular length.
  */
object Sext {
  def apply(x: UInt, length: Int): UInt = {
    if (x.getWidth == length) return x
    else return Cat(Fill(length - x.getWidth, x(x.getWidth - 1)), x)
  }
}

/** Object to increment an input value, wrapping it if necessary.
  */
object WrapInc {
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value + 1.U)(log2Ceil(n) - 1, 0)
    } else {
      val wrap = (value === (n - 1).U)
      Mux(wrap, 0.U, value + 1.U)
    }
  }
}

/** Object to decrement an input value, wrapping it if necessary.
  */
object WrapDec {
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value - 1.U)(log2Ceil(n) - 1, 0)
    } else {
      val wrap = (value === 0.U)
      Mux(wrap, (n - 1).U, value - 1.U)
    }
  }
}

/** Set all bits at or above the lowest order '1'.
  */
object MaskUpper {
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => (in << i.U)(n - 1, 0)).reduce(_ | _)
  }
}

/** Set all bits at or below the highest order '1'.
  */
object MaskLower {
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => in >> i.U).reduce(_ | _)
  }
}

/** Transpose a matrix of Chisel Vecs.
  */
object Transpose {
  def apply[T <: chisel3.Data](in: Vec[Vec[T]]) = {
    val n = in(0).size
    VecInit((0 until n).map(i => VecInit(in.map(row => row(i)))))
  }
}

/** Connect the first k of n valid input interfaces to k output interfaces.
  */
class Compactor[T <: chisel3.Data](n: Int, k: Int, gen: T) extends Module {
  require(n >= k)

  val io = IO(new Bundle {
    val in  = Vec(n, Flipped(DecoupledIO(gen)))
    val out = Vec(k, DecoupledIO(gen))
  })

  if (n == k) {
    io.out <> io.in
  } else {
    val counts     = io.in.map(_.valid).scanLeft(1.U(k.W))((c, e) => Mux(e, (c << 1)(k - 1, 0), c))
    val sels       = Transpose(VecInit(counts map (c => VecInit(c.asBools)))) map (col => (col zip io.in.map(_.valid)) map { case (c, v) => c && v })
    val in_readys  = counts map (row => (row.asBools zip io.out.map(_.ready)) map { case (c, r) => c && r } reduce (_ || _))
    val out_valids = sels map (col => col.reduce(_ || _))
    val out_data   = sels map (s => Mux1H(s, io.in.map(_.bits)))

    in_readys zip io.in foreach { case (r, i) => i.ready := r }
    out_valids zip out_data zip io.out foreach { case ((v, d), o) => o.valid := v; o.bits := d }
  }
}
