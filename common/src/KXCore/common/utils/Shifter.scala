package KXCore.common.utils

import chisel3._
import chisel3.util._

object Shifter {
  def apply(in: UInt, shamt: UInt, isLeft: Bool, isArith: Bool): UInt = {
    val width = in.getWidth
    require(width == (1 << shamt.getWidth))
    val sll = in << shamt
    val srl = in >> shamt
    val sra = (in.asSInt >> shamt).asUInt
    val out = Wire(UInt(width.W))
    out := Mux(isLeft, sll, Mux(isArith, sra, srl))
    out
  }
}

object BarrelShifter {
  def apply(in: UInt, shamt: UInt, isLeft: Bool, isArith: Bool): UInt = {
    val w = in.getWidth
    require(w == (1 << shamt.getWidth))
    val leftIn = Mux(isArith, in(w - 1), false.B) // 右移时从左边移入的位
    def layer(din: Seq[Bool], n: Int): Seq[Bool] = { // 描述第n级选择器如何排布
      val s                  = 1 << n                                 // 需要移动的位数
      def shiftRight(i: Int) = if (i + s >= w) leftIn else din(i + s) // 描述右移时第i位输出
      def shiftLeft(i: Int)  = if (i < s) false.B else din(i - s)     // 描述左移时第i位输出
      val sel                = Cat(isLeft, shamt(n))                  // 将移位方向和移位量作为选择器的选择信号
      din.zipWithIndex.map { case (b, i) => // 对于每一位输入b,
        VecInit(b, shiftRight(i), b, shiftLeft(i))(sel)
      } // 都从4种输入中选择一种作为输出
    }
    def barrelshift(din: Seq[Bool], k: Int): Seq[Bool] = // 描述有k级的桶形移位器如何排布
      if (k == 0) din // 若移位器只有0级, 则结果和输入相同
      // 否则实例化一个有k-1级的桶形移位器和第k-1级选择器, 并将后者的输出作为前者的输入
      else barrelshift(layer(din, k - 1), k - 1)
    val out = Wire(UInt(w.W))
    out := Cat(barrelshift(in.asBools, log2Up(w)).reverse) // 实例化一个有log2(w)级的桶形移位器
    out
  }
}
