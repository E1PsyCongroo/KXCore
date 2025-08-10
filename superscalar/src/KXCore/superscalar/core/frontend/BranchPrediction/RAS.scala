package KXCore.superscalar.core.frontend

import chisel3._
import chisel3.util._
import chisel3.util.random._
import KXCore.superscalar._
import KXCore.superscalar.core._

class RAS(implicit params: CoreParameters) extends Module {
  import params.{commonParams, frontendParams}
  import commonParams.{vaddrWidth}
  import frontendParams._
  val io = IO(new Bundle {
    val read = new Bundle {
      val idx  = Input(UInt(log2Ceil(rasNum).W))
      val addr = Output(UInt(vaddrWidth.W))
    }
    val write = new Bundle {
      val valid = Input(Bool())
      val idx   = Input(UInt(log2Ceil(rasNum).W))
      val addr  = Input(UInt(vaddrWidth.W))
    }
  })

  val ras = Reg(Vec(rasNum, UInt(vaddrWidth.W)))

  io.read.addr := Mux(io.write.valid && io.write.idx === io.read.idx, io.write.addr, ras(io.read.idx))

  when(io.write.valid) {
    ras(io.write.idx) := io.write.addr
  }
}
