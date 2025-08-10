package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._
import dataclass.data

abstract class AbstractALU(implicit params: CoreParameters) extends Module {
  import params.commonParams.{dataWidth}

  val io = IO(new Bundle {
    val cmd       = Input(UInt(EXUType.getWidth.W))
    val in1       = Input(UInt(dataWidth.W))
    val in2       = Input(UInt(dataWidth.W))
    val out       = Output(UInt(dataWidth.W))
    val adder_out = Output(UInt(dataWidth.W))
    val cmp_out   = Output(Bool())
  })
}

class ALU(implicit params: CoreParameters) extends AbstractALU {
  import params.commonParams.{dataWidth}
  import EXUType._

  // ADD, SUB
  val in2_inv     = Mux(isSub(io.cmd), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ io.in2
  io.adder_out := io.in1 + in2_inv + isSub(io.cmd)

  // SLT, SLTU
  val slt = Mux(
    io.in1(dataWidth - 1) === io.in2(dataWidth - 1),
    io.adder_out(dataWidth - 1),
    Mux(cmpUnsigned(io.cmd), io.in2(dataWidth - 1), io.in1(dataWidth - 1)),
  )
  io.cmp_out := cmpInverted(io.cmd) ^ Mux(cmpEq(io.cmd), in1_xor_in2 === 0.U, slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) = (io.in2(4, 0), io.in1)
  val shin            = Mux(shiftReverse(io.cmd), Reverse(shin_r), shin_r)
  val shout_r         = (Cat(shiftArith(io.cmd) & shin(dataWidth - 1), shin).asSInt >> shamt)(dataWidth - 1, 0)
  val shout_l         = Reverse(shout_r)
  val shout = Mux(io.cmd === EXU_SRL.asUInt || io.cmd === EXU_SRA.asUInt, shout_r, 0.U) |
    Mux(io.cmd === EXU_SLL.asUInt, shout_l, 0.U)

  // AND, OR, XOR, NOR
  val logic = MuxLookup(io.cmd, 0.U)(
    Seq(
      EXU_XOR.asUInt -> in1_xor_in2,
      EXU_OR.asUInt  -> (io.in1 | io.in2),
      EXU_NOR.asUInt -> ~(io.in1 | io.in2),
      EXU_AND.asUInt -> (io.in1 & io.in2),
    ),
  )

  val shift_logic = (isCmp(io.cmd) && slt) | logic | shout

  val out = MuxLookup(io.cmd, shift_logic)(
    Seq(
      EXU_ADD.asUInt -> io.adder_out,
      EXU_SUB.asUInt -> io.adder_out,
    ),
  )

  io.out := out
}
