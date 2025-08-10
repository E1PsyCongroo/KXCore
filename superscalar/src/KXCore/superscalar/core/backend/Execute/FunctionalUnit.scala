package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._
import KXCore.superscalar.EXUType._

/** Abstract top level functional unit class that wraps a lower level hand made functional unit
  */
abstract class FunctionalUnit(val isAluUnit: Boolean = false)(implicit params: CoreParameters) extends Module {
  val io = IO(new Bundle {
    val kill = Input(Bool())

    val req  = Flipped(new DecoupledIO(new FuncUnitReq))
    val resp = new DecoupledIO(new ExeUnitResp)
  })
}

class ALUUnit(implicit params: CoreParameters) extends FunctionalUnit(isAluUnit = true) {
  import params.{commonParams, frontendParams, fetchBytes}
  import commonParams.{instBytes, instWidth, dataWidth, vaddrWidth}
  import frontendParams.{fetchWidth}

  io.req.ready := true.B
  val uop = io.req.bits.uop

  // operand 1 select
  // Get the uop PC for jumps
  val block_pc = params.fetchAlign(io.req.bits.ftq_info(0).entry.fetchPC)
  val uop_pc   = (block_pc | Cat(uop.idx, Fill(log2Ceil(instBytes), 0.U)))

  val op1_data = MuxLookup(uop.op1Sel, 0.U)(
    Seq(
      OP1Type.OP1_RS1.asUInt  -> io.req.bits.rs1_data,
      OP1Type.OP1_PC.asUInt   -> uop_pc,
      OP1Type.OP1_ZERO.asUInt -> 0.U,
    ),
  )

  // operand 2 select
  val op2_data = MuxLookup(uop.op2Sel, io.req.bits.rs2_data)(
    Seq(
      OP2Type.OP2_RS2.asUInt  -> io.req.bits.rs2_data,
      OP2Type.OP2_IMM.asUInt  -> io.req.bits.uop.imm,
      OP2Type.OP2_NEXT.asUInt -> 4.U,
    ),
  )

  val alu = Module(new ALU())

  alu.io.in1 := op1_data.asUInt
  alu.io.in2 := op2_data.asUInt
  alu.io.cmd := uop.exuCmd

  val rs1 = io.req.bits.rs1_data
  val rs2 = io.req.bits.rs2_data

  val brInfo = Wire(new BrUpdateInfo)
  brInfo.mispredict := MuxLookup(io.req.bits.uop.cfiType, false.B)(
    Seq(
      CFIType.CFI_JIRL.asUInt -> (!io.req.bits.ftq_info(1).valid ||
        brInfo.target =/= io.req.bits.ftq_info(1).entry.fetchPC),
      CFIType.CFI_BR.asUInt -> (alu.io.cmp_out(0) =/= io.req.bits.ftq_info(0).entry.taken ||
        io.req.bits.ftq_info(0).entry.cfiIdx =/= io.req.bits.uop.idx),
    ),
  )
  brInfo.cfiIdx.valid := brInfo.cfiIsB || brInfo.cfiIsJirl || (brInfo.cfiIsBr && alu.io.cmp_out(0))
  brInfo.cfiIdx.bits  := io.req.bits.uop.idx
  brInfo.cfiIsB       := io.req.bits.uop.cfiType === CFIType.CFI_B.asUInt
  brInfo.cfiIsJirl    := io.req.bits.uop.cfiType === CFIType.CFI_JIRL.asUInt
  brInfo.cfiIsBr      := io.req.bits.uop.cfiType === CFIType.CFI_BR.asUInt
  brInfo.target := Mux(
    io.req.bits.uop.cfiType === CFIType.CFI_JIRL.asUInt,
    io.req.bits.rs1_data,
    uop_pc,
  ) + Mux(brInfo.cfiIdx.valid, io.req.bits.uop.imm, 4.U)

  io.resp.valid                := io.req.valid
  io.resp.bits.uop             := io.req.bits.uop
  io.resp.bits.uop.debug.wdata := alu.io.out
  io.resp.bits.data            := alu.io.out
  io.resp.bits.brInfo.valid    := io.req.bits.uop.cfiType =/= CFIType.CFI_NONE.asUInt
  io.resp.bits.brInfo.bits     := brInfo
  assert(io.resp.ready)
}

class MultiplyUnit(implicit params: CoreParameters) extends FunctionalUnit {
  import params.{commonParams, backendParams}
  import commonParams.{dataWidth}

  val multiplier = Module(new WallaceMultiplier(dataWidth, backendParams.mulPipeDepth))

  val uopReg = RegEnable(io.req.bits.uop, io.req.ready)

  multiplier.io.flush := io.kill

  multiplier.io.in.valid             := io.req.valid
  io.req.ready                       := multiplier.io.in.ready
  multiplier.io.in.bits.signed       := Fill(2, !EXUType.mul_divUnsigned(io.req.bits.uop.exuCmd))
  multiplier.io.in.bits.multiplicand := io.req.bits.rs1_data
  multiplier.io.in.bits.multiplier   := io.req.bits.rs2_data

  io.resp.valid           := multiplier.io.out.valid
  multiplier.io.out.ready := io.resp.ready
  io.resp.bits.uop        := uopReg
  io.resp.bits.data := Mux(
    EXUType.ismulh_mod(uopReg.exuCmd),
    multiplier.io.out.bits.result_hi,
    multiplier.io.out.bits.result_lo,
  )
  io.resp.bits.brInfo.valid := false.B
  io.resp.bits.brInfo.bits  := DontCare
}

class DivUnit(implicit params: CoreParameters) extends FunctionalUnit {
  val divider = Module(new BoothDivider(params.commonParams.dataWidth))

  val uopReg = RegEnable(io.req.bits.uop, io.req.ready)

  divider.io.flush := io.kill

  divider.io.in.valid         := io.req.valid
  io.req.ready                := divider.io.in.ready
  divider.io.in.bits.signed   := Fill(2, !EXUType.mul_divUnsigned(io.req.bits.uop.exuCmd))
  divider.io.in.bits.dividend := io.req.bits.rs1_data
  divider.io.in.bits.divisor  := io.req.bits.rs2_data

  io.resp.valid        := divider.io.out.valid
  divider.io.out.ready := io.resp.ready
  io.resp.bits.uop     := uopReg
  io.resp.bits.data := Mux(
    EXUType.ismulh_mod(uopReg.exuCmd),
    divider.io.out.bits.remainder,
    divider.io.out.bits.quotient,
  )
  io.resp.bits.brInfo.valid := false.B
  io.resp.bits.brInfo.bits  := DontCare
}
