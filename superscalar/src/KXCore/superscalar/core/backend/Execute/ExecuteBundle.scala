package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._

/** Bundle for signals sent to the functional unit
  */
class FuncUnitReq(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams}
  import commonParams.{dataWidth}
  val uop      = new MicroOp
  val rs1_data = UInt(dataWidth.W)
  val rs2_data = UInt(dataWidth.W)
  val ftq_info = Vec(2, new FTQInfo)
}

class ExeUnitResp(implicit params: CoreParameters) extends Bundle {
  val uop    = new MicroOp
  val data   = UInt(params.commonParams.dataWidth.W)
  val brInfo = Valid(new BrUpdateInfo)
}
