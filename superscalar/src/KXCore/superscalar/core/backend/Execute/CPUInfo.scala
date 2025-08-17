package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._

class CPUInfo(implicit params: CoreParameters) extends Module {
  import params.{commonParams, frontendParams,backendParams}
  import frontendParams.{icacheParams}
  import backendParams.{dcacheParams}

  val io = IO(new Bundle {
    val idx = Input(UInt(32.W))
    val info = Output(UInt(32.W))
  })

  // val cfg0x10 = 0b101.U
  val cfg0x10 = 0b100.U
  val cfg0x11 = ((icacheParams.nWays - 1) | (icacheParams.bankWidth << 16) | (icacheParams.blockWidth << 24)).U
  val cfg0x12 = ((dcacheParams.nWays - 1) | (dcacheParams.bankWidth << 16) | (dcacheParams.blockWidth << 24)).U

  io.info := MuxLookup(io.idx, 0.U(32.W))(Seq(
    0x10.U -> cfg0x10,
    0x11.U -> cfg0x11,
    0x12.U -> cfg0x12
  ))
}
