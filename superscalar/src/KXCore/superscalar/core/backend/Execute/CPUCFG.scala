package KXCore.superscalar.core.backend.Execute

import chisel3._
import chisel3.util.MuxLookup

import KXCore.common.peripheral.CacheParameters

class CPUCFG(implicit icacheParams: CacheParameters, dcacheParams: CacheParameters) extends Module {
  val io = IO(new Bundle {
    val index = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
  })

  val cfg0x10 = 0b101.U
  val cfg0x11 = ((icacheParams.nWays - 1) | (icacheParams.bankWidth << 16) | (icacheParams.blockWidth << 24)).U
  val cfg0x12 = ((dcacheParams.nWays - 1) | (dcacheParams.bankWidth << 16) | (dcacheParams.blockWidth << 24)).U

  io.rdata := MuxLookup(io.index, 0.U(32.W))(Seq(
    0x10.U -> cfg0x10,
    0x11.U -> cfg0x11,
    0x12.U -> cfg0x12
  ))
}
