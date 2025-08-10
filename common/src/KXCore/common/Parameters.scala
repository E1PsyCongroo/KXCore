package KXCore.common

import chisel3._
import chisel3.util._

case class CommonParameters(
    dataWidth: Int = 32,
    instWidth: Int = 32,
    vaddrWidth: Int = 32,
    paddrWidth: Int = 32,
    pcReset: BigInt = 0x1c00_0000L,
    tlbCount: Int = 16,
    debug: Boolean = true,
) {
  val dataBytes: Int  = dataWidth / 8
  val instBytes: Int  = instWidth / 8
  val vaddrBytes: Int = vaddrWidth / 8
  val paddrBytes: Int = paddrWidth / 8
  require(vaddrWidth == 32)
  require(paddrWidth >= vaddrWidth)
}
