package KXCore.common.peripheral

import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.experimental.dataview._

case class CacheParameters(
    id: Int = 0,
    nSets: Int = 64,
    nWays: Int = 4,
    nBanks: Int = 1,
    replacer: Option[String] = Some("random"),
    latency: Int = 2,
    fetchBytes: Int = 16,
    singlePorted: Boolean = true,
) {
  val setWidth: Int   = log2Ceil(nSets)
  val wayWidth: Int   = log2Ceil(nWays)
  val blockBytes: Int = fetchBytes
  val blockBits: Int  = blockBytes * 8
  val blockWidth: Int = log2Ceil(blockBytes)
  val bankBytes: Int  = blockBytes / nBanks
  val bankBits: Int   = bankBytes * 8
  val bankWidth: Int  = log2Ceil(bankBytes)
  require(id >= 0 && id < 16)
  require(latency == 2)
  require(setWidth + blockWidth <= 12)
  require(isPow2(nSets) && isPow2(nWays))
  require(wayWidth <= blockWidth, "Cacop Index Invalidate Need wayWidth <= blockWidth")
  require(nBanks == 1 || nBanks == 2)
}
