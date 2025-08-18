package KXCore.superscalar

import scala.math._
import chisel3._
import chisel3.util._
import KXCore.common.peripheral.AXIBundleParameters

case class CommonParameters(
    dataWidth: Int = 32,
    instWidth: Int = 32,
    vaddrWidth: Int = 32,
    paddrWidth: Int = 32,
    pcReset: BigInt = 0x1c00_0000L,
    debug: Boolean = true,
) {
  val dataBytes: Int  = dataWidth / 8
  val instBytes: Int  = instWidth / 8
  val vaddrBytes: Int = vaddrWidth / 8
  val paddrBytes: Int = paddrWidth / 8
  require(vaddrWidth == 32)
  require(paddrWidth >= vaddrWidth)
}

case class CacheParameters(
    id: Int = 0,
    // nSets: Int = 512,
    // nWays: Int = 16,
    nSets: Int = 64,
    nWays: Int = 4,
    nBanks: Int = 1,
    replacer: Option[String] = Some("random"),
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
  // require(setWidth + blockWidth <= 12)
  require(isPow2(nSets) && isPow2(nWays))
  require(wayWidth <= blockWidth, "Cacop Index Invalidate Need wayWidth <= blockWidth")
  require(nBanks == 1 || nBanks == 2)
}

case class FAMicroBTBParameters(
    nWays: Int = 16,
    tagWidth: Int = 12,
    useDualEntries: Boolean = true,
) {
  require(isPow2(nWays))
}

case class BIMParams(
    nSets: Int = 2048,
) {
  require(isPow2(nSets))
}

case class BTBParams(
    nSets: Int = 128,
    nWays: Int = 2,
    offsetWidth: Int = 18,
    extendedNSets: Int = 128,
) {
  require(isPow2(nSets))
  require(isPow2(nWays))
  require(isPow2(extendedNSets))
  require(extendedNSets <= nSets)
  require(extendedNSets >= 1)
}

case class FrontendParmaeters(
    fetchWidth: Int = 4, // Number of instructions fetched per request
    fbNum: Int = 32,     // Number of entries in the fetch buffer
    ftqNum: Int = 32,    // Number of entries in the fetch target queue
    rasNum: Int = 32,    // Number of entries in the return address stack
    icacheParams: CacheParameters = CacheParameters(),
    faubtbParams: FAMicroBTBParameters = FAMicroBTBParameters(),
    bimParams: BIMParams = BIMParams(),
    btbParams: BTBParams = BTBParams(),
) {
  require(isPow2(fetchWidth))
  require(fbNum > fetchWidth)
  val ftqIdxWidth = log2Ceil(ftqNum)
}

/** Class used for configurations
  *
  * @param issueWidth
  *   amount of things that can be issued
  * @param numEntries
  *   size of issue queue
  * @param iqType
  *   type of issue queue
  */
case class IssueParams(
    dispatchWidth: Int = 1,
    issueWidth: Int = 1,
    numEntries: Int = 8,
    iqType: UInt,
) {
  require(dispatchWidth >= issueWidth)
}

case class BackendParameters(
    coreWidth: Int = 2,   // Number of instructions piped per cycle
    retireWidth: Int = 2, // Number of instructions retired per cycle
    lregNum: Int = 32,
    pregNum: Int = 80,
    robNum: Int = 32, // Number of entries in the ROB
    mulPipeDepth: Int = 3,
    dcacheParams: CacheParameters = CacheParameters(id = 1),
    issueParams: Seq[IssueParams],
) {
  require(issueParams.length == 3)
  require(robNum % coreWidth == 0)
  require(retireWidth <= coreWidth)
  val lregWidth   = log2Ceil(lregNum)
  val pregWidth   = log2Ceil(pregNum)
  val robRowNum   = robNum / coreWidth
  val robIdxWidth = log2Ceil(robNum)
  def memIQParams = issueParams(0)
  def unqIQParams = issueParams(1)
  def intIQParams = issueParams(2)
  val wbPortNum   = issueParams.map(_.issueWidth).sum + 1
}

case class CoreParameters(
    debug: Boolean = true,
    difftest: Boolean = true,
    tlbNum: Int = 32,
)(
    implicit val commonParams: CommonParameters = CommonParameters(),
    implicit val axiParams: AXIBundleParameters = AXIBundleParameters(),
    implicit val frontendParams: FrontendParmaeters = FrontendParmaeters(),
    implicit val backendParams: BackendParameters = BackendParameters(issueParams =
      Seq(
        IssueParams(2, 1, 12, IQType.IQT_MEM.asUInt),
        IssueParams(2, 1, 12, IQType.IQT_UNQ.asUInt),
        IssueParams(2, 2, 20, IQType.IQT_INT.asUInt),
      ),
    ),
) {
  val fetchBytes = frontendParams.fetchWidth * commonParams.instBytes
  def fetchIdx(addr: UInt): UInt = {
    addr >> log2Ceil(fetchBytes)
  }
  def fetchAlign(addr: UInt) = addr &
    (Fill(commonParams.vaddrWidth - log2Ceil(fetchBytes), 1.U) ##
      Fill(log2Ceil(fetchBytes) - log2Ceil(commonParams.instBytes), 0.U) ##
      Fill(log2Ceil(commonParams.instBytes), 1.U))
  def nextFetch(addr: UInt) = {
    fetchAlign(addr) + fetchBytes.U
  }
  def fetchMask(addr: UInt) = {
    val idx = addr(log2Ceil(fetchBytes) - 1, log2Ceil(commonParams.instBytes))
    ((fetchBytes - 1).U << idx)(frontendParams.fetchWidth - 1, 0)
  }

}
