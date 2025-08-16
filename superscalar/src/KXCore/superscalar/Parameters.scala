package KXCore.superscalar

import chisel3._
import chisel3.util._
import scala.math._
import KXCore.common.CommonParameters
import KXCore.common.peripheral.{AXIBundleParameters, CacheParameters}

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
    offsetWidth: Int = 16,
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
    retireWidth: Int = 1, // Number of instructions retired per cycle
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
  val wbPortNum   = issueParams.map(_.issueWidth).sum + 2
}

case class CoreParameters(
    debug: Boolean = true,
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
  def fetchAlign(addr: UInt) = addr & ~(fetchBytes - 1).U(commonParams.vaddrWidth.W)
  def nextFetch(addr: UInt) = {
    fetchAlign(addr) + fetchBytes.U
  }
  def fetchMask(addr: UInt) = {
    val idx = addr(log2Ceil(fetchBytes) - 1, log2Ceil(commonParams.instBytes))
    ((fetchBytes - 1).U << idx)(frontendParams.fetchWidth - 1, 0)
  }

}
