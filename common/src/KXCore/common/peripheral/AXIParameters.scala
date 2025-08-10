package KXCore.common.peripheral

import chisel3._
import chisel3.util._
import scala.math.max

object AXIParameters {
  // These are all fixed by the AXI standard:
  val wlastBits = 1
  val rlastBIts = 1
  val lenBits   = 4
  val sizeBits  = 3
  val burstBits = 2
  val respBits  = 2
  val lockBits  = 2
  val cacheBits = 4
  val protBits  = 3

  def CACHE_WALLOCATE  = 8.U(cacheBits.W)
  def CACHE_RALLOCATE  = 4.U(cacheBits.W)
  def CACHE_CACHEABLE  = 2.U(cacheBits.W)
  def CACHE_BUFFERABLE = 1.U(cacheBits.W)

  def PROT_PRIVILEGED  = 1.U(protBits.W)
  def PROT_INSECURE    = 2.U(protBits.W)
  def PROT_INSTRUCTION = 4.U(protBits.W)

  def BURST_FIXED = 0.U(burstBits.W)
  def BURST_INCR  = 1.U(burstBits.W)
  def BURST_WRAP  = 2.U(burstBits.W)

  def RESP_OKAY   = 0.U(respBits.W)
  def RESP_EXOKAY = 1.U(respBits.W)
  def RESP_SLVERR = 2.U(respBits.W)
  def RESP_DECERR = 3.U(respBits.W)
}

case class AXIBundleParameters(
    addrBits: Int = 32,
    dataBits: Int = 32,
    idBits: Int = 4,
    wlastPresent: Boolean = true,
    rlastPresent: Boolean = true,
    sizePresent: Boolean = true,
    lenPrenset: Boolean = true,
    burstPresent: Boolean = true,
    wstrbPresent: Boolean = true,
    brespPresent: Boolean = true,
    rrespPresent: Boolean = true,
    cachePrenset: Boolean = true,
    protPrensent: Boolean = true,
    exclusiveAccesses: Boolean = true,
) {
  require(dataBits >= 8, s"AXI data bits must be >= 8 (got $dataBits)")
  require(addrBits >= 1, s"AXI addr bits must be >= 1 (got $addrBits)")
  require(idBits >= 1, s"AXI id bits must be >= 1 (got $idBits)")
  require(isPow2(dataBits), s"AXI data bits must be pow2 (got $dataBits)")

  // Bring the globals into scope
  val wlastBits = if (wlastPresent) AXIParameters.wlastBits else 0
  val rlastBits = if (rlastPresent) AXIParameters.rlastBIts else 0
  val sizeBits  = if (sizePresent) AXIParameters.sizeBits else 0
  val lenBits   = if (lenPrenset) AXIParameters.lenBits else 0
  val burstBits = if (burstPresent) AXIParameters.burstBits else 0
  val wstrBits  = if (wstrbPresent) dataBits / 8 else 0
  val brespBits = if (brespPresent) AXIParameters.respBits else 0
  val rrespBits = if (rrespPresent) AXIParameters.respBits else 0
  val cacheBits = if (cachePrenset) AXIParameters.cacheBits else 0
  val protBits  = if (protPrensent) AXIParameters.protBits else 0
  val lockBits  = if (exclusiveAccesses) AXIParameters.lockBits else 0

  def union(x: AXIBundleParameters) =
    AXIBundleParameters(
      max(addrBits, x.addrBits),
      max(dataBits, x.dataBits),
      max(idBits, x.idBits),
      wlastPresent && x.wlastPresent,
      rlastPresent && x.rlastPresent,
      sizePresent && x.sizePresent,
      lenPrenset && x.lenPrenset,
      burstPresent && x.burstPresent,
      wstrbPresent && x.wstrbPresent,
      brespPresent && x.brespPresent,
      rrespPresent && x.rrespPresent,
      cachePrenset && x.cachePrenset,
      protPrensent && x.protPrensent,
      exclusiveAccesses && x.exclusiveAccesses,
    )
}
