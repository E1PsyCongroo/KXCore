package KXCore.superscalar.core.frontend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.Privilege._
import KXCore.common.peripheral._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._

class FetchBundle(implicit params: CoreParameters) extends Bundle {
  import params.commonParams.{vaddrWidth, instWidth}
  import params.frontendParams._
  val pc        = UInt(vaddrWidth.W)
  val npc       = UInt(vaddrWidth.W)
  val pcs       = Vec(fetchWidth, UInt(vaddrWidth.W))
  val insts     = Vec(fetchWidth, UInt(instWidth.W))
  val mask      = UInt(fetchWidth.W) // mark which words are valid instructions
  val brMask    = UInt(fetchWidth.W)
  val bMask     = UInt(fetchWidth.W)
  val jirlMask  = UInt(fetchWidth.W)
  val cfiIdx    = Valid(UInt(log2Ceil(fetchWidth).W))
  val ftqIdx    = UInt(ftqIdxWidth.W)
  val exception = Valid(UInt(ECODE.getWidth.W))
  val bpuMeta = new Bundle {
    val bim = Vec(fetchWidth, UInt(2.W))
    val btb = UInt(log2Ceil(btbParams.nWays).W)
  }
}

// A branch prediction for a single instruction
class BranchPrediction(implicit params: CoreParameters) extends Bundle {
  // Is this a branch?
  val isBr = Bool()
  // If this is a branch, do we take it?
  val taken = Bool()
  // Is this a B/BL/JIRL?
  val isJmp = Bool()
  // What is the target of his branch/jump? Do we know the target?
  val target = Valid(UInt(params.commonParams.vaddrWidth.W))
}

// A branch update for a fetch-width worth of instructions
class BranchPredictionUpdate(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, frontendParams}
  import commonParams.{vaddrWidth}
  import frontendParams._
  val valid   = Bool()
  val fetchPC = UInt(vaddrWidth.W)
  val brMask  = UInt(fetchWidth.W)
  // Which CFI was taken/mispredicted (if any)
  val cfiIdx = Valid(UInt(log2Ceil(fetchWidth).W))
  // Was the cfi a br?
  val cfiIsBr = Bool()
  // Was the cfi a b/bl?
  val cfiIsB = Bool()
  // Was the cfi a jirl?
  val cfiIsJirl = Bool()
  // What did this CFI jump to?
  val target = UInt(vaddrWidth.W)
  val meta = new Bundle {
    val bim = Vec(fetchWidth, UInt(2.W))
    val btb = UInt(log2Ceil(frontendParams.btbParams.nWays).W)
  }
}

class FetchBufferResp(implicit params: CoreParameters) extends Bundle {
  val uops = Vec(params.backendParams.coreWidth, Valid(new MicroOp()))
}

/** Bundle to add to the FTQ RAM and to be used as the pass in IO
  */
class FTQBundle(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, frontendParams}
  import commonParams.{vaddrWidth}
  import frontendParams.{fetchWidth}
  val fetchPC   = UInt(vaddrWidth.W)
  val brMask    = UInt(fetchWidth.W)
  val cfiIdx    = Valid(UInt(log2Ceil(fetchWidth).W))
  val cfiIsB    = Bool()
  val cfiIsJirl = Bool()
  val cfiIsBr   = Bool()
  val meta = new Bundle {
    val bim = Vec(fetchWidth, UInt(2.W))
    val btb = UInt(log2Ceil(frontendParams.btbParams.nWays).W)
  }
}

class FTQInfo(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, frontendParams}
  val valid = Bool()
  val entry = new FTQBundle
}
