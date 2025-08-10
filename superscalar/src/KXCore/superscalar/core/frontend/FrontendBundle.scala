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
  val pc     = UInt(vaddrWidth.W)
  val npc    = UInt(vaddrWidth.W)
  val pcs    = Vec(fetchWidth, UInt(vaddrWidth.W))
  val insts  = Vec(fetchWidth, UInt(instWidth.W))
  val mask   = UInt(fetchWidth.W) // mark which words are valid instructions
  val cfiIdx = Valid(UInt(log2Ceil(fetchWidth).W))

  val ftqIdx = UInt(log2Ceil(ftqNum).W)

  val exception = Valid(UInt(ECODE.getWidth.W))

  // val ghist = new GlobalHistory
  // val lhist = Vec(nBanks, UInt(localHistoryLength.W))

  // val xcpt_pf_if = Bool() // I-TLB miss (instruction fetch fault).
  // val xcpt_ae_if = Bool() // Access exception.

  // val bp_debug_if_oh = Vec(fetchWidth, Bool())
  // val bp_xcpt_if_oh  = Vec(fetchWidth, Bool())

  // val end_half = Valid(UInt(16.W))

  val bpuMeta = new Bundle {
    val bim = Vec(params.frontendParams.fetchWidth, UInt(2.W))
    val btb = UInt(log2Ceil(params.frontendParams.btbParams.nWays).W)
  }
}

// A branch prediction for a single instruction
class BranchPrediction(implicit params: CoreParameters) extends Bundle {
  // Is this a branch?
  val isBr = Bool()
  // Is this a B/BL?
  val isJmp = Bool()
  // If this is a cfi, do we take it?
  val taken = Bool()
  // What is the target of his branch/jump? Do we know the target?
  val target = Valid(UInt(params.commonParams.vaddrWidth.W))
}

// A branch prediction for a entire fetch-width worth of instructions
// This is typically merged from individual predictions from the banked
// predictor
class BranchPredictionBundle(implicit params: CoreParameters) extends Bundle {
  val pc    = UInt(params.commonParams.vaddrWidth.W)
  val preds = Vec(params.frontendParams.fetchWidth, new BranchPrediction)
}

// A branch update for a fetch-width worth of instructions
class BranchPredictionUpdate(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, frontendParams}
  import commonParams.{vaddrWidth}
  import frontendParams._
  val fetchPC = UInt(vaddrWidth.W)
  // Which CFI was taken/mispredicted (if any)
  val cfiIdx = UInt(log2Ceil(fetchWidth).W)
  // Was the cfi taken?
  val cfiTaken = Bool()
  // Was the cfi a br?
  val cfiIsBr = Bool()
  // Was the cfi a b/bl?
  val cfiIsB = Bool()
  // Was the cfi a jirl?
  val cfiIsJirl = Bool()

  // What did this CFI jump to?
  val target = UInt(vaddrWidth.W)

  val meta = new Bundle {
    val bim = Vec(params.frontendParams.fetchWidth, UInt(2.W))
    val btb = UInt(log2Ceil(params.frontendParams.btbParams.nWays).W)
  }
}

class BrUpdateInfo(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, axiParams, frontendParams}
  val mispredict = Bool()
  val cfiIdx     = Valid(UInt(log2Ceil(frontendParams.fetchWidth).W))
  val cfiIsB     = Bool()
  val cfiIsJirl  = Bool()
  val cfiIsBr    = Bool()
  val target     = UInt(commonParams.vaddrWidth.W)
}

class FetchBufferResp(implicit params: CoreParameters) extends Bundle {
  val uops = Vec(params.backendParams.coreWidth, Valid(new MicroOp()))
}

/** Bundle to add to the FTQ RAM and to be used as the pass in IO
  */
class FTQBundle(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, frontendParams}
  val fetchPC = UInt(commonParams.vaddrWidth.W)
  val taken   = Bool()
  val cfiIdx  = UInt(log2Ceil(frontendParams.fetchWidth).W)
  val meta = new Bundle {
    val bim = Vec(params.frontendParams.fetchWidth, UInt(2.W))
    val btb = UInt(log2Ceil(params.frontendParams.btbParams.nWays).W)
  }
}

class FTQInfo(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, frontendParams}
  val valid = Bool()
  val entry = new FTQBundle
}

/** IO to provide a port for a FunctionalUnit to get the PC of an instruction. And for JIRL, the PC of the next instruction.
  */
class GetPCFromFtqIO(implicit params: CoreParameters) extends Bundle {
  val ftqIdx = Flipped(UInt(log2Ceil(params.frontendParams.ftqNum).W))
  val info   = new FTQInfo
}
