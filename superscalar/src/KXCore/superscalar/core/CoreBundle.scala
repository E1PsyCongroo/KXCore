package KXCore.superscalar.core

import chisel3._
import chisel3.util._
import KXCore.common.Privilege._
import KXCore.superscalar._

/** MicroOp passing through the pipeline
  */
class MicroOp(implicit params: CoreParameters) extends Bundle {
  import params.{fetchBytes, commonParams, axiParams, frontendParams, backendParams}
  import commonParams.{instWidth, dataWidth, vaddrWidth, paddrWidth}
  import frontendParams.{fetchWidth, icacheParams, ftqIdxWidth}
  import backendParams.{lregWidth, pregWidth, robIdxWidth}

  val pcLow  = UInt(log2Ceil(fetchBytes).W)
  val inst   = UInt(instWidth.W)
  val iqType = UInt(IQType.getWidth.W) // which issue unit do we use?
  val fuType = UInt(FUType.getWidth.W) // which functional unit do we use?
  val ftqIdx = UInt(ftqIdxWidth.W)

  val isB    = Bool()
  val isJirl = Bool()
  val isBr   = Bool()
  val isErtn = Bool()
  val isIBar = Bool()
  val isIdle = Bool()

  val op1Sel = UInt(OP1Type.getWidth.W)
  val op2Sel = UInt(OP2Type.getWidth.W)

  val imm = UInt(dataWidth.W) // densely pack the imm in decode

  val ldst = UInt(lregWidth.W) // logical destination register
  val lrs1 = UInt(lregWidth.W) // logical source register 1
  val lrs2 = UInt(lregWidth.W) // logical source register 2

  val stalePdst = UInt(pregWidth.W) // stale physical destination register
  val pdst      = UInt(pregWidth.W) // physical destination register
  val prs1      = UInt(pregWidth.W) // physical source register 1
  val prs2      = UInt(pregWidth.W) // physical source register 2
  val prs1Busy  = Bool()
  val prs2Busy  = Bool()

  val robIdx        = UInt(robIdxWidth.W)
  val exception     = Bool()
  val ecode         = UInt(ECODE.getWidth.W)
  val badv          = UInt(vaddrWidth.W)
  val exuCmd        = UInt(EXUType.getWidth.W)
  val cacopCode     = UInt(5.W)
  val isUnique      = Bool() // only allow this instruction in the pipeline, tell ROB to un-ready until empty
  val flushOnCommit = Bool() // some instructions need to flush the pipeline behind them
  val busy          = Bool() // need execute?

  val debug = new Bundle {
    val pc             = UInt(vaddrWidth.W)
    val inst           = UInt(instWidth.W)
    val is_TLBFILL     = Bool()
    val TLBFILL_index  = UInt(5.W)
    val is_CNTinst     = Bool()
    val timer_64_value = UInt(64.W)
    val wen            = Bool()
    val wdest          = UInt(8.W)
    val wdata          = UInt(dataWidth.W)
    val csr_rstat      = Bool()
    val csr_data       = UInt(32.W)

    val eret          = Bool()
    val cause         = UInt(32.W)
    val exceptionPC   = UInt(32.W)
    val exceptionInst = UInt(32.W)

    val store      = UInt(8.W)
    val storePaddr = UInt(paddrWidth.W)
    val storeVaddr = UInt(vaddrWidth.W)
    val storeData  = UInt(dataWidth.W)

    val load      = UInt(8.W)
    val loadPaddr = UInt(paddrWidth.W)
    val loadVaddr = UInt(vaddrWidth.W)
    val loadData  = UInt(dataWidth.W)
  }
}
