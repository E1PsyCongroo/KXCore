package KXCore.superscalar.core

import chisel3._
import chisel3.util._
import KXCore.common.Privilege._
import KXCore.superscalar._

/** MicroOp passing through the pipeline
  */
class MicroOp(implicit params: CoreParameters) extends Bundle {
  import params.{commonParams, axiParams, frontendParams, backendParams}
  import commonParams.{instWidth, dataWidth, vaddrWidth, paddrWidth}
  import frontendParams.{fetchWidth, icacheParams, ftqIdxWidth}
  import backendParams.{lregWidth, pregWidth, robIdxWidth}

  val idx     = UInt(log2Ceil(fetchWidth).W)
  val inst    = UInt(instWidth.W)
  val iqType  = UInt(IQType.getWidth.W) // which issue unit do we use?
  val fuType  = UInt(FUType.getWidth.W) // which functional unit do we use?
  val cfiType = UInt(CFIType.getWidth.W)

  val ftqIdx = UInt(ftqIdxWidth.W)

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

  val robIdx    = UInt(robIdxWidth.W)
  val exception = Bool()
  val ecode     = UInt(ECODE.getWidth.W)
  val badv      = UInt(vaddrWidth.W)
  val exuCmd    = UInt(EXUType.getWidth.W)
  val csrCmd    = UInt(CSRType.getWidth.W)
  val lsuCmd    = UInt(LSUType.getWidth.W)
  val ertn      = Bool()
  val isUnique  = Bool() // only allow this instruction in the pipeline, tell ROB to un-ready until empty
  val flush     = Bool() // some instructions need to flush the pipeline behind them
  val busy      = Bool() // need execute?

  val debug = new Bundle {
    val pc         = UInt(vaddrWidth.W)
    val inst       = UInt(instWidth.W)
    val store      = UInt(8.W)
    val storePaddr = UInt(paddrWidth.W)
    val storeVaddr = UInt(vaddrWidth.W)
    val storeData  = UInt(dataWidth.W)
    val load       = UInt(8.W)
    val loadPaddr  = UInt(paddrWidth.W)
    val loadVaddr  = UInt(vaddrWidth.W)
    val loadData   = UInt(dataWidth.W)
    val wdata      = UInt(dataWidth.W)
  }
}
