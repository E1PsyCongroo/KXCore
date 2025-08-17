package KXCore.superscalar

import chisel3._
import chisel3.util._

object EXUType extends ChiselEnum {
  val EXU_SLL = Value("b0000".U)
  val EXU_EQ  = Value("b0001".U)
  val EXU_SRL = Value("b0010".U)
  val EXU_SRA = Value("b0011".U)
  val EXU_NEQ = Value("b0100".U)
  val EXU_ADD = Value("b0101".U)
  val EXU_XOR = Value("b0110".U)
  val EXU_AND = Value("b0111".U)

  val EXU_SUB  = Value("b1000".U)
  val EXU_SLT  = Value("b1001".U)
  val EXU_SLTU = Value("b1010".U)
  val EXU_OR   = Value("b1011".U)
  val EXU_NOR  = Value("b1100".U)
  val EXU_SGE  = Value("b1101".U)
  val EXU_SGEU = Value("b1110".U)

  def EXU_MUL   = EXU_SLL
  def EXU_MULH  = EXU_EQ
  def EXU_MULHU = EXU_SRA
  def EXU_DIV   = EXU_SUB
  def EXU_MOD   = EXU_SLT
  def EXU_DIVU  = EXU_SLTU
  def EXU_MODU  = EXU_OR

  val EXU_LDB  = EXU_SLL
  val EXU_LDH  = EXU_EQ
  val EXU_LDBU = EXU_SRL
  val EXU_LDHU = EXU_SRA
  val EXU_LDW  = EXU_NEQ
  val EXU_STB  = EXU_SUB
  val EXU_STH  = EXU_SLT
  val EXU_STW  = EXU_SLTU

  val EXU_CPUCFG  = EXU_SLL
  val EXU_TLBNONE = EXU_SLL
  val EXU_TLBSRCH = EXU_EQ
  val EXU_TLBRD   = EXU_SRL
  val EXU_TLBWR   = EXU_SRA
  val EXU_TLBFILL = EXU_NEQ
  val EXU_INVTLB  = EXU_ADD
  val EXU_RDCNTID = EXU_AND
  val EXU_RDCNTVL = EXU_SUB
  val EXU_RDCNTVH = EXU_SLT
  val EXU_CSRRD   = EXU_SLTU
  val EXU_CSRXCHG = EXU_NOR
  val EXU_CSRWR   = EXU_SGE

  def isSub(cmd: UInt)           = cmd(3)
  def isCmp(cmd: UInt)           = cmd(3) & (cmd(0) ^ cmd(1))
  def cmpUnsigned(cmd: UInt)     = cmd(1)
  def cmpInverted(cmd: UInt)     = cmd(2)
  def cmpEq(cmd: UInt)           = !cmd(3)
  def shiftReverse(cmd: UInt)    = !cmd(1)
  def shiftArith(cmd: UInt)      = cmd(0)
  def mul_divUnsigned(cmd: UInt) = cmd(1)
  def ismulh_mod(cmd: UInt)      = cmd(0)
  def csrWen(cmd: UInt)          = cmd(3) & cmd(2)
  def isSotre(cmd: UInt)         = cmd(3)
}

// RS1 Operand Select Signal
object OP1Type extends ChiselEnum {
  val OP1_RS1  = Value // Register Source #1
  val OP1_PC   = Value // Program Counter
  val OP1_ZERO = Value
}

// RS2 Operand Select Signal
object OP2Type extends ChiselEnum {
  val OP2_RS2  = Value // Register Source #2
  val OP2_IMM  = Value // immediate
  val OP2_NEXT = Value // constant 4 (for PC+4)
}

object IMMType extends ChiselEnum {
  val LONGEST_IMM_WIDTH = 26
  val IMM_5U            = Value
  val IMM_12            = Value
  val IMM_12U           = Value
  val IMM_14U           = Value
  val IMM_15U           = Value
  val IMM_16            = Value
  val IMM_20            = Value
  val IMM_26            = Value
}

object IQType extends ChiselEnum {
  val IQT_INT = Value(0.U)
  val IQT_UNQ = Value(1.U)
  val IQT_MEM = Value(2.U)
}

object FUType extends ChiselEnum {
  // bit mask, since a given execution pipeline may support multiple functional units
  val FUT_ALU = Value(1.U)
  val FUT_CFI = Value(2.U)
  val FUT_MEM = Value(4.U)
  val FUT_MUL = Value(8.U)
  val FUT_DIV = Value(16.U)
  val FUT_CSR = Value(32.U)
}

object CFIType extends ChiselEnum {
  val CFI_NONE = Value("b000".U)
  val CFI_BR   = Value("b001".U)
  val CFI_B    = Value("b010".U)
  val CFI_JIRL = Value("b100".U)

  def isBr(cfiType: UInt)   = cfiType(0)
  def isB(cfiType: UInt)    = cfiType(1)
  def isJIRL(cfiType: UInt) = cfiType(2)
}

object REDIRECTType extends ChiselEnum {
  val REDIRECT_MISPREDICT = Value
  val REDIRECT_EXCEPTION  = Value
  val REDIRECT_ETRN       = Value
  val REDIRECT_NEXT       = Value
}
