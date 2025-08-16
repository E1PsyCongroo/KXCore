package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.superscalar._
import KXCore.superscalar.core._

/** IO bundle to interact with Issue slot
  */
class IssueSlotIO(implicit params: CoreParameters) extends Bundle {
  import params.{backendParams}
  import backendParams.{pregWidth, wbPortNum}

  val valid      = Output(Bool())
  val request    = Output(Bool())
  val request_hp = Output(Bool()) // high priority
  val grant      = Input(Bool())

  val kill  = Input(Bool()) // pipeline flush
  val clear = Input(Bool()) // entry being moved elsewhere (not mutually exclusive with grant)

  val wakeup_ports = Flipped(Vec(wbPortNum, Valid(UInt(pregWidth.W))))
  val in_uop       = Flipped(Valid(new MicroOp())) // if valid, this WILL overwrite an entry!
  val out_uop      = Output(new MicroOp())         // the updated slot uop; will be shifted upwards in a collasping queue.
  val uop          = Output(new MicroOp())         // the current Slot's uop. Sent down the pipeline when issued.
}

/** Single issue slot. Holds a uop within the issue queue
  *
  * @param numWakeupPorts
  *   number of wakeup ports
  */
class IssueSlot(implicit params: CoreParameters) extends Module {
  import params.backendParams.{wbPortNum}

  val io = IO(new IssueSlotIO)

  val valid = RegInit(false.B)
  val p1    = RegInit(false.B)
  val p2    = RegInit(false.B)

  val slot_uop = Reg(new MicroOp)
  val next_uop = Mux(io.in_uop.valid, io.in_uop.bits, slot_uop)

  valid := MuxCase(
    valid,
    Seq(
      io.kill                                                               -> false.B,
      (io.in_uop.valid && io.in_uop.bits.busy && !io.in_uop.bits.exception) -> true.B,
      (io.grant || io.clear)                                                -> false.B,
    ),
  )

  when(io.in_uop.valid) {
    slot_uop := io.in_uop.bits
    assert(!valid || io.clear || io.kill, "trying to overwrite a valid issue slot.")
  }

  when(io.in_uop.valid) {
    p1 := io.in_uop.bits.lrs1 === 0.U || !io.in_uop.bits.prs1Busy
    p2 := io.in_uop.bits.lrs2 === 0.U || !io.in_uop.bits.prs2Busy
  }

  for (i <- 0 until wbPortNum) {
    when(io.wakeup_ports(i).valid && (io.wakeup_ports(i).bits === next_uop.prs1)) {
      p1 := true.B
    }
    when(io.wakeup_ports(i).valid && (io.wakeup_ports(i).bits === next_uop.prs2)) {
      p2 := true.B
    }
  }

  io.request := valid && p1 && p2 && !io.kill
  val high_priority = slot_uop.isB || slot_uop.isBr || slot_uop.isJirl
  io.request_hp := io.request && high_priority

  io.valid := valid
  io.uop   := slot_uop

  io.out_uop          := slot_uop
  io.out_uop.prs1Busy := !p1
  io.out_uop.prs2Busy := !p2
}
