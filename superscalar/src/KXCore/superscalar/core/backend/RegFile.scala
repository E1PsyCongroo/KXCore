package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.utils._
import KXCore.superscalar._
import KXCore.superscalar.core._

abstract class RegisterFile(
    numRegisters: Int,
    numReadPorts: Int,
    numWritePorts: Int,
)(implicit params: CoreParameters)
    extends Module {
  import params.{commonParams, backendParams}
  import commonParams.{dataWidth}
  import backendParams.{pregWidth, pregNum}

  val io = IO(new Bundle {
    val read_reqs  = Vec(numReadPorts, Flipped(Decoupled(UInt(log2Ceil(numRegisters).W))))
    val read_resps = Vec(numReadPorts, Output(UInt(dataWidth.W)))

    val write_ports = Vec(
      numWritePorts,
      Flipped(Valid(new Bundle {
        val addr = UInt(pregWidth.W)
        val data = UInt(dataWidth.W)
      })),
    )

    val debug = Vec(pregNum, Output(UInt(dataWidth.W)))
  })

  // ensure there is only 1 writer per register (unless to preg0)
  if (numWritePorts > 1) {
    for (i <- 0 until (numWritePorts - 1)) {
      for (j <- (i + 1) until numWritePorts) {
        assert(
          !io.write_ports(i).valid ||
            !io.write_ports(j).valid ||
            (io.write_ports(i).bits.addr =/= io.write_ports(j).bits.addr),
          "[regfile] too many writers a register",
        )
      }
    }
  }
}

class FullyPortedRF(
    numRegisters: Int,
    numReadPorts: Int,
    numWritePorts: Int,
)(implicit params: CoreParameters)
    extends RegisterFile(numRegisters, numReadPorts, numWritePorts) {

  io.read_reqs.map(p => p.ready := true.B)

  val regfile = Mem(numRegisters, UInt(params.commonParams.dataWidth.W))

  (0 until numReadPorts) map { p =>
    io.read_resps(p) := regfile(io.read_reqs(p).bits)
    // bypass
    for (w <- 0 until numWritePorts) {
      when(io.write_ports(w).valid && io.read_reqs(p).bits === io.write_ports(w).bits.addr) {
        io.read_resps(p) := io.write_ports(w).bits.data
      }
    }
  }

  io.write_ports map { p => when(p.valid) { regfile(p.bits.addr) := p.bits.data } }

  regfile(0) := 0.U
  io.debug   := VecInit((0 until numRegisters).map(regfile(_)))
}

class PartiallyPortedRF(
    numRegisters: Int,
    numLogicalReadPorts: Int,
    numPhysicalReadPorts: Int,
    numWritePorts: Int,
)(implicit params: CoreParameters)
    extends RegisterFile(numRegisters, numLogicalReadPorts, numWritePorts) {

  val rf = Module(
    new FullyPortedRF(
      numRegisters = numRegisters,
      numReadPorts = numPhysicalReadPorts,
      numWritePorts = numWritePorts,
    ),
  )
  rf.io.write_ports := io.write_ports

  val port_issued = Array.fill(numPhysicalReadPorts) { false.B }
  val port_addrs  = Array.fill(numPhysicalReadPorts) { 0.U(log2Ceil(numRegisters).W) }
  val data_sels   = Wire(Vec(numLogicalReadPorts, UInt(numPhysicalReadPorts.W)))
  data_sels := DontCare

  for (i <- 0 until numLogicalReadPorts) {
    var read_issued = false.B
    for (j <- 0 until numPhysicalReadPorts) {
      val issue_read = WireInit(false.B)
      val use_port   = WireInit(false.B)
      when(!read_issued && !port_issued(j) && io.read_reqs(i).valid) {
        issue_read   := true.B
        use_port     := true.B
        data_sels(i) := UIntToOH(j.U)
      }
      val was_port_issued_yet = port_issued(j)
      port_issued(j) = use_port || port_issued(j)
      port_addrs(j) = port_addrs(j) | Mux(was_port_issued_yet || !use_port, 0.U, io.read_reqs(i).bits)
      read_issued = issue_read || read_issued
    }
    io.read_reqs(i).ready := PopCount(io.read_reqs.take(i).map(_.valid)) < numPhysicalReadPorts.U
    assert(!(io.read_reqs(i).fire && !read_issued))
  }

  for (j <- 0 until numPhysicalReadPorts) {
    rf.io.read_reqs(j).valid := port_issued(j)
    rf.io.read_reqs(j).bits  := port_addrs(j)
    assert(rf.io.read_reqs(j).ready)
  }

  for (i <- 0 until numLogicalReadPorts) {
    io.read_resps(i) := Mux1H(data_sels(i).asBools, rf.io.read_resps)
  }
}

// TODO BankedRF
