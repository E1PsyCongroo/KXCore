package KXCore.superscalar.core.backend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.common.Privilege._
import KXCore.common.utils._
import KXCore.common.peripheral._
import KXCore.superscalar._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._
import EXUType._

class LoadStoreUnit(implicit params: CoreParameters) extends Module {
  import params.{commonParams, axiParams}
  import commonParams.{vaddrWidth, paddrWidth, dataWidth}

  val io = IO(new Bundle {
    val axi = new AXIBundle(axiParams)
    val req = Flipped(Decoupled(new Bundle {
      val uop       = new MicroOp
      val vaddr     = UInt(vaddrWidth.W)
      val paddr     = UInt(paddrWidth.W)
      val isWrite   = Bool()
      val wmask     = UInt((dataWidth / 8).W)
      val writeData = UInt(dataWidth.W)
    }))
    val resp = Decoupled(new ExeUnitResp)
  })

  // assume store never flush
  val sHandleReq :: sWaitAWfire :: sWaitWfire :: sWaitResp :: sIgnore :: sSendResp :: Nil = Enum(6)

  val state = RegInit(sHandleReq)
  state := MuxLookup(state, sHandleReq)(
    Seq(
      sHandleReq -> MuxCase(
        sHandleReq,
        Seq(
          (!io.req.valid)                   -> sHandleReq,
          (io.axi.ar.fire)                  -> sWaitResp,
          (io.axi.aw.fire && io.axi.w.fire) -> sHandleReq,
          (io.axi.aw.fire)                  -> sWaitWfire,
          (io.axi.w.fire)                   -> sWaitAWfire,
        ),
      ),
      sWaitAWfire -> Mux(io.axi.aw.fire, sHandleReq, sWaitAWfire),
      sWaitWfire  -> Mux(io.axi.w.fire, sHandleReq, sWaitWfire),
      sWaitResp -> Mux(
        io.req.valid,
        Mux(io.axi.r.fire && io.axi.r.bits.id === 1.U, sSendResp, sWaitResp),
        Mux(io.axi.r.fire && io.axi.r.bits.id === 1.U, sHandleReq, sIgnore),
      ),
      sIgnore   -> Mux(io.axi.r.fire && io.axi.r.bits.id === 1.U, sHandleReq, sIgnore),
      sSendResp -> Mux(!io.req.valid || io.resp.ready, sHandleReq, sSendResp),
    ),
  )

  io.axi.ar.valid      := state === sHandleReq && io.req.valid && !io.req.bits.isWrite
  io.axi.ar.bits.addr  := io.req.bits.paddr
  io.axi.ar.bits.id    := 1.U
  io.axi.ar.bits.len   := 0.U
  io.axi.ar.bits.size  := log2Ceil(dataWidth / 8).U
  io.axi.ar.bits.burst := AXIParameters.BURST_INCR
  io.axi.ar.bits.lock  := 0.U
  io.axi.ar.bits.cache := 0.U
  io.axi.ar.bits.prot  := 0.U

  io.axi.r.ready := (state === sWaitResp) || (state === sIgnore)

  io.axi.aw.valid      := (state === sHandleReq && io.req.valid && io.req.bits.isWrite) || (state === sWaitAWfire)
  io.axi.aw.bits.addr  := io.req.bits.paddr
  io.axi.aw.bits.id    := 1.U
  io.axi.aw.bits.len   := 0.U
  io.axi.aw.bits.size  := log2Ceil(dataWidth / 8).U
  io.axi.aw.bits.burst := AXIParameters.BURST_INCR
  io.axi.aw.bits.lock  := 0.U
  io.axi.aw.bits.cache := 0.U
  io.axi.aw.bits.prot  := 0.U

  io.axi.w.valid     := (state === sHandleReq && io.req.valid && io.req.bits.isWrite) || (state === sWaitWfire)
  io.axi.w.bits.id   := 1.U
  io.axi.w.bits.last := 1.U
  io.axi.w.bits.data := io.req.bits.writeData
  io.axi.w.bits.strb := Mux(io.req.valid, io.req.bits.wmask, 0.U)

  io.axi.b.ready := true.B

  val loffset = io.req.bits.paddr(1, 0) << 3.U
  val lshift  = io.axi.r.bits.data >> loffset
  val rdata = RegEnable(
    MuxCase(
      lshift,
      Seq(
        EXU_LDB  -> Fill(24, lshift(7)) ## lshift(7, 0),
        EXU_LDH  -> Fill(16, lshift(15)) ## lshift(15, 0),
        EXU_LDHU -> Fill(16, 0.U(1.W)) ## lshift(15, 0),
        EXU_LDBU -> Fill(24, 0.U(1.W)) ## lshift(7, 0),
      ).map { case (key, data) => (io.req.bits.uop.exuCmd === key.asUInt, data) },
    ),
    io.axi.r.fire && io.axi.r.bits.id === 1.U,
  )

  io.req.ready := io.resp.fire

  io.resp.valid := MuxLookup(state, false.B)(
    Seq(
      sHandleReq  -> (io.axi.aw.fire && io.axi.w.fire),
      sWaitAWfire -> io.axi.aw.fire,
      sWaitWfire  -> io.axi.w.fire,
      sSendResp   -> io.req.valid,
    ),
  )
  io.resp.bits.uop  := io.req.bits.uop
  io.resp.bits.data := rdata

  if (params.debug) {
    dontTouch(state)
  }
}
