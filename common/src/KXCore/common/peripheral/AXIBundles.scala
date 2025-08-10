package KXCore.common.peripheral

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._

abstract class AXIBundleBase(params: AXIBundleParameters) extends Bundle

/** Common signals of AW and AR channels of AXI protocol
  */
abstract class AXIBundleA(params: AXIBundleParameters) extends AXIBundleBase(params) {
  val id    = UInt(params.idBits.W)
  val addr  = UInt(params.addrBits.W)
  val len   = UInt(params.lenBits.W)  // number of beats - 1
  val size  = UInt(params.sizeBits.W) // bytes in beat = 2^size
  val burst = UInt(params.burstBits.W)
  val lock  = UInt(params.lockBits.W)
  val cache = UInt(params.cacheBits.W)
  val prot  = UInt(params.protBits.W)

  // Number of bytes-1 in this operation
  def bytes1(x: Int = 0) = {
    val maxShift = 1 << params.sizeBits
    val tail     = ((BigInt(1) << maxShift) - 1).U
    (Cat(len, tail) << size) >> maxShift
  }
}

/** AW channel of AXI protocol
  */
class AXIBundleAW(params: AXIBundleParameters) extends AXIBundleA(params)

/** AR channel of AXI protocol
  */
class AXIBundleAR(params: AXIBundleParameters) extends AXIBundleA(params)

/** W channel of AXI protocol
  */
class AXIBundleW(params: AXIBundleParameters) extends AXIBundleBase(params) {
  // id ... removed in AXI
  val id   = UInt(params.idBits.W)
  val data = UInt(params.dataBits.W)
  val strb = UInt((params.wstrBits).W)
  val last = UInt(params.wlastBits.W)
}

/** R channel of AXI protocol
  */
class AXIBundleR(params: AXIBundleParameters) extends AXIBundleBase(params) {
  val id   = UInt(params.idBits.W)
  val data = UInt(params.dataBits.W)
  val resp = UInt(params.rrespBits.W)
  val last = UInt(params.rlastBits.W)
}

/** B channel of AXI protocol
  */
class AXIBundleB(params: AXIBundleParameters) extends AXIBundleBase(params) {
  val id   = UInt(params.idBits.W)
  val resp = UInt(params.brespBits.W)
}

/** AXI protocol bundle
  */
class AXIBundle(params: AXIBundleParameters) extends AXIBundleBase(params) {
  val axi4params = params
  val aw         = Irrevocable(new AXIBundleAW(params))
  val w          = Irrevocable(new AXIBundleW(params))
  val b          = Flipped(Irrevocable(new AXIBundleB(params)))
  val ar         = Irrevocable(new AXIBundleAR(params))
  val r          = Flipped(Irrevocable(new AXIBundleR(params)))
}

class StandardAXIBundle(params: AXIBundleParameters) extends Bundle {
  val axi4params = params
  val awready    = Input(Bool())
  val awvalid    = Output(Bool())
  val awid       = Output(UInt(params.idBits.W))
  val awaddr     = Output(UInt(params.addrBits.W))
  val awlen      = Output(UInt(params.lenBits.W))
  val awsize     = Output(UInt(params.sizeBits.W))
  val awburst    = Output(UInt(params.burstBits.W))
  val awlock     = Output(UInt(params.lockBits.W))
  val awcache    = Output(UInt(params.cacheBits.W))
  val awprot     = Output(UInt(params.protBits.W))

  val wready = Input(Bool())
  val wvalid = Output(Bool())
  val wid    = Output(UInt(params.idBits.W))
  val wdata  = Output(UInt(params.dataBits.W))
  val wstrb  = Output(UInt(params.wstrBits.W))
  val wlast  = Output(UInt(params.wlastBits.W))

  val bready = Output(Bool())
  val bvalid = Input(Bool())
  val bid    = Input(UInt(params.idBits.W))
  val bresp  = Input(UInt(params.brespBits.W))

  val arready = Input(Bool())
  val arvalid = Output(Bool())
  val arid    = Output(UInt(params.idBits.W))
  val araddr  = Output(UInt(params.addrBits.W))
  val arlen   = Output(UInt(params.lenBits.W))
  val arsize  = Output(UInt(params.sizeBits.W))
  val arburst = Output(UInt(params.burstBits.W))
  val arlock  = Output(UInt(params.lockBits.W))
  val arcache = Output(UInt(params.cacheBits.W))
  val arprot  = Output(UInt(params.protBits.W))

  val rready = Output(Bool())
  val rvalid = Input(Bool())
  val rid    = Input(UInt(params.idBits.W))
  val rdata  = Input(UInt(params.dataBits.W))
  val rresp  = Input(UInt(params.rrespBits.W))
  val rlast  = Input(UInt(params.rlastBits.W))
}

object AXIBundle {
  def apply(params: AXIBundleParameters) = new AXIBundle(params)
  implicit val axiView: DataView[StandardAXIBundle, AXIBundle] = DataView(
    std => new AXIBundle(std.axi4params),
    // AW
    _.awready -> _.aw.ready,
    _.awvalid -> _.aw.valid,
    _.awid    -> _.aw.bits.id,
    _.awaddr  -> _.aw.bits.addr,
    _.awlen   -> _.aw.bits.len,
    _.awsize  -> _.aw.bits.size,
    _.awburst -> _.aw.bits.burst,
    _.awlock  -> _.aw.bits.lock,
    _.awcache -> _.aw.bits.cache,
    _.awprot  -> _.aw.bits.prot,
    // W
    _.wready -> _.w.ready,
    _.wvalid -> _.w.valid,
    _.wid    -> _.w.bits.id,
    _.wdata  -> _.w.bits.data,
    _.wstrb  -> _.w.bits.strb,
    _.wlast  -> _.w.bits.last,
    // B
    _.bready -> _.b.ready,
    _.bvalid -> _.b.valid,
    _.bid    -> _.b.bits.id,
    _.bresp  -> _.b.bits.resp,
    // AR
    _.arready -> _.ar.ready,
    _.arvalid -> _.ar.valid,
    _.arid    -> _.ar.bits.id,
    _.araddr  -> _.ar.bits.addr,
    _.arlen   -> _.ar.bits.len,
    _.arsize  -> _.ar.bits.size,
    _.arburst -> _.ar.bits.burst,
    _.arlock  -> _.ar.bits.lock,
    _.arcache -> _.ar.bits.cache,
    _.arprot  -> _.ar.bits.prot,
    // R
    _.rready -> _.r.ready,
    _.rvalid -> _.r.valid,
    _.rid    -> _.r.bits.id,
    _.rdata  -> _.r.bits.data,
    _.rresp  -> _.r.bits.resp,
    _.rlast  -> _.r.bits.last,
  )
}

object StandardAXIBundle {
  def apply(params: AXIBundleParameters) = new StandardAXIBundle(params)
  implicit val axiView: DataView[AXIBundle, StandardAXIBundle] =
    AXIBundle.axiView.invert(axi => StandardAXIBundle(axi.axi4params))
}
