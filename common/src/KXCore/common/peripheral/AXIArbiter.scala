package KXCore.common.peripheral

import chisel3._
import chisel3.util._
import KXCore.common.utils._

class AXIInterconnect(params: AXIBundleParameters, fanInNum: Int, fanOutArea: Seq[Seq[AddressSet]], fanOutTrans: Seq[Boolean]) extends Module {
  require(fanOutArea.length == fanOutTrans.length)
  val fanOutNum = fanOutArea.length

  val io = IO(new Bundle {
    val fanIn  = Vec(fanInNum, Flipped(AXIBundle(params)))
    val fanOut = Vec(fanOutNum, AXIBundle(params))
  })

  val sWriteIdle :: sWaitBresp :: Nil = Enum(2)
  val sReadIdle :: sWaitRresp :: Nil  = Enum(2)

  val writeState  = RegInit(sWriteIdle)
  val isWriteIdle = writeState === sWriteIdle
  val isWaitBresp = writeState === sWaitBresp

  val readState   = RegInit(sReadIdle)
  val isReadIdle  = readState === sReadIdle
  val isWaitRresp = readState === sWaitRresp

  val writeRequests = VecInit(io.fanIn.map(in => in.aw.valid)).asUInt
  val readRequests  = VecInit(io.fanIn.map(in => in.ar.valid)).asUInt

  val writeSelected    = PriorityEncoder(writeRequests)
  val writeSelectedReg = RegEnable(writeSelected, isWriteIdle)
  val write            = writeRequests =/= 0.U

  val readSelected    = PriorityEncoder(readRequests)
  val readSelectedReg = RegEnable(readSelected, isReadIdle)
  val read            = readRequests =/= 0.U

  val awfire = io.fanIn(writeSelectedReg).aw.fire
  val wfire  = io.fanIn(writeSelectedReg).w.fire
  val bfire  = io.fanIn(writeSelectedReg).b.fire
  val arfire = io.fanIn(readSelectedReg).ar.fire
  val rfire  = io.fanIn(readSelectedReg).r.fire
  val rlast  = io.fanIn(readSelectedReg).r.bits.last(0)

  writeState := MuxLookup(writeState, sWriteIdle)(
    Seq(
      sWriteIdle -> Mux(write, sWaitBresp, sWriteIdle),
      sWaitBresp -> Mux(bfire, sWriteIdle, sWaitBresp),
    ),
  )

  readState := MuxLookup(readState, sReadIdle)(
    Seq(
      sReadIdle  -> Mux(read, sWaitRresp, sReadIdle),
      sWaitRresp -> Mux(rfire && rlast, sReadIdle, sWaitRresp),
    ),
  )

  val awaddr         = io.fanIn(writeSelected).aw.bits.addr
  val wmatches       = VecInit(fanOutArea.map(_.foldLeft(false.B)(_ || _.contains(awaddr)))).asUInt
  val writeOutSelect = RegEnable(PriorityEncoder(wmatches), isWriteIdle)

  val araddr        = io.fanIn(readSelected).ar.bits.addr
  val rmatches      = VecInit(fanOutArea.map(_.foldLeft(false.B)(_ || _.contains(araddr)))).asUInt
  val readOutSelect = RegEnable(PriorityEncoder(rmatches), isReadIdle)

  for (i <- 0 until fanInNum) {
    io.fanIn(i).aw.ready := (i.U === writeSelectedReg) && isWaitBresp && io.fanOut(writeOutSelect).aw.ready
    io.fanIn(i).w.ready  := (i.U === writeSelectedReg) && isWaitBresp && io.fanOut(writeOutSelect).w.ready
    io.fanIn(i).b.valid  := (i.U === writeSelectedReg) && isWaitBresp && io.fanOut(writeOutSelect).b.valid
    io.fanIn(i).b.bits   := io.fanOut(writeOutSelect).b.bits
    io.fanIn(i).ar.ready := (i.U === readSelectedReg) && isWaitRresp && io.fanOut(readOutSelect).ar.ready
    io.fanIn(i).r.valid  := (i.U === readSelectedReg) && isWaitRresp && io.fanOut(readOutSelect).r.valid
    io.fanIn(i).r.bits   := io.fanOut(readOutSelect).r.bits
  }
  for (i <- 0 until fanOutNum) {
    io.fanOut(i).aw.valid := (i.U === writeOutSelect) && isWaitBresp && io.fanIn(writeSelectedReg).aw.valid
    io.fanOut(i).aw.bits  := io.fanIn(writeSelectedReg).aw.bits
    if (fanOutTrans(i)) {
      io.fanOut(i).aw.bits.addr := io.fanIn(writeSelectedReg).aw.bits.addr ^ fanOutArea(i).minBy(_.base).base.U
    }
    io.fanOut(i).w.valid  := (i.U === writeOutSelect) && isWaitBresp && io.fanIn(writeSelectedReg).w.valid
    io.fanOut(i).w.bits   := io.fanIn(writeSelectedReg).w.bits
    io.fanOut(i).b.ready  := (i.U === writeOutSelect) && isWaitBresp && io.fanIn(writeSelectedReg).b.ready
    io.fanOut(i).ar.valid := (i.U === readOutSelect) && isWaitRresp && io.fanIn(readSelectedReg).ar.valid
    io.fanOut(i).ar.bits  := io.fanIn(readSelectedReg).ar.bits
    if (fanOutTrans(i)) {
      io.fanOut(i).ar.bits.addr := io.fanIn(readSelectedReg).ar.bits.addr ^ fanOutArea(i).minBy(_.base).base.U
    }
    io.fanOut(i).r.ready := (i.U === readOutSelect) && isWaitRresp && io.fanIn(readSelectedReg).r.ready
  }
}

object AXIInterconnect {
  def apply(
      params: AXIBundleParameters,
      fanIn: Seq[AXIBundle],
      fanOut: Seq[AXIBundle],
      fanOutArea: Seq[Seq[AddressSet]],
      fanOutTrans: Seq[Boolean],
  ) = {
    val AXIInterconnect = Module(new AXIInterconnect(params, fanIn.length, fanOutArea, fanOutTrans))
    for (i <- 0 until fanIn.length) {
      AXIInterconnect.io.fanIn(i) <> fanIn(i)
    }
    for (i <- 0 until fanOut.length) {
      AXIInterconnect.io.fanOut(i) <> fanOut(i)
    }
  }
}

class AXIArbiter(params: AXIBundleParameters, ids: Seq[Int]) extends Module {
  private val inNum = ids.length

  val io = IO(new Bundle {
    val in  = Vec(inNum, Flipped(AXIBundle(params)))
    val out = AXIBundle(params)
  })

  val arAribter = Module(new Arbiter(new AXIBundleAR(params), inNum))
  val awAribter = Module(new Arbiter(new AXIBundleAW(params), inNum))
  val wAribter  = Module(new Arbiter(new AXIBundleW(params), inNum))

  for (i <- 0 until inNum) {
    arAribter.io.in(i) <> io.in(i).ar
    awAribter.io.in(i) <> io.in(i).aw
    wAribter.io.in(i)  <> io.in(i).w
    io.in(i).r :<= io.out.r
    io.in(i).b :<= io.out.b
  }
  io.out.ar <> arAribter.io.out
  io.out.aw <> awAribter.io.out
  io.out.w  <> wAribter.io.out

  io.out.r.ready := io.in zip ids map { case (in, id) =>
    in.r.ready && in.r.bits.id === id.U
  } reduce (_ || _)

  io.out.b.ready := io.in zip ids map { case (in, id) =>
    in.b.ready && in.b.bits.id === id.U
  } reduce (_ || _)
}
