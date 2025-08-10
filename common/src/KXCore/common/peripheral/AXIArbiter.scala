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

// object AXIArbiter {
//   type Policy = (Int, UInt, Bool) => UInt

//   val lowestIndexFirst: Policy  = (width, valids, select) => ~(scanLeftOr(valids) << 1)(width - 1, 0)
//   val highestIndexFirst: Policy = (width, valids, select) => ~((scanRightOr(valids) >> 1).pad(width))

//   def apply[T <: Data](policy: Policy)(sink: IrrevocableIO[T], sources: IrrevocableIO[T]*): Unit = {
//     if (sources.isEmpty) {
//       sink.valid := false.B
//     } else {
//       returnWinner(policy)(sink, sources: _*)
//     }
//   }

//   def returnWinner[T <: Data](policy: Policy)(sink: IrrevocableIO[T], sources: IrrevocableIO[T]*) = {
//     require(!sources.isEmpty)

//     // The arbiter is irrevocable; when !idle, repeat last request
//     val idle = RegInit(true.B)

//     // Who wants access to the sink?
//     val valids   = sources.map(_.valid)
//     val anyValid = valids.reduce(_ || _)
//     // Arbitrate amongst the requests
//     val readys = VecInit(policy(valids.length, Cat(valids.reverse), idle).asBools)
//     // Which request wins arbitration?
//     val winner = VecInit((readys.zip(valids)).map { case (r, v) => r && v })

//     // Confirm the policy works properly
//     require(readys.size == valids.size)
//     // Never two winners
//     val prefixOR = winner.scanLeft(false.B)(_ || _).init
//     assert((prefixOR.zip(winner)).map { case (p, w) => !p || !w }.reduce { _ && _ })
//     // If there was any request, there is a winner
//     assert(!anyValid || winner.reduce(_ || _))

//     // The one-hot source granted access in the previous cycle
//     val state    = RegInit(VecInit.fill(sources.size)(false.B))
//     val muxState = Mux(idle, winner, state)
//     state := muxState

//     // Determine when we go idle
//     idle := Mux(sink.fire, true.B, Mux(anyValid, false.B, idle))

//     if (sources.size > 1) {
//       // 没有发出请求的 source 可以置位 ready，使用 readys 可以缩短路径，降低延迟
//       val allowed = Mux(idle, readys, state)
//       (sources.zip(allowed)).foreach { case (s, r) =>
//         s.ready := sink.ready && r
//       }
//     } else {
//       sources(0).ready := sink.ready
//     }

//     sink.valid := Mux(idle, anyValid, Mux1H(state, valids))
//     sink.bits :<= Mux1H(muxState, sources.map(_.bits))
//     muxState
//   }
// }
