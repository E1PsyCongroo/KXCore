package KXCore.superscalar.core.frontend

import chisel3._
import chisel3.util._
import KXCore.common._
import KXCore.superscalar._
import KXCore.superscalar.core._

// class BranchPredictor(implicit params: CoreParameters) extends Module {
//   import params.commonParams.{vaddrWidth}
//   import params.frontendParams._
//   val io = IO(new Bundle {
//     val flush = Input(new Bundle {
//       val stage1 = Bool()
//       val stage2 = Bool()
//     })
//     val req = new Bundle {
//       val stage0 = Vec(2, Flipped(Decoupled(UInt(vaddrWidth.W))))
//       val stage1 = Vec(3, Flipped(Decoupled(UInt(vaddrWidth.W))))
//     }
//     val resp = new Bundle {
//       val stage1 = Decoupled(Vec(fetchWidth, new BranchPrediction))
//       val stage2 = Decoupled(Vec(fetchWidth, new BranchPrediction))
//     }
//     val update = Flipped(Valid(new BranchPredictionUpdate))
//   })

//   val faubtb = Module(new FAMicroBTB)
//   val btb    = Module(new BTB)
//   val bim    = Module(new BIM)

//   btb.io.flush := io.flush.stage1
//   bim.io.flush := io.flush.stage1

//   faubtb.io.update := io.update
//   btb.io.update    := io.update
//   bim.io.update    := io.update

//   btb.io.req.stage0 <> io.req.stage0(0)
//   bim.io.req.stage0 <> io.req.stage0(1)

//   faubtb.io.req     <> io.req.stage1(0)
//   btb.io.req.stage1 <> io.req.stage1(1)
//   bim.io.req.stage1 <> io.req.stage1(2)
//   io.resp.stage1    <> faubtb.io.resp

//   val stage2Resp = WireDefault(0.U.asTypeOf(io.resp.stage2.cloneType))
//   stage2Resp.valid  := btb.io.resp.valid && bim.io.resp.valid
//   btb.io.resp.ready := stage2Resp.ready
//   bim.io.resp.ready := stage2Resp.ready
//   stage2Resp.bits   := faubtb.io.resp
//   for (i <- 0 until fetchWidth) {
//     stage2Resp.bits(i).taken := bim.io.resp.bits.pred(i).taken
//     when(btb.io.resp.bits.meta.hits(i)) {
//       stage2Resp.bits(i).isBr         := btb.io.resp.bits.pred(i).isBr
//       stage2Resp.bits(i).isJmp        := btb.io.resp.bits.pred(i).isJmp
//       stage2Resp.bits(i).target.valid := btb.io.resp.bits.pred(i).target.valid
//       stage2Resp.bits(i).target.bits  := btb.io.resp.bits.pred(i).target.bits
//     }
//   }
//   PipeConnect(Some(io.flush.stage2), stage2Resp, io.resp.stage2)
// }

class SimpleBranchPredictor(implicit params: CoreParameters) extends Module {
  import params.commonParams.{vaddrWidth}
  import params.frontendParams._
  val io = IO(new Bundle {
    val flush = Input(new Bundle {
      val stage1 = Bool()
      val stage2 = Bool()
    })
    val req = new Bundle {
      val stage0 = Flipped(Decoupled(UInt(vaddrWidth.W)))
      val stage1 = Flipped(Decoupled(UInt(vaddrWidth.W)))
    }
    val resp = new Bundle {
      val stage1 = Decoupled(Vec(fetchWidth, new BranchPrediction))
      val stage2 = Decoupled(new Bundle {
        val pred = Vec(fetchWidth, new BranchPrediction)
        val meta = new Bundle {
          val btb = UInt(log2Ceil(btbParams.nWays).W)
          val bim = Vec(fetchWidth, UInt(2.W))
        }
      })
    }
    val update = Flipped(Valid(new BranchPredictionUpdate))
  })

  val btb = Module(new BTB)
  val bim = Module(new BIM)

  btb.io.flush := io.flush.stage1
  bim.io.flush := io.flush.stage1

  btb.io.update := io.update
  bim.io.update := io.update

  val stage0ReqExt = ReadyValidIOExpand(io.req.stage0, 2)
  btb.io.req.stage0.valid := stage0ReqExt.valid(0)
  stage0ReqExt.ready(0)   := btb.io.req.stage0.ready
  btb.io.req.stage0.bits  := stage0ReqExt.bits

  bim.io.req.stage0.valid := stage0ReqExt.valid(1)
  stage0ReqExt.ready(1)   := bim.io.req.stage0.ready
  bim.io.req.stage0.bits  := stage0ReqExt.bits

  btb.io.req.stage1.valid := io.req.stage1.valid
  btb.io.req.stage1.bits  := io.req.stage1.bits

  bim.io.req.stage1.valid := io.req.stage1.valid
  bim.io.req.stage1.bits  := io.req.stage1.bits

  val stage1Resp = Wire(Decoupled(io.resp.stage1.bits.cloneType))
  stage1Resp.valid  := btb.io.resp.valid && bim.io.resp.valid
  btb.io.resp.ready := stage1Resp.ready
  bim.io.resp.ready := stage1Resp.ready
  for (i <- 0 until fetchWidth) {
    stage1Resp.bits(i)       := btb.io.resp.bits.pred(i)
    stage1Resp.bits(i).taken := bim.io.resp.bits.pred(i).taken
  }
  io.req.stage1.ready := stage1Resp.ready

  val stage1RespExt = ReadyValidIOExpand(stage1Resp, 2)
  io.resp.stage1.valid   := stage1RespExt.valid(0)
  stage1RespExt.ready(0) := io.resp.stage1.ready
  io.resp.stage1.bits    := stage1RespExt.bits

  val stage1Data = Wire(Decoupled(io.resp.stage2.bits.cloneType))
  stage1Data.valid         := stage1RespExt.valid(1)
  stage1RespExt.ready(1)   := stage1Data.ready
  stage1Data.bits.pred     := stage1Resp.bits
  stage1Data.bits.meta.bim := bim.io.resp.bits.meta
  stage1Data.bits.meta.btb := btb.io.resp.bits.meta.writeWay
  PipeConnect(Some(io.flush.stage2), stage1Data, io.resp.stage2)
}
