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

// class SimpleBranchPredictor(implicit params: CoreParameters) extends Module {
//   import params.commonParams.{vaddrWidth}
//   import params.frontendParams._
//   val io = IO(new Bundle {
//     val flush = Input(new Bundle {
//       val stage1 = Bool()
//       val stage2 = Bool()
//     })
//     val req = new Bundle {
//       val stage0 = Flipped(Decoupled(UInt(vaddrWidth.W)))
//     }
//     val resp = new Bundle {
//       val stage1 = Decoupled(Vec(fetchWidth, new BranchPrediction))
//       val stage2 = Decoupled(new Bundle {
//         val pred = Vec(fetchWidth, new BranchPrediction)
//         val meta = new Bundle {
//           val btb = UInt(log2Ceil(btbParams.nWays).W)
//           val bim = Vec(fetchWidth, UInt(2.W))
//         }
//       })
//     }
//     val update = Flipped(Valid(new BranchPredictionUpdate))
//   })

//   val btb = Module(new BTB)
//   val bim = Module(new BIM)

//   btb.io.flush := io.flush.stage1
//   bim.io.flush := io.flush.stage1

//   btb.io.update := io.update
//   bim.io.update := io.update

//   val stage0Ready = btb.io.req.ready && bim.io.req.ready
//   io.req.stage0.ready := stage0Ready

//   btb.io.req.valid := io.req.stage0.valid && stage0Ready
//   btb.io.req.bits  := io.req.stage0.bits

//   bim.io.req.valid := io.req.stage0.valid && stage0Ready
//   bim.io.req.bits  := io.req.stage0.bits

//   val stage1Resp = Wire(Decoupled(io.resp.stage1.bits.cloneType))
//   stage1Resp.valid  := btb.io.resp.valid && bim.io.resp.valid
//   btb.io.resp.ready := stage1Resp.ready
//   bim.io.resp.ready := stage1Resp.ready
//   for (i <- 0 until fetchWidth) {
//     stage1Resp.bits(i)       := btb.io.resp.bits.pred(i)
//     stage1Resp.bits(i).taken := bim.io.resp.bits.pred(i).taken
//   }

//   val stage1RespExt = ReadyValidIOExpand(stage1Resp, 2)
//   io.resp.stage1.valid   := stage1RespExt.valid(0)
//   stage1RespExt.ready(0) := io.resp.stage1.ready
//   io.resp.stage1.bits    := stage1RespExt.bits

//   val stage1Data = Wire(Decoupled(io.resp.stage2.bits.cloneType))
//   stage1Data.valid         := stage1RespExt.valid(1)
//   stage1RespExt.ready(1)   := stage1Data.ready
//   stage1Data.bits.pred     := stage1Resp.bits
//   stage1Data.bits.meta.bim := bim.io.resp.bits.meta
//   stage1Data.bits.meta.btb := btb.io.resp.bits.meta.writeWay
//   PipeConnect(Some(io.flush.stage2), stage1Data, io.resp.stage2)
// }

object BranchPredictor {
  class BranchPredictorStorage(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import commonParams.{vaddrWidth}
    import frontendParams._

    val io = IO(new Bundle {
      val bim = new Bundle {
        val en   = Input(Bool())
        val set  = Input(UInt(log2Ceil(bimParams.nSets).W))
        val data = Output(new BIM.BIMMeta)
      }
      val btb = new Bundle {
        val en   = Input(Bool())
        val set  = Input(UInt(log2Ceil(btbParams.nSets).W))
        val meta = Output(Vec(btbParams.nWays, Vec(fetchWidth, new BTB.BTBMeta)))
        val btb  = Output(Vec(btbParams.nWays, Vec(fetchWidth, new BTB.BTBEntry)))
        val ebtb = Output(UInt(vaddrWidth.W))
      }
      val update = Flipped(new BranchPredictionUpdate)
    })

    val bim = Module(new BIM.BIMStorage)
    val btb = Module(new BTB.BTBStorage)

    bim.io.update := io.update
    btb.io.update := io.update

    bim.io.read <> io.bim
    btb.io.read <> io.btb
  }

  class BranchPredictorStage0to1(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import commonParams.{vaddrWidth}
    import frontendParams._

    val io = IO(new Bundle {
      val flush = Input(Bool())
      val bimRead = new Bundle {
        val en   = Output(Bool())
        val set  = Output(UInt(log2Ceil(bimParams.nSets).W))
        val data = Input(new BIM.BIMMeta)
      }
      val btbRead = new Bundle {
        val en   = Output(Bool())
        val set  = Output(UInt(log2Ceil(btbParams.nSets).W))
        val meta = Input(Vec(btbParams.nWays, Vec(fetchWidth, new BTB.BTBMeta)))
        val btb  = Input(Vec(btbParams.nWays, Vec(fetchWidth, new BTB.BTBEntry)))
        val ebtb = Input(UInt(vaddrWidth.W))
      }
      val holdRead = Input(UInt(vaddrWidth.W))
      val req      = Flipped(Decoupled(UInt(vaddrWidth.W)))
      val resp = Decoupled(new Bundle {
        val bim = new BIM.BIMMeta
        val btb = new Bundle {
          val meta = Vec(btbParams.nWays, Vec(fetchWidth, new BTB.BTBMeta))
          val btb  = Vec(btbParams.nWays, Vec(fetchWidth, new BTB.BTBEntry))
          val ebtb = UInt(vaddrWidth.W)
        }
      })
    })

    val bim = Module(new BIM.BIMStage0to1)
    val btb = Module(new BTB.BTBStage0to1)

    bim.io.flush := io.flush
    btb.io.flush := io.flush

    bim.io.metaRead <> io.bimRead
    btb.io.infoRead <> io.btbRead

    bim.io.holdRead := io.holdRead
    btb.io.holdRead := io.holdRead

    bim.io.req.valid := io.req.valid
    btb.io.req.valid := io.req.valid
    io.req.ready     := bim.io.req.ready && btb.io.req.ready
    bim.io.req.bits  := io.req.bits
    btb.io.req.bits  := io.req.bits

    io.resp.valid     := bim.io.resp.valid && btb.io.resp.valid
    bim.io.resp.ready := io.resp.ready
    btb.io.resp.ready := io.resp.ready
    io.resp.bits.bim  := bim.io.resp.bits
    io.resp.bits.btb  := btb.io.resp.bits
  }

  class BranchPredictorStage1(implicit params: CoreParameters) extends Module {
    import params.{commonParams, frontendParams}
    import commonParams.{vaddrWidth}
    import frontendParams._

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new Bundle {
        val bim = new BIM.BIMMeta
        val btb = new Bundle {
          val fetchPC = UInt(vaddrWidth.W)
          val meta    = Vec(btbParams.nWays, Vec(fetchWidth, new BTB.BTBMeta))
          val btb     = Vec(btbParams.nWays, Vec(fetchWidth, new BTB.BTBEntry))
          val ebtb    = UInt(vaddrWidth.W)
        }
      }))
      val resp = Decoupled(new Bundle {
        val pred = Vec(fetchWidth, new BranchPrediction)
        val meta = new Bundle {
          val bim = Vec(fetchWidth, UInt(2.W))
          val btb = UInt(log2Ceil(btbParams.nWays).W)
        }
      })
    })

    val bim = Module(new BIM.BIMStage1)
    val btb = Module(new BTB.BTBStage1)

    bim.io.req.valid := io.req.valid
    btb.io.req.valid := io.req.valid
    bim.io.req.bits  := io.req.bits.bim
    btb.io.req.bits  := io.req.bits.btb
    io.req.ready     := bim.io.req.ready && btb.io.req.ready

    io.resp.valid     := bim.io.resp.valid && btb.io.resp.valid
    bim.io.resp.ready := io.resp.ready
    btb.io.resp.ready := io.resp.ready
    io.resp.bits.pred zip bim.io.resp.bits.pred zip btb.io.resp.bits.pred foreach { case ((resp, bim), btb) =>
      resp       := btb
      resp.taken := bim.taken
    }
    io.resp.bits.meta.bim := bim.io.resp.bits.meta
    io.resp.bits.meta.btb := btb.io.resp.bits.meta
  }
}
