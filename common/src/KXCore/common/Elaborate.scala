package KXCore.common

import chisel3._
import KXCore.common._
import KXCore.common.utils._
import KXCore.common.peripheral._

object Elaborate extends App {
  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket",
    ).mkString(","),
    "-default-layer-specialization=enable",
  )

  implicit val commonParams: CommonParameters = CommonParameters()
  implicit val cacheParams: CacheParameters   = CacheParameters()
  implicit val axiParams: AXIBundleParameters = AXIBundleParameters()
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new ICache.ICacheStorage,
    args,
    firtoolOptions,
  )
}
