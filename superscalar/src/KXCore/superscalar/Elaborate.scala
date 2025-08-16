package KXCore.superscalar

import KXCore.common._
import KXCore.superscalar.core._
import KXCore.superscalar.core.frontend._
import KXCore.superscalar.core.backend._

object Elaborate extends App {
  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket",
    ).mkString(","),
    "-default-layer-specialization=enable",
  )

  implicit val params: CoreParameters = CoreParameters()
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new BIM.BIMStorage,
    args,
    firtoolOptions,
  )
}
