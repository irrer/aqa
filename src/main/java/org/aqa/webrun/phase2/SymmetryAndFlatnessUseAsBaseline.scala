package org.aqa.webrun.phase2

import org.aqa.web.WebUtil.SubUrlRun
import org.restlet.Restlet
import org.restlet.Response
import org.restlet.Request
import org.aqa.web.WebUtil._

object SymmetryAndFlatnessUseAsBaseline {
  val outputPKTag = "outputPK"
  val confirmTag = "confirm"
}

class SymmetryAndFlatnessUseAsBaseline extends Restlet with SubUrlRun {

  import SymmetryAndFlatnessUseAsBaseline._

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    (valueMap.get(outputPKTag), valueMap.get(confirmTag)) match {
      case (Some(outputPKText), Some(confirm)) => ???
      case (Some(outputPKText), _) => ???
      case (_, _) => ???
    }

  }

}