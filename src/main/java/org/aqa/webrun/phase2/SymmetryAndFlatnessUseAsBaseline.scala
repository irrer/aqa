package org.aqa.webrun.phase2

import org.aqa.web.WebUtil.SubUrlRun
import org.restlet.Restlet
import org.restlet.Response
import org.restlet.Request
import org.aqa.web.WebUtil._
import org.aqa.web.WebUtil
import org.aqa.db.Output
import org.aqa.db.Machine
import org.aqa.db.SymmetryAndFlatness

object SymmetryAndFlatnessUseAsBaseline {
  val outputPKTag = "outputPK"
  val confirmTag = "confirm"
}

class SymmetryAndFlatnessUseAsBaseline extends Restlet with SubUrlRun {

  import SymmetryAndFlatnessUseAsBaseline._

  private def setBaseline(outputPK: Long) = {
    val output = Output.get(outputPK).get
    val machine = Machine.get(output.machinePK.get).get
    val userPK = output.userPK
    
    val resultList = SymmetryAndFlatness.getByOutput(outputPK)
    
    resultList.head.SOPInstanceUID
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    try {
      (valueMap.get(outputPKTag), valueMap.get(confirmTag)) match {
        case (Some(outputPKText), Some(confirm)) => setBaseline(outputPKText.toLong)
        case (Some(outputPKText), _) => ???
        case (_, _) => ???
      }

    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }

  }

}