package org.aqa.webrun.phase2.symmetryAndFlatness

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.restlet.data.Status

/**
 * Support for generating JS scripts on request.
 */
object SymmetryAndFlatnessHistoryRestlet {
  private val path = new String((new SymmetryAndFlatnessHistoryRestlet).pathOf)

  private val outputPKTag = "outputPK"
  private val beamNameTag = "beamName"

  def makeReference(beamName: String, outputPK: Long) = {
    "<script src='" + path + "?" + outputPKTag + "=" + outputPK + "&amp;" + beamNameTag + "=" + beamName + "'></script>"
  }
}

class SymmetryAndFlatnessHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap(SymmetryAndFlatnessHistoryRestlet.outputPKTag).toInt
      val beamName = valueMap(SymmetryAndFlatnessHistoryRestlet.beamNameTag).replaceAll("%20", " ")
      val js = new SymmetryAndFlatnessBeamHistoryHTML(beamName, outputPK).javascript
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable => {
        internalFailure(response, t)
      }
    }
  }

}

