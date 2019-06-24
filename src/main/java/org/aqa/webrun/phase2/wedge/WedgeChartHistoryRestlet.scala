package org.aqa.webrun.phase2.wedge

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
object WedgeChartHistoryRestlet {
  private val path = new String((new WedgeChartHistoryRestlet).pathOf)
  def makeReference(outputPK: Long) = {
    "<script src='" + path + "?outputPK=" + outputPK + "'></script>"
  }
}

class WedgeChartHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap("outputPK").toInt
      val js = new WedgeChartHistory(outputPK).chartScript
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable => {
        internalFailure(response, t)
      }
    }
  }

}

