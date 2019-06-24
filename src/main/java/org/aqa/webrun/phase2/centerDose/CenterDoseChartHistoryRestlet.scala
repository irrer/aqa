package org.aqa.webrun.phase2.centerDose

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.Util
import java.io.File
import org.aqa.Config
import org.restlet.routing.Filter
import edu.umro.ScalaUtil.Trace
import scala.xml.XML
import scala.xml.Elem
import org.restlet.data.Status

/**
 * Support for generating JS scripts on request.
 */
object CenterDoseChartHistoryRestlet {
  private val path = new String((new CenterDoseChartHistoryRestlet).pathOf)
  def makeReference(outputPK: Long) = {
    "<script src='" + path + "?outputPK=" + outputPK + "'></script>"
  }
}

class CenterDoseChartHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap("outputPK").toInt
      val cdc = new CenterDoseChart(outputPK)
      val js = cdc.chartScript
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable => {
        internalFailure(response, t)
      }
    }
  }

}

