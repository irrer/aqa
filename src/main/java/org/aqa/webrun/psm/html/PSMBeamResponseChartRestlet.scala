package org.aqa.webrun.psm.html

import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.Logging
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.internalFailure
import org.aqa.webrun.psm.PSMCharts
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.Request
import org.restlet.Response

object PSMBeamResponseChartRestlet {
  private val path = new String((new PSMBeamResponseChartRestlet).pathOf)

  def makeReference(outputPK: Long): String = {
    "<script src='" + path + "?outputPK=" + outputPK + "'></script>"
  }
}

class PSMBeamResponseChartRestlet extends Restlet with SubUrlRoot with Logging {
  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap("outputPK").toInt

      val charts = new PSMCharts(outputPK)

      val js = charts.meanChart.javascript + charts.stdDevChart.javascript
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }
}
