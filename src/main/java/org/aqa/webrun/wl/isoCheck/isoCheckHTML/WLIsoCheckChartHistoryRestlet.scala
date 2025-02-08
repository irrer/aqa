package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.internalFailure
import org.aqa.Logging
import org.restlet.Request
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.Response
import org.restlet.Restlet

class WLIsoCheckChartHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap("outputPK").toInt
      val chart = new WLIsoCheckChart(outputPK)

      val js: String = {

        val js1 = chart.isoCheckChart.javascript

        val js2 = if (chart.isoTableChart.isDefined) chart.isoTableChart.get.javascript else ""

        val js3 = if (chart.isoTableBB_RSqChart.isDefined) chart.isoTableBB_RSqChart.get.javascript else ""

        s"$js1\n$js2\n$js3"
      }

      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
