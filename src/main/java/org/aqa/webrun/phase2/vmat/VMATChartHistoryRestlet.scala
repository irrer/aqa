package org.aqa.webrun.phase2.vmat

import org.aqa.Logging
import org.aqa.db.VMAT
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

object VMATChartHistoryRestlet {
  private val path = new String((new VMATChartHistoryRestlet).pathOf)
  def makeReference(outputPK: Long): String = {
    "<script src='" + path + "?outputPK=" + outputPK + "'></script>"
  }
}

class VMATChartHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap("outputPK").toInt
      val beamNameSeq = VMAT.getByOutput(outputPK).map(vmat => vmat.beamNameMLC).distinct.sorted
      val js = beamNameSeq.map(beamName => new VMATChartHistory(outputPK, beamName).chartScript).mkString("\n")
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }
}
