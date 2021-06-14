package org.aqa.webrun.phase2.phase2csv

import org.aqa.Logging
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.internalFailure
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet

class Phase2CsvRestlet extends Restlet with SubUrlRoot with Logging {
  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val institutionPK = WebUtil.getUser(valueMap).get.institutionPK
      val dir = Phase2Csv.institutionCsvDir(institutionPK)
      response.redirectSeeOther(WebServer.urlOfResultsFile(dir))
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
