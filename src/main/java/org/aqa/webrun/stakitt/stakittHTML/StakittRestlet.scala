package org.aqa.webrun.stakitt.stakittHTML

import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.Logging
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.internalFailure
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.stakitt.Analysis
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import scala.xml.Elem

class StakittRestlet(extendedData: ExtendedData, analysis: Analysis) extends Restlet with SubUrlRoot with Logging {

  private def content(request: Request): Elem = {

    <div class="row">
      <div class="col-md-10 col-md-offset-1">
      </div>
    </div>

  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val runScript = ""
      val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content(request)), "Leaf Gap and Skew", refresh = None, c3 = true, runScript = Some(runScript))
      WebUtil.setResponse(text, response, Status.SUCCESS_OK)
      WebUtil.respond(content(request), "Stakitt", response)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }
}
