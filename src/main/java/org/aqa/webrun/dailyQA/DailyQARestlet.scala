package org.aqa.webrun.dailyQA

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.aqa.web.WebUtil._
import org.restlet.data.MediaType
import org.aqa.Logging
import org.restlet.data.Status

/**
 * Support for generating JS scripts on request.
 */
object DailyQARestlet {
  private val path = new String((new DailyQARestlet).pathOf)
  def makeReference(outputPK: Long) = {
    "<script src='" + path + "?date=" + outputPK + "'></script>"
  }
}

class DailyQARestlet extends Restlet with SubUrlRoot with Logging {

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val columns = (name.size / 15) + 1
    new FormButton(name, columns, 0, subUrl, pathOf, buttonType)
  }

  private val prevButton = makeButton("< Previous", false, ButtonType.BtnDefault)
  private val refreshButton = makeButton("Refresh", false, ButtonType.BtnDefault)
  private val nextButton = makeButton("Next >", false, ButtonType.BtnDefault)

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap("outputPK").toInt
      //      val cdc = new BBbyCBCTChart(outputPK)
      //      val js = cdc.chartScript
      //      response.setStatus(Status.SUCCESS_OK)
      //      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable => {
        internalFailure(response, t)
      }
    }
  }

}
