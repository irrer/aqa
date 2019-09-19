package org.aqa.webrun.dailyQA

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.aqa.web.WebUtil._
import org.restlet.data.MediaType
import org.aqa.Logging
import org.restlet.data.Status
import edu.umro.ScalaUtil.Trace
import org.aqa.Util

/**
 * Support for generating JS scripts on request.
 */
object DailyQARestlet {

  private val pageTitle = "Daily QA Summary"
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

  private val prevButton = makeButton("Previous", false, ButtonType.BtnDefault)
  private val refreshButton = makeButton("Refresh", false, ButtonType.BtnDefault)
  private val nextButton = makeButton("Next", false, ButtonType.BtnDefault)

  val buttonList: WebRow = List(prevButton, refreshButton, nextButton)

  private val date = new WebInputDatePicker("Date", 5, 1)

  private def formCreate(valueMap: ValueMapT) = new WebForm(pathOf, List(List(date), buttonList))

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      Trace.trace("valueMap: " + valueMap.keys.map(k => k + " : " + valueMap(k)).mkString("\n"))
      formCreate(valueMap).setFormResponse(valueMap, styleNone, DailyQARestlet.pageTitle, response, Status.SUCCESS_OK)
      if (valueMap.get("Date").isDefined) {
        val text = valueMap("Date")
        val sdf = new java.text.SimpleDateFormat("yyyy MMM dd")
        val d = sdf.parse(text)
        val std = Util.standardDateFormat.format(d)
        Trace.trace("text: " + text)
        Trace.trace("d   : " + d)
        Trace.trace("std : " + std)
      }
      Trace.trace
    } catch {
      case t: Throwable => {
        internalFailure(response, t)
      }
    }
  }

}
