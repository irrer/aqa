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
import java.util.Date
import scala.xml.Elem
import org.aqa.db.User
import org.aqa.db.Institution
import org.aqa.db.BBbyCBCT
import org.aqa.db.BBbyEPID
import org.aqa.db.Output
import org.aqa.db.Machine
import java.text.SimpleDateFormat
import org.aqa.db.BBbyEPIDComposite
import org.aqa.db.DicomSeries
import com.pixelmed.dicom.TagFromName
import org.aqa.webrun.bbByEpid.BBbyEPIDRun
import org.aqa.web.ViewOutput

/**
 * Support for generating JS scripts on request.
 */
object DailyQASummary {

  private val pageTitle = "Daily QA Summary"
  private val path = new String((new DailyQASummary).pathOf)

  val dateFormat = new SimpleDateFormat("EEE MMM dd HH:mm")
  def makeReference(outputPK: Long) = {
    "<script src='" + path + "?date=" + outputPK + "'></script>"
  }
}

class DailyQASummary extends Restlet with SubUrlRoot with Logging {

  private def fmt(d: Double) = d.formatted("%10.3f").trim

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val refreshButton = makeButton("Refresh", false, ButtonType.BtnDefault)

  // let user choose date to display
  private val dateField = new WebInputDatePicker("Date", 4, 0, false)

  private def getDateText(valueMap: ValueMapT): String = {
    valueMap.get(dateField.label) match {
      case Some(text) => text
      case _ => dateField.dateFormat.format(new Date)
    }
  }

  private def csvLink(valueMap: ValueMapT): Elem = {
    <a href={ DailyQASummary.path + "?CSV=" + getDateText(valueMap) } title="Download a spreadsheet of all DailyQA data for this institution.">CSV</a>
  }

  private val csvField = new WebPlainText("CSV", false, 2, 0, csvLink)

  private def getDisplayedDate(valueMap: ValueMapT): Elem = {
    <h4>Results for { getDateText(valueMap) }</h4>
  }

  private val displayedDate = new WebPlainText("DisplayedDate", false, 2, 0, getDisplayedDate)

  val controlRow: WebRow = List(displayedDate, refreshButton, dateField, csvField)

  // bulk of the displayed information
  private val report = {
    def getDate(valueMap: ValueMapT): Date = {
      try {
        dateField.dateFormat.parse(valueMap(dateField.label))
      } catch {
        case t: Throwable => edu.umro.ScalaUtil.Util.roundToDate(new Date)
      }
    }

    new WebPlainText("report", false, 12, 0, (valueMap: ValueMapT) => {
      val institutionPK = getUser(valueMap).get.institutionPK
      DailyQAHTML.makeReport(getDataSetListByDateAndInstitution(valueMap), institutionPK, getDate(valueMap))
    })
  }

  val contentRow: WebRow = List(report)

  private def formCreate(valueMap: ValueMapT) = new WebForm(pathOf, List(controlRow, contentRow))

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  private def getDataSetListByDateAndInstitution(valueMap: ValueMapT): Seq[BBbyEPIDComposite.DailyDataSet] = {

    val user = getUser(valueMap)

    val institution: Option[Institution] = {
      user match {
        case Some(u) => Institution.get(u.institutionPK)
        case _ => None
      }
    }

    val date = {
      if (valueMap.get(dateField.label).isDefined)
        dateField.dateFormat.parse(valueMap(dateField.label))
      else
        dateField.dateFormat.parse(dateField.dateFormat.format(new Date)) // today rounded off to midnight
    }

    val list = BBbyEPIDComposite.getReportingDataSet(date, institution.get.institutionPK.get)
    list
  }

  /**
   * Get midnight of selected date.  If there is a formatting or other problem, return the
   *  current date.  Midnight facilitates searching the entire day.
   */
  private def getSelectedDate(valueMap: ValueMapT): Date = {

    /** Midnight of current date.  Facilitates searching the entire day. */
    def now: Date = {
      val text = Util.standardDateFormat.format((new Date).getTime)
      val date = Util.standardDateFormat.parse(text.replaceAll("T.*", "T00:00:00"))
      date
    }

    try {
      val date = dateField.dateFormat.parse(valueMap(dateField.label))
      date
    } catch {
      case t: Throwable => now
    }
  }

  private def show(response: Response, valueMap: ValueMapT) = {
    formCreate(valueMap).setFormResponse(valueMap, styleNone, DailyQASummary.pageTitle, response, Status.SUCCESS_OK)
    //Trace.trace(getSelectedDate(valueMap))
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      if (valueMap.get(csvField.label).isDefined)
        DailyQACSV.getCsv(BBbyEPIDComposite.getReportingDataSet(getUser(valueMap).get.institutionPK), response)
      else
        show(response, valueMap)
    } catch {
      case t: Throwable => {
        internalFailure(response, t)
      }
    }
  }

}
