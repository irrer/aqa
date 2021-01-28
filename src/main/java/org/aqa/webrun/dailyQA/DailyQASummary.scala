package org.aqa.webrun.dailyQA

import org.aqa.Logging
import org.aqa.db.BBbyEPIDComposite
import org.aqa.db.Institution
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

/**
 * Support for generating JS scripts on request.
 */
object DailyQASummary {

  private val pageTitle = "Daily QA Summary"
  private val path = new String((new DailyQASummary).pathOf)

  val dateFormat = new SimpleDateFormat("EEE MMM dd")
  val timeFormat = new SimpleDateFormat("H:mm a")

  def makeReference(outputPK: Long): String = {
    "<script src='" + path + "?date=" + outputPK + "'></script>"
  }
}

class DailyQASummary extends Restlet with SubUrlRoot with Logging {

  private val checksumLabel = "checksum"

  private val refreshButton = new FormButton(label = "Refresh", col = 1, offset = 0, subUrl, pathOf, ButtonType.BtnDefault)

  // let user choose date to display
  private val dateField = new WebInputDatePicker("Date", 6, 0, false, true)

  private def getDateText(valueMap: ValueMapT): String = {
    valueMap.get(dateField.label) match {
      case Some(text) => text
      case _ => dateField.dateFormat.format(new Date)
    }
  }

  private def csvLink(valueMap: ValueMapT): Elem = {
    <a href={DailyQASummary.path + ".csv?CSV=" + getDateText(valueMap)} title="Download a spreadsheet of all DailyQA data for this institution.">CSV</a>
  }

  private val csvField = new WebPlainText("CSV", false, 1, 0, csvLink)

  private val displayedDate = {
    def getDisplayedDate(valueMap: ValueMapT): Elem = {
      <h4>Results for
        {getDateText(valueMap)}
      </h4>
    }

    new WebPlainText("DisplayedDate", false, 2, 0, getDisplayedDate)
  }

  val controlRow: WebRow = List(displayedDate, coordinateDiagramCol(65), refreshButton, dateField, csvField)

  private def getDate(valueMap: ValueMapT): Date = {
    try {
      dateField.dateFormat.parse(valueMap(dateField.label))
    } catch {
      case _: Throwable => edu.umro.ScalaUtil.Util.roundToDate(new Date)
    }
  }

  // bulk of the displayed information
  private val report = {
    new WebPlainText("report", false, 12, 0, (valueMap: ValueMapT) => {
      val institutionPK = getUser(valueMap).get.institutionPK
      DailyQAHTML.makeReport(getDataSetListByDateAndInstitution(valueMap), institutionPK, getDate(valueMap))
    })
  }

  val contentRow: WebRow = List(report)

  val runScript =
    """
    // Reload the page when there is new data, indicated by
    // a change in status

    var date=document.getElementById("Date").getAttribute("value");
    var baseUrl='/DailyQASummary?checksum=true&Date=' + date;
    var WebRefreshTime=2000;

    function watchStatus() {
        $.ajax({url:baseUrl,
            success:function(result){
                var status = document.getElementById("checksum").innerHTML;
                // Must remove whitespace because IntelliJ source formatter adds it.
                if (status.trim() == result.trim()) {
                    setTimeout(watchStatus, WebRefreshTime);
                }
                else {
                    document.getElementById("mainForm").submit();
                }
            },
            error:function(result){
                setTimeout(watchStatus, WebRefreshTime);
            }
        });
    }

    setTimeout(watchStatus, WebRefreshTime);
"""

  private def formCreate() = new WebForm(pathOf, title = None, List(controlRow, contentRow), fileUpload = -1, runScript = Some(runScript))

  private def getDataSetListByDateAndInstitution(valueMap: ValueMapT): Seq[BBbyEPIDComposite.DailyDataSetComposite] = {

    val user = getUser(valueMap)

    val institution: Option[Institution] = {
      user match {
        case Some(u) => Institution.get(u.institutionPK)
        case _ => None
      }
    }

    val date = {
      if (valueMap.contains(dateField.label))
        dateField.dateFormat.parse(valueMap(dateField.label).replace("%20", " "))
      else
        dateField.dateFormat.parse(dateField.dateFormat.format(new Date)) // today rounded off to midnight
    }

    val list = BBbyEPIDComposite.getForOneDay(date, institution.get.institutionPK.get)
    list
  }


  private def show(response: Response, valueMap: ValueMapT): Unit = {
    formCreate().setFormResponse(valueMap, styleNone, DailyQASummary.pageTitle, response, Status.SUCCESS_OK)
  }


  /**
   * Respond to the client with the checksum of the data so they can decide whether or not they need to reload it.
   */
  private def handleChecksum(response: Response, valueMap: ValueMapT): Unit = {
    val checksum = DailyQAHTML.makeChecksum(getDataSetListByDateAndInstitution(valueMap))
    response.setEntity(checksum, MediaType.TEXT_PLAIN)
    response.setStatus(Status.SUCCESS_OK)
  }


  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      if (valueMap.contains(csvField.label)) {
        val assembler = new DailyQACSVCacheComposite(request.getHostRef.toString(), WebUtil.getUser(request).get.institutionPK)
        assembler.assemble(response)
      } else if (valueMap.contains(checksumLabel))
        handleChecksum(response, valueMap)
      else
        show(response, valueMap)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
