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

  private val latestChangeLabel = "latestChange"

  private val refreshButton = new FormButton(label = "Refresh", col = 1, offset = 0, subUrl, pathOf, ButtonType.BtnDefault)

  // let user choose date to display
  private val dateField = new WebInputDatePicker("Date", 5, 0, false, true)

  private def getDateText(valueMap: ValueMapT): String = {
    valueMap.get(dateField.label) match {
      case Some(text) => text
      case _ => dateField.dateFormat.format(new Date)
    }
  }

  private def csvLink(tag: String, name: String, title: String) = {
    // Text version of time when page is loaded.  This is intended to make each version of the downloaded file uniquely named.
    val nowText = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss").format(new Date)

    <center title={title} style="border-style:solid; border-color:lightgray; border-width:1px 1px;">
      <a href={DailyQASummary.path + "/" + tag + "-" + nowText + ".csv?" + tag + "=true"} style="margin: 6px;">
        {name}<br/>
        CSV
      </a>
    </center>
  }

  private val CsvCompositeTag = "DailyQA-Composite"

  private def csvCompositeLink(valueMap: ValueMapT): Elem = {
    if (false) println(valueMap) // gets rid of compiler warning about unused parameter
    csvLink(CsvCompositeTag, name = "Composite", title = "Download a spreadsheet of all DailyQA data with CBCTs paired" + WebUtil.titleNewline + "with EPIDs.  Values that can not be paired are not shown.")
  }

  private val CsvCbctTag = "DailyQA-CBCT"

  private def csvCbctLink(valueMap: ValueMapT): Elem = {
    if (false) println(valueMap) // gets rid of compiler warning about unused parameter
    csvLink(CsvCbctTag, name = "CBCT", title = "Download a spreadsheet of all DailyQA CBCT data " + WebUtil.titleNewline + "including values that are not paired with EPID data.")
  }

  private val CsvEpidTag = "DailyQA-EPID"

  private def csvEpidLink(valueMap: ValueMapT): Elem = {
    if (false) println(valueMap) // gets rid of compiler warning about unused parameter
    csvLink(CsvEpidTag, name = "EPID", title = "Download a spreadsheet of all DailyQA EPID data " + WebUtil.titleNewline + "including values that are not paired with CBCT data.")
  }

  private val csvFieldComposite = new WebPlainText("CSV", false, 1, 0, csvCompositeLink)
  private val csvFieldCbct = new WebPlainText("CBCT CSV", false, 1, 0, csvCbctLink)
  private val csvFieldEpid = new WebPlainText("EPID CSV", false, 1, 0, csvEpidLink)

  private val displayedDate = {
    def getDisplayedDate(valueMap: ValueMapT): Elem = {
      <h4>Results for
        {getDateText(valueMap)}
      </h4>
    }

    new WebPlainText("DisplayedDate", false, 2, 0, getDisplayedDate)
  }

  val controlRow: WebRow = List(displayedDate, coordinateDiagramCol(65), refreshButton, dateField, csvFieldComposite, csvFieldCbct, csvFieldEpid)

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

  val runScript: String = {
    """
    // Reload the page when there is new data, indicated by
    // a change in status

    var date=document.getElementById("Date").getAttribute("value");
    var baseUrl='/DailyQASummary?latestChange=true&Date=' + date;
    var WebRefreshTime=2000;

    function watchStatus() {$.ajax({
      url: baseUrl , success: function
      (result) {
        var status = document.getElementById("latestChange").innerHTML;
        // Must remove whitespace because IntelliJ source formatter adds it.
        if (status.trim() == result.trim()) {
          setTimeout(watchStatus, WebRefreshTime);
        }
        else {
          document.getElementById("mainForm").submit();
        }
      },
      error: function
      (result) {
        setTimeout(watchStatus, WebRefreshTime);
      }
    });}

    setTimeout(watchStatus, WebRefreshTime);
    """
  }

  private def formCreate() = new WebForm(pathOf, title = None, List(controlRow, contentRow), fileUpload = -1, runScript = Some(runScript))

  private def dateFromValueMap(valueMap: ValueMapT): Date = {
    if (valueMap.contains(dateField.label))
      dateField.dateFormat.parse(valueMap(dateField.label).replace("%20", " ").trim)
    else
      dateField.dateFormat.parse(dateField.dateFormat.format(new Date)) // today rounded off to midnight
  }

  private def getDataSetListByDateAndInstitution(valueMap: ValueMapT): Seq[DailyDataSetComposite] = {
    val user = getUser(valueMap)

    val institution: Option[Institution] = {
      user match {
        case Some(u) => Institution.get(u.institutionPK)
        case _ => None
      }
    }

    val list = BBbyEPIDComposite.getForOneDay(dateFromValueMap(valueMap), institution.get.institutionPK.get)
    list
  }


  private def show(response: Response, valueMap: ValueMapT): Unit = {
    formCreate().setFormResponse(valueMap, styleNone, DailyQASummary.pageTitle, response, Status.SUCCESS_OK)
  }


  /**
   * Respond to the client with the latest change of the data so they can decide whether or not they need to reload it.
   */
  private def handleLatestChange(response: Response): Unit = {
    val latestChange = DailyQAActivity.get
    response.setEntity(latestChange, MediaType.TEXT_PLAIN)
    response.setStatus(Status.SUCCESS_OK)
  }


  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      if (valueMap.contains(CsvCompositeTag)) {
        new DailyQACSVCacheComposite(request.getHostRef.toString(), WebUtil.getUser(request).get.institutionPK).assemble(response)
      } else if (valueMap.contains(CsvCbctTag)) {
        new DailyQACSVCacheCBCT(request.getHostRef.toString(), WebUtil.getUser(request).get.institutionPK).assemble(response)
      } else if (valueMap.contains(CsvEpidTag)) {
        new DailyQACSVCacheEPID(request.getHostRef.toString(), WebUtil.getUser(request).get.institutionPK).assemble(response)
      } else if (valueMap.contains(latestChangeLabel))
        handleLatestChange(response)
      else
        show(response, valueMap)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
