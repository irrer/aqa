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

  private val userPkTag = "userPkTag"

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private val prevButton = makeButton("Previous", false, ButtonType.BtnDefault)
  private val refreshButton = makeButton("Refresh", false, ButtonType.BtnDefault)
  private val nextButton = makeButton("Next", false, ButtonType.BtnDefault)

  val buttonList: WebRow = List(prevButton, refreshButton, nextButton)

  // let user choose date to display
  private val dateField = new WebInputDatePicker("Date", 5, 1)

  // bulk of the displayed information
  private val report = new WebPlainText("report", false, 10, 1, makeReport _)
  // override def toHtml(valueMap: ValueMapT, errorMap: StyleMapT, response: Option[Response]): Elem = {

  private def formCreate(valueMap: ValueMapT) = new WebForm(pathOf, List(dateField ++ List(buttonList), List(report)))

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  private val dateFormat = new SimpleDateFormat("EEE MMM dd HH:mm")

  private def makeReport(valueMap: ValueMapT): Elem = {

    val user: Option[User] = {
      valueMap.get(userPkTag) match {
        case Some(userPkText) =>
          User.get(userPkText.toLong)
          None
      }
    }

    val institution: Option[Institution] = {
      user match {
        case Some(u) => Institution.get(u.institutionPK)
        case _ => None
      }
    }

    case class Col(name: String, title: String, toElem: (DataSet) => Elem) {
      def toHeader = <th title={ title }>{ name }</th>
    }

    case class DataSet(epid: BBbyEPIDComposite, cbct: BBbyCBCT, machine: Machine, output: Output);

    def colMachine(dataSet: DataSet): Elem = {
      wrapAlias(<div title="Machine Name">{ dataSet.machine.id }</div>)
    }

    def colPatient(dataSet: DataSet): Elem = { // TODO
      <div>Patient</div>
    }

    def colDateTime(dataSet: DataSet): Elem = {
      <div>{ dateFormat.format(dataSet.output.dataDate.get) }</div>
    }

    def colXYZ(dataSet: DataSet): Elem = { // TODO
      <div>{ "Helloooooo" }</div>
    }

    val colList: List[Col] = List(
      new Col("Machine", "Name of treatment machine", colMachine _),
      new Col("Patient", "Name of test patient", colPatient _),
      new Col("Date+Time", "Time of EPID acquisition", colDateTime _)
    // TODO NEXT: make more columns.  RE: Justin's email (flagged) from Sep 9.
    )

    def dataSetToRow(dataSet: DataSet) = {
      colList.map(col => col.toElem(dataSet))
    }

    def getDataSetList: List[DataSet] = {
      val date = {
        if (valueMap.get(dateField.label).isDefined)
          dateField.dateFormat.parse(valueMap(dateField.label))
        else
          dateField.dateFormat.parse(dateField.dateFormat.format(new Date)) // today rounded off to midnight
      }

      // val list = BBbyEPIDComposite.getReportingDataSet(date, institution.get.institutionPK.get)  // TODO
      val fakeDate = Util.standardDateFormat.parse("2019-05-24T00:00:00")
      val list = BBbyEPIDComposite.getReportingDataSet(fakeDate, 1)
      //??? // TODO
      List[DataSet]()
    }

    val content = {
      <div class="row">
        <div class="col-md-10 col-md-offset-1">
          <table class="table table-responsive">
            <thead><tr>{ colList.map(col => col.toHeader) }</tr></thead>
            { getDataSetList.map(dataSet => dataSetToRow(dataSet)) }
          </table>
        </div>
      </div>
    }

    content
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
    val userPkValue = {
      getUser(response.getRequest) match {
        case Some(user) => Map((userPkTag, user.userPK.get.toString))
        case _ => Map[String, String]()
      }
    }
    formCreate(valueMap).setFormResponse(valueMap ++ userPkValue, styleNone, DailyQARestlet.pageTitle, response, Status.SUCCESS_OK)
    Trace.trace(getSelectedDate(valueMap))
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      show(response, valueMap)
    } catch {
      case t: Throwable => {
        internalFailure(response, t)
      }
    }
  }

}
