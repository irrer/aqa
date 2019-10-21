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
object DailyQARestlet {

  private val pageTitle = "Daily QA Summary"
  private val path = new String((new DailyQARestlet).pathOf)
  def makeReference(outputPK: Long) = {
    "<script src='" + path + "?date=" + outputPK + "'></script>"
  }
}

class DailyQARestlet extends Restlet with SubUrlRoot with Logging {

  private def fmt(d: Double) = d.formatted("%10.3f").trim

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val refreshButton = makeButton("Refresh", false, ButtonType.BtnDefault)

  // let user choose date to display
  private val dateField = new WebInputDatePicker("Date", 4, 0, false)

  val controlRow: WebRow = List(refreshButton, dateField)

  // bulk of the displayed information
  private val report = new WebPlainText("report", false, 12, 0, makeReport _)
  val contentRow: WebRow = List(report)

  private def formCreate(valueMap: ValueMapT) = new WebForm(pathOf, List(controlRow, contentRow))

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  private val dateFormat = new SimpleDateFormat("EEE MMM dd HH:mm")

  private def makeReport(valueMap: ValueMapT): Elem = {

    val user = getUser(valueMap)

    val institution: Option[Institution] = {
      user match {
        case Some(u) => Institution.get(u.institutionPK)
        case _ => None
      }
    }

    def fmtAngle(angle: Double) = angle.formatted("%12.8f").trim

    case class Col(name: String, title: String, toElem: (BBbyEPIDComposite.DailyDataSet) => Elem) {
      def toHeader = <th title={ title }>{ name }</th>
    }

    def colMachine(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      <td title="Machine Name">{ wrapAlias(dataSet.machine.id) }</td>
    }

    def colPatient(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {

      val patientName: Elem = DicomSeries.getBySeriesInstanceUID(dataSet.epid.epidSeriesInstanceUID).headOption match {
        case Some(ds) => {
          val pn = ds.attributeListList.head.get(TagFromName.PatientName).getSingleStringValueOrEmptyString
          <td>{ wrapAlias(pn) }</td>
        }
        case _ => <td>Unknown</td>
      }
      patientName
    }

    def posnRow(posn: Double) = {
      val text = posn.formatted("%7.2f").trim
      val title = posn.formatted("%12.6f").trim

      <td title={ title }>{ text }</td>
    }

    def colDateTime(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      <td>{ dateFormat.format(dataSet.output.dataDate.get) }</td>
    }

    def colCbctX(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      val x = dataSet.cbct.cbctX_mm - dataSet.cbct.rtplanX_mm
      posnRow(x)
    }

    def colCbctY(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      val y = dataSet.cbct.cbctY_mm - dataSet.cbct.rtplanY_mm
      posnRow(y)
    }

    def colCbctZ(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      val z = dataSet.cbct.cbctZ_mm - dataSet.cbct.rtplanZ_mm
      posnRow(z)
    }

    def colVertGantryAngle(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      val angle = dataSet.vertList.head.gantryAngle_deg
      <td title={ fmtAngle(angle) }>{ Util.angleRoundedTo90(angle) }</td>
    }

    def colVertXCax(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      posnRow(dataSet.epid.xAdjusted_mm.get)
    }

    def colVertZCax(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      val offset = dataSet.vertList.head.epid3DZ_mm - (dataSet.cbct.rtplanZ_mm - dataSet.cbct.cbctZ_mm)
      posnRow(offset)
    }

    def colHorzGantryAngle(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      val angle = dataSet.horzList.head.gantryAngle_deg
      <td title={ fmtAngle(angle) }>{ Util.angleRoundedTo90(angle) }</td>
    }

    def colHorzYCax(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      posnRow(dataSet.epid.yAdjusted_mm.get)
    }

    def colHorzZCax(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      val offset = dataSet.horzList.head.epid3DZ_mm - (dataSet.cbct.rtplanZ_mm - dataSet.cbct.cbctZ_mm)
      <td>{ Util.fmtDbl(dataSet.epid.xAdjusted_mm.get) }</td>
      posnRow(dataSet.epid.zAdjusted_mm.get)
    }

    def colCbctImages(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      <td><a href={ ViewOutput.viewOutputUrl(dataSet.cbct.outputPK) }>CBCT Details</a></td>
    }

    def colEpidImages(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      <td><a href={ ViewOutput.viewOutputUrl(dataSet.epid.outputPK) }>EPID Details</a></td>
    }

    val colList: List[Col] = List(
      new Col("Machine", "Name of treatment machine", colMachine _),
      new Col("Patient", "Name of test patient", colPatient _),
      new Col("Date+Time", "Time of EPID acquisition", colDateTime _),

      new Col("X CBCT - PLAN mm", "(plan X) - (plan X) in mm", colCbctX _),
      new Col("Y CBCT - PLAN mm", "(plan Y) - (plan Y) in mm", colCbctY _),
      new Col("Z CBCT - PLAN mm", "(plan Z) - (plan Z) in mm", colCbctZ _),

      new Col("Vert Angle deg", "Angle of gantry for vertical image in degrees", colVertGantryAngle _),
      new Col("Vert EPID - CAX(X) mm", "X offset Vertical EPID image - CAX in mm", colVertXCax _),
      new Col("Vert EPID - CAX(Z) mm", "Z offset Vertical EPID image - CAX in mm", colVertZCax _),

      new Col("Horz Angle deg", "Angle of gantry for horizontal image in degrees", colHorzGantryAngle _),
      new Col("Horz EPID - CAX(Y) mm", "Y offset Horizontal EPID image - CAX in mm", colHorzYCax _),
      new Col("Horz EPID - CAX(Z) mm", "Z offset Horizontal EPID image - CAX in mm", colHorzZCax _),

      new Col("CBCT Details", "Images and other details for CBCT", colCbctImages _),
      new Col("EPID Details", "Images and other details for EPID", colEpidImages _))

    def dataSetToRow(dataSet: BBbyEPIDComposite.DailyDataSet) = {
      <tr>{ colList.map(col => col.toElem(dataSet)) }</tr>
    }

    def getDataSetList: Seq[BBbyEPIDComposite.DailyDataSet] = {
      val date = {
        if (valueMap.get(dateField.label).isDefined)
          dateField.dateFormat.parse(valueMap(dateField.label))
        else
          dateField.dateFormat.parse(dateField.dateFormat.format(new Date)) // today rounded off to midnight
      }

      val list = BBbyEPIDComposite.getReportingDataSet(date, institution.get.institutionPK.get)
      list
    }

    val content = {
      <div class="row">
        <table class="table table-responsive table-bordered">
          <thead><tr>{ colList.map(col => col.toHeader) }</tr></thead>
          {
            getDataSetList.map(dataSet => dataSetToRow(dataSet))
          }
        </table>
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
    formCreate(valueMap).setFormResponse(valueMap, styleNone, DailyQARestlet.pageTitle, response, Status.SUCCESS_OK)
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
