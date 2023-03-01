package org.aqa.webrun.wl

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.OutputList
import org.aqa.web.ViewOutput
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.web.WebUtil.SubUrlRoot
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

class WLNav extends Restlet with SubUrlRoot with Logging {

  private val rowsPerPageDefault = 100

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action: String = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, _ => action, buttonType, name, Some(""))
  }

  private val newestButton = makeButton(" << Newest ", primary = false, ButtonType.BtnDefault)
  private val prevButton = makeButton(" < Prev Page ", primary = false, ButtonType.BtnDefault)

  private val nextButton = makeButton("Next Page > ", primary = false, ButtonType.BtnDefault)
  private val oldestButton = makeButton(" Oldest >> ", primary = true, ButtonType.BtnDefault)

  // class WebInputText(override val label: String, showLabel: Boolean, col: Int, offset: Int, placeholder: String, aqaAlias: Boolean) extends IsInput(label) with ToHtml {
  private val rowsPerPageField = new WebInputText(label = "Rows/Page", showLabel = true, col = 1, offset = 0, placeholder = rowsPerPageDefault.toString, aqaAlias = false)

  private val datePicker = new WebInputDatePicker(label = "Date", col = 6, offset = 0, showLabel = false, submitOnChange = true)

  private def list = new WebUtil.WebPlainText(label = "Winston Lutz Results", showLabel = false, col = 10, offset = 0, html = makeList)

  //  class WebForm(action: String, title: Option[String], rowList: List[WebRow], fileUpload: Int, runScript: Option[String] = None)
  private def form =
    new WebForm(
      pathOf,
      title = None,
      rowList = List(List(newestButton, prevButton, nextButton, oldestButton, rowsPerPageField), List(datePicker), List(list)),
      fileUpload = -1,
      runScript = Some(WLUpdateRestlet.makeJS)
    )
  private def setFormResponse(valueMap: ValueMapT, response: Response): Unit = form.setFormResponse(valueMap, errorMap = styleNone, pageTitle = "Winston Lutz", response, Status.SUCCESS_OK)

  private val wlProcedurePK = Procedure.ProcOfWinstonLutz.get.procedurePK.get

  /* shows the small WL icon. */
  private val imageElem = <img height="16px" src="/static/images/WL_EPID.png"/>

  /* Number of ms in a 24 hour day. */
  private val day_ms = 24 * 60 * 60 * 1000

  private def rowsPerPage(valueMap: ValueMapT) = {
    try {
      val rpp = valueMap(rowsPerPageField.label).toInt
      rpp
    } catch {
      case _: Throwable => rowsPerPageDefault
    }
  }

  private def makeList(valueMap: ValueMapT): Elem = {
    val ms: Long = {
      valueMap.get(datePicker.label) match {
        case Some(dateText) =>
          val d = datePicker.dateFormat.parse(dateText)
          d.getTime
        case None =>
          (new Date).getTime
      }
    }

    val user = getUser(valueMap)
    val machineList = Machine.listMachinesFromInstitution(user.get.institutionPK)

    val dataList = Output.getOutputChunk(institutionPK = user.get.institutionPK, date = new Timestamp(ms + day_ms), count = rowsPerPage(valueMap), procedurePK = wlProcedurePK)

    val dateFormat = new SimpleDateFormat("EEE MMM d YYYY HH:mm")

    val padding = "padding:12px;"

    /**
      * Convert one output row to a line in the HTML table.
      *
      * @param output For this output.
      * @return HTML tr
      */
    def toRow(output: Output): Elem = {

      val machineName = machineList.find(_.machinePK.get == output.machinePK.get).get.id

      val link = {
        val dateText = dateFormat.format(new Date(output.dataDate.get.getTime))
        val href = ViewOutput.viewOutputUrl(output.outputPK.get)
        <a title="Data analysis time" href={href}> {dateText}</a>
      }

      <tr>
        <td style={padding}>{link}</td>
        <td style={padding}>{imageElem}</td>
        <td style={padding}>{WebUtil.wrapAlias(machineName)}</td>
        <td style={padding}>{OutputList.redoUrl(output.outputPK.get)}</td>
      </tr>
    }

    <div>
      <table>
        <tr>
          <td style={padding}><b>Date</b></td>
          <td style={padding}></td>
          <td style={padding}><b>Machine</b></td>
          <td style={padding}></td>
        </tr>
        {dataList.map(toRow)}
      </table>
    </div>
    // Output.getByProcedure(Procedure.ProcOfWinstonLutz.get.procedurePK.get)
  }

  private def showOldest(valueMap: ValueMapT, response: Response): Unit = {

    val user = getUser(valueMap)
    val date = {
      new Timestamp(0)
    }
    val dataList = Output.getOutputChunk(institutionPK = user.get.institutionPK, date = date, count = -rowsPerPage(valueMap), procedurePK = wlProcedurePK).takeRight(rowsPerPage(valueMap))
    val d: String = {
      dataList.headOption match {
        case Some(output) => datePicker.dateFormat.format(new Date(output.dataDate.get.getTime + 0))
        case _            => datePicker.dateFormat.format(new Date)
      }
    }
    val vm = valueMap + (datePicker.label -> d)
    setFormResponse(vm, response)
  }

  private def showNewest(valueMap: ValueMapT, response: Response): Unit = {
    val date = new Timestamp((new Date).getTime)
    val d = datePicker.dateFormat.format(date)
    val vm = valueMap + (datePicker.label -> d)
    setFormResponse(vm, response)
  }

  private def showPrev(valueMap: ValueMapT, response: Response): Unit = {
    val newDateText: String = {
      val date = new Timestamp(datePicker.dateFormat.parse(valueMap(datePicker.label)).getTime)
      val user = getUser(valueMap)
      val list = Output.getOutputChunk(institutionPK = user.get.institutionPK, date = date, count = -rowsPerPage(valueMap), procedurePK = wlProcedurePK).takeRight(rowsPerPage(valueMap))
      val dateText: String = list.headOption match {
        case Some(output) => datePicker.dateFormat.format(new Date(output.dataDate.get.getTime + 0))
        case _            => datePicker.dateFormat.format(new Date)
      }
      dateText
    }
    val vm = valueMap + (datePicker.label -> newDateText)
    setFormResponse(vm, response)
  }

  private def showNext(valueMap: ValueMapT, response: Response): Unit = {
    val user = getUser(valueMap)
    val date = {
      val d = datePicker.dateFormat.parse(valueMap(datePicker.label))
      new Timestamp(d.getTime + day_ms)
    }

    val dataList = Output.getOutputChunk(institutionPK = user.get.institutionPK, date = date, count = rowsPerPage(valueMap) * 2, procedurePK = wlProcedurePK).takeRight(rowsPerPage(valueMap))

    if (dataList.nonEmpty) {
      val d = datePicker.dateFormat.format(dataList.head.dataDate.get.getTime + 0)
      val vm = valueMap + (datePicker.label -> d)
      setFormResponse(vm, response)
    } else
      showOldest(valueMap, response)
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      0 match {
        case _ if buttonIs(valueMap, oldestButton) => showOldest(valueMap, response)
        case _ if buttonIs(valueMap, newestButton) => showNewest(valueMap, response)
        case _ if buttonIs(valueMap, prevButton)   => showPrev(valueMap, response)
        case _ if buttonIs(valueMap, nextButton)   => showNext(valueMap, response)
        case _                                     => setFormResponse(valueMap, response)
      }
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }
}
