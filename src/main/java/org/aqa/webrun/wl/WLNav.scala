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
import org.aqa.Config
import org.aqa.db.CachedUser
import org.aqa.db.IsoCheck
import org.aqa.db.OutputApproval
import org.aqa.db.User
import org.aqa.db.WinstonLutz
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.WLMap
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

class WLNav extends Restlet with SubUrlRoot with Logging {

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action: String = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, _ => action, buttonType, name, Some(""))
  }

  private val newestButton = makeButton(" << Newest ", primary = false, ButtonType.BtnDefault)
  private val prevButton = makeButton(" < Prev Page ", primary = false, ButtonType.BtnDefault)

  private val refreshButton = makeButton("Refresh", primary = false, ButtonType.BtnDefault)
  private val nextButton = makeButton("Next Page > ", primary = false, ButtonType.BtnDefault)
  private val oldestButton = makeButton(" Oldest >> ", primary = true, ButtonType.BtnDefault)

  private val rowsPerPageField = new WebInputText(label = "Items/Page", showLabel = true, col = 1, offset = 0, placeholder = Config.WLRowsPerPageDefault.toString, aqaAlias = false)

  private val datePicker = new WebInputDatePicker(label = "On-Or-Before", col = 4, offset = 0, showLabel = true, submitOnChange = true)

  private def makeMachineList(response: Option[Response]): Seq[(String, String)] = {
    val allSelector = Seq(("0", "All Machines"))

    if (response.isDefined) {
      try {
        val institutionPK = CachedUser.get(response.get).get.institutionPK

        val machineList: Seq[(String, String)] = {
          val sorted = Machine.listMachinesFromInstitution(institutionPK).sortWith(Machine.orderMachine)
          val textList = sorted.map(m => (m.machinePK.get.toString, m.getRealId))
          textList
        }

        val list = allSelector ++ machineList

        list
      } catch {
        case _: Throwable => allSelector
      }
    } else
      allSelector
  }

  private val machineSelector = new WebInputSelect(
    label = "Machine:", //
    showLabel = true, //
    col = 2, //
    offset = 0, //
    makeMachineList, //
    aqaAlias = false, //
    submitOnChange = true
  ) //

  private val requireIsoCheck =
    new WebInputCheckbox(label = "IsoCheck", showLabel = true, title = Some("Check to only list IsoCheck data sets."), col = 1, offset = 0, submitOnChange = true)

  private def list = new WebUtil.WebPlainText(label = "Winston Lutz Results", showLabel = false, col = 10, offset = 0, html = makeList)

  //  class WebForm(action: String, title: Option[String], rowList: List[WebRow], fileUpload: Int, runScript: Option[String] = None)
  private def form =
    new WebForm(
      pathOf,
      title = None,
      rowList = List(List(refreshButton, newestButton, prevButton, nextButton, oldestButton, rowsPerPageField), List(machineSelector, requireIsoCheck, datePicker), List(list)),
      fileUpload = -1,
      runScript = Some(WLUpdateRestlet.makeJS)
    )
  private def setFormResponse(valueMap: ValueMapT, response: Response): Unit = form.setFormResponse(valueMap, errorMap = styleNone, pageTitle = "Winston Lutz", response, Status.SUCCESS_OK)

  private val wlProcedurePK: Long = {
    try {
      Procedure.ProcOfWinstonLutz.get.procedurePK.get
    } catch {
      case _: Throwable =>
        logger.error("Unable to get Winston-Lutz procedure key.  This is a configuration problem.")
        -1
    }
  }

  /* Number of ms in a 24-hour day. */
  private val day_ms = 24 * 60 * 60 * 1000

  private def rowsPerPage(valueMap: ValueMapT) = {
    try {
      val rpp = valueMap(rowsPerPageField.label).toInt
      rpp
    } catch {
      case _: Throwable => Config.WLRowsPerPageDefault
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

    val machinePK: Option[Long] = {
      if (valueMap.contains(machineSelector.label) && (valueMap(machineSelector.label).toLong > 0)) {
        Some(valueMap(machineSelector.label).toLong)
      } else
        None
    }

    val isoCheck: Boolean = valueMap.contains(requireIsoCheck.label)

    val dataList =
      Output.getOutputChunk(institutionPK = user.get.institutionPK, date = new Timestamp(ms + day_ms), count = rowsPerPage(valueMap), procedurePK = wlProcedurePK, machinePK = machinePK, isoCheck)

    val dateFormat = new SimpleDateFormat("EEE MMM d YYYY HH:mm")

    val padding = "padding:12px;"

    val userElemMap = scala.collection.mutable.HashMap[Long, Elem]()

    def getUserElem(approval: Option[OutputApproval]): Elem = {
      if (approval.isDefined) {
        userElemMap.get(approval.get.userPK) match {
          case Some(elem) => elem
          case _ =>
            val elem = WebUtil.wrapAlias(User.get(approval.get.userPK).get.id)
            userElemMap.put(approval.get.userPK, elem)
            elem
        }
      } else { <span> </span> }
    }

    /**
      * Convert one output row to a line in the HTML table.
      *
      * @param output For this output.
      * @return HTML
      */
    def toRow(output: Output): Elem = {

      val machineName = machineList.find(_.machinePK.get == output.machinePK.get).get.id

      val approval = OutputApproval.getByOutput(output.outputPK.get).lastOption

      val approvalElem: Elem = {
        val text = if (approval.isDefined && OutputApproval.stringToStatus(approval.get.status).isDefined) {
          OutputApproval.stringToStatus(approval.get.status).get.htmlPrefix
        } else {
          OutputApproval.UNAPPROVED.htmlPrefix
        }
        val href = ViewOutput.viewOutputUrl(output.outputPK.get)

        <a href={href}>{text} {getUserElem(approval)}</a>
      }

      val link = {
        val dateText = dateFormat.format(new Date(output.dataDate.get.getTime))
        val href = ViewOutput.viewOutputUrl(output.outputPK.get)
        <a title="Data analysis time" href={href}> {dateText}</a>
      }

      val wlList = WinstonLutz.getByOutput(output.outputPK.get).sortBy(_.dataDate.getTime)

      val beams: String = {
        wlList.size.toString
      }

      val isoCheckHtml: String = {
        val isoCheck = IsoCheck.getByOutput(output.outputPK.get)
        if (isoCheck.nonEmpty) {
          val wlMap = new WLMap(wlList)

          val wlIsoTable = WLIsoTable.make(wlMap)

          // one of the beams is used for both table and non-table calculations, so use 8 insteat of 9.
          val size = if (wlIsoTable.isDefined) wlIsoTable.get.beamList.size + 8 else 9
          s"IsoCheck $size"
        } else
          ""
      }

      <tr>
        <td style={padding}>{link}</td>
        <td style={padding}>{approvalElem}</td>
        <td style={padding}>{WebUtil.wrapAlias(machineName)}</td>
        <td style={padding}>{OutputList.redoUrl(output.outputPK.get)}</td>
        <td style={padding}>{beams}</td>
        <td style={padding}>{isoCheckHtml}</td>
      </tr>
    }

    <div>
      <table style="text-align: left;">
        <tr style="text-align: center;">
          <td style={padding}><b>Date</b></td>
          <td style={padding}><b>Approval</b></td>
          <td style={padding}><b>Machine</b></td>
          <td style={padding}><b>Machine</b></td>
          <td style={padding}><b>No. of Beams</b></td>
          <td style={padding}><b>IsoCheck</b></td>
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

  private def refresh(valueMap: ValueMapT, response: Response): Unit = {
    setFormResponse(valueMap, response)
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
        case _ if buttonIs(valueMap, refreshButton) => refresh(valueMap, response)
        case _ if buttonIs(valueMap, oldestButton)  => showOldest(valueMap, response)
        case _ if buttonIs(valueMap, newestButton)  => showNewest(valueMap, response)
        case _ if buttonIs(valueMap, prevButton)    => showPrev(valueMap, response)
        case _ if buttonIs(valueMap, nextButton)    => showNext(valueMap, response)
        case _                                      => setFormResponse(valueMap, response)
      }
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }
}
