package org.aqa.webrun.wl

import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.WinstonLutz
import org.aqa.web.OutputList
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.web.WebUtil.SubUrlRoot
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.util.Date
import scala.xml.Elem

class WLNav extends Restlet with SubUrlRoot with Logging {

  private val rowsPerPage = 25 // TODO make this a config ?

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action: String = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, _ => action, buttonType, name, Some(""))
  }

  private val oldestButton = makeButton("Oldest", primary = true, ButtonType.BtnPrimary)
  private val prevButton = makeButton("Prev Page", primary = false, ButtonType.BtnDanger)
  private val nextButton = makeButton("Next Page", primary = false, ButtonType.BtnDefault)
  private val newestButton = makeButton("Newest", primary = false, ButtonType.BtnDefault)
  private val datePicker = new WebInputDateTimePicker("Date", 6, 0)

  private def list = new WebUtil.WebPlainText(label = "Winston Lutz Results", showLabel = false, col = 10, offset = 0, html = makeList)

  private val form = new WebForm(pathOf, List(List(prevButton, nextButton), List(oldestButton), List(newestButton), List(datePicker), List(list)))

  private def makeList(valueMap: ValueMapT): Elem = {

    Trace.trace() // TODO rm
    val date = {
      valueMap.get(datePicker.label) match {
        case Some(dateText) => Trace.trace("dateText: " + dateText) // TODO
        case None           =>
      }
    }

    val user = getUser(valueMap)
    val machineList = Machine.listMachinesFromInstitution(user.get.institutionPK)

    val dataList = WinstonLutz.historyByDate(date = (new Date), count = rowsPerPage, institutionPK = user.get.institutionPK)

    Trace.trace() // TODO rm

    def toRow(h: WinstonLutz.WinstonLutzHistory): Elem = {
      val machineName = machineList.find(_.machinePK.get == h.output.machinePK.get).get.id
      <tr>
        <td>{h.output.dataDate.get}</td>
        <td>{WebUtil.wrapAlias(machineName)}</td>
        <td>{"image"}</td>
        <td>{OutputList.redoUrl(h.output.outputPK.get)}</td>
      </tr>
    }

    <table>
      <tr>
        <td>Date</td>
        <td>Machine</td>
        <td>Image</td>
        <td></td>
      </tr>
      { dataList.map(toRow) }
    </table>
    // Output.getByProcedure(Procedure.ProcOfWinstonLutz.get.procedurePK.get)
  }

  private def showOldest(valueMap: ValueMapT, response: Response) = {
    ???
  }

  private def showNewest(valueMap: ValueMapT, response: Response) = {
    ???
  }

  private def showPrev(valueMap: ValueMapT, response: Response) = {
    ???
  }

  private def showNext(valueMap: ValueMapT, response: Response) = {
    ???
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
        case _                                     => form.setFormResponse(valueMap, errorMap = styleNone, pageTitle = "Winston Lutz", response, Status.SUCCESS_OK)
      }
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }
}
