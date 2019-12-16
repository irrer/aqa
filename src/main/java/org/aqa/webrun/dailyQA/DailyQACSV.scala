package org.aqa.webrun.dailyQA

import org.restlet.Response
import java.util.Date
import org.aqa.db.BBbyEPIDComposite
import org.aqa.Util
import org.aqa.web.ViewOutput
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.aqa.web.WebUtil
import org.aqa.AnonymizeUtil

object DailyQACSV {

  def getCsv(dataSetList: Seq[BBbyEPIDComposite.DailyDataSet], response: Response): Unit = {

    def sorter(a: BBbyEPIDComposite.DailyDataSet, b: BBbyEPIDComposite.DailyDataSet): Boolean = {
      if (a.machine.machinePK.get != b.machine.machinePK.get) (a.machine.machinePK.get < b.machine.machinePK.get)
      else {
        if (a.output.dataDate.get.getTime != b.output.dataDate.get.getTime) (a.output.dataDate.get.getTime < b.output.dataDate.get.getTime)
        else {
          a.output.startDate.getTime < b.output.startDate.getTime
        }
      }
    }

    /**
     * Build map of machine id --> real id.
     */
    val machineNameSet: Map[String, String] = {
      val institutionPK = WebUtil.getUser(response.getRequest).get.institutionPK
      val machList = dataSetList.groupBy(_.machine.machinePK.get).map(group => group._2.head.machine)
      machList.map(mach => (mach.id, AnonymizeUtil.decryptWithNonce(institutionPK, mach.id_real.get))).toMap
    }

    val urlPrefix = response.getRequest.getHostRef

    case class Col(header: String, toText: (BBbyEPIDComposite.DailyDataSet) => String);

    val colList = Seq[Col](
      new Col("Machine", (dataSet) => machineNameSet(dataSet.machine.id)),
      new Col("Acquired", (dataSet) => Util.standardDateFormat.format(dataSet.output.dataDate.get)),
      new Col("Analysis", (dataSet) => Util.standardDateFormat.format(dataSet.output.startDate)),
      new Col("Status", (dataSet) => dataSet.output.status),

      new Col("X CBCT - PLAN mm", (dataSet) => (dataSet.cbct.cbctX_mm - dataSet.cbct.rtplanX_mm).toString),
      new Col("Y CBCT - PLAN mm", (dataSet) => (dataSet.cbct.cbctY_mm - dataSet.cbct.rtplanY_mm).toString),
      new Col("Z CBCT - PLAN mm", (dataSet) => (dataSet.cbct.cbctZ_mm - dataSet.cbct.rtplanZ_mm).toString),

      new Col("Gantry Angle for XZ (vert) deg", (dataSet) => dataSet.vertList.head.gantryAngle_deg.toString),
      new Col("Vert EPID - CAX(X) mm", (dataSet) => dataSet.epid.xAdjusted_mm.get.toString),
      new Col("Vert EPID - CAX(Z) mm", (dataSet) => (dataSet.vertList.head.epid3DZ_mm - (dataSet.cbct.rtplanZ_mm - dataSet.cbct.cbctZ_mm)).toString),

      new Col("Gantry Angle for YZ (horz) deg", (dataSet) => dataSet.horzList.head.gantryAngle_deg.toString),
      new Col("Horz EPID - CAX(Y) mm", (dataSet) => dataSet.epid.yAdjusted_mm.get.toString),
      new Col("Horz EPID - CAX(Z) mm", (dataSet) => (dataSet.horzList.head.epid3DZ_mm - (dataSet.cbct.rtplanZ_mm - dataSet.cbct.cbctZ_mm)).toString),

      new Col("CBCT Details", (dataSet) => urlPrefix + ViewOutput.viewOutputUrl(dataSet.cbct.outputPK)),
      new Col("EPID Details", (dataSet) => urlPrefix + ViewOutput.viewOutputUrl(dataSet.epid.outputPK)))

    val headerList = colList.map(col => '"' + col.header + '"').mkString(",")

    def makeRow(dataSet: BBbyEPIDComposite.DailyDataSet) = colList.map(col => col.toText(dataSet)).mkString(",")

    val rowList = dataSetList.sortWith(sorter _).map(dataSet => makeRow(dataSet)).mkString("\n")

    val csv = headerList + "\n" + rowList

    response.setEntity(csv, MediaType.TEXT_CSV)
    response.setStatus(Status.SUCCESS_OK)
  }
}