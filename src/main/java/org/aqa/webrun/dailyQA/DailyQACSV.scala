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
import org.aqa.db.Input
import org.aqa.db.DicomAnonymous
import com.pixelmed.dicom.TagFromName

object DailyQACSV {

  def getCsv(dataSetList: Seq[BBbyEPIDComposite.DailyDataSetComposite], response: Response): Unit = {

    def sorter(a: BBbyEPIDComposite.DailyDataSetComposite, b: BBbyEPIDComposite.DailyDataSetComposite): Boolean = {
      if (a.machine.machinePK.get != b.machine.machinePK.get) (a.machine.machinePK.get < b.machine.machinePK.get)
      else {
        if (a.output.dataDate.get.getTime != b.output.dataDate.get.getTime) (a.output.dataDate.get.getTime < b.output.dataDate.get.getTime)
        else {
          a.output.startDate.getTime < b.output.startDate.getTime
        }
      }
    }

    val institutionPK = WebUtil.getUser(response.getRequest).get.institutionPK
    val patIdMap = DicomAnonymous.getAttributesByTag(institutionPK, Seq(TagFromName.PatientID)).
      map(da => (da.value, AnonymizeUtil.decryptWithNonce(institutionPK, da.value_real))).toMap

    /**
     * Build map of machine id --> real id.
     */
    val machineNameSet: Map[String, String] = {
      val machList = dataSetList.groupBy(_.machine.machinePK.get).map(group => group._2.head.machine)
      machList.map(mach => (mach.id, AnonymizeUtil.decryptWithNonce(institutionPK, mach.id_real.get))).toMap
    }

    def patientIdOf(dataSet: BBbyEPIDComposite.DailyDataSetComposite): String = {
      val unknown = "unknown"
      val patId = try {
        val anonPatId = Input.get(dataSet.output.inputPK).get.patientId.get
        val p = patIdMap.get(anonPatId) match {
          case Some(text) => text
          case _ => unknown
        }
        p
      } catch {
        case t: Throwable => unknown
      }

      patId
    }

    val urlPrefix = response.getRequest.getHostRef

    def dblOptToString10(d: Option[Double]) = if (d.isDefined) (d.get / 10).toString else "NA"

    case class Col(header: String, toText: (BBbyEPIDComposite.DailyDataSetComposite) => String);

    val colList = Seq[Col](
      new Col("Machine", (dataSet) => machineNameSet(dataSet.machine.id)),
      new Col("Acquired", (dataSet) => Util.standardDateFormat.format(dataSet.output.dataDate.get)),
      new Col("Analysis", (dataSet) => Util.standardDateFormat.format(dataSet.output.startDate)),
      new Col("PatientID", (dataSet) => patientIdOf(dataSet)),
      new Col("Status", (dataSet) => dataSet.output.status),

      new Col("X CBCT - ISO mm", (dataSet) => (dataSet.cbct.cbctX_mm - dataSet.cbct.rtplanX_mm).toString),
      new Col("Y CBCT - ISO mm", (dataSet) => (dataSet.cbct.cbctY_mm - dataSet.cbct.rtplanY_mm).toString),
      new Col("Z CBCT - ISO mm", (dataSet) => (dataSet.cbct.cbctZ_mm - dataSet.cbct.rtplanZ_mm).toString),

      new Col("X/lat Table Movement cm", (dataSet) => dblOptToString10(dataSet.composite.tableXlateral_mm)),
      new Col("Y/vert Table Movement cm", (dataSet) => dblOptToString10(dataSet.composite.tableYvertical_mm)),
      new Col("Z/lng Table Movement cm", (dataSet) => dblOptToString10(dataSet.composite.tableZlongitudinal_mm)),

      new Col("X/lat Table Posn CBCT cm", (dataSet) => (dataSet.cbct.tableXlateral_mm / 10).toString),
      new Col("Y/vert Table Posn CBCT cm", (dataSet) => (dataSet.cbct.tableYvertical_mm / 10).toString),
      new Col("Z/lng Table Posn CBCT cm", (dataSet) => (dataSet.cbct.tableZlongitudinal_mm / 10).toString),

      new Col("X/lat Table Posn EPID cm", (dataSet) => (dataSet.bbByEpid.head.tableXlateral_mm / 10).toString),
      new Col("Y/vert Table Posn EPID cm", (dataSet) => (dataSet.bbByEpid.head.tableYvertical_mm / 10).toString),
      new Col("Z/lng Table Posn EPID cm", (dataSet) => (dataSet.bbByEpid.head.tableZlongitudinal_mm / 10).toString),

      new Col("Gantry Angle for XZ (vert) deg", (dataSet) => dataSet.vertList.head.gantryAngle_deg.toString),
      new Col("Vert (EPID-ISO) X mm", (dataSet) => dataSet.vertList.head.epid3DX_mm.toString),
      new Col("Vert (EPID-ISO) Z mm", (dataSet) => dataSet.vertList.head.epid3DZ_mm.toString),
      new Col("Vert (EPID-ISO) - (CBCT-ISO) X mm", (dataSet) => dataSet.composite.xAdjusted_mm.get.toString),
      new Col("Vert (EPID-ISO) - (CBCT-ISO) Z mm", (dataSet) => (dataSet.vertList.head.epid3DZ_mm - dataSet.cbct.err_mm.getZ).toString),

      new Col("Gantry Angle for YZ (horz) deg", (dataSet) => dataSet.horzList.head.gantryAngle_deg.toString),
      new Col("Horz (EPID-ISO) Y mm", (dataSet) => dataSet.horzList.head.epid3DY_mm.toString),
      new Col("Horz (EPID-ISO) Z mm", (dataSet) => dataSet.horzList.head.epid3DZ_mm.toString),
      new Col("Horz (EPID-ISO) - (CBCT-ISO) Y mm", (dataSet) => dataSet.composite.yAdjusted_mm.get.toString),
      new Col("Horz (EPID-ISO) - (CBCT-ISO) Z mm", (dataSet) => (dataSet.horzList.head.epid3DZ_mm - dataSet.cbct.err_mm.getZ).toString),

      new Col("Avg (EPID-ISO) - (CBCT-ISO) Z mm", (dataSet) => (dataSet.composite.zAdjusted_mm.get).toString),

      new Col("CBCT Details", (dataSet) => urlPrefix + ViewOutput.viewOutputUrl(dataSet.cbct.outputPK)),
      new Col("EPID Details", (dataSet) => urlPrefix + ViewOutput.viewOutputUrl(dataSet.composite.outputPK)))

    val headerList = colList.map(col => '"' + col.header + '"').mkString(",")

    def makeRow(dataSet: BBbyEPIDComposite.DailyDataSetComposite) = colList.map(col => col.toText(dataSet)).mkString(",")

    val rowList = dataSetList.sortWith(sorter _).map(dataSet => makeRow(dataSet)).mkString("\n")

    val csv = headerList + "\n" + rowList

    response.setEntity(csv, MediaType.TEXT_CSV)
    response.setStatus(Status.SUCCESS_OK)
  }
}