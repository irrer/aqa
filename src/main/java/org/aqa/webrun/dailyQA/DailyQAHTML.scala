package org.aqa.webrun.dailyQA

import org.aqa.web.WebUtil._
import scala.xml.Elem
import org.aqa.db.BBbyEPIDComposite
import com.pixelmed.dicom.TagFromName
import org.aqa.db.DicomSeries
import org.aqa.Util
import org.aqa.web.ViewOutput

object DailyQAHTML {

  def makeReport(dataSetList:  Seq[BBbyEPIDComposite.DailyDataSet]): Elem = {

    // val dataSetList: Seq[BBbyEPIDComposite.DailyDataSet]

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
      <td>{ DailyQASummary.dateFormat.format(dataSet.output.dataDate.get) }</td>
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

      new Col("Gantry Angle for XZ", "Angle of gantry for vertical image in degrees used to calculate values for Y and Z", colVertGantryAngle _),
      new Col("Vert EPID - CAX(X) mm", "X offset Vertical EPID image - CAX in mm", colVertXCax _),
      new Col("Vert EPID - CAX(Z) mm", "Z offset Vertical EPID image - CAX in mm", colVertZCax _),

      new Col("Gantry Angle for YZ", "Angle of gantry for horizontal image in degrees used to calculate values for X and Z", colHorzGantryAngle _),
      new Col("Horz EPID - CAX(Y) mm", "Y offset Horizontal EPID image - CAX in mm", colHorzYCax _),
      new Col("Horz EPID - CAX(Z) mm", "Z offset Horizontal EPID image - CAX in mm", colHorzZCax _),

      new Col("CBCT Details", "Images and other details for CBCT", colCbctImages _),
      new Col("EPID Details", "Images and other details for EPID", colEpidImages _))

    def dataSetToRow(dataSet: BBbyEPIDComposite.DailyDataSet) = {
      <tr>{ colList.map(col => col.toElem(dataSet)) }</tr>
    }

    val content = {
      <div class="row">
        <table class="table table-responsive table-bordered">
          <thead><tr>{ colList.map(col => col.toHeader) }</tr></thead>
          {
            dataSetList.map(dataSet => dataSetToRow(dataSet))
          }
        </table>
      </div>
    }

    content
  }
}