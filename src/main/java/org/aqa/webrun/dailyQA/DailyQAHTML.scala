package org.aqa.webrun.dailyQA

import org.aqa.web.WebUtil._
import scala.xml.Elem
import org.aqa.db.BBbyEPIDComposite
import com.pixelmed.dicom.TagFromName
import org.aqa.db.DicomSeries
import org.aqa.Util
import org.aqa.web.ViewOutput
import org.aqa.Crypto
import org.aqa.AnonymizeUtil
import java.util.Date
import org.aqa.Config
import org.aqa.db.Machine

object DailyQAHTML {

  def makeReport(dataSetList: Seq[BBbyEPIDComposite.DailyDataSet], institutionPK: Long, date: Date): Elem = {

    def sortDataSetPair(a: BBbyEPIDComposite.DailyDataSet, b: BBbyEPIDComposite.DailyDataSet): Boolean = {
      if (a.machine.machinePK.get == b.machine.machinePK.get) {
        a.output.dataDate.get.getTime < b.output.dataDate.get.getTime
      } else {
        val aMach = AnonymizeUtil.decryptWithNonce(a.machine.institutionPK, a.machine.id_real.get)
        val bMach = AnonymizeUtil.decryptWithNonce(b.machine.institutionPK, b.machine.id_real.get)
        def toNum(text: String): Option[Int] = {
          if (text.matches(".*[0-9].*"))
            Some(text.replaceAll("[^0-9]", "").toInt)
          else
            None
        }

        if (toNum(aMach).isDefined && toNum(bMach).isDefined) {
          toNum(aMach).get < toNum(bMach).get
        } else {
          aMach.compareTo(bMach) <= 0
        }
      }
    }

    def fmtAngle(angle: Double) = angle.formatted("%12.8f").trim

    var machinePassed = true

    case class Col(name: String, title: String, toElem: (BBbyEPIDComposite.DailyDataSet) => Elem) {
      def toHeader = <th title={ title }>{ name }</th>
    }

    def colMachine(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      val machElem = wrapAlias(dataSet.machine.id)
      if (machinePassed) {
        <td title="Machine Name" style="color: #000000; background: #1dc32b;"><h4>{ machElem }<br></br>Pass</h4></td>
      } else {
        <td class="danger" title={ "At least one value is out of tolerance" } style="color: #000000; background: #e00034;"><h4>{ machElem }<br></br>Fail</h4></td>
      }
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

    def posnRow(posn: Double): Elem = {
      val text = posn.formatted("%7.2f").trim
      val title = posn.formatted("%12.6f").trim
      if (posn.abs > Config.DailyQATolerance_mm) {
        machinePassed = false
        <td class="danger" title={ title + " is out of tolerance of " + Config.DailyQATolerance_mm + " mm" }>{ text }</td>
      } else {
        <td title={ title }>{ text }</td>
      }
    }

    def colDateTime(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      <td>{ DailyQASummary.timeFormat.format(dataSet.output.dataDate.get) }</td>
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

    def colEpidPlanCbct(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      if (dataSet.epid.offsetAdjusted_mm.isDefined) posnRow(dataSet.epid.offsetAdjusted_mm.get)
      else <div>undefined</div>
    }

    def colCbctImages(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      <td><a href={ ViewOutput.viewOutputUrl(dataSet.cbct.outputPK) }>CBCT Details</a></td>
    }

    def colEpidImages(dataSet: BBbyEPIDComposite.DailyDataSet): Elem = {
      <td><a href={ ViewOutput.viewOutputUrl(dataSet.epid.outputPK) }>EPID Details</a></td>
    }

    val reportDate: String = {
      try {
        DailyQASummary.dateFormat.format(dataSetList.head.output.dataDate.get)
      } catch {
        case t: Throwable => ""
      }
    }

    val colList: List[Col] = List(
      new Col("Machine", "Name of treatment machine", colMachine _),
      new Col("Patient", "Name of test patient", colPatient _),
      new Col("EPID Time " + reportDate, "Time of EPID acquisition", colDateTime _),

      new Col("X CBCT - PLAN mm", "(plan X) - (plan X) in mm", colCbctX _),
      new Col("Y CBCT - PLAN mm", "(plan Y) - (plan Y) in mm", colCbctY _),
      new Col("Z CBCT - PLAN mm", "(plan Z) - (plan Z) in mm", colCbctZ _),

      new Col("Gantry Angle for XZ", "Angle of gantry for vertical image in degrees used to calculate values for Y and Z", colVertGantryAngle _),
      new Col("Vert EPID - CAX(X) mm", "X offset Vertical EPID image - CAX in mm", colVertXCax _),
      new Col("Vert EPID - CAX(Z) mm", "Z offset Vertical EPID image - CAX in mm", colVertZCax _),

      new Col("Gantry Angle for YZ", "Angle of gantry for horizontal image in degrees used to calculate values for X and Z", colHorzGantryAngle _),
      new Col("Horz EPID - CAX(Y) mm", "Y offset Horizontal EPID image - CAX in mm", colHorzYCax _),
      new Col("Horz EPID - CAX(Z) mm", "Z offset Horizontal EPID image - CAX in mm", colHorzZCax _),

      new Col("EPID - (PLAN - CBCT)", "total offset of EPID - (PLAN - CBCT)", colEpidPlanCbct _),

      new Col("CBCT Details", "Images and other details for CBCT", colCbctImages _),
      new Col("EPID Details", "Images and other details for EPID", colEpidImages _))

    def dataSetToRow(dataSet: BBbyEPIDComposite.DailyDataSet) = {
      machinePassed = true
      val tdList = colList.tail.map(col => col.toElem(dataSet))
      <tr>{ colList.head.toElem(dataSet) :+ tdList }</tr>
    }

    val machinesMissingResults = {
      val haveData = dataSetList.map(ds => ds.machine.id).distinct
      val allMachines = Machine.listMachinesFromInstitution(institutionPK).filter(m => m.active).map(m => m.id)
      val idList = allMachines.diff(haveData)
      val title = Seq(
        "These machine do not have at least one pair",
        "Of CBCT and EPID image sets required to ",
        "calculate results.  The CBCT data must",
        "be captured before the EPID, and",
        "both must be captured on the same day.").mkString(titleNewline)
      def idToElem(id: String) = {
        <span style="margin-right: 15px;">{ wrapAlias(id) }</span>
      }
      if (idList.isEmpty) <span></span>
      else {
        <h4 class="p-3 mb-2 bg-danger text-light">
          <div>
            <div title={ title } style="margin: 8px;">Machines without results: { idList.map(id => idToElem(id)) }</div>
          </div>
        </h4>
      }
    }

    val content = {
      <div class="row">
        <div class="row">
          { machinesMissingResults }
        </div>
        <div class="row">
          <table class="table table-responsive table-bordered">
            <col/>
            <col/>
            <col width="110"/>
            <thead><tr>{ colList.map(col => col.toHeader) }</tr></thead>
            {
              dataSetList.sortWith(sortDataSetPair).map(dataSet => dataSetToRow(dataSet))
            }
          </table>
        </div>
        <div class="row">
          <div class="col-md-8 col-md-offset-2 col-sm-12">
            <center>
              Machines above that have any measurements out of tolerance by{ Util.fmtDbl(Config.DailyQATolerance_mm) }
              mm or more are marked as failed.  To produce a final result for a single machine, there must be both CBCT
              and EPID results.  Both must be valid (found the BB near isocenter), the CBCT must scanned before the EPID,
              and they must be scanned on the same day.
            </center>
          </div>
        </div>
        <div class="row">
          <div class="col-md-6 col-md-offset-3 col-sm-12">
            { DailyQAFullResults.get(institutionPK, date) }
          </div>
        </div>
      </div>
    }

    content
  }
}