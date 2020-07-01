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
import org.aqa.db.BBbyCBCT
import org.aqa.db.BBbyEPID
import org.aqa.db.Output
import org.aqa.AngleType
import org.aqa.run.ProcedureStatus
import org.aqa.Logging
import edu.umro.ScalaUtil.Trace
import org.aqa.db.Procedure
import java.sql.Timestamp
import java.text.SimpleDateFormat

object DailyQAHTML extends Logging {

  def makeReport(dataSetList: Seq[BBbyEPIDComposite.DailyDataSetComposite], institutionPK: Long, date: Date): Elem = {

    /** Local class for Machine, Output, and Elem. */
    case class MOE(machine: Machine, output: Option[Output], elem: Elem);

    def sortMOE(a: MOE, b: MOE): Boolean = {
      if ((a.machine.machinePK.get == b.machine.machinePK.get) && a.output.isDefined && b.output.isDefined) {
        a.output.get.dataDate.get.getTime < b.output.get.dataDate.get.getTime
      } else {
        val aMach = AnonymizeUtil.decryptWithNonce(a.machine.institutionPK, a.machine.id_real.get)
        val bMach = AnonymizeUtil.decryptWithNonce(b.machine.institutionPK, b.machine.id_real.get)
        def toNum(text: String): Option[Int] = {
          if (text.matches(".*[0-9].*"))
            Some(text.replaceAll("[^0-9]", "").toInt)
          else
            None
        }

        val cmpr =
          if (toNum(aMach).isDefined && toNum(bMach).isDefined && (toNum(aMach).get != toNum(bMach).get)) {
            toNum(aMach).get < toNum(bMach).get
          } else {
            aMach.compareTo(bMach) <= 0
          }
        cmpr
      }
    }

    def fmtAngle(angle: Double) = angle.formatted("%12.8f").trim

    var machinePassed = true

    case class Col(name: String, title: String, toElem: (BBbyEPIDComposite.DailyDataSetComposite) => Elem) {
      def toHeader = <th title={ title }>{ name }</th>
    }

    val stylePass = "color: #000000; background: #1dc32b;"
    val styleFail = "color: #000000; background: #e00034;"
    val styleNoData = "color: #000000; background: #888888;"
    val styleWarn = "color: #000000; background: yellow;"
    val col0Title = "Machine Name"

    def colMachine(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val machElem = wrapAlias(dataSet.machine.id)
      if (machinePassed) {
        <td title={ col0Title } style={ stylePass }><h4>{ machElem }<br/>Pass</h4></td>
      } else {
        <td class="danger" title={ "At least one value is out of tolerance" } style={ styleFail }><h4>{ machElem }<br/>Fail</h4></td>
      }
    }

    def colPatient(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {

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

    def colDateTime(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      <td>{ DailyQASummary.timeFormat.format(dataSet.output.dataDate.get) }</td>
    }

    def colCbctX(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val x = dataSet.cbct.cbctX_mm - dataSet.cbct.rtplanX_mm
      posnRow(x)
    }

    def colCbctY(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val y = dataSet.cbct.cbctY_mm - dataSet.cbct.rtplanY_mm
      posnRow(y)
    }

    def colCbctZ(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val z = dataSet.cbct.cbctZ_mm - dataSet.cbct.rtplanZ_mm
      posnRow(z)
    }

    def colVertGantryAngle(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val angle = dataSet.vertList.head.gantryAngle_deg
      <td title={ fmtAngle(angle) }>{ Util.angleRoundedTo90(angle) }</td>
    }

    def colVertXCax(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      posnRow(dataSet.epid.xAdjusted_mm.get)
    }

    def colVertZCax(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val offset = dataSet.vertList.head.epid3DZ_mm - (dataSet.cbct.rtplanZ_mm - dataSet.cbct.cbctZ_mm)
      posnRow(offset)
    }

    def colHorzGantryAngle(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val angle = dataSet.horzList.head.gantryAngle_deg
      <td title={ fmtAngle(angle) }>{ Util.angleRoundedTo90(angle) }</td>
    }

    def colHorzYCax(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      posnRow(dataSet.epid.yAdjusted_mm.get)
    }

    def colHorzZCax(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val offset = dataSet.horzList.head.epid3DZ_mm - (dataSet.cbct.rtplanZ_mm - dataSet.cbct.cbctZ_mm)
      <td>{ Util.fmtDbl(dataSet.epid.xAdjusted_mm.get) }</td>
      posnRow(dataSet.epid.zAdjusted_mm.get)
    }

    def colEpidPlanCbct(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      if (dataSet.epid.offsetAdjusted_mm.isDefined) posnRow(dataSet.epid.offsetAdjusted_mm.get)
      else <div>undefined</div>
    }

    def colCbctImages(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      <td><a href={ ViewOutput.viewOutputUrl(dataSet.cbct.outputPK) }>CBCT Details</a></td>
    }

    def colEpidImages(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
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

    def dataSetToRow(dataSet: BBbyEPIDComposite.DailyDataSetComposite): MOE = {
      machinePassed = true
      val tdList = colList.tail.map(col => col.toElem(dataSet))
      val elem = <tr>{ colList.head.toElem(dataSet) :+ tdList }</tr>
      new MOE(dataSet.machine, Some(dataSet.output), elem)
    }

    val machinesMissingResults = {
      val haveData = dataSetList.map(ds => ds.machine.id).distinct
      val allMachines = Machine.listMachinesFromInstitution(institutionPK).filter(m => m.active)
      val noData = allMachines.filterNot(m => haveData.contains(m.id))

      noData
    }

    /** Procedures that we are interested in. */
    val procedureSeq = Procedure.list.filter(p => p.isBBbyCBCT || p.isBBbyEPID)
    val cbctProc = procedureSeq.find(p => p.isBBbyCBCT).get
    val epidProc = procedureSeq.find(p => p.isBBbyEPID).get

    /** all outputs for CBCT and EPID for all machines from this institution with data from this day sorted by data (acquisition) date. */
    val allOutputs = {
      val dataDateBegin = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(date).replaceAll("T.*", "T00:00:00")).getTime)
      val dataDateEnd = new Timestamp(dataDateBegin.getTime + (24 * 60 * 60 * 1000))

      val procedurePkSet = procedureSeq.map(p => p.procedurePK.get)
      Output.getOutputByDateRange(institutionPK, dataDateBegin, dataDateEnd).filter(o => procedurePkSet.contains(o.procedurePK)).sortBy(o => o.dataDate.get.getTime)
    }

    def outputCBCT(machinePK: Long) = allOutputs.filter(o => ((o.machinePK.get == machinePK) && (o.procedurePK == cbctProc.procedurePK.get)))
    def outputEPID(machinePK: Long) = allOutputs.filter(o => ((o.machinePK.get == machinePK) && (o.procedurePK == epidProc.procedurePK.get)))

    // List of reasons that each machine is missing a full set of results.
    val missingResultsExplanations: Seq[MOE] = {

      val allCbctSeq = BBbyCBCT.getForOneDay(date, institutionPK)
      val allEpidSeq = BBbyEPID.getForOneDay(date, institutionPK)
      Trace.trace(allCbctSeq.mkString("\n    "))
      Trace.trace(allEpidSeq.mkString("\n    "))

      def explain(mach: Machine): Elem = {
        val cbctResults = allCbctSeq.filter(c => c.machine.machinePK.get == mach.machinePK.get)
        val epidResults = allEpidSeq.filter(c => c.machine.machinePK.get == mach.machinePK.get)
        val cbctOutput = outputCBCT(mach.machinePK.get)
        val epidOutput = outputEPID(mach.machinePK.get)

        val listColSpanSize = 6
        val listColSpan = listColSpanSize.toString
        val messageColspan = (colList.size - (listColSpanSize + 1)).toString
        val timeFormat = new SimpleDateFormat("H:mm")

        def cbctBBnotFound(cOut: Output): Boolean = cbctResults.find(c => c.output.outputPK.get == cOut.outputPK.get).isEmpty

        /**
         * True if this EPID output has data for horizontal gantry angle.
         */
        def hasHorzAngle(output: Output): Boolean = {
          val eSeq = epidResults.filter(e => e.bbByEPID.outputPK == output.outputPK.get)
          val horz = eSeq.find(e => e.isHorz)
          if (true) { // TODO rm
            Trace.trace(output.outputPK.get)
            Trace.trace(horz)
            Trace.trace(eSeq.map(e => (e.bbByEPID.outputPK, Util.fmtDbl(e.bbByEPID.gantryAngle_deg))).mkString("    "))
          }
          horz.isDefined
        }

        /**
         * True if this EPID output has data for the vertical gantry angle.
         */
        def hasVertAngle(output: Output): Boolean = {
          val eSeq = epidResults.filter(e => e.bbByEPID.outputPK == output.outputPK.get)
          val vert = eSeq.find(e => e.isVert)
          if (true) { // TODO rm
            Trace.trace(output.outputPK.get)
            Trace.trace(vert)
            Trace.trace(eSeq.map(e => (e.bbByEPID.outputPK, Util.fmtDbl(e.bbByEPID.gantryAngle_deg))).mkString("    "))
          }
          vert.isDefined
        }

        val machHistory = {
          val outputSeq = allOutputs.filter(o => o.machinePK.get == mach.machinePK.get)
          def ref(o: Output): Elem = {
            val isCBCT = o.procedurePK == cbctProc.procedurePK.get
            val procName = if (isCBCT) "CBCT" else "EPID"
            val url = ViewOutput.path + "?" + ViewOutput.outputPKTag + "=" + o.outputPK.get
            val text = timeFormat.format(o.dataDate.get) + " " + procName

            def badCBCT: Elem = {
              val title = {
                "Click to view details of CBCT.  Not finding the BB is usually the result " + titleNewline +
                  "of the phantom being mis-aligned or incorrect table height/position." + titleNewline +
                  "The data from this scan can not be used, and the CBCT scan must" + titleNewline +
                  "be re-done."
              }
              <p>
                <a href={ url } title={ title }>{ text }<span style={ styleFail }>BB not found</span></a>
              </p>
            }

            def missingHorzEPIDGantryAngle: Elem = {
              val title = {
                "There is no BB found in an image from the horizontal (90 or 270 degree)" + titleNewline +
                  "gantry angle, or, there was no horizontal image captured. The BB must" + titleNewline +
                  "be visible in both a horizontal and vertical image to" + titleNewline +
                  "calculate its position. The scan must be re-done. Click to view details of EPID."
              }
              <p>
                <a href={ url } title={ title }>{ text }<span style={ styleFail }>No horizontal gantry angle image with BB found</span></a>
              </p>
            }
            //  + titleNewline +
            def missingVertEPIDGantryAngle: Elem = {
              val title = {
                "There is no BB found in an image from the vertical (0, 360, or 180 degree)" + titleNewline +
                  "gantry angle, or, there was no vertical image captured. The BB must be " + titleNewline +
                  "visible in both a vertical and horizontal image to calculate its" + titleNewline +
                  "position. The scan must be re-done. Click to view details of EPID."
              }
              <p>
                <a href={ url } title={ title }>{ text }<span style={ styleFail }>No vertical gantry angle image with BB found</span></a>
              </p>
            }

            def scanOk = {
              <p>
                <a href={ url } title="Click to view details of scan">{ text }</a>
              </p>
            }

            val elem: Elem = 0 match {
              case _ if (isCBCT && cbctBBnotFound(o)) => badCBCT
              case _ if (!isCBCT) && (!hasHorzAngle(o)) => missingHorzEPIDGantryAngle
              case _ if (!isCBCT) && (!hasVertAngle(o)) => missingVertEPIDGantryAngle
              case _ => scanOk
            }

            elem
          }

          if (outputSeq.nonEmpty) {
            <td colspan={ listColSpan }>
              <center>
                Hover for info
              </center>
              <p/>
              { outputSeq.map(o => ref(o)) }
            </td>
          } else { <td colspan={ listColSpan }></td> }
        }

        def showNoData: Elem = {
          <tr>
            <td title={ col0Title } style={ styleNoData }><h4>{ wrapAlias(mach.id) }<br/>No Data</h4></td>
            <td colspan={ messageColspan }>There are no CBCT or EPID scans for this machine.</td>
            { machHistory }
          </tr>
        }

        def showWarn(msg: String): Elem = { // show links to CBCT and EPID outputs
          <tr>
            <td title={ col0Title } style={ styleWarn }><h4>{ wrapAlias(mach.id) }<br/>Warning</h4></td>
            <td colspan={ messageColspan }>{ msg }</td>
            { machHistory }
          </tr>
        }

        def showFail(msg: String): Elem = { // show links to CBCT and EPID outputs
          <tr>
            <td title={ col0Title } style={ styleFail }><h4>{ wrapAlias(mach.id) }<br/>Fail</h4></td>
            <td colspan={ messageColspan }>{ msg }</td>
            { machHistory }
          </tr>
        }

        /**
         * Return true if the EPID was done before the CBCT.
         */
        def epidBeforeCbct = {
          Trace.trace
          if (cbctResults.nonEmpty && epidResults.nonEmpty) {
            val firstCbct = cbctResults.minBy(_.output.dataDate.get.getTime).output.dataDate.get.getTime
            val lastEpid = epidResults.maxBy(_.output.dataDate.get.getTime).output.dataDate.get.getTime
            lastEpid < firstCbct
          } else
            false
        }

        val explanation: Elem = 0 match {
          case _ if cbctOutput.isEmpty && epidOutput.isEmpty => showNoData
          case _ if cbctOutput.nonEmpty && cbctResults.isEmpty => showFail("One or more CBCTs were done but the BB was not found.  Probably mis-alignment of table or phantom.  It is recommended that the CBCT scan be repeated.")
          case _ if (cbctResults.size == 1) && epidResults.isEmpty => showWarn("There is a successful CBCT scan but no EPID scans.  It is recommended that an EPID scan be performed.")
          case _ if cbctResults.nonEmpty && epidResults.isEmpty => showWarn("There are " + cbctResults.size + " successful CBCT scans but no EPID scans.  It is recommended that an EPID scan be performed.")
          case _ if cbctResults.isEmpty && (epidOutput.nonEmpty) => showFail("There is one or more EPID scans but no CBCT scans.")
          case _ if cbctResults.isEmpty && epidResults.nonEmpty => showFail("There are " + epidOutput.size + " EPID scans but no successful CBCT scans.")
          case _ if cbctResults.nonEmpty && epidResults.isEmpty => showWarn("There are " + cbctResults.size + " CBCT scans but zero EPID scans.  The EPID scan needs to be done.")
          case _ if epidBeforeCbct => showFail("The EPID scan was done prior to CBCT.  The CBCT needs to be done first.")
          case _ => showFail("There are no results for this machine.")
        }
        explanation
      }

      machinesMissingResults.map(mach => new MOE(mach, None, explain(mach)))
    }

    val resultSeq = dataSetList.map(dataSet => dataSetToRow(dataSet))
    val all = (resultSeq ++ missingResultsExplanations).sortWith(sortMOE)

    val content = {
      <div class="row">
        <div class="row">
          <table class="table table-responsive table-bordered">
            <col/>
            <col/>
            <col width="110"/>
            <thead><tr>{ colList.map(col => col.toHeader) }</tr></thead>
            { all.map(moe => moe.elem) }
          </table>
        </div>
        <div class="row">
          <div class="col-md-8 col-md-offset-2 col-sm-12">
            <center>
              Machines above that have any measurements out of tolerance by{ Util.fmtDbl(Config.DailyQATolerance_mm) }
              mm or more are marked as failed.  To produce a final result for a single machine, there must be both CBCT
              and EPID results.  Both must be valid (found the BB near isocenter), the CBCT must scanned before the EPID,
              and they must be scanned on the same day.
              <p></p>
              A warning or failure may be cleared by re-doing the Daily QA for that machine.  If there there at least
              one set of data that passed for a machine, then that machine is marked as passed.
            </center>
          </div>
        </div>
        <div class="row">
          <div class="col-md-6 col-md-offset-3 col-sm-12">
            { DailyQAIndividualResults.get(institutionPK, date) }
          </div>
        </div>
      </div>
    }

    content
  }
}
