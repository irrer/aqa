package org.aqa.webrun.dailyQA

import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.Trace
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BBbyCBCT
import org.aqa.db.BBbyEPID
import org.aqa.db.BBbyEPIDComposite
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.MachineDailyQA
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.web.ViewOutput
import org.aqa.web.WebUtil._

import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

object DailyQAHTML extends Logging {

  private def makeChecksumX(institutionPK: Long, date: Date, dataSetList: Seq[BBbyEPIDComposite.DailyDataSetComposite]): String = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

    /** Truncate (floor) to the date. */
    def dateFloor(d: Date): Date = dateFormat.parse(dateFormat.format(d))

    val lo = dateFloor(date)

    val msInHour = 60 * 60 * 1000 // milliseconds in an hour

    val hi = dateFloor(new Date(lo.getTime + (26 * msInHour)))

    val outputList = {
      val all = Output.getOutputByDateRange(institutionPK, new Timestamp(lo.getTime), new Timestamp(hi.getTime))
      all.filter(o => (o.procedurePK == Procedure.ProcOfBBbyCBCT) || (o.procedurePK == Procedure.ProcOfBBbyEPID))
    }

    val checksum2 = outputList.sortBy(_.outputPK.get).map(o => o.outputPK.get + ":" + o.status).mkString(" / ")

    val cs3 = {
      BBbyEPIDComposite.getChecksum(date, institutionPK)
    }

    val oldC = "checksum size: " + dataSetList.size + " : " + dataSetList.map(_.checksum).sorted.mkString(" / ")  // TODO rm
    Trace.trace("\nOld: " + oldC + "\nNew: " + checksum2)

    checksum2
  }

  def makeReport(dataSetList: Seq[BBbyEPIDComposite.DailyDataSetComposite], institutionPK: Long, date: Date): Elem = {

    /** Local class for Machine, Output, and Elem. */
    case class MOE(machine: Machine, output: Option[Output], elem: Elem) {}

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

        val compare =
          if (toNum(aMach).isDefined && toNum(bMach).isDefined && (toNum(aMach).get != toNum(bMach).get)) {
            toNum(aMach).get < toNum(bMach).get
          } else {
            aMach.compareTo(bMach) <= 0
          }
        compare
      }
    }

    def fmtAngle(angle: Double) = angle.formatted("%12.8f").trim

    case class Col(name: String, title: String, toElem: BBbyEPIDComposite.DailyDataSetComposite => Elem) {
      def toHeader: Elem = <th title={title}>
        {name}
      </th>
    }

    val stylePass = "color: #000000; background: #1dc32b;"
    val styleFail = "color: #000000; background: #e00034;"
    val styleNoData = "color: #000000; background: #888888;"
    val styleWarn = "color: #000000; background: yellow;"
    val col0Title = "Machine Name"

    def colMachine(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val machElem = wrapAlias(dataSet.machine.id)

      if (ProcedureStatus.eq(dataSet.status, ProcedureStatus.pass)) {
        <td title={col0Title} style={stylePass}>
          <h4>
            {machElem}<br/>
            Pass</h4>
        </td>
      } else {
        if (ProcedureStatus.eq(dataSet.status, ProcedureStatus.warning)) {
          <td class="warning" title={"At least one value is close to being out of tolerance"} style={styleWarn}>
            <h4>
              {machElem}<br/>
              Warning</h4>
          </td>
        } else {
          <td class="danger" title={"At least one value is completely out of tolerance"} style={styleFail}>
            <h4>
              {machElem}<br/>
              Fail</h4>
          </td>
        }
      }
    }

    def colPatient(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {

      val patientName: Elem = DicomSeries.getBySeriesInstanceUID(dataSet.composite.epidSeriesInstanceUID).headOption match {
        case Some(ds) =>
          val pn = ds.attributeListList.head.get(TagFromName.PatientName).getSingleStringValueOrEmptyString
          <td>
            {wrapAlias(pn)}
          </td>
        case _ => <td>Unknown</td>
      }
      patientName
    }

    def posnRow(posn: Double, machineDailyQA: MachineDailyQA): Elem = {
      val text = posn.formatted("%7.2f").trim
      val title = posn.formatted("%12.6f").trim
      if (posn.abs > machineDailyQA.warningLimit_mm) {
        <td class="danger" title={title + " is above warning limit of " + machineDailyQA.warningLimit_mm + " mm"}>
          {text}
        </td>
      } else if (posn.abs > machineDailyQA.passLimit_mm) {
        <td class="warning" title={title + " is above pass limit of " + machineDailyQA.passLimit_mm + " mm but below warning limit of " + machineDailyQA.warningLimit_mm + " mm"}>
          {text}
        </td>
      } else {
        <td title={title}>
          {text}
        </td>
      }
    }

    def colDateTime(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      <td>
        {DailyQASummary.timeFormat.format(dataSet.output.dataDate.get)}
      </td>
    }

    /**
     * Format CBCT data.  If the system-wide limit is exceeded then mark it as failed.
     */
    def colCbctXYZ(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val x = dataSet.cbct.err_mm.getX
      val y = dataSet.cbct.err_mm.getY
      val z = dataSet.cbct.err_mm.getZ

      val text = x.formatted("%7.2f").trim + ", " + y.formatted("%7.2f").trim + ", " + z.formatted("%7.2f").trim
      val title = x.formatted("%12.6f").trim + ", " + y.formatted("%12.6f").trim + ", " + z.formatted("%12.6f").trim

      def exceeds = {
        val limit = Config.DailyQACBCTLimit_mm
        (x.abs > limit) || (y.abs > limit) || (z.abs > limit)
      }

      if (exceeds) {
        <td class="danger" title={title + " exceeded warning limit of " + Config.DailyQACBCTLimit_mm + " mm"}>
          {text}
        </td>
      } else {
        <td title={title}>
          {text}
        </td>
      }
    }

    def colTableMovement(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val composite = dataSet.composite

      def fmt(d: Option[Double]) = (d.get / 10).formatted("%4.1f")

      if (composite.tableXlateral_mm.isDefined) {
        <td title={Util.fmtDbl(composite.tableXlateral_mm.get / 10) + ", " + Util.fmtDbl(composite.tableYvertical_mm.get / 10) + ", " + Util.fmtDbl(composite.tableZlongitudinal_mm.get / 10) + ", "}>
          {fmt(composite.tableXlateral_mm) + ", " + fmt(composite.tableYvertical_mm) + ", " + fmt(composite.tableZlongitudinal_mm)}
        </td>
      } else <td></td>
    }

    def colVertGantryAngle(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val angle = dataSet.vertList.head.gantryAngle_deg
      <td title={fmtAngle(angle)}>
        {Util.angleRoundedTo90(angle)}
      </td>
    }

    def colVertXCax(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      posnRow(dataSet.composite.xAdjusted_mm.get, dataSet.machineDailyQA)
    }

    def colVertZCax(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val offset = dataSet.vertList.head.epid3DZ_mm - dataSet.cbct.err_mm.getZ
      posnRow(offset, dataSet.machineDailyQA)
    }

    def colHorzGantryAngle(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val angle = dataSet.horzList.head.gantryAngle_deg
      <td title={fmtAngle(angle)}>
        {Util.angleRoundedTo90(angle)}
      </td>
    }

    def colHorzYCax(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      posnRow(dataSet.composite.yAdjusted_mm.get, dataSet.machineDailyQA)
    }

    def colHorzZCax(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      val offset = dataSet.horzList.head.epid3DZ_mm - dataSet.cbct.err_mm.getZ
      posnRow(offset, dataSet.machineDailyQA)
    }

    def colEpidPlanCbct(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      if (dataSet.composite.offsetAdjusted_mm.isDefined) posnRow(dataSet.composite.offsetAdjusted_mm.get, dataSet.machineDailyQA)
      else <div>undefined</div>
    }

    def colCbctImages(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {

      val sliceThickness = {
        val at = dataSet.cbct.attributeList.get(TagFromName.SliceThickness)
        if (at == null)
          None
        else {
          Some(at.getDoubleValues.head)
        }
      }

      if (sliceThickness.isEmpty || (sliceThickness.get <= Config.BBbyCBCTMaximumSliceThickness_mm)) {
        <td>
          <a href={ViewOutput.viewOutputUrl(dataSet.cbct.outputPK)}>CBCT Details</a>
        </td>
      } else {
        val thicknessText = sliceThickness.get.toString.take(4)
        val title = "Slices too thick at " + thicknessText + " mm.  Limit is " + Config.BBbyCBCTMaximumSliceThickness_mm + " mm."
        val msg = thicknessText + " mm Slice Thickness "
        <td title={title} class="warning">
          <a href={ViewOutput.viewOutputUrl(dataSet.cbct.outputPK)}>CBCT Details
            <br/> <b style="color:red;">
            {msg}
          </b>
          </a>
        </td>
      }
    }

    def colEpidImages(dataSet: BBbyEPIDComposite.DailyDataSetComposite): Elem = {
      def f(d: Double) = d.formatted("%12.2f").trim()

      val tableMovement = dataSet.bbByEpid.map(e => f(e.tableXlateral_mm) + ", " + f(e.tableYvertical_mm) + ", " + f(e.tableZlongitudinal_mm)).distinct
      if (tableMovement.size > 1) {
        val title = "The different EPID images have different table positions: " + titleNewline + tableMovement.mkString(titleNewline)
        <td title={title} class="warning">
          <a href={ViewOutput.viewOutputUrl(dataSet.composite.outputPK)}>EPID Details
            <br/> <b style="color:red;">Table Moved</b>
          </a>
        </td>
      } else {
        <td>
          <a href={ViewOutput.viewOutputUrl(dataSet.composite.outputPK)}>EPID Details</a>
        </td>
      }
    }

    val reportDate: String = {
      try {
        DailyQASummary.dateFormat.format(dataSetList.head.output.dataDate.get)
      } catch {
        case _: Throwable => ""
      }
    }

    val checksum = BBbyEPIDComposite.getChecksum(date, institutionPK)

    val colList: List[Col] = List(
      Col("Machine", "Name of treatment machine", colMachine),
      Col("Patient", "Name of test patient", colPatient),
      Col("EPID Time " + reportDate, "Time of EPID acquisition", colDateTime),

      Col("X,Y,Z CBCT-PLAN mm", "CBCT - PLAN in mm", colCbctXYZ),
      Col("X,Y,Z / lat,vert,long Table Movement cm", "RTIMAGE-CT in cm, X,Y,Z = lat,vert,lng", colTableMovement),

      Col("Gantry Angle for XZ", "Angle of gantry for vertical image in degrees used to calculate values for Y and Z", colVertGantryAngle),
      Col("Vert EPID-CAX(X) mm", "X offset Vertical EPID image-CAX in mm", colVertXCax),
      Col("Vert EPID-CAX(Z) mm", "Z offset Vertical EPID image-CAX in mm", colVertZCax),

      Col("Gantry Angle for YZ", "Angle of gantry for horizontal image in degrees used to calculate values for X and Z", colHorzGantryAngle),
      Col("Horz EPID-CAX(Y) mm", "Y offset Horizontal EPID-CAX in mm", colHorzYCax),
      Col("Horz EPID-CAX(Z) mm", "Z offset Horizontal EPID-CAX in mm", colHorzZCax),

      Col("(EPID-CAX)-(PLAN-CBCT)", "total offset of (EPID-CAX)-(PLAN-CBCT)", colEpidPlanCbct),

      Col("CBCT Details", "Images and other details for CBCT", colCbctImages),
      Col("EPID Details", "Images and other details for EPID", colEpidImages))

    def dataSetToRow(dataSet: BBbyEPIDComposite.DailyDataSetComposite): MOE = {
      val tdList = colList.tail.map(col => col.toElem(dataSet))
      val elem = <tr>
        {colList.head.toElem(dataSet) :+ tdList}
      </tr>
      MOE(dataSet.machine, Some(dataSet.output), elem)
    }

    val machinesMissingResults = {
      val haveData = dataSetList.map(ds => ds.machine.id).distinct
      val allMachines = Machine.listMachinesFromInstitution(institutionPK).filter(m => m.active)
      val noData = allMachines.filterNot(m => haveData.contains(m.id))

      noData
    }

    val cbctPK = Procedure.ProcOfBBbyCBCT.get.procedurePK.get
    val epidPK = Procedure.ProcOfBBbyEPID.get.procedurePK.get
    /** Procedures that we are interested in. */
    val procedurePkSet = Set(cbctPK, epidPK)

    /** all outputs for CBCT and EPID for all machines from this institution with data from this day sorted by data (acquisition) date. */
    val allOutputs = {
      val dataDateBegin = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(date).replaceAll("T.*", "T00:00:00")).getTime)
      val dataDateEnd = new Timestamp(dataDateBegin.getTime + (24 * 60 * 60 * 1000))

      Output.getOutputByDateRange(institutionPK, dataDateBegin, dataDateEnd).filter(o => procedurePkSet.contains(o.procedurePK)).sortBy(o => o.dataDate.get.getTime)
    }

    def outputCBCT(machinePK: Long) = allOutputs.filter(o => (o.machinePK.get == machinePK) && (o.procedurePK == cbctPK))

    def outputEPID(machinePK: Long) = allOutputs.filter(o => (o.machinePK.get == machinePK) && (o.procedurePK == epidPK))

    // List of reasons that each machine is missing a full set of results.
    val missingResultsExplanations: Seq[MOE] = {

      val allCbctSeq = BBbyCBCT.getForOneDay(date, institutionPK)
      val allEpidSeqWithErrors = BBbyEPID.getForOneDay(date, institutionPK)
      val allEpidSeq = allEpidSeqWithErrors.filter(d => d.data.isRight)

      def explain(mach: Machine): Elem = {
        val machineCbctResults = allCbctSeq.filter(c => c.machine.machinePK.get == mach.machinePK.get)
        val machineEpidResultsWithErrors = allEpidSeqWithErrors.filter(c => c.machine.machinePK.get == mach.machinePK.get)
        val machineEpidResultsWithoutErrors = allEpidSeq.filter(c => c.machine.machinePK.get == mach.machinePK.get)

        val epidOutput = outputEPID(mach.machinePK.get)

        val listColSpanSize = 6
        val listColSpan = listColSpanSize.toString
        val messageColSpan = (colList.size - (listColSpanSize + 1)).toString
        val timeFormat = new SimpleDateFormat("H:mm")

        def cbctBBNotFound(cOut: Output): Boolean = !machineCbctResults.exists(c => c.output.outputPK.get == cOut.outputPK.get)

        /**
         * True if this EPID output has data for horizontal gantry angle.
         */
        def hasHorzAngle(output: Output): Boolean = {
          val eSeq = machineEpidResultsWithoutErrors.filter(e => e.data.right.get.outputPK == output.outputPK.get)
          val horz = eSeq.find(e => e.isHorz)
          horz.isDefined
        }

        /**
         * True if this EPID output has data for the vertical gantry angle.
         */
        def hasVertAngle(output: Output): Boolean = {
          val eSeq = machineEpidResultsWithoutErrors.filter(e => e.data.right.get.outputPK == output.outputPK.get)
          val vert = eSeq.find(e => e.isVert)
          vert.isDefined
        }

        val machHistory = {
          val outputSeq = allOutputs.filter(o => o.machinePK.get == mach.machinePK.get)

          def ref(o: Output): Elem = {
            val isCBCT = o.procedurePK == cbctPK
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
                <a href={url} title={title}>
                  {text}<span style={styleFail}>BB not found</span>
                </a>
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
                <a href={url} title={title}>
                  {text}<span style={styleFail}>No horizontal gantry angle image with BB found</span>
                </a>
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
                <a href={url} title={title}>
                  {text}<span style={styleFail}>No vertical gantry angle image with BB found</span>
                </a>
              </p>
            }

            def scanOk = {
              <p>
                <a href={url} title="Click to view details of scan">
                  {text}
                </a>
              </p>
            }

            val elem: Elem = 0 match {
              case _ if isCBCT && cbctBBNotFound(o) => badCBCT
              case _ if (!isCBCT) && (!hasHorzAngle(o)) => missingHorzEPIDGantryAngle
              case _ if (!isCBCT) && (!hasVertAngle(o)) => missingVertEPIDGantryAngle
              case _ => scanOk
            }

            elem
          }

          if (outputSeq.nonEmpty) {
            <td colspan={listColSpan}>
              <center>
                Hover for info
              </center>
              <p/>{outputSeq.map(o => ref(o))}
            </td>
          } else {
            <td colspan={listColSpan}></td>
          }
        }

        def showNoData: Elem = {
          <tr>
            <td title={col0Title} style={styleNoData}>
              <h4>
                {wrapAlias(mach.id)}<br/>
                No Data</h4>
            </td>
            <td colspan={messageColSpan}>There are no CBCT or EPID scans for this machine.</td>{machHistory}
          </tr>
        }

        def showWarn(msg: String): Elem = { // show links to CBCT and EPID outputs
          <tr>
            <td title={col0Title} style={styleWarn}>
              <h4>
                {wrapAlias(mach.id)}<br/>
                Warning</h4>
            </td>
            <td colspan={messageColSpan}>
              {msg}
            </td>{machHistory}
          </tr>
        }

        def showFail(msg: String): Elem = { // show links to CBCT and EPID outputs
          <tr>
            <td title={col0Title} style={styleFail}>
              <h4>
                {wrapAlias(mach.id)}<br/>
                Fail</h4>
            </td>
            <td colspan={messageColSpan}>
              {msg}
            </td>{machHistory}
          </tr>
        }

        /**
         * Return true if the EPID was done before the CBCT.
         */
        def epidBeforeCbct = {
          if (machineCbctResults.nonEmpty && machineEpidResultsWithoutErrors.nonEmpty) {
            val firstCbct = machineCbctResults.minBy(_.output.dataDate.get.getTime).output.dataDate.get.getTime
            val lastEpid = machineEpidResultsWithoutErrors.maxBy(_.output.dataDate.get.getTime).output.dataDate.get.getTime
            lastEpid < firstCbct
          } else
            false
        }

        val explanation: Elem = 0 match {
          case _ if machineCbctResults.isEmpty && epidOutput.isEmpty => showNoData
          case _ if machineCbctResults.nonEmpty && machineCbctResults.isEmpty => showFail("One or more CBCTs were done but the BB was not found.  Probably mis-alignment of table or phantom.  It is recommended that the CBCT scan be repeated.")
          case _ if (machineCbctResults.size == 1) && allEpidSeqWithErrors.isEmpty => showWarn("There is a successful CBCT scan but no EPID results.  A new EPID scan is recommended.")
          case _ if (machineCbctResults.size == 1) && machineEpidResultsWithErrors.nonEmpty => showFail("There is a successful CBCT scan but EPID results failed.  A new EPID scan is recommended.")
          case _ if (machineCbctResults.size == 1) && machineEpidResultsWithoutErrors.isEmpty => showWarn("There is a successful CBCT scan but no EPID scans.  It is recommended that an EPID scan be performed.")
          case _ if machineCbctResults.nonEmpty && machineEpidResultsWithoutErrors.isEmpty => showWarn("There are " + machineCbctResults.size + " successful CBCT scans but no EPID scans.  It is recommended that an EPID scan be performed.")
          case _ if machineCbctResults.isEmpty && epidOutput.nonEmpty => showFail("There is one or more EPID scans but no CBCT scans.")
          case _ if machineCbctResults.isEmpty && machineEpidResultsWithoutErrors.nonEmpty => showFail("There are " + epidOutput.size + " EPID scans but no successful CBCT scans.")
          case _ if machineCbctResults.nonEmpty && machineEpidResultsWithoutErrors.isEmpty => showWarn("There are " + machineCbctResults.size + " CBCT scans but zero EPID scans.  The EPID scan needs to be done.")
          case _ if epidBeforeCbct => showFail("The EPID scan was done prior to CBCT.  The CBCT needs to be done first.")
          case _ => showFail("There are no results for this machine.")
        }
        explanation
      }

      machinesMissingResults.map(mach => MOE(mach, None, explain(mach)))
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
            <thead>
              <tr>
                {colList.map(col => col.toHeader)}
              </tr>
            </thead>{all.map(moe => moe.elem)}
          </table>
        </div>
        <div class="row">
          <div class="col-md-8 col-md-offset-2 col-sm-12">
            <center>
              Machines above that have any measurements out of tolerance by their machine's warning limit
              are marked as failed. To produce a final result for a single machine, there must be both CBCT
              and EPID results. Both must be valid (found the BB near isocenter), the CBCT must scanned before the EPID,
              and they must be scanned on the same day.
              <p></p>
              A warning or failure may be cleared by re-doing the Daily QA for that machine. If there there at least
              one set of data that passed for a machine, then that machine is marked as passed.
            </center>
            <span hidden="true" id="checksum">
              {checksum}
            </span>
          </div>
        </div>
        <div class="row">
          <div class="col-md-6 col-md-offset-3 col-sm-12">
            {DailyQAIndividualResults.get(institutionPK, date)}
          </div>
        </div>
      </div>
    }

    content
  }
}
