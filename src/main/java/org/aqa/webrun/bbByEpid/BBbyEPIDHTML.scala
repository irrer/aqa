package org.aqa.webrun.bbByEpid

import org.aqa.webrun.ExtendedData
import org.aqa.db.BBbyEPID
import org.aqa.run.ProcedureStatus
import org.aqa.db.BBbyEPIDComposite
import org.aqa.Util
import java.io.File
import scala.xml.Elem
import java.text.SimpleDateFormat
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil
import org.aqa.db.Output
import java.awt.geom.Point2D
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.db.BBbyCBCT
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.web.WebServer
import org.aqa.db.DicomSeries
import com.pixelmed.dicom.AttributeTag
import org.aqa.web.OutputList
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.AngleType
import java.util.Date
import org.aqa.Config
import org.aqa.web.ViewOutput

/**
 * Generate and write HTML for EPID BB analysis.
 */
object BBbyEPIDHTML {

  private val closeUpImagePrefix = "CloseUp"
  private val fullImagePrefix = "Full"
  val matlabFileName = "matlab.txt"

  private val mainReportFileName = Output.displayFilePrefix + ".html"

  private def fmtApprox(d: Double) = d.formatted("%6.2f").trim
  private def fmtPrecise(d: Double) = d.formatted("%12.6f").trim // match precision shown in Matlab

  /**
   * Get either the content or acquisition date+time in ms.
   */
  private def result2ms(al: AttributeList): Long = {
    val c = DicomUtil.getTimeAndDate(al, TagFromName.ContentDate, TagFromName.ContentTime)
    val a = DicomUtil.getTimeAndDate(al, TagFromName.AcquisitionDate, TagFromName.AcquisitionTime)
    Seq(c, a).flatten.head.getTime
  }

  /**
   * Get either the content or acquisition date+time in ms.
   */
  private def result2ms(r: Either[BBbyEPIDImageAnalysis.FailedResult, BBbyEPIDImageAnalysis.Result]): Long = {
    result2ms(BBbyEPIDImageAnalysis.alOf(r))
  }

  def generateHtml(extendedData: ExtendedData, bbByEPIDList: Seq[Either[BBbyEPIDImageAnalysis.FailedResult, BBbyEPIDImageAnalysis.Result]], composite: Either[String, (BBbyEPIDComposite, Option[String])], runReq: BBbyEPIDRunReq, status: ProcedureStatus.Value): Unit = {

    val bbByCBCT: Option[BBbyCBCT] = {
      if ((composite.isRight) && (composite.right.get._1.bbByCBCTPK.isDefined))
        BBbyCBCT.get(composite.right.get._1.bbByCBCTPK.get)
      else
        None
    }

    val cbctOutput: Option[Output] = {
      if (bbByCBCT.isDefined)
        Output.get(bbByCBCT.get.outputPK)
      else
        None
    }

    def wrap(content: Elem) = {
      val twoLineDate = new SimpleDateFormat("MMM dd yyyy\nHH:mm")
      def wrapElement(col: Int, name: String, value: String, asAlias: Boolean): Elem = {
        val html =
          if (asAlias) {
            <span aqaalias="">{ value }</span>
          } else {
            val valueList = value.split("\n");
            { <span>{ valueList.head }{ valueList.tail.map(line => { <span><br/> { line } </span> }) }</span> }
          }

        { <div class={ "col-md-" + col }><em>{ name }:</em><br/>{ html }</div> }

      }

      val dataAcquisitionDate = {
        if (extendedData.output.dataDate.isDefined) twoLineDate.format(extendedData.output.dataDate.get)
        else "unknown"
      }

      val elapsed: String = {
        val fin = extendedData.output.finishDate match {
          case Some(finDate) => finDate.getTime
          case _ => System.currentTimeMillis
        }
        val elapsed = fin - extendedData.output.startDate.getTime
        Util.elapsedTimeHumanFriendly(elapsed)
      }

      val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

      val showMachine = {
        val href = "/admin/MachineUpdate?machinePK=22"
        <div class="col-md-1">
          <h2 title="Treatment machine.  Click for details.">{ MachineUpdate.linkToMachineUpdate(extendedData.machine.machinePK.get, extendedData.machine.id) }</h2>
        </div>
      }

      def wrapWithHeader = {
        <div class="row">
          <div class="row">
            <div class="col-md-10 col-md-offset-1">
              { showMachine }
              { wrapElement(2, "Institution", extendedData.institution.name, true) }
              { wrapElement(1, "Data Acquisition", dataAcquisitionDate, false) }
              { wrapElement(1, "Analysis Started", twoLineDate.format(extendedData.output.startDate), false) }
              { wrapElement(1, "User", extendedData.user.id, true) }
              { wrapElement(1, "Elapsed", elapsed, false) }
              { wrapElement(1, "Procedure", procedureDesc, false) }
              <div class="col-md-1">{ OutputList.redoUrl(extendedData.output.outputPK.get) }</div>
            </div>
          </div>
          <div class="row">
            <div class="col-md-10 col-md-offset-1">
              { content }
            </div>
          </div>
        </div>
      }

      wrapWithHeader
    }

    class ImageSet(result: Either[BBbyEPIDImageAnalysis.FailedResult, BBbyEPIDImageAnalysis.Result], id: Int) {
      val al = BBbyEPIDImageAnalysis.alOf(result)
      val SOPInstanceUID = Util.sopOfAl(al)

      def r2epid = {
        if (result.isLeft) throw new RuntimeException("Unexpected failure accessing failed epid data.")
        result.right.get.bbByEpid
      }

      val description: Option[String] = {
        result match {
          case _ if result.isRight && r2epid.isVert => Some(fmtApprox(r2epid.epid3DX_mm) + ", NA, " + fmtApprox(r2epid.epid3DZ_mm) + " mm")
          case _ if result.isRight && r2epid.isHorz => Some("NA, " + fmtApprox(r2epid.epid3DY_mm) + ", " + fmtApprox(r2epid.epid3DZ_mm) + " mm")
          case _ if result.isLeft => Some(result.left.get.error)
          case _ => None
        }
      }

      val bbLoc_mm = if (result.isRight) Some(new Point2D.Double(r2epid.epidImageX_mm, r2epid.epidImageY_mm)) else None
      val gantryAngle = Util.angleRoundedTo90(Util.gantryAngle(al))
      val gantryAngleRounded = Util.angleRoundedTo90(gantryAngle)

      val suffix = "_" + gantryAngleRounded + "_" + (id + 1) + ".png"
      val closeUpImageFileName = closeUpImagePrefix + suffix
      val fullImageFileName = fullImagePrefix + suffix

      def name2File(name: String) = new File(extendedData.output.dir, name)

      val bbCenter_pix = {
        if (result.isRight) {
          Some(result.right.get.pix)
        } else None
      }
      val imageSet = new BBbyEPIDAnnotateImages(al, bbLoc_mm, description, bbCenter_pix)

      Util.writePng(imageSet.closeupBufImg, name2File(closeUpImageFileName))
      Util.writePng(imageSet.fullBufImg, name2File(fullImageFileName))

      def viewDicomMetadata = {

        val sop = Util.sopOfAl(al)
        val dicomFile = runReq.epidList.find(al => Util.sopOfAl(al).equals(sop)).get

        val content = {
          <div>
            <div class="row">
              <div class="col-md-3 col-md-offset-1">
                <h2>Gantry Angle { gantryAngle.toString } </h2>
              </div>
              <div class="col-md-2">
                <h2> </h2><a href={ mainReportFileName } title="Return to main EPID report">Main Report</a>
              </div>
            </div>
            <div class="row">
              <pre>
                { WebUtil.nl + DicomUtil.attributeListToString(al) }
              </pre>
              { WebUtil.nl }
              <p> </p>
            </div>
          </div>
        }

        val fileName = "EPID_" + id + ".html"
        val text = WebUtil.wrapBody(wrap(content), "EPID " + gantryAngle + " deg", None, true, None)
        val file = new File(extendedData.output.dir, fileName)
        Util.writeFile(file, text)

        <a href={ fileName } title={ "View / download DICOM for gantry angle " + gantryAngle }>View DICOM</a>
      }

      val html: Elem = {

        def imgRef(name: String, title: String) = {
          <a href={ name }>
            <h4>{ title }</h4>
            <img width='400' src={ name }/>
          </a>
        }

        def imgRefWithZoom(name: String, title: String, border: Boolean = false) = {

          <a href={ name }>
            <h4>{ title }</h4>
            <div class='zoom' id={ Util.textToId(name) }>
              { if (border) { <img width='400' src={ name } style="border: 1px solid lightgrey;"/> } else { <img width='400' src={ name }/> } }
            </div>
          </a>
        }

        <td>
          <center>
            <h3 title={ "Gantry angle in degrees: " + gantryAngle }>
              { "Gantry " + gantryAngleRounded }
            </h3>
            <br/>
            { viewDicomMetadata }
            <br/>
            { imgRefWithZoom(closeUpImageFileName, "Closeup of BB") }
            <br/>
            { imgRefWithZoom(fullImageFileName, "Full Image", true) }
          </center>
        </td>
      }

      val script = {
        Seq(closeUpImageFileName, fullImageFileName).
          map(fn => "      $(document).ready(function(){ $('#" + Util.textToId(fn) + "').zoom(); });\n").
          mkString("\n      ")
      }
    }

    val outputDir = extendedData.output.dir

    val chart = new BBbyEPIDChart(extendedData.output.outputPK.get)

    // make all of the images and get their names
    val imageSetList = bbByEPIDList.zipWithIndex.par.map(ri => new ImageSet(ri._1, ri._2)).toList
    // val imageSetList = bbByEPIDList.zipWithIndex.map(ri => new ImageSet(ri._1, ri._2)).toList // non-parallel version for debug only

    val numberText = {

      def dataCol(name: String, title: String, value: Double, cols: Int) = {
        <span title={ title } class={ "col-md-" + cols }>
          { name + " : " + fmtPrecise(value) }
        </span>
      }

      def tableMovement(composite: BBbyEPIDComposite) = {
        def fmt(d: Option[Double]) = <span title={ fmtPrecise(d.get / 10) }>{ fmtApprox(d.get / 10) }</span>
        fmt(composite.tableXlateral_mm) + ","
        <div title="Table movement between CBCT and RTIMAGE, calculated with RTIMAGE - CT">
          Table Movement X,Y,Z cm:{ fmt(composite.tableXlateral_mm) }
          ,{ fmt(composite.tableYvertical_mm) }
          ,{ fmt(composite.tableZlongitudinal_mm) }
        </div>
      }

      val tablePosition = {
        val al = if (bbByEPIDList.head.isLeft) bbByEPIDList.head.left.get.al else bbByEPIDList.head.right.get.al

        /** Get double value, convert from mm to cm, and format to text. */
        def db(tag: AttributeTag) = { Util.fmtDbl(al.get(tag).getDoubleValues().head / 10) }

        val x = db(TagFromName.TableTopLateralPosition) // tableXlateral_mm
        val y = db(TagFromName.TableTopVerticalPosition) // tableYvertical_mm
        val z = db(TagFromName.TableTopLongitudinalPosition) // tableZlongitudinal_mm

        <div title="Table position when CBCT was captured.">
          Table Position X,Y,Z cm:{ x + "," + y + "," + z }
        </div>
      }

      val numbersWithCbct = {
        val sp = WebUtil.nbsp + " " + WebUtil.nbsp + " " + WebUtil.nbsp

        if (composite.isRight) {
          if (cbctOutput.isDefined) {
            val cbctReference = {
              val timeText = new SimpleDateFormat("K:mm a").format(cbctOutput.get.dataDate.get)
              <div title="View CBCT report">
                <a href={ ViewOutput.viewOutputUrl(cbctOutput.get.outputPK.get) }>CBCT at { timeText }</a>
              </div>
            }

            def matlabReference: Elem = {
              <div title="Executable Matlab script showing calculations. Copy and paste to Matlab.">
                <a href={ matlabFileName } style="margin:20px;">Matlab</a>
              </div>
            }

            val title = "Composite results.  Distance in mm between plan isocenter and position of BB compensated by CBCT (difference of EPID - CBCT) " + WebUtil.titleNewline +
              fmtPrecise(composite.right.get._1.xAdjusted_mm.get) + ", " +
              fmtPrecise(composite.right.get._1.yAdjusted_mm.get) + ", " +
              fmtPrecise(composite.right.get._1.zAdjusted_mm.get) + " :: " +
              fmtPrecise(composite.right.get._1.offsetAdjusted_mm.get)

            <div>
              <h3 title={ title }>
                {
                  "With CBCT Offset (mm): " + sp +
                    "X:" + fmtApprox(composite.right.get._1.xAdjusted_mm.get) + sp +
                    "Y:" + fmtApprox(composite.right.get._1.yAdjusted_mm.get) + sp +
                    "Z:" + fmtApprox(composite.right.get._1.zAdjusted_mm.get) + sp +
                    " :: " + fmtApprox(composite.right.get._1.offsetAdjusted_mm.get)
                }
              </h3>
              <div class="row">
                <div class="col-md-4">{ tableMovement(composite.right.get._1) }</div>
                <div class="col-md-2">{ cbctReference }</div>
                <div class="col-md-3">{ matlabReference }</div>
              </div>
            </div>
          } else {
            <div title="There was no corresponding CBCT.  CBCT must be taken earlier than the EPID and on the same day.">
              <h3>No CBCT results corresponding to these EPID images.</h3>
              { tablePosition }
            </div>
          }
        } else {
          <div title="Common causes are BB not found or there were not two images with perpendicular gantry angles.">
            <h3>Error: { composite.left.get }</h3>
          </div>

        }
      }

      def planReference: Elem = {
        if (runReq.sopOfRTPlan.isEmpty || DicomSeries.getBySopInstanceUID(runReq.sopOfRTPlan.get).isEmpty) {
          if (runReq.sopOfRTPlan.isEmpty) {
            <div>No plan reference</div>
          } else {
            val title = "The actual plan that was used for these images may be different than" + WebUtil.titleNewline +
              "the plan used for the CBCT.  The requirement is that they have the same frame of" + WebUtil.titleNewline +
              "reference.  The SOP to capture RTIMAGEs was " + runReq.sopOfRTPlan
            <div title={ title }>Could not find referenced plan</div>
          }
        } else {
          val rtplanAl = DicomSeries.getBySopInstanceUID(runReq.sopOfRTPlan.get).head.attributeListList.head
          val dicomFile = new File(extendedData.output.dir, "rtplan.dcm")
          DicomUtil.writeAttributeListToFile(rtplanAl, dicomFile, "AQA")
          val htmlRtplanFileName = "rtplan.html"

          val content = {
            <div>
              <div class="row">
                <div class="col-md-3 col-md-offset-1">
                  <h2>RTPLAN</h2>
                </div>
                <div class="col-md-2">
                  <h2> </h2><a href={ mainReportFileName } title="Return to main EPID report">Main Report</a>
                </div>
                <div class="col-md-2">
                  <h2> </h2><a href={ dicomFile.getName } title="Download anonymized DICOM">Download DICOM</a>
                </div>
              </div>
              <div class="row">
                <pre>
                  { WebUtil.nl + DicomUtil.attributeListToString(rtplanAl) }
                </pre>
                { WebUtil.nl }
                <p> </p>
              </div>
            </div>
          }

          val text = WebUtil.wrapBody(wrap(content), "RTPLAN for EPID", None, true, None)
          val file = new File(extendedData.output.dir, htmlRtplanFileName)
          Util.writeFile(file, text)

          <a href={ htmlRtplanFileName } title={ "View / download RTPLAN DICOM" }>View RTPLAN DICOM</a>
        }
      }

      val elem = {
        <div class="row">
          <div class="col-md-2">
            <h2 title="Procedure performed">EPID BB Location</h2>
          </div>
          <div class="col-md-1">
            { WebUtil.coordinateDiagramElem(80) }
          </div>
          <div class="col-md-8">
            { numbersWithCbct }
          </div>
          <div class="col-md-1">
            { planReference }
          </div>
        </div>
      }
      elem
    }

    def imageHtml(index: Int, getImageFileName: Int => String, title: String) = {
      val name = getImageFileName(index)
      <a href={ name } title={ title }>
        <div id={ Util.textToId(name) }>
          <a href={ name }>
            <img src={ name } class="img-responsive"/>
          </a>
        </div>
      </a>
    }

    val numberTable = {
      if (bbByCBCT.isDefined && composite.isRight) {
        val cbct = bbByCBCT.get
        val epidComposite = composite.right.get._1
        def fmt(d: Double) = { <td title={ fmtPrecise(d) }>{ fmtApprox(d) }</td> }

        def fmtTd(d: Double) = { <td title={ fmtPrecise(d) }>{ fmtApprox(d) }</td> }

        def epidToDate(epid: BBbyEPID): Option[Date] = {
          val dsOpt = DicomSeries.getBySopInstanceUID(epid.epidSOPInstanceUid).headOption
          if (dsOpt.isDefined) {
            val j = dsOpt.get.attributeListList
            val attrList = dsOpt.get.attributeListList.find(al => Util.sopOfAl(al).equals(epid.epidSOPInstanceUid))
            if (attrList.isDefined)
              DicomUtil.getTimeAndDate(attrList.get, TagFromName.ContentDate, TagFromName.ContentTime)
            else
              None
          } else
            None
        }

        val epidDateListSorted = bbByEPIDList.sortBy(r => result2ms(r))

        val msFirst = result2ms(epidDateListSorted.head)

        def vectorLengthColumn(x: Double, y: Double, z: Double) = {
          val d = Math.sqrt((x * x) + (y * y) + (z * z))
          <td title={ fmtPrecise(d) }>{ fmtApprox(d) }</td>
        }

        def elapsedText(ms: Long): String = {
          val elapsed = ms - msFirst
          val sec = (elapsed / 1000) % 60
          val minutes = (elapsed / (60 * 1000))
          minutes.toString + ":" + sec.formatted("%02d")
        }

        def fmtEpidWithoutCbct(rslt: BBbyEPIDImageAnalysis.Result) = {
          val na = { <td>NA</td> }
          val gantryAngle = Util.angleRoundedTo90(rslt.bbByEpid.gantryAngle_deg)

          <tr style="text-align: center;">
            <td style="text-align: right;">MV G<b>{ gantryAngle.toString }</b> (BB - DIGITAL_CAX) @ ISOCENTER PLANE</td>
            <td>{ elapsedText(result2ms(rslt.al)) }</td>
            { if (rslt.bbByEpid.isVert) fmtTd(rslt.bbByEpid.epid3DX_mm) else na }
            { if (rslt.bbByEpid.isHorz) fmtTd(rslt.bbByEpid.epid3DY_mm) else na }
            { fmtTd(rslt.bbByEpid.epid3DZ_mm) }
            {
              vectorLengthColumn(
                if (rslt.bbByEpid.isVert) rslt.bbByEpid.epid3DX_mm else 0.0,
                if (rslt.bbByEpid.isHorz) rslt.bbByEpid.epid3DY_mm else 0.0,
                rslt.bbByEpid.epid3DZ_mm)
            }
          </tr>
        }

        def fmtEpidWithCbct(rslt: BBbyEPIDImageAnalysis.Result) = {
          val na = { <td>NA</td> }
          val gantryAngle = Util.angleRoundedTo90(rslt.bbByEpid.gantryAngle_deg)
          val isVert = AngleType.isAngleType(gantryAngle, AngleType.vertical)
          val isHorz = !isVert

          <tr style="text-align: center;">
            <td style="text-align: right;">MV G<b>{ gantryAngle.toString }</b> (BB - DIGITAL_CAX) @ ISOCENTER PLANE - CBCT(BB - DIGITAL_PLANNED_ISOCENTER)</td>
            <td>{ elapsedText(result2ms(rslt.al)) }</td>
            { if (isVert) fmtTd(rslt.bbByEpid.epid3DX_mm - cbct.err_mm.getX) else na }
            { if (isHorz) fmtTd(rslt.bbByEpid.epid3DY_mm - cbct.err_mm.getY) else na }
            { fmtTd(rslt.bbByEpid.epid3DZ_mm - cbct.err_mm.getZ) }
            {
              vectorLengthColumn(
                if (isVert) rslt.bbByEpid.epid3DX_mm - cbct.err_mm.getX else 0.0,
                if (isHorz) rslt.bbByEpid.epid3DY_mm - cbct.err_mm.getY else 0.0,
                rslt.bbByEpid.epid3DZ_mm - cbct.err_mm.getZ)
            }
          </tr>
        }

        <div>
          <h3><center>Detailed Values</center></h3>
          <table class="table table-bordered">
            <thead>
              <td colspan="6" style="text-align: center;">
                <b>
                  DETAILED VALUES: signs are consistent with diagram world coordinate systems displayed. All BB, DIGITAL_CAX, and DIGITAL_PLANNED_ISO are vector results in world coordinates.
                  <br/>
                  DIGITAL_CAX vectors are corrected for 3002,000d XRayImageReceptorTranslation
                </b>
              </td>
            </thead>
            <thead>
              <tr>
                <th></th>
                <th style="text-align: center;">EPID Time</th>
                <th style="text-align: center;">HFS PATIENT<br/>LEFT(+)/RIGHT(-) [mm]</th>
                <th style="text-align: center;">HFS PATIENT<br/>POST(+)/ANT(-) [mm]</th>
                <th style="text-align: center;">HFS PATIENT<br/>SUP (+)/INF(-) [mm]</th>
                <th style="text-align: center;">Vector Length [mm]</th>
              </tr>
            </thead>
            <tr style="text-align: center;">
              <td style="text-align: right;">CBCT(BB - DIGITAL_PLANNED_ISOCENTER)</td>
              <td></td>
              { fmt(cbct.err_mm.getX) }
              { fmt(cbct.err_mm.getY) }
              { fmt(cbct.err_mm.getZ) }
              {
                vectorLengthColumn(
                  cbct.err_mm.getX,
                  cbct.err_mm.getY,
                  cbct.err_mm.getZ)
              }
            </tr>
            { epidDateListSorted.filter(_.isRight).map(r => fmtEpidWithoutCbct(r.right.get)) }
            { epidDateListSorted.filter(_.isRight).map(r => fmtEpidWithCbct(r.right.get)) }
            <tr style="text-align: center;">
              <td style="text-align: right;">AVERAGE MV(BB - DIGITAL_CAX) @ ISOCENTER PLANE - CBCT(BB - DIGITAL_PLANNED_ISOCENTER)</td>
              <td></td>
              { fmt(epidComposite.xAdjusted_mm.get) }
              { fmt(epidComposite.yAdjusted_mm.get) }
              { fmt(epidComposite.zAdjusted_mm.get) }
              {
                vectorLengthColumn(
                  epidComposite.xAdjusted_mm.get,
                  epidComposite.yAdjusted_mm.get,
                  epidComposite.zAdjusted_mm.get)
              }
            </tr>
          </table>
        </div>
      } else
        <div></div>
    }

    def content = {
      <div>
        <div class="row">
          { numberText }
        </div>
        <div class="row">
          <div class="col-md-12">
            { numberTable }
          </div>
        </div>
        <div class="row">
          { chart.chartReference }
        </div>
        <div class="row">
          <table class="table table-responsive">
            <tr>
              { imageSetList.map(imageSet => imageSet.html) }
            </tr>
          </table>
        </div>
      </div>
    }

    val runScript = {
      val zoomList = imageSetList.map(imageSet => imageSet.script)
      val zoomScript = zoomList.mkString("\n<script>\n      ", "\n      ", "\n</script>")
      val chartRef = BBbyEPIDChartHistoryRestlet.makeReference(extendedData.output.outputPK.get)

      zoomScript + "\n" + chartRef
    }

    def writeMatlabFile = {
      if (composite.isRight && composite.right.get._2.isDefined) {
        val text = composite.right.get._2.get
        val file = new File(extendedData.output.dir, matlabFileName)
        Util.writeFile(file, text)
      }
    }

    writeMatlabFile
    val text = WebUtil.wrapBody(wrap(content), "BB Location by EPID", None, true, Some(runScript))
    val file = new File(extendedData.output.dir, mainReportFileName)
    Util.writeFile(file, text)
  }

}
