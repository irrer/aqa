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

/**
 * Generate and write HTML for EPID BB analysis.
 */
object BBbyEPIDHTML {

  private val closeUpImagePrefix = "CloseUp"
  private val detailImagePrefix = "Detail"
  private val fullImagePrefix = "Full"

  private val mainReportFileName = Output.displayFilePrefix + ".html"

  def generateHtml(extendedData: ExtendedData, bbByEPIDList: Seq[Option[BBbyEPID]], composite: Either[String, BBbyEPIDComposite], runReq: BBbyEPIDRunReq, status: ProcedureStatus.Value) = {

    val bbByCBCT: Option[BBbyCBCT] = {
      if ((composite.isRight) && (composite.right.get.bbByCBCTPK.isDefined))
        BBbyCBCT.get(composite.right.get.bbByCBCTPK.get)
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

    class ImageSet(index: Int) {
      val al = runReq.epidList(index)
      val SOPInstanceUID = Util.sopOfAl(al)
      val bbByEpid = bbByEPIDList.flatten.find(b => b.epidSOPInstanceUid.equals(SOPInstanceUID))
      val bbLoc_mm = if (bbByEpid.isDefined) Some(new Point2D.Double(bbByEpid.get.epidImageX_mm, bbByEpid.get.epidImageY_mm)) else None
      val gantryAngle = Util.angleRoundedTo90(Util.gantryAngle(al))
      val gantryAngleRounded = Util.angleRoundedTo90(gantryAngle)

      val suffix = "_" + gantryAngleRounded + "_" + (index + 1) + ".png"
      val closeUpImageFileName = closeUpImagePrefix + suffix
      val detailImageFileName = detailImagePrefix + suffix
      val fullImageFileName = fullImagePrefix + suffix

      def name2File(name: String) = new File(extendedData.output.dir, name)

      val imageSet = new BBbyEPIDAnnotateImages(al, bbLoc_mm)

      Util.writePng(imageSet.closeupBufImg, name2File(closeUpImageFileName))
      Util.writePng(imageSet.detailBufImg, name2File(detailImageFileName))
      Util.writePng(imageSet.fullBufImg, name2File(fullImageFileName))

      def viewDicomMetadata = {

        val sop = Util.sopOfAl(al)
        val dicomFile = runReq.epidListDicomFile.find(df => Util.sopOfAl(df.attributeList.get).equals(sop)).get

        val content = {
          <div>
            <div class="row">
              <div class="col-md-3 col-md-offset-1">
                <h2>Gantry Angle { gantryAngle.toString } </h2>
              </div>
              <div class="col-md-2">
                <h2> </h2><a href={ mainReportFileName } title="Return to main EPID report">Main Report</a>
              </div>
              <div class="col-md-2">
                <h2> </h2><a href={ "../" + dicomFile.file.getName } title="Download anonymized DICOM">Download DICOM</a>
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

        val fileName = "EPID_" + index + ".html"
        val text = WebUtil.wrapBody(wrap(content), "EPID " + gantryAngle + " deg", None, true, None)
        val file = new File(extendedData.output.dir, fileName)
        Util.writeFile(file, text)

        <a href={ fileName } title={ "View / download DICOM for gantry angle " + gantryAngle }>View DICOM</a>
      }

      val html: Elem = {

        def imgRef(name: String, title: String) = {
          <a href={ name }>
            <h4>{ title }</h4>
            <div class='zoom' id={ Util.textToId(name) }>
              <img width='400' src={ name }/>
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
            { imgRef(closeUpImageFileName, "Closeup of BB") }
            <br/>
            { imgRef(detailImageFileName, "Detail of BB Area") }
            <br/>
            { imgRef(fullImageFileName, "Full image") }
          </center>
        </td>
      }

      val script = {
        Seq(closeUpImageFileName, detailImageFileName, fullImageFileName).
          map(fn => "      $(document).ready(function(){ $('#" + Util.textToId(fn) + "').zoom(); });\n").
          mkString("\n      ")
      }
    }

    val outputDir = extendedData.output.dir

    val chart = new BBbyEPIDChart(extendedData.output.outputPK.get)

    // make all of the images and get their names
    val imageSetList = runReq.epidList.indices.par.map(index => new ImageSet(index)).toList

    val numberText = {
      def fmt(d: Double) = d.formatted("%5.2f")

      def dataCol(name: String, title: String, value: Double, cols: Int) = {
        <span title={ title } class={ "col-md-" + cols }>
          { name + " : " + fmt(value) }
        </span>
      }

      val numbersWithCbct = {
        val sp = WebUtil.nbsp + " " + WebUtil.nbsp + " " + WebUtil.nbsp

        if (composite.isRight) {
          if (cbctOutput.isDefined) {
            val cbctTime = {
              val timeText = new SimpleDateFormat("K:mm a").format(cbctOutput.get.dataDate.get)
              "CBCT at " + timeText
            }
            <h3 title="Composite results.  Distance in mm between plan isocenter and position of BB compensated by CBCT (difference of EPID - CBCT)">
              {
                "With CBCT Offset (mm):" + fmt(composite.right.get.offsetAdjusted_mm.get) + sp +
                  "X:" + fmt(composite.right.get.x_mm) + sp +
                  "Y:" + fmt(composite.right.get.y_mm) + sp +
                  "Z:" + fmt(composite.right.get.z_mm) + sp +
                  cbctTime
              }
            </h3>
          } else {
            <div title="There was no corresponding CBCT.  CBCT must be taken earlier than the EPID and on the same day.">
              <h3>No CBCT results corresponding to these EPID images.</h3>
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
          <div class="col-md-8">
            <h2 title="Procedure performed">EPID BB Location</h2>
            <br/>
            { numbersWithCbct }
          </div>
          <div class="col-md-2">
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

    def imageHtmlWithZoom(index: Int, getImageFileName: Int => String, title: String, width: Int) = {
      val name = getImageFileName(index)
      <a href={ name } title={ title }>
        <div id={ Util.textToId(name) }>
          <a href={ name }>
            <img src={ name } class="img-responsive" width={ width.toString }/>
          </a>
        </div>
      </a>
    }

    val numberTable = {
      if (bbByCBCT.isDefined && composite.isRight) {
        val cbct = bbByCBCT.get
        val epid = composite.right.get
        def fmt(d: Double) = { <td>{ d.formatted("%12.6f").trim }</td> }

        <div>
          <h3><center>Raw Values</center></h3>
          <table class="table table-responsive">
            <thead>
              <tr>
                <th>Dimension</th>
                <th>EPID</th>
                <th>CBCT</th>
                <th>PLAN</th>
                <th>PLAN - CBCT</th>
                <th>EPID - (PLAN - CBCT)</th>
              </tr>
            </thead>
            <tr>
              <td>X</td>
              { fmt(epid.x_mm) }
              { fmt(cbct.cbctX_mm) }
              { fmt(cbct.rtplanX_mm) }
              { fmt(cbct.rtplanX_mm - cbct.cbctX_mm) }
              { fmt(epid.x_mm - (cbct.rtplanX_mm - cbct.cbctX_mm)) }
            </tr>
            <tr>
              <td>Y</td>
              { fmt(epid.y_mm) }
              { fmt(cbct.cbctY_mm) }
              { fmt(cbct.rtplanY_mm) }
              { fmt(cbct.rtplanY_mm - cbct.cbctY_mm) }
              { fmt(epid.y_mm - (cbct.rtplanY_mm - cbct.cbctY_mm)) }
            </tr>
            <tr>
              <td>Z</td>
              { fmt(epid.z_mm) }
              { fmt(cbct.cbctZ_mm) }
              { fmt(cbct.rtplanZ_mm) }
              { fmt(cbct.rtplanZ_mm - cbct.cbctZ_mm) }
              { fmt(epid.z_mm - (cbct.rtplanZ_mm - cbct.cbctZ_mm)) }
            </tr>
          </table>
        </div>
      } else
        <div></div>
    }

    def content = {
      <div class="row">
        { numberText }
        { numberTable }
        { chart.chartReference }
        <table class="table table-responsive">
          <tr>
            { imageSetList.map(imageSet => imageSet.html) }
          </tr>
        </table>
      </div>
    }

    val runScript = {
      val zoomList = imageSetList.map(imageSet => imageSet.script)
      val zoomScript = zoomList.mkString("\n<script>\n      ", "\n      ", "\n</script>")
      val chartRef = BBbyEPIDChartHistoryRestlet.makeReference(extendedData.output.outputPK.get)

      zoomScript + "\n" + chartRef
    }

    val text = WebUtil.wrapBody(wrap(content), "BB Location by EPID", None, true, Some(runScript))
    val file = new File(extendedData.output.dir, mainReportFileName)
    Util.writeFile(file, text)
  }

}
