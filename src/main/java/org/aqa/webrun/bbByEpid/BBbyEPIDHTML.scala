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

/**
 * Generate and write HTML for EPID BB analysis.
 */
object BBbyEPIDHTML {

  private val closeUpImagePrefix = "CloseUp"
  private val detailImagePrefix = "Detail"
  private val fullImagePrefix = "Full"

  def generateHtml(extendedData: ExtendedData, bbByEPIDList: Seq[Option[BBbyEPID]], composite: Option[BBbyEPIDComposite], runReq: BBbyEPIDRunReq, status: ProcedureStatus.Value) = {

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

      val numbers = {
        val sp = WebUtil.nbsp + " " + WebUtil.nbsp + " " + WebUtil.nbsp
        if (composite.isDefined) {
          <h3 title="Composite results.  Distance in mm between plan isocenter and position of BB">
            {
              "Total Offset(mm)" + fmt(composite.get.offset_mm) + sp +
                "X:" + fmt(composite.get.x_mm) + sp +
                "Y:" + fmt(composite.get.y_mm) + sp +
                "Z:" + fmt(composite.get.z_mm)
            }
          </h3>
        } else {
          <div>
            <h3>Final analysis not available.  Usually due to BB not found.</h3>
          </div>
        }
      }

      val elem = {
        <div class="row">
          <div class="col-md-6">
            <h2 title="Procedure performed">EPID BB Location</h2>
            <br/>
            { numbers }
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

    def content = {
      <div class="row">
        { numberText }
        { chart.chartReference }
        <table class="table table-responsive">
          <tr>
            { imageSetList.map(imageSet => imageSet.html) }
          </tr>
        </table>
      </div>
    }

    def wrap = {
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

      val elem = {
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

      elem
    }

    val runScript = {
      val zoomList = imageSetList.map(imageSet => imageSet.script)
      val zoomScript = zoomList.mkString("\n<script>\n      ", "\n      ", "\n</script>")
      val chartRef = BBbyEPIDChartHistoryRestlet.makeReference(extendedData.output.outputPK.get)

      zoomScript + "\n" + chartRef
    }

    val text = WebUtil.wrapBody(wrap, "BB Location by EPID", None, true, Some(runScript))
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(file, text)
  }

}
