package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.PositioningCheck
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.collection.Seq
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.Config
import org.aqa.db.BadPixel
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import org.aqa.web.WebServer
import org.aqa.web.DicomAccess
import java.io.File
import java.awt.Point

/**
 * Store bad pixels in the database and generate HTML.
 */
object BadPixelAnalysis extends Logging {
  val fileName = "ViewDicom.html"

  /**
   * Validate the given data, and, if it is valid, return None, else return a message indicating the problem.
   */
  def validate(runReq: RunReq): Option[String] = {
    if (runReq.flood == null) Some("Flood field is required for ") else None
  }

  /**
   * Store the bad pixels in the database and return the number of bad pixels.
   */
  private def storeToDb(extendedData: ExtendedData, runReq: RunReq): Seq[BadPixel] = {

    /**
     * Given a point, create CSV text showing values for the region around that point.
     */
    def makeCsv(bpX: Int, bpY: Int, dicomImage: DicomImage): String = {
      def fmtPoint(x: Int, y: Int): String = if (dicomImage.validPoint(x, y)) dicomImage.get(x, y).round.toInt.toString else "NA"
      def rangeOf(i: Int) = (-BadPixel.radius to BadPixel.radius).map(o => i + o)
      def fmtRow(y: Int) = rangeOf(bpX).map(x => fmtPoint(x, y)).mkString(",")
      rangeOf(bpY).map(y => fmtRow(y)).mkString("\n")
    }

    val outputPK = extendedData.output.outputPK.get

    def beamToBadPixels(beamName: String, SOPInstanceUID: String, badPixelList: IndexedSeq[Point], originalImage: DicomImage): Seq[BadPixel] = {
      badPixelList.map(bp => {
        val x = bp.getX.round.toInt
        val y = bp.getY.round.toInt
        val j = new BadPixel(None, extendedData.output.outputPK.get, x, y, SOPInstanceUID, beamName, makeCsv(x, y, originalImage))
        j
      })
    }

    val beamList = runReq.rtimageMap.keys.map(beamName =>
      {
        val SOPInstanceUID = runReq.rtimageMap(beamName).attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
        beamToBadPixels(beamName, SOPInstanceUID, runReq.derivedMap(beamName).badPixelList, runReq.derivedMap(beamName).originalImage)
      }).flatten.toSeq

    val floodSOPInstanceUID = runReq.flood.attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
    val floodBadPixelList = beamToBadPixels(Config.FloodFieldBeamName, floodSOPInstanceUID, runReq.floodBadPixelList, runReq.floodOriginalImage)

    val list = beamList ++ floodBadPixelList
    BadPixel.insert(list)
    list
  }

  /**
   * Make the DICOM files web viewable.
   */
  private def makeDicomViews(extendedData: ExtendedData, runReq: RunReq, badPixelList: Seq[BadPixel]): Unit = {
    val outputDir = extendedData.output.dir
    val colorMap = ImageUtil.rgbColorMap(Color.cyan)
    val smallImageWidth = 100.toString

    def dicomView(beamName: String): Option[String] = {
      val rtimage = runReq.rtimageMap(beamName)
      val derived = runReq.derivedMap(beamName)

      val bufImage = derived.originalImage.toBufferedImage(colorMap, derived.pixelCorrectedImage.min, derived.pixelCorrectedImage.max)
      val url = WebServer.urlOfResultsFile(rtimage.file)
      DicomAccess.write(rtimage, url, "RTIMAGE Beam " + beamName, outputDir, Some(bufImage), Some(derived.originalImage), derived.badPixelList)
    }

    // write the rtplan
    val planLink = extendedData.dicomHref(runReq.rtplan)
    DicomAccess.write(runReq.rtplan, planLink, "RTPLAN", outputDir, None, None, IndexedSeq[Point]())

    val floodLink = extendedData.dicomHref(runReq.flood)
    val floodBufImage = runReq.floodOriginalImage.toBufferedImage(colorMap, runReq.floodCorrectedImage.min, runReq.floodCorrectedImage.max)
    val floodPngHref = DicomAccess.write(runReq.flood, floodLink, Config.FloodFieldBeamName, outputDir, Some(floodBufImage), Some(runReq.floodOriginalImage), runReq.floodBadPixelList).get

    val pngImageMap = runReq.rtimageMap.keys.map(beamName => (beamName, dicomView(beamName).get)).toMap

    def beamRef(beamName: String): Elem = {
      <a href={ extendedData.dicomHref(runReq.rtimageMap(beamName)) }>{ beamName }<br/><img src={ pngImageMap(beamName) } width={ smallImageWidth }/></a>
    }

    val content = {
      val planRef = { <a href={ planLink }>Plan</a> }
      val floodRef = { <a href={ floodLink }>{ Config.FloodFieldBeamName }<br/><img src={ floodPngHref } width={ smallImageWidth }/></a> }
      val rtimgRef = runReq.rtimageMap.keys.map(beamName => beamRef(beamName))
      val allRef = (rtimgRef ++ Seq(floodRef, planRef)).map(ref => { <div class="col-md-2" style="margin:20px;">{ ref }</div> })
      val perRow = 4
      val rowList = (0 until ((allRef.size + perRow - 1) / perRow)).map(row => allRef.drop(row * perRow).take(perRow))
      val matrix = rowList.map(row => { <div class="row">{ row }</div> })

      val imageCount = runReq.rtimageMap.size + 1 // beams plus flood
      val badPixelCount = badPixelList.size
      val distinctBadPixelCount = badPixelList.map(bp => (bp.x, bp.y)).distinct.size

      val subTitle = {
        <table class="table table-responsive">
          <tr>
            <td>
              Images:{ imageCount.toString }
            </td>
            <td>
              Total bad pixels:{ badPixelCount.toString }
            </td>
            <td>
              Distinct bad pixels:{ distinctBadPixelCount.toString }
            </td>
          </tr>
        </table>
      }

      val mainElem = {
        <div>
          { subTitle }
          { matrix }
        </div>
      }

      mainElem
    }

    val text = Phase2Util.wrapSubProcedure(extendedData, content, "View Dicom", ProcedureStatus.pass, None)
    val file = new File(outputDir, fileName)
    Util.writeBinaryFile(file, text.getBytes)
  }

  /**
   * Run the PositioningCheck sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): (ProcedureStatus.Value, Elem) = {
    val badPixelList = storeToDb(extendedData, runReq)
    makeDicomViews(extendedData, runReq, badPixelList)

    val summary = {

      val imageCount = runReq.rtimageMap.size + 1 // beams plus flood
      val badPixelCount = badPixelList.size
      val distinctBadPixelCount = badPixelList.map(bp => (bp.x, bp.y)).distinct.size

      val title = "Click to view and download DICOM files.  Images: " + imageCount + "  Bad pixels: " + badPixelCount + "  Distinct bad pixels: " + distinctBadPixelCount
      <div title={ title }>
        <a href={ fileName }>
          View DICOM
          <br/>
          <img src={ Config.passImageUrl } height="32"/>
        </a>
      </div>
    }

    (ProcedureStatus.done, summary)
  }
}
