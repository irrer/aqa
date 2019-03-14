package org.aqa.webrun.phase2

import org.aqa.Logging
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.xml.Elem
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
import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.TimeAttribute
import org.aqa.DicomFile
import java.text.SimpleDateFormat
import java.util.Date
import javax.imageio.ImageIO
import edu.umro.ImageUtil.Watermark

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

    def beamToBadPixels(beamName: String, SOPInstanceUID: String, badPixels: Seq[DicomImage.PixelRating], originalImage: DicomImage): Seq[BadPixel] = {
      badPixels.map(bp => {
        val j = new BadPixel(None, extendedData.output.outputPK.get, bp.x, bp.y, SOPInstanceUID, beamName, makeCsv(bp.x, bp.y, originalImage))
        j
      })
    }

    val beamList = runReq.rtimageMap.keys.par.map(beamName =>
      {
        val SOPInstanceUID = runReq.rtimageMap(beamName).attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
        beamToBadPixels(beamName, SOPInstanceUID, runReq.derivedMap(beamName).badPixels, runReq.derivedMap(beamName).originalImage)
      }).flatten.toList

    val floodSOPInstanceUID = runReq.flood.attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
    val floodBadPixelList = beamToBadPixels(Config.FloodFieldBeamName, floodSOPInstanceUID, runReq.floodBadPixelList, runReq.floodOriginalImage)

    val list = beamList ++ floodBadPixelList
    BadPixel.insert(list)
    list
  }

  /**
   * Get the gantry and collimator angles in a human friendly format.  Angles are rounded to 90.  If either or
   * both of the angles are missing from the DICOM attribute list, then return an empty string in their place.
   */
  private def gantryCollAngles(al: AttributeList): String = {
    def getAngle(prefix: String, tag: AttributeTag): String = {
      val attr = al.get(tag)
      if (attr == null) ""
      else {
        val d = attr.getDoubleValues
        if (d.isEmpty) ""
        else prefix + Util.angleRoundedTo90(d.head).toString
      }
    }

    getAngle("G", TagFromName.GantryAngle) + " " + getAngle("C", TagFromName.BeamLimitingDeviceAngle).trim
  }

  /**
   * Make the DICOM files web viewable.
   */
  private val makeDicomViewsSync = 0
  private def makeDicomViews(extendedData: ExtendedData, runReq: RunReq, badPixelList: Seq[BadPixel]): Unit = makeDicomViewsSync.synchronized({
    val outputDir = extendedData.output.dir
    val colorMap = ImageUtil.rgbColorMap(Color.cyan)
    val smallImageWidth = 100.toString

    val viewDir = new File(extendedData.output.dir, "view")
    def dicomView(beamName: String): Option[String] = {
      logger.info("Making DICOM view for beam " + beamName)
      println("Making DICOM view for beam " + beamName) // TODO rm
      val rtimage = runReq.rtimageMap(beamName)
      val derived = runReq.derivedMap(beamName)
      val angles = gantryCollAngles(rtimage.attributeList.get)

      val bufImage = derived.originalImage.toDeepColorBufferedImage(derived.pixelCorrectedImage.minPixelValue, derived.pixelCorrectedImage.maxPixelValue)
      Config.applyWatermark(bufImage)
      val url = WebServer.urlOfResultsFile(rtimage.file)
      val result = DicomAccess.write(rtimage, url, angles + " : " + beamName, viewDir,
        Phase2Util.dicomViewBaseName(beamName, rtimage.attributeList.get),
        Some(bufImage), Some(derived.originalImage), derived.badPixels)
      logger.info("Finished making DICOM view for beam " + beamName)
      result
    }

    // write the RTPLAN
    val planLink = Phase2Util.dicomViewHref(runReq.rtplan.attributeList.get, extendedData, runReq)
    val rtPlanAl = runReq.rtplan.attributeList.get
    val planBaseName = Phase2Util.dicomViewBaseName(runReq.beamNameOfAl(rtPlanAl), rtPlanAl)
    DicomAccess.write(runReq.rtplan, planLink, "RTPLAN", viewDir, planBaseName, None, None, Seq[DicomImage.PixelRating]())

    val floodLink = Phase2Util.dicomViewHref(runReq.flood.attributeList.get, extendedData, runReq)
    val floodBufImage = runReq.floodOriginalImage.toDeepColorBufferedImage(runReq.floodCorrectedImage.minPixelValue, runReq.floodCorrectedImage.maxPixelValue)
    Config.applyWatermark(floodBufImage)
    val floodBaseName = Phase2Util.dicomViewBaseName(Config.FloodFieldBeamName, runReq.flood.attributeList.get)
    val floodPngHref = DicomAccess.write(runReq.flood, floodLink, Config.FloodFieldBeamName, viewDir, floodBaseName, Some(floodBufImage), Some(runReq.floodOriginalImage), runReq.floodBadPixelList).get

    val pngImageMap = runReq.rtimageMap.keys.par.map(beamName => (beamName, dicomView(beamName).get)).toList.toMap

    def timeOf(al: AttributeList): Option[Long] = {
      val timeAttr = Seq(TagFromName.ContentTime, TagFromName.AcquisitionTime, TagFromName.InstanceCreationTime).
        map(tag => al.get(tag)).filter(at => (at != null) && at.isInstanceOf[TimeAttribute] && (at.asInstanceOf[TimeAttribute].getDoubleValues.nonEmpty)).
        map(at => at.asInstanceOf[TimeAttribute]).headOption

      timeAttr match {
        case Some(at) => Some((at.getDoubleValues.head * 1000).round.toLong)
        case _ => None
      }
    }

    def timeOfBeam(beamName: String): Option[Long] = {
      timeOf(runReq.rtimageMap(beamName).attributeList.get)
    }

    def orderBeams(beamA: String, beamB: String): Boolean = {
      val alA = runReq.rtimageMap(beamA).attributeList.get
      val alB = runReq.rtimageMap(beamB).attributeList.get
      (timeOf(alA), timeOf(alB)) match {
        case (Some(tA), Some(tB)) => tA < tB
        case _ => beamA.compareTo(beamB) < 0
      }
    }

    val elapsedTimeFormat = new SimpleDateFormat("m:ss")

    val earliestTime = {
      val floodTime = Seq(timeOf(runReq.flood.attributeList.get))
      (floodTime ++ runReq.rtimageMap.values.map(df => df.attributeList).flatten.map(al => timeOf(al))).flatten.min
    }

    def sortBeams = runReq.rtimageMap.keys.toList.sortWith(orderBeams _)

    def beamRef(beamName: String, dicomFile: DicomFile): (Option[Long], Elem) = {
      val al = dicomFile.attributeList.get
      def angleOf(tag: AttributeTag) = Util.angleRoundedTo90(al.get(tag).getDoubleValues.head).toString

      val relativeTime = timeOf(al)

      val relativeTimeText = {
        relativeTime match {
          case Some(t) => elapsedTimeFormat.format(new Date(t - earliestTime))
          case _ => ""
        }
      }

      def boolToString(b: Boolean) = if (b) "yes" else ""

      val dicomHref = Phase2Util.dicomViewHref(al, extendedData, runReq)
      val pngHref = Phase2Util.dicomViewImageHref(al, extendedData, runReq)

      val elem = {
        <tr align="center">
          <td style="text-align: center;" title="Click for DICOM metadata"><a href={ dicomHref }>{ beamName }</a></td>
          <td style="text-align: center;" title="Click for full size image"><a href={ Phase2Util.dicomViewImageHtmlHref(al, extendedData, runReq) }><img src={ pngHref } width={ smallImageWidth }/></a></td>
          <td style="text-align: center;" title="Gantry Angle deg">{ angleOf(TagFromName.GantryAngle) }</td>
          <td style="text-align: center;" title="Collimator Angle deg">{ angleOf(TagFromName.BeamLimitingDeviceAngle) }</td>
          <td style="text-align: center;" title="Collimator opening in CM">{ Phase2Util.jawDescription(al) }</td>
          <td style="text-align: center;" title="Time since first image capture (mm:ss)">{ relativeTimeText }</td>
          <td style="text-align: center;" title="Metadata Check">{ boolToString(Config.MetadataCheckBeamNameList.contains(beamName)) }</td>
          <td style="text-align: center;" title="Collimator Centering">{ boolToString(Config.CollimatorCentering090BeamName.equals(beamName) || Config.CollimatorCentering270BeamName.equals(beamName)) }</td>
          <td style="text-align: center;" title="Center Dose">{ boolToString(Config.CenterDoseBeamNameList.contains(beamName)) }</td>
          <td style="text-align: center;" title="Collimator Position">{ boolToString(Config.CollimatorPositionBeamList.contains(beamName)) }</td>
          <td style="text-align: center;" title="Wedge">{ boolToString(Config.WedgeBeamList.contains(beamName)) }</td>
          <td style="text-align: center;" title="Symmetry and Flatness">{ boolToString(Config.SymmetryAndFlatnessBeamList.contains(beamName)) }</td>
          <td style="text-align: center;" title="Leaf Position">{ boolToString(Config.LeafPositionBeamNameList.contains(beamName)) }</td>
        </tr>
      }
      (relativeTime, elem)
    }

    val content = {

      val allElem = {
        val floodRef = beamRef(Config.FloodFieldBeamName, runReq.flood)
        val rtimgRef = sortBeams.map(beamName => beamRef(beamName, runReq.rtimageMap(beamName)))
        def cmpr(a: (Option[Long], Elem), b: (Option[Long], Elem)): Boolean = {
          (a._1, b._1) match {
            case (Some(ta), Some(tb)) => ta < tb
            case (Some(ta), _) => false
            case (_, Some(tb)) => true
            case (_, _) => true
          }
        }
        (rtimgRef :+ floodRef).sortWith(cmpr _).map(te => te._2)
      }

      val tableHead = {
        <thead>
          <tr>
            <th style="text-align: center;" title='Click to view DICOM metadata'>Beam<br/>Name</th>
            <th style="text-align: center;" title='Click to view image'>Image</th>
            <th style="text-align: center;" title='Gantry angle rounded to nearest 90 degrees'>Gantry Angle<br/>degrees</th>
            <th style="text-align: center;" title='Collimator angle rounded to nearest 90 degrees'>Collimator Angle<br/>degrees</th>
            <th style="text-align: center;" title='Width times height of field in cm'>Field Size<br/>cm</th>
            <th style="text-align: center;" title='Time since first image capture (mm:ss)'>Acquisition<br/>Time</th>
            <th style="text-align: center;" title='"yes" if used in Metadata Check'>Metadata<br/>Check</th>
            <th style="text-align: center;" title='"yes" if used in Collimator Centering'>Collimator<br/>Centering</th>
            <th style="text-align: center;" title='"yes" if used in Center Dose'>Center<br/>Dose</th>
            <th style="text-align: center;" title='"yes" if used in Collimator Position'>Collimator<br/>Position</th>
            <th style="text-align: center;" title='"yes" if used in Wedge'>Wedge</th>
            <th style="text-align: center;" title='"yes" if used in Symmetry and Flatness'>Symmetry and Flatness</th>
            <th style="text-align: center;" title='"yes" if used in Leaf Position (Picket Fence)'>Leaf Position</th>
          </tr>
        </thead>
      }

      val table = {
        <table class="table table-bordered">
          { tableHead }
          { allElem }
        </table>
      }

      val imageCount = runReq.rtimageMap.size + 1 // beams plus flood
      val badPixelCount = badPixelList.size
      val distinctBadPixelCount = badPixelList.map(bp => (bp.x, bp.y)).distinct.size

      val subTitle = {
        <table class="table table-responsive">
          <tr>
            <td>
              <a href={ planLink }>View Plan</a>
            </td>
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
        <div class="col-md-10 col-md-offset-1">
          { subTitle }
          { table }
        </div>
      }

      mainElem
    }

    val text = Phase2Util.wrapSubProcedure(extendedData, content, "View Dicom", ProcedureStatus.pass, None, runReq)
    val file = new File(outputDir, fileName)
    Util.writeBinaryFile(file, text.getBytes)
  })

  private val subProcedureName = "Bad Pixel"

  case class BadPixelResult(sum: Elem, sts: ProcedureStatus.Value, resultList: Seq[BadPixel]) extends SubProcedureResult(sum, sts, subProcedureName)

  /**
   * Run the BadPixelAnalysis sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, BadPixelResult] = {
    try {
      logger.info("Starting analysis of BadPixel")
      val badPixelList = storeToDb(extendedData, runReq)
      logger.info("Finished analysis of BadPixel, generating Bad Pixel reports")
      makeDicomViews(extendedData, runReq, badPixelList)

      val maxAllowedBadPixels = ((runReq.floodOriginalImage.Rows * runReq.floodOriginalImage.Columns) / 1000000.0) * Config.MaxAllowedBadPixelsPerMillion
      val maxBadInSingleImage = badPixelList.groupBy(b => b.SOPInstanceUID).values.map(list => list.size).max
      val pass = maxBadInSingleImage <= maxAllowedBadPixels
      val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      val summary = {
        val imageCount = runReq.rtimageMap.size + 1 // beams plus flood
        val badPixelCount = badPixelList.size
        val distinctBadPixelCount = badPixelList.map(bp => (bp.x, bp.y)).distinct.size

        val title = "Click to view and download DICOM files.  Images: " + imageCount + "  Bad pixels: " + badPixelCount + "  Distinct bad pixels: " + distinctBadPixelCount
        <div title={ title }>
          <a href={ fileName }>
            View DICOM
            <br/>
            <img src={ if (pass) Config.passImageUrl else Config.failImageUrl } height="32"/>
          </a>
        </div>
      }

      val result = Right(new BadPixelResult(summary, status, badPixelList))
      logger.info("Finished analysis of BadPixel")
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of BadPixel: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }

  }
}
