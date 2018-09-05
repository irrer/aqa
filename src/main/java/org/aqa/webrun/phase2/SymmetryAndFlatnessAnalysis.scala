package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CollimatorPosition
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.collection.Seq
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.DicomFile
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.geom.Point2D
import org.aqa.Config
import java.awt.Rectangle
import edu.umro.ImageUtil.LocateEdge
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke
import edu.umro.ScalaUtil.Trace
import scala.collection.parallel.ParSeq
import org.aqa.db.CollimatorCentering
import java.awt.Point
import edu.umro.ImageUtil.ImageText
import java.io.File

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessAnalysis extends Logging {

  val htmlFileName = "SymmetryAndFlatness.html"
  val subDirName = "SymmetryAndFlatness"

  private def boolToStatus(pass: Boolean) = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

  /**
   * Encapsulate data to support both recording to the database and generating a report.
   */
  case class SymmetryAndFlatnessBeamResult(
    beamName: String,
    pointMap: Map[SymmetryAndFlatnessPoint, Double],
    axialSymmetry: Double,
    transverseSymmetry: Double,
    flatness: Double,
    axialSymmetryStatus: ProcedureStatus.Value,
    transverseSymmetryStatus: ProcedureStatus.Value,
    flatnessStatus: ProcedureStatus.Value,
    annotatedImage: BufferedImage,
    transverseProfile: Seq[Double], transverse_mm: IndexedSeq[Double],
    axialProfile: Seq[Double], axial_mm: IndexedSeq[Double]) {

    /** True if everything is ok. */
    val pass = Seq(axialSymmetryStatus, transverseSymmetryStatus, flatnessStatus).filter(s => !(s.toString.equals(ProcedureStatus.pass.toString))).isEmpty

    /** Aggregate status. */
    val status = boolToStatus(pass)
  }

  private def makeAnnotatedImage(dicomImage: DicomImage, attributeList: AttributeList, pointMap: Map[SymmetryAndFlatnessPoint, Double]): BufferedImage = {
    val img = dicomImage.toBufferedImage(Color.cyan)
    val graphics = ImageUtil.getGraphics(img)
    graphics.setColor(Color.black)

    val translator = new IsoImagePlaneTranslator(attributeList)
    val radius = translator.circleRadiusInPixels
    val circleSize = (radius * 2).round.toInt

    def annotatePoint(point: SymmetryAndFlatnessPoint) = {
      val value = pointMap(point)
      val center = translator.iso2Pix(point.asPoint)
      graphics.drawOval((center.getX - radius).round.toInt, (center.getY - radius).round.toInt, circleSize, circleSize)
      val text = value.formatted("%12.6f") + " : " + point.name
      ImageText.drawTextCenteredAt(graphics, center.getX, center.getY, text)
    }

    pointMap.keys.map(p => annotatePoint(p))

    img
  }

  private def getDicomImage(beamName: String, runReq: RunReq): DicomImage = {
    val isFlood = beamName.equalsIgnoreCase(Config.FloodFieldBeamName)
    if (isFlood) runReq.floodCorrectedImage
    else runReq.derivedMap(beamName).pixelCorrectedImage
  }

  private def getAttributeList(beamName: String, runReq: RunReq): AttributeList = {
    val isFlood = beamName.equalsIgnoreCase(Config.FloodFieldBeamName)
    if (isFlood) runReq.flood.attributeList.get
    else runReq.derivedMap(beamName).dicomFile.attributeList.get
  }

  private def analyzeSymmetry(maxPoint: SymmetryAndFlatnessPoint, minPoint: SymmetryAndFlatnessPoint, pointMap: Map[SymmetryAndFlatnessPoint, Double]): Double = {
    val max = pointMap(maxPoint)
    val min = pointMap(minPoint)
    (max - min) / min
  }

  /**
   * Get the flatness
   */
  private def analyzeFlatness(pointMap: Map[SymmetryAndFlatnessPoint, Double]): Double = {
    val min = pointMap.values.min
    val max = pointMap.values.max
    val flatness = (max - min) / (max + min)
    flatness
  }

  /**
   * For each point (circle) on the image, make a list of pixels that are included in it.
   */
  private def makePointMap(dicomImage: DicomImage, attributeList: AttributeList, RescaleSlope: Double, RescaleIntercept: Double) = {
    val pixMap = SymmetryAndFlatnessAnalysisPixelMap.getPixelMap(attributeList)

    /**
     * Get the average pixel value for one spot in HU or CU or whatever units the image is using.
     */
    def evalPoint(point: SymmetryAndFlatnessPoint): Double = {
      val pixList = pixMap(point)
      val avg = pixList.map(p => dicomImage.get(p.getX.toInt, p.getY.toInt)).sum / pixList.size
      (avg * RescaleSlope) + RescaleIntercept // convert
    }
    pixMap.keys.toSeq.map(p => (p, evalPoint(p))).toMap
  }

  /**
   * Analyze for symmetry and flatness.  The results should be sufficient to support both recording to
   * the database and generating a report.
   *
   */
  private def analyze(beamName: String, extendedData: ExtendedData, runReq: RunReq): SymmetryAndFlatnessBeamResult = {
    val image = getDicomImage(beamName, runReq)
    val attributeList: AttributeList = getAttributeList(beamName, runReq)
    val dicomImage = new DicomImage(attributeList)
    val RescaleSlope = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head
    val translator = new IsoImagePlaneTranslator(attributeList)
    val widthOfBand = translator.circleRadiusInPixels.round.toInt

    val pointMap = makePointMap(dicomImage, attributeList, RescaleSlope, RescaleIntercept)

    val axialSymmetry = analyzeSymmetry(Config.SymmetryPointTop, Config.SymmetryPointBottom, pointMap)
    val axialSymmetryStatus = boolToStatus(Config.SymmetryLimit >= axialSymmetry)
    val transverseSymmetry = analyzeSymmetry(Config.SymmetryPointRight, Config.SymmetryPointLeft, pointMap)
    val transverseSymmetryStatus = boolToStatus(Config.SymmetryLimit >= transverseSymmetry)

    val flatness = analyzeFlatness(pointMap)
    val flatnessStatus = boolToStatus(Config.FlatnessLimit >= flatness)

    val annotatedImage = makeAnnotatedImage(dicomImage, attributeList, pointMap)

    val transverseProfile = {
      val y = ((translator.height - widthOfBand) / 2.0).round.toInt
      val rectangle = new Rectangle(0, y, translator.width, widthOfBand)
      dicomImage.getSubimage(rectangle).columnSums.map(c => ((c / widthOfBand) * RescaleSlope) + RescaleIntercept)
    }

    val axialProfile = {
      val x = ((translator.width - widthOfBand) / 2.0).round.toInt
      val rectangle = new Rectangle(x, 0, widthOfBand, translator.height)
      dicomImage.getSubimage(rectangle).columnSums.map(c => ((c / widthOfBand) * RescaleSlope) + RescaleIntercept)
    }

    val transverse_mm = (0 until translator.width).map(x => translator.pix2Iso(x, 0).getX)
    val axial_mm = (0 until translator.height).map(y => translator.pix2Iso(0, y).getY)

    new SymmetryAndFlatnessBeamResult(beamName, pointMap,
      axialSymmetry,
      transverseSymmetry,
      flatness,
      axialSymmetryStatus,
      transverseSymmetryStatus,
      flatnessStatus,
      annotatedImage,
      transverseProfile, transverse_mm,
      axialProfile, axial_mm)
  }

  val subProcedureName = "SymmetryAndFlatness";

  class SymmetryAndFlatnessResult(summary: Elem, status: ProcedureStatus.Value) extends SubProcedureResult(summary, status, subProcedureName)

  /**
   * Run the CollimatorPosition sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, SymmetryAndFlatnessResult] = {
    try {
      logger.info("Starting analysis of SymmetryAndFlatness")

      val beamNameList = Config.SymmetryAndFlatnessBeamList.filter(beamName => runReq.derivedMap.contains(beamName))

      // only process beams that are both configured and have been uploaded
      val resultList = beamNameList.par.map(beamName => analyze(beamName, extendedData, runReq)).toList

      val pass = resultList.map(r => r.status.toString.equals(ProcedureStatus.pass)).reduce(_ && _)

      val subDir = new File(extendedData.output.dir, subDirName)
      subDir.mkdirs

      def saveImage(beamName: String, image: BufferedImage) = {
        val fileName = "Sym_Flat_" + beamName.replace(' ', '_') + ".png"
        val pngFile = new File(subDir, fileName)
        Util.writePng(image, pngFile)
      }

      resultList.map(r => saveImage(r.beamName, r.annotatedImage))

      val summary = {
        val elem = {
          <div title="Click for details.">
            <a href={ htmlFileName }>
              Center Dose.  Images:{ resultList.size }<br/>
              <img src={ Config.passImageUrl } height="32"/>
            </a>
          </div>
        }
        elem
      }

      Right(new SymmetryAndFlatnessResult(summary, ProcedureStatus.done))
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
