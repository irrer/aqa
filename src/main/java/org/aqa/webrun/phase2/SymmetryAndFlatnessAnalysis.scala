package org.aqa.webrun.phase2

import org.aqa.Logging
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import scala.xml.Elem
import org.aqa.run.ProcedureStatus
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import org.aqa.Config
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import edu.umro.ScalaUtil.Trace
import edu.umro.ImageUtil.ImageText
import java.awt.BasicStroke

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessAnalysis extends Logging {

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
    Trace.trace("pass: " + pass)

    /** Aggregate status. */
    val status = boolToStatus(pass)
    Trace.trace("status: " + status)
  }

  private def addGrats(imageWidth: Int, imageHeight: Int, xToPix: (Double) => Int, yToPix: (Double) => Int, color: Color, graphics: Graphics2D) = {
    // TODO
  }

  private val lightlyDashedLine = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(0.5f, 4), 0)

  private def addGraticules(img: BufferedImage, translator: IsoImagePlaneTranslator) = {
    val graphics = ImageUtil.getGraphics(img)

    val xMin = 0
    val yMin = 0
    val xMax = translator.width - 1
    val yMax = translator.height - 1
    val xMiddle = xMax / 2
    val yMiddle = yMax / 2

    val min = translator.pix2Iso(xMin, yMin)
    val max = translator.pix2Iso(xMax, yMax)

    val xGrat = ImageUtil.graticule(min.getX, max.getX, 8)
    val yGrat = ImageUtil.graticule(min.getY, max.getY, 8)

    val gratMajorLength = 10
    val gratMinorLength = 5

    graphics.setColor(Color.gray)
    ImageUtil.setSolidLine(graphics)

    // draw borders and center lines
    graphics.drawLine(xMin, yMin, xMin, yMax)
    graphics.drawLine(xMax, yMin, xMax, yMax)
    graphics.drawLine(xMin, yMin, xMax, yMin)
    graphics.drawLine(xMin, yMax, xMax, yMax)
    graphics.drawLine(xMin, yMiddle, xMax, yMiddle)
    graphics.drawLine(xMiddle, yMin, xMiddle, yMax)

    def xToPix(xIso: Double) = translator.iso2Pix(xIso, 0).getX.round.toInt
    def yToPix(yIso: Double) = translator.iso2Pix(0, yIso).getY.round.toInt

    val offset = 1 // Offset of tic mark from number.

    def closeTogether(aPix: Double, bPix: Double) = (aPix - bPix).abs < 2

    val numMinorTics = 4 // number of minor tics between major tics
    val xMinorInc = (xToPix(xGrat(1)) - xToPix(xGrat(0))) / (numMinorTics + 1)
    val yMinorInc = (yToPix(xGrat(1)) - yToPix(yGrat(0))) / (numMinorTics + 1)

    // draw top, bottom, and center graticules
    for (xIso <- xGrat) {
      val x = xToPix(xIso)
      val minorStart = if (xIso == xGrat.head) -numMinorTics else 1
      val text = xIso.round.toInt.toString + " "

      if (!closeTogether(x, xMiddle)) { // do not overwrite middle line
        // draw light grid lines
        graphics.setStroke(lightlyDashedLine)
        graphics.drawLine(x, yMin, x, yMax)
        ImageUtil.setSolidLine(graphics)
      }

      // top
      graphics.drawLine(x, yMin, x, yMin + gratMajorLength) // draw major graticule
      for (mt <- minorStart to numMinorTics) graphics.drawLine(x + (mt * xMinorInc), yMin, x + (mt * xMinorInc), yMin + gratMinorLength)
      ImageText.drawTextOffsetFrom(graphics, x, yMin + gratMajorLength + offset, text, 270) // draw number corresponding to major graticule
      // bottom
      graphics.drawLine(x, yMax, x, yMax - gratMajorLength) // draw major graticule
      for (mt <- minorStart to numMinorTics) graphics.drawLine(x + (mt * xMinorInc), yMax, x + (mt * xMinorInc), yMax - gratMinorLength)
      ImageText.drawTextOffsetFrom(graphics, x, yMax - gratMajorLength - offset, text, 90) // draw number corresponding to major graticule
      // center
      graphics.drawLine(x, yMiddle - gratMajorLength, x, yMiddle + gratMajorLength) // draw major graticule
      for (mt <- minorStart to numMinorTics) graphics.drawLine(x + (mt * xMinorInc), yMiddle - gratMinorLength, x + (mt * xMinorInc), yMiddle + gratMinorLength)
      ImageText.drawTextOffsetFrom(graphics, x, yMiddle - gratMajorLength - offset, text, 90) // draw number corresponding to major graticule
    }

    // draw left and right graticules
    for (yIso <- yGrat) {
      val y = yToPix(yIso)
      val minorStart = if (yIso == yGrat.head) -numMinorTics else 1
      val text = yIso.round.toInt.toString + " "

      if (!closeTogether(y, yMiddle)) { // do not overwrite middle line
        // draw light grid lines
        graphics.setStroke(lightlyDashedLine)
        graphics.drawLine(xMin, y, xMax, y)
        ImageUtil.setSolidLine(graphics)
      }

      val textWidth = ImageText.getTextDimensions(graphics, text).getWidth.round.toInt
      // left
      graphics.drawLine(xMin, y, xMin + gratMajorLength, y) // draw major graticule
      for (mt <- minorStart to numMinorTics) graphics.drawLine(xMin, y + (mt * yMinorInc), xMin + gratMinorLength, y + (mt * yMinorInc))
      ImageText.drawTextOffsetFrom(graphics, xMin + gratMajorLength + offset, y, text, 0) // draw number corresponding to major graticule
      // right
      graphics.drawLine(xMax, y, xMax - gratMajorLength, y) // draw major graticule
      for (mt <- minorStart to numMinorTics) graphics.drawLine(xMax, y + (mt * yMinorInc), xMax - gratMinorLength, y + (mt * yMinorInc))
      ImageText.drawTextOffsetFrom(graphics, xMax - (gratMajorLength + offset + textWidth), y, text, 0) // draw number corresponding to major graticule
      // center
      graphics.drawLine(xMiddle - gratMajorLength, y, xMiddle + gratMajorLength, y) // draw major graticule
      for (mt <- minorStart to numMinorTics) graphics.drawLine(xMiddle - gratMinorLength, y + (mt * yMinorInc), xMiddle + gratMinorLength, y + (mt * yMinorInc))
      ImageText.drawTextOffsetFrom(graphics, xMax - (xMiddle - gratMajorLength + offset + textWidth), y, text, 0) // draw number corresponding to major graticule
    }

  }

  private def makeAnnotatedImage(dicomImage: DicomImage, attributeList: AttributeList, pointMap: Map[SymmetryAndFlatnessPoint, Double]): BufferedImage = {
    val img = dicomImage.toBufferedImage(new Color(200, 255, 200))
    val graphics = ImageUtil.getGraphics(img)

    val translator = new IsoImagePlaneTranslator(attributeList)
    val radius = translator.circleRadiusInPixels
    val circleSize = (radius * 2).round.toInt
    addGraticules(img, translator)

    def annotatePoint(point: SymmetryAndFlatnessPoint) = {
      graphics.setColor(Color.black)
      val value = pointMap(point)
      val center = translator.iso2Pix(point.asPoint)
      graphics.drawOval((center.getX - radius).round.toInt, (center.getY - radius).round.toInt, circleSize, circleSize)
      ImageText.drawTextOffsetFrom(graphics, center.getX, center.getY - radius, point.name, 90)
      ImageText.drawTextOffsetFrom(graphics, center.getX, center.getY + radius, value.formatted("%6.4f"), 270)
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
    val axialSymmetryStatus = boolToStatus(Config.SymmetryLimit >= axialSymmetry.abs)
    val transverseSymmetry = analyzeSymmetry(Config.SymmetryPointRight, Config.SymmetryPointLeft, pointMap)
    val transverseSymmetryStatus = boolToStatus(Config.SymmetryLimit >= transverseSymmetry.abs)

    val flatness = analyzeFlatness(pointMap)
    val flatnessStatus = boolToStatus(Config.FlatnessLimit >= flatness.abs)

    val annotatedImage = makeAnnotatedImage(dicomImage, attributeList, pointMap)

    val transverseProfile = {
      val y = ((translator.height - widthOfBand) / 2.0).round.toInt
      val rectangle = new Rectangle(0, y, translator.width, widthOfBand)
      dicomImage.getSubimage(rectangle).columnSums.map(c => ((c / widthOfBand) * RescaleSlope) + RescaleIntercept)
    }

    val axialProfile = {
      val x = ((translator.width - widthOfBand) / 2.0).round.toInt
      val rectangle = new Rectangle(x, 0, widthOfBand, translator.height)
      dicomImage.getSubimage(rectangle).rowSums.map(c => ((c / widthOfBand) * RescaleSlope) + RescaleIntercept)
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
      //val resultList = beamNameList.par.map(beamName => analyze(beamName, extendedData, runReq)).toList
      val resultList = beamNameList.map(beamName => analyze(beamName, extendedData, runReq)).toList // change back to 'par' when debugged

      val pass = resultList.map(r => r.status.toString.equals(ProcedureStatus.pass)).reduce(_ && _)

      val summary = SymmetryAndFlatnessHTML.makeDisplay(extendedData, resultList, boolToStatus(pass), runReq)

      Right(new SymmetryAndFlatnessResult(summary, ProcedureStatus.done))
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
