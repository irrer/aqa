package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CollimatorCentering
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
import java.awt.Point
import edu.umro.ScalaUtil.Trace
import edu.umro.ImageUtil.ImageText
import edu.umro.ScalaUtil.DicomUtil

/**
 * Measure the four edges in an image (TBLR : top, bottom, left, right).
 */
object MeasureTBLREdges extends Logging {

  case class X1X2Y1Y2(X1: Double, X2: Double, Y1: Double, Y2: Double) {
    def minus(other: X1X2Y1Y2) = new X1X2Y1Y2(
      X1 - other.X1,
      X2 - other.X2,
      Y1 - other.Y1,
      X2 - other.X2)

    def toSeq = Seq(X1, X2, Y1, Y2)

    def toString(fmt: String) = {
      "X1: " + X1.formatted(fmt) + "    X2: " + X2.formatted(fmt) + "    Y1: " + Y1.formatted(fmt) + "    Y2: " + Y2.formatted(fmt)
    }
  }

  def TBLRtoX1X2Y1Y2(tblr: TBLR) = new X1X2Y1Y2(tblr.left, tblr.right, tblr.top, tblr.bottom)

  def TBLRtoX1X2Y1Y2(collimatorAngle: Double, tblr: TBLR) = {
    Util.angleRoundedTo90(collimatorAngle) match {
      case 0 => new X1X2Y1Y2(tblr.left, tblr.right, tblr.bottom, tblr.top)
      case 90 => new X1X2Y1Y2(tblr.bottom, tblr.top, tblr.right, tblr.left)
      case 180 => new X1X2Y1Y2(tblr.right, tblr.left, tblr.top, tblr.bottom)
      case 270 => new X1X2Y1Y2(tblr.top, tblr.bottom, tblr.left, tblr.right)
    }
  }

  /**
   * Given a collimator angle, return the respective names of the top, bottom, left, and right edges.
   */
  def edgeNames(collimatorAngle: Double): Seq[String] = {
    Util.angleRoundedTo90(collimatorAngle) match {
      case 0 => Seq("Y2", "Y1", "X1", "X2")
      case 90 => Seq("X2", "X1", "Y2", "Y1")
      case 180 => Seq("Y1", "Y2", "X2", "X1")
      case 270 => Seq("X1", "X2", "Y1", "Y2")
    }
  }

  case class TBLR(top: Double, bottom: Double, left: Double, right: Double) {
    val center = new Point2D.Double((right + left) / 2, (top + bottom) / 2)
    val width = (right - left).abs
    val height = (bottom - top).abs

    def translate(floodOffset: Point, ImagePlanePixelSpacing: Point2D.Double) = {
      def transX(x: Double) = (x + floodOffset.getX) * ImagePlanePixelSpacing.getX
      def transY(y: Double) = (y + floodOffset.getY) * ImagePlanePixelSpacing.getY
      new TBLR(transY(top), transY(bottom), transX(left), transX(right))
    }

    def toX1X2Y1Y2(collimatorAngle: Double) = TBLRtoX1X2Y1Y2(collimatorAngle, this)
  }

  def getCollimatorPositions(BeamLimitingDeviceSequence: Seq[AttributeList]): X1X2Y1Y2 = {
    def getPair(nameList: Seq[String]): Array[Double] = {
      BeamLimitingDeviceSequence.filter(s => nameList.contains(s.get(TagFromName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString)).head.get(TagFromName.LeafJawPositions).getDoubleValues
    }

    val xPair = getPair(Util.xOrientation)
    val yPair = getPair(Util.yOrientation)

    new X1X2Y1Y2(xPair.min, xPair.max, yPair.min, yPair.max)
  }

  def planCollimatorPositions(beamName: String, plan: AttributeList): X1X2Y1Y2 = {
    val beamSeq = DicomUtil.seqToAttr(plan, TagFromName.BeamSequence).find(bs => bs.get(TagFromName.BeamName).getSingleStringValueOrEmptyString.equals(beamName)).get
    val controlPtSeq = DicomUtil.seqToAttr(beamSeq, TagFromName.ControlPointSequence).head
    val BeamLimitingDeviceSequence = DicomUtil.seqToAttr(controlPtSeq, TagFromName.BeamLimitingDevicePositionSequence)
    getCollimatorPositions(BeamLimitingDeviceSequence)
  }

  def imageCollimatorPositions(al: AttributeList): MeasureTBLREdges.X1X2Y1Y2 = {
    val ExposureSequence = DicomUtil.seqToAttr(al, TagFromName.ExposureSequence).head
    val BeamLimitingDeviceSequence = DicomUtil.seqToAttr(ExposureSequence, TagFromName.BeamLimitingDeviceSequence)
    getCollimatorPositions(BeamLimitingDeviceSequence)
  }

  case class AnalysisResult(measurementSet: TBLR, bufferedImage: BufferedImage)

  private def getCoarseTopRectangle(image: DicomImage, cntrOfMass: Point2D, ImagePlanePixelSpacing: Point2D.Double): Rectangle = {
    val width = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / ImagePlanePixelSpacing.getX).toInt
    val x = (cntrOfMass.getX - (width / 2.0)).round.toInt
    new Rectangle(x, 0, width, image.height / 2)
  }

  private def getCoarseBottomRectangle(image: DicomImage, cntrOfMass: Point2D, ImagePlanePixelSpacing: Point2D.Double): Rectangle = {
    val width = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / ImagePlanePixelSpacing.getX).toInt
    val x = (cntrOfMass.getX - (width / 2.0)).round.toInt
    new Rectangle(x, image.height / 2, width, image.height / 2)
  }

  //

  private def getCoarseLeftRectangle(image: DicomImage, cntrOfMass: Point2D, ImagePlanePixelSpacing: Point2D.Double): Rectangle = {
    val height = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / ImagePlanePixelSpacing.getY).toInt
    val y = (cntrOfMass.getY - (height / 2.0)).round.toInt
    new Rectangle(0, y, image.width / 2, height)
  }

  private def getCoarseRightRectangle(image: DicomImage, cntrOfMass: Point2D, ImagePlanePixelSpacing: Point2D.Double): Rectangle = {
    val height = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / ImagePlanePixelSpacing.getY).toInt
    val y = (cntrOfMass.getY - (height / 2.0)).round.toInt
    new Rectangle(image.width / 2, y, image.width / 2, height)
  }

  //

  /**
   * Calculate the point at the given percent between the highest and loleft pixel value.
   *
   * @param image: Contains range of pixel values
   *
   * @param thresholdPercent: Must be from 0 to 1 non-inclusive, indicates where the actual edge should be considered to be.
   */
  private def calcPercentPixelValue(image: DicomImage, thresholdPercent: Double): Double = {
    if (!((thresholdPercent > 0) && (thresholdPercent < 1))) throw new IllegalArgumentException("thresholdPercent is " + thresholdPercent + " but must be 0 < t < 1.")
    val pixelCount = ((Config.PenumbraPlateauPixelsPerMillion / 1000000.0) * image.width * image.height).round.toInt
    if (pixelCount < 1) {
      val ex = new IllegalArgumentException("Image has zero pixels, impossible to calculate threshold")
      val msg = ex.toString + "\n" + fmtEx(ex)
      logger.error(msg)
      throw ex
    }
    val min = image.minPixelValues(pixelCount).sum / pixelCount
    val max = image.maxPixelValues(pixelCount).sum / pixelCount
    val pctThresh = ((max - min) * thresholdPercent) + min
    pctThresh
  }

  private def coarseMeasure(image: DicomImage, halfwayPixelValue: Double, ImagePlanePixelSpacing: Point2D.Double, floodOffset: Point2D): TBLR = {
    val cntrOfMass = new Point2D.Double(ImageUtil.centerOfMass(image.columnSums), ImageUtil.centerOfMass(image.rowSums))

    val coarseTopRectangle = getCoarseTopRectangle(image, cntrOfMass, ImagePlanePixelSpacing)
    val coarseTopEdge = LocateEdge.locateEdge(image.getSubimage(coarseTopRectangle).rowSums, halfwayPixelValue * coarseTopRectangle.getWidth)

    val coarseBottomRectangle = getCoarseBottomRectangle(image, cntrOfMass, ImagePlanePixelSpacing)
    val coarseBottomEdge = LocateEdge.locateEdge(image.getSubimage(coarseBottomRectangle).rowSums, halfwayPixelValue * coarseBottomRectangle.getWidth) + coarseBottomRectangle.getY

    val coarseLeftRectangle = getCoarseLeftRectangle(image, cntrOfMass, ImagePlanePixelSpacing)
    val coarseLeftEdge = LocateEdge.locateEdge(image.getSubimage(coarseLeftRectangle).columnSums, halfwayPixelValue * coarseLeftRectangle.getHeight)

    val coarseRightRectangle = getCoarseRightRectangle(image, cntrOfMass, ImagePlanePixelSpacing)
    val coarseRightEdge = LocateEdge.locateEdge(image.getSubimage(coarseRightRectangle).columnSums, halfwayPixelValue * coarseRightRectangle.getHeight) + coarseRightRectangle.getX

    new TBLR(coarseTopEdge, coarseBottomEdge, coarseLeftEdge, coarseRightEdge)
  }

  private val imageColor = Color.green
  private val annotationColor = Color.gray

  private val dashedLine = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(1, 4), 0)
  private val solidLine = new BasicStroke

  private def annotateTopBottom(bufImg: BufferedImage, graphics: Graphics2D, pixelEdge: Double, name: String, scaledEdge: Double, rect: Rectangle, floodOffset: Point) = {

    val xOff = floodOffset.getX.round.toInt
    val yOff = floodOffset.getY.round.toInt

    val xRight = rect.getX.round.toInt + xOff
    val xLeft = (rect.getX + rect.getWidth).round.toInt + xOff
    val xMid = (xRight + xLeft) / 2

    val yTop = rect.getY.round.toInt + yOff
    val yBottom = (rect.getY + rect.getHeight).round.toInt + yOff
    val yMid = pixelEdge.round.toInt + yOff

    graphics.setStroke(dashedLine)
    graphics.drawLine(xRight, yMid, xLeft, yMid) // horizontal line through edge that was found
    graphics.setStroke(solidLine)
    graphics.drawLine(xRight, yTop, xRight, yBottom) // vertical line at left side of line
    graphics.drawLine(xLeft, yTop, xLeft, yBottom) // vertical line at right side of line

    val text = name + scaledEdge.formatted(" %7.2f")

    val textOffset = 16
    val y = pixelEdge.round.toInt + yOff
    val yText = if (y > bufImg.getHeight / 2) y + textOffset else y - textOffset
    ImageText.drawTextCenteredAt(graphics, xMid, yText, text)
  }

  private def annotateRightLeft(bufImg: BufferedImage, graphics: Graphics2D, pixelEdge: Double, name: String, scaledEdge: Double, rect: Rectangle, floodOffset: Point) = {

    val xOff = floodOffset.getX.round.toInt
    val yOff = floodOffset.getY.round.toInt

    val xRight = rect.getX.round.toInt + xOff
    val xLeft = (rect.getX + rect.getWidth).round.toInt + xOff
    val xMid = pixelEdge.round.toInt + xOff

    val yTop = rect.getY.round.toInt + yOff
    val yBottom = (rect.getY + rect.getHeight).round.toInt + yOff
    val yMid = (yTop + yBottom) / 2

    graphics.setStroke(dashedLine)
    graphics.drawLine(xMid, yTop, xMid, yBottom) // vertical line through edge that was found
    graphics.setStroke(solidLine)
    graphics.drawLine(xRight, yTop, xLeft, yTop) // horizontal line at left side of line
    graphics.drawLine(xRight, yBottom, xLeft, yBottom) // horizontal line at right side of line

    val text = name + scaledEdge.formatted(" %7.2f")

    val textOffset = 30
    val x = pixelEdge.round.toInt + xOff
    val xText = if (x > bufImg.getWidth / 2) x + textOffset else x - textOffset
    ImageText.drawTextCenteredAt(graphics, xText, yMid, text)
  }

  private def annotateCenter(bufImg: BufferedImage, graphics: Graphics2D, pixelEdges: TBLR, transMeasurementSet: TBLR, ImagePlanePixelSpacing: Point2D.Double) = {
    // center of image (not center of edges)
    val pixelImageCenter = new Point2D.Double(bufImg.getWidth / 2.0, bufImg.getHeight / 2.0)
    val scaledImageCenter = new Point2D.Double(pixelImageCenter.getX * ImagePlanePixelSpacing.getX, pixelImageCenter.getY * ImagePlanePixelSpacing.getY)

    def fmt(d: Double) = d.formatted("%7.2f").trim

    graphics.setStroke(solidLine)
    graphics.setColor(annotationColor)
    val pixCntrX = pixelImageCenter.getX.round.toInt
    val pixCntrY = pixelImageCenter.getY.round.toInt

    val len = 5
    val gap = 2
    graphics.drawLine(pixCntrX, pixCntrY - (len + gap), pixCntrX, pixCntrY - gap)
    graphics.drawLine(pixCntrX, pixCntrY + len + gap, pixCntrX, pixCntrY + gap)
    graphics.drawLine(pixCntrX - (len + gap), pixCntrY, pixCntrX - gap, pixCntrY)
    graphics.drawLine(pixCntrX + gap, pixCntrY, pixCntrX + len + gap, pixCntrY)

    val ascent = graphics.getFontMetrics.getAscent

    def drawText(text: String, textRow: Int) = {
      val textRect = graphics.getFontMetrics.getStringBounds(text, graphics)
      val x = pixelImageCenter.getX - textRect.getCenterX
      val y = pixelImageCenter.getY + len + gap + ascent + (textRect.getHeight * textRow)
      graphics.drawString(text, x.toFloat, y.toFloat)
    }

    drawText("Collimator Center: " + fmt(transMeasurementSet.center.getX) + ", " + fmt(transMeasurementSet.center.getY), 0)
    drawText("Image Center: " + fmt(scaledImageCenter.getX) + ", " + fmt(scaledImageCenter.getY), 1)
    drawText("Off Center: " + fmt(transMeasurementSet.center.getX - scaledImageCenter.getX) + ", " + fmt(transMeasurementSet.center.getY - scaledImageCenter.getY), 2)
  }

  /**
   * Make an annotated image that illustrates the edges.
   */
  private def makeAnnotatedImage(image: DicomImage, measurementSet: TBLR, transMeasurementSet: TBLR, collimatorAngle: Double,
    topRect: Rectangle, bottomRect: Rectangle, rightRect: Rectangle, leftRect: Rectangle, floodOffset: Point, ImagePlanePixelSpacing: Point2D.Double): BufferedImage = {
    //val bufImg = image.toBufferedImage(imageColor)
    val bufImg = image.toDeepColorBufferedImage
    Config.applyWatermark(bufImg)
    val graphics = ImageUtil.getGraphics(bufImg)
    graphics.setColor(annotationColor)

    val names = edgeNames(collimatorAngle)
    annotateTopBottom(bufImg, graphics, measurementSet.top, names(0), transMeasurementSet.top, topRect, floodOffset)
    annotateTopBottom(bufImg, graphics, measurementSet.bottom, names(1), transMeasurementSet.bottom, bottomRect, floodOffset)
    annotateRightLeft(bufImg, graphics, measurementSet.right, names(2), transMeasurementSet.right, rightRect, floodOffset)
    annotateRightLeft(bufImg, graphics, measurementSet.left, names(3), transMeasurementSet.left, leftRect, floodOffset)

    annotateCenter(bufImg, graphics, measurementSet, transMeasurementSet, ImagePlanePixelSpacing)
    bufImg
  }

  private def topBottomRectangles(coarse: TBLR, penumbraX: Double, penumbraY: Double): (Rectangle, Rectangle) = {
    val nsHeight = penumbraY // top and bottom height
    val nsWidth = (coarse.right - coarse.left) - penumbraX // top and bottom width
    val nsX = coarse.left + (penumbraX / 2)

    val topRectangle = new Rectangle(nsX.round.toInt, (coarse.top - (penumbraY / 2)).round.toInt, nsWidth.round.toInt, nsHeight.round.toInt)
    val bottomRectangle = new Rectangle(nsX.round.toInt, (coarse.bottom - (penumbraY / 2)).round.toInt, nsWidth.round.toInt, nsHeight.round.toInt)
    (topRectangle, bottomRectangle)
  }

  private def leftRightRectangles(coarse: TBLR, penumbraX: Double, penumbraY: Double): (Rectangle, Rectangle) = {
    val ewHeight = (coarse.bottom - coarse.top) - penumbraY // right and left height
    val ewWidth = penumbraX // right and left height
    val ewY = coarse.top + (penumbraX / 2)

    val leftRectangle = new Rectangle((coarse.left - (penumbraX / 2)).round.toInt, ewY.round.toInt, ewWidth.round.toInt, ewHeight.round.toInt)
    val rightRectangle = new Rectangle((coarse.right - (penumbraX / 2)).round.toInt, ewY.round.toInt, ewWidth.round.toInt, ewHeight.round.toInt)
    (leftRectangle, rightRectangle)
  }

  /**
   * Measure the four edges in the image, and create an annotated image.
   *
   * @param image: Image to analyze.  Should have already been corrected for flood field if necessary.
   *
   * @param ImagePlanePixelSpacing: Size of X and Y pixels in mm.
   *
   * @param annotate: Image containing pixels to be annotated.
   *
   * @param floodOffset: XY offset of image to annotate.
   */
  def measure(image: DicomImage, ImagePlanePixelSpacing: Point2D.Double, collimatorAngle: Double, annotate: DicomImage, floodOffset: Point, thresholdPercent: Double): AnalysisResult = {
    val threshold = calcPercentPixelValue(image, thresholdPercent)
    val coarse = coarseMeasure(image, threshold, ImagePlanePixelSpacing, floodOffset)

    val penumbraX = Config.PenumbraThickness_mm / ImagePlanePixelSpacing.getX // penumbra thickness in pixels
    val penumbraY = Config.PenumbraThickness_mm / ImagePlanePixelSpacing.getY // penumbra thickness in pixels

    val nsRect = topBottomRectangles(coarse, penumbraX, penumbraY)
    val ewRect = leftRightRectangles(coarse, penumbraX, penumbraY)

    val topEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._1).rowSums, threshold * nsRect._1.width) + nsRect._1.y
    val bottomEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._2).rowSums, threshold * nsRect._2.width) + nsRect._2.y
    val leftEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._1).columnSums, threshold * ewRect._1.height) + ewRect._1.x
    val rightEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._2).columnSums, threshold * ewRect._2.height) + ewRect._2.x

    val measurementSet = new TBLR(topEdge, bottomEdge, leftEdge, rightEdge)
    val transMeasurementSet = measurementSet.translate(floodOffset, ImagePlanePixelSpacing)

    val bufferedImage = makeAnnotatedImage(annotate, measurementSet, transMeasurementSet, collimatorAngle, nsRect._1, nsRect._2, ewRect._1, ewRect._2, floodOffset, ImagePlanePixelSpacing)
    new AnalysisResult(transMeasurementSet, bufferedImage)
  }

  def measure(image: DicomImage, ImagePlanePixelSpacing: Point2D.Double, collimatorAngle: Double, annotate: DicomImage, floodOffset: Point): AnalysisResult = {
    measure(image, ImagePlanePixelSpacing, collimatorAngle, annotate, floodOffset, 0.5)
  }

}
