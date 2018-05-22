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

/**
 * Measure the four edges in an image (north, south, east, west).
 */
object MeasureNSEWEdges extends Logging {

  case class NSEW(north: Double, south: Double, east: Double, west: Double) {
    val center = new Point2D.Double((east + west) / 2, (north + south) / 2)

    def scale(ImagePlanePixelSpacing: Point2D.Double) = new NSEW(
      north * ImagePlanePixelSpacing.getY, south * ImagePlanePixelSpacing.getY,
      east * ImagePlanePixelSpacing.getX, west * ImagePlanePixelSpacing.getX)

    def offset(diff: Point) = new NSEW(north + diff.getY, south + diff.getY, east + diff.getX, west + diff.getX)
  }

  case class AnalysisResult(measurementSet: NSEW, bufferedImage: BufferedImage)

  private def getCoarseNorthRectangle(image: DicomImage, cntrOfMass: Point2D, ImagePlanePixelSpacing: Point2D.Double): Rectangle = {
    val width = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / ImagePlanePixelSpacing.getX).toInt
    val x = (cntrOfMass.getX - (width / 2.0)).round.toInt
    new Rectangle(x, 0, width, image.height - image.height / 2)
  }

  private def getCoarseSouthRectangle(image: DicomImage, cntrOfMass: Point2D, ImagePlanePixelSpacing: Point2D.Double): Rectangle = {
    val width = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / ImagePlanePixelSpacing.getX).toInt
    val x = (cntrOfMass.getX - (width / 2.0)).round.toInt
    new Rectangle(x, image.height / 2, width, image.height - image.height / 2)
  }

  //

  private def getCoarseEastRectangle(image: DicomImage, cntrOfMass: Point2D, ImagePlanePixelSpacing: Point2D.Double): Rectangle = {
    val height = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / ImagePlanePixelSpacing.getY).toInt
    val y = (cntrOfMass.getY - (height / 2.0)).round.toInt
    new Rectangle(image.width / 2, y, image.width - image.width / 2, height)
  }

  private def getCoarseWestRectangle(image: DicomImage, cntrOfMass: Point2D, ImagePlanePixelSpacing: Point2D.Double): Rectangle = {
    val height = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / ImagePlanePixelSpacing.getY).toInt
    val y = (cntrOfMass.getY - (height / 2.0)).round.toInt
    new Rectangle(0, y, image.width / 2, height)
  }

  //

  /**
   * Calculate the halfway point between the highest and lowest pixel value.
   */
  private def calcHalfwayPixelValue(image: DicomImage): Double = {
    val pixelCount = ((Config.PenumbraPlateauPixelsPerMillion / 1000000.0) * image.width * image.height).round.toInt
    val min = image.minPixelValues(pixelCount).sum / pixelCount
    val max = image.maxPixelValues(pixelCount).sum / pixelCount
    ((min + max) / 2.0)
  }

  private def coarseMeasure(image: DicomImage, halfwayPixelValue: Double, ImagePlanePixelSpacing: Point2D.Double, floodOffset: Point2D): NSEW = {
    val cntrOfMass = new Point2D.Double(ImageUtil.centerOfMass(image.columnSums), ImageUtil.centerOfMass(image.rowSums))

    val coarseNorthRectangle = getCoarseNorthRectangle(image, cntrOfMass, ImagePlanePixelSpacing)
    val coarseNorthEdge = LocateEdge.locateEdge(image.getSubimage(coarseNorthRectangle).rowSums, halfwayPixelValue * coarseNorthRectangle.getWidth)

    val coarseSouthRectangle = getCoarseSouthRectangle(image, cntrOfMass, ImagePlanePixelSpacing)
    val coarseSouthEdge = LocateEdge.locateEdge(image.getSubimage(coarseSouthRectangle).rowSums, halfwayPixelValue * coarseSouthRectangle.getWidth) + coarseSouthRectangle.getY

    val coarseEastRectangle = getCoarseEastRectangle(image, cntrOfMass, ImagePlanePixelSpacing)
    val coarseEastEdge = LocateEdge.locateEdge(image.getSubimage(coarseEastRectangle).columnSums, halfwayPixelValue * coarseEastRectangle.getHeight) + coarseEastRectangle.getX

    val coarseWestRectangle = getCoarseWestRectangle(image, cntrOfMass, ImagePlanePixelSpacing)
    val coarseWestEdge = LocateEdge.locateEdge(image.getSubimage(coarseWestRectangle).columnSums, halfwayPixelValue * coarseWestRectangle.getHeight)

    new NSEW(coarseNorthEdge, coarseSouthEdge, coarseEastEdge, coarseWestEdge)
  }

  private val imageColor = Color.green
  private val annotationColor = Color.yellow

  private val dashedLine = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(1, 4), 0)
  private val solidLine = new BasicStroke

  private def annotateNorthSouth(bufImg: BufferedImage, graphics: Graphics2D, pixelEdge: Double, scaledEdge: Double, rect: Rectangle) = {

    val xEast = rect.getX.round.toInt
    val xWest = (rect.getX + rect.getWidth).round.toInt
    val xMid = (xEast + xWest) / 2

    val yNorth = rect.getY.round.toInt
    val ySouth = (rect.getY + rect.getHeight).round.toInt
    val yMid = pixelEdge.round.toInt

    graphics.setStroke(dashedLine)
    graphics.drawLine(xEast, yMid, xWest, yMid) // horizontal line through edge that was found
    graphics.setStroke(solidLine)
    graphics.drawLine(xEast, yNorth, xEast, ySouth) // vertical line at west side of line
    graphics.drawLine(xWest, yNorth, xWest, ySouth) // vertical line at east side of line

    val text = scaledEdge.formatted("%7.2f")
    ImageUtil.annotatePixel(bufImg, xMid, pixelEdge.round.toInt, annotationColor, text, false)
  }

  private def annotateEastWest(bufImg: BufferedImage, graphics: Graphics2D, pixelEdge: Double, scaledEdge: Double, rect: Rectangle) = {

    val xEast = rect.getX.round.toInt
    val xWest = (rect.getX + rect.getWidth).round.toInt
    val xMid = pixelEdge.round.toInt

    val yNorth = rect.getY.round.toInt
    val ySouth = (rect.getY + rect.getHeight).round.toInt
    val yMid = (yNorth + ySouth) / 2

    graphics.setStroke(dashedLine)
    graphics.drawLine(xMid, yNorth, xMid, ySouth) // vertical line through edge that was found
    graphics.setStroke(solidLine)
    graphics.drawLine(xEast, yNorth, xWest, yNorth) // horizontal line at west side of line
    graphics.drawLine(xEast, ySouth, xWest, ySouth) // horizontal line at east side of line

    val text = scaledEdge.formatted("%7.2f")
    ImageUtil.annotatePixel(bufImg, pixelEdge.round.toInt, yMid, annotationColor, text, false)
  }

  private def annotateCenter(bufImg: BufferedImage, graphics: Graphics2D, pixelEdges: NSEW, scaledEdges: NSEW, ImagePlanePixelSpacing: Point2D.Double) = {
    // center of image (not center of edges)
    val pixelImageCenter = new Point2D.Double(bufImg.getWidth / 2.0, bufImg.getHeight / 2.0)
    // scaled center of image (not center of edges)
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

    drawText("Collimator Center: " + fmt(scaledEdges.center.getX) + ", " + fmt(scaledEdges.center.getY), 0)
    drawText("Image Center: " + fmt(scaledImageCenter.getX) + ", " + fmt(scaledImageCenter.getY), 1)
    drawText("Off Center: " + fmt(scaledEdges.center.getX - scaledImageCenter.getX) + ", " + fmt(scaledEdges.center.getY - scaledImageCenter.getY), 2)
  }

  /**
   * Make an annotated image that illustrates the edges.
   */
  private def makeAnnotatedImage(image: DicomImage, measurementSet: NSEW, ImagePlanePixelSpacing: Point2D.Double,
    northRect: Rectangle, southRect: Rectangle, eastRect: Rectangle, westRect: Rectangle,
    imageResolution: Point2D): BufferedImage = {
    val bufImg = image.toBufferedImage(imageColor)
    val graphics = ImageUtil.getGraphics(bufImg)
    graphics.setColor(annotationColor)

    val scaledMeasurementSet = measurementSet.scale(ImagePlanePixelSpacing)
    annotateNorthSouth(bufImg, graphics, measurementSet.north, scaledMeasurementSet.north, northRect)
    annotateNorthSouth(bufImg, graphics, measurementSet.south, scaledMeasurementSet.south, southRect)
    annotateEastWest(bufImg, graphics, measurementSet.east, scaledMeasurementSet.east, eastRect)
    annotateEastWest(bufImg, graphics, measurementSet.west, scaledMeasurementSet.west, westRect)

    annotateCenter(bufImg, graphics, measurementSet, scaledMeasurementSet, ImagePlanePixelSpacing)
    bufImg
  }

  private def northSouthRectangles(coarse: NSEW, penumbraX: Double, penumbraY: Double): (Rectangle, Rectangle) = {
    val nsHeight = penumbraY // north and south height
    val nsWidth = (coarse.east - coarse.west) - penumbraX // north and south width
    val nsX = coarse.west + (penumbraX / 2)

    val northRectangle = new Rectangle(nsX.round.toInt, (coarse.north - (penumbraY / 2)).round.toInt, nsWidth.round.toInt, nsHeight.round.toInt)
    val southRectangle = new Rectangle(nsX.round.toInt, (coarse.south - (penumbraY / 2)).round.toInt, nsWidth.round.toInt, nsHeight.round.toInt)
    (northRectangle, southRectangle)
  }

  private def eastWestRectangles(coarse: NSEW, penumbraX: Double, penumbraY: Double): (Rectangle, Rectangle) = {
    val ewHeight = (coarse.south - coarse.north) - penumbraY // east and west height
    val ewWidth = penumbraX // east and west height
    val ewY = coarse.north + (penumbraX / 2)

    val eastRectangle = new Rectangle((coarse.east - (penumbraX / 2)).round.toInt, ewY.round.toInt, ewWidth.round.toInt, ewHeight.round.toInt)
    val westRectangle = new Rectangle((coarse.west - (penumbraX / 2)).round.toInt, ewY.round.toInt, ewWidth.round.toInt, ewHeight.round.toInt)
    (eastRectangle, westRectangle)
  }

  /**
   * Measure the four edges in the image, and create an annotated image.
   *
   * @param image: Image to analyze.  Should consist of a single rectangle - anything else will produce unpredictable results.
   *
   * @param ImagePlanePixelSpacing: Size of X and Y pixels in mm.
   *
   * @param annotate: Image containing pixels to be annotated.
   *
   * @param floodOffset: XY offset of image to annotate.
   */
  def measure(image: DicomImage, ImagePlanePixelSpacing: Point2D.Double, annotate: DicomImage, floodOffset: Point): (NSEW, BufferedImage) = {
    val halfwayPixelValue = calcHalfwayPixelValue(image)

    val coarse = coarseMeasure(image, halfwayPixelValue, ImagePlanePixelSpacing, floodOffset)

    val penumbraX = Config.PenumbraThickness_mm / ImagePlanePixelSpacing.getX // penumbra thickness in pixels
    val penumbraY = Config.PenumbraThickness_mm / ImagePlanePixelSpacing.getY // penumbra thickness in pixels

    //val nsX = coarse.west + (penumbraX / 2)

    val nsRect = northSouthRectangles(coarse, penumbraX, penumbraY)

    val ewRect = eastWestRectangles(coarse, penumbraX, penumbraY)

    val northEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._1).rowSums, halfwayPixelValue * nsRect._1.width) + nsRect._1.y
    val southEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._2).rowSums, halfwayPixelValue * nsRect._2.width) + nsRect._2.y
    val eastEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._1).columnSums, halfwayPixelValue * ewRect._1.height) + ewRect._1.x
    val westEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._2).columnSums, halfwayPixelValue * ewRect._2.height) + ewRect._2.x

    val measurementSet = new NSEW(northEdge, southEdge, eastEdge, westEdge)
    val scaledMeasurementSet = measurementSet.scale(ImagePlanePixelSpacing)

    val bufferedImage = makeAnnotatedImage(annotate, measurementSet, ImagePlanePixelSpacing, nsRect._1, nsRect._2, ewRect._1, ewRect._2, ImagePlanePixelSpacing)
    (measurementSet, bufferedImage)

  }

}
