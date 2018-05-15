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

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object CollimatorCenteringAnalysis extends Logging {

  case class MeasurementSet(north: Double, south: Double, east: Double, west: Double) {
    val center = new Point2D.Double((east + west) / 2, (north + south) / 2)
  }

  case class AnalysisResult(measurementSet: MeasurementSet, bufferedImage: BufferedImage)

  private def getCoarseNorthRectangle(image: DicomImage, cntrOfMass: Point2D, imageResolution: Point2D): Rectangle = {
    val width = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / imageResolution.getX).toInt
    val x = (cntrOfMass.getX - (width / 2.0)).round.toInt
    new Rectangle(x, 0, width, image.height - image.height / 2)
  }

  private def getCoarseSouthRectangle(image: DicomImage, cntrOfMass: Point2D, imageResolution: Point2D): Rectangle = {
    val width = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / imageResolution.getX).toInt
    val x = (cntrOfMass.getX - (width / 2.0)).round.toInt
    new Rectangle(x, image.height / 2, width, image.height - image.height / 2)
  }

  //

  private def getCoarseEastRectangle(image: DicomImage, cntrOfMass: Point2D, imageResolution: Point2D): Rectangle = {
    val height = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / imageResolution.getY).toInt
    val y = (cntrOfMass.getY - (height / 2.0)).round.toInt
    new Rectangle(image.width / 2, y, image.width - image.width / 2, height)
  }

  private def getCoarseWestRectangle(image: DicomImage, cntrOfMass: Point2D, imageResolution: Point2D): Rectangle = {
    val height = Math.ceil(Config.CollimatorCenteringCoarseBandWidth_mm / imageResolution.getY).toInt
    val y = (cntrOfMass.getY - (height / 2.0)).round.toInt
    new Rectangle(0, y, image.width / 2, height)
  }

  //

  /**
   * Calculate the halfway point between the highest and lowest pixel value.
   */
  private def calcHalfwayPixelValue(image: DicomImage): Double = {
    val pixelCount = ((Config.CollimatorCenteringPenumbraPlateauSizePerMillion / 1000000.0) * image.width * image.height).round.toInt
    val min = image.minPixelValues(pixelCount).sum / pixelCount
    val max = image.maxPixelValues(pixelCount).sum / pixelCount
    ((min + max) / 2.0)
  }

  private def coarseMeasure(image: DicomImage, halfwayPixelValue: Double, imageResolution: Point2D): MeasurementSet = {
    val cntrOfMass = new Point2D.Double(ImageUtil.centerOfMass(image.columnSums), ImageUtil.centerOfMass(image.rowSums))

    val coarseNorthRectangle = getCoarseNorthRectangle(image, cntrOfMass, imageResolution)
    val coarseNorthEdge = LocateEdge.locateEdge(image.getSubimage(coarseNorthRectangle).rowSums, halfwayPixelValue * coarseNorthRectangle.getWidth)

    val coarseSouthRectangle = getCoarseSouthRectangle(image, cntrOfMass, imageResolution)
    val coarseSouthEdge = LocateEdge.locateEdge(image.getSubimage(coarseSouthRectangle).rowSums, halfwayPixelValue * coarseSouthRectangle.getWidth) + coarseSouthRectangle.getY

    val coarseEastRectangle = getCoarseEastRectangle(image, cntrOfMass, imageResolution)
    val coarseEastEdge = LocateEdge.locateEdge(image.getSubimage(coarseEastRectangle).columnSums, halfwayPixelValue * coarseEastRectangle.getHeight) + coarseEastRectangle.x

    val coarseWestRectangle = getCoarseWestRectangle(image, cntrOfMass, imageResolution)
    val coarseWestEdge = LocateEdge.locateEdge(image.getSubimage(coarseWestRectangle).columnSums, halfwayPixelValue * coarseWestRectangle.getHeight)

    new MeasurementSet(coarseNorthEdge, coarseSouthEdge, coarseEastEdge, coarseWestEdge)
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

  private def annotateCenter(bufImg: BufferedImage, graphics: Graphics2D, pixelEdges: MeasurementSet, scaledEdges: MeasurementSet, imageResolution: Point2D) = {
    // center of image (not center of edges)
    val pixelImageCenter = new Point2D.Double(bufImg.getWidth / 2.0, bufImg.getHeight / 2.0)
    // scaled center of image (not center of edges)
    val scaledImageCenter = new Point2D.Double(pixelImageCenter.getX * imageResolution.getX, pixelImageCenter.getY * imageResolution.getY)

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
  private def makeAnnotatedImage(image: DicomImage, pixelEdges: MeasurementSet, scaledEdges: MeasurementSet, northRect: Rectangle, southRect: Rectangle, eastRect: Rectangle, westRect: Rectangle, imageResolution: Point2D): BufferedImage = {
    val bufImg = image.toBufferedImage(imageColor)
    val graphics = ImageUtil.getGraphics(bufImg)
    graphics.setColor(annotationColor)

    annotateNorthSouth(bufImg, graphics, pixelEdges.north, scaledEdges.north, northRect)
    annotateNorthSouth(bufImg, graphics, pixelEdges.south, scaledEdges.south, southRect)
    annotateEastWest(bufImg, graphics, pixelEdges.east, scaledEdges.east, eastRect)
    annotateEastWest(bufImg, graphics, pixelEdges.west, scaledEdges.west, westRect)

    annotateCenter(bufImg, graphics, pixelEdges, scaledEdges, imageResolution)
    bufImg
  }

  private def northSouthRectangles(coarse: MeasurementSet, penumbraX: Double, penumbraY: Double): (Rectangle, Rectangle) = {
    val nsHeight = penumbraY // north and south height
    val nsWidth = (coarse.east - coarse.west) - penumbraX // north and south width
    val nsX = coarse.west + (penumbraX / 2)

    val northRectangle = new Rectangle(nsX.round.toInt, (coarse.north - (penumbraY / 2)).round.toInt, nsWidth.round.toInt, nsHeight.round.toInt)
    val southRectangle = new Rectangle(nsX.round.toInt, (coarse.south - (penumbraY / 2)).round.toInt, nsWidth.round.toInt, nsHeight.round.toInt)
    (northRectangle, southRectangle)
  }

  private def eastWestRectangles(coarse: MeasurementSet, penumbraX: Double, penumbraY: Double): (Rectangle, Rectangle) = {
    val ewHeight = (coarse.south - coarse.north) - penumbraY // east and west height
    val ewWidth = penumbraX // east and west height
    val ewY = coarse.north + (penumbraX / 2)

    val eastRectangle = new Rectangle((coarse.east - (penumbraX / 2)).round.toInt, ewY.round.toInt, ewWidth.round.toInt, ewHeight.round.toInt)
    val westRectangle = new Rectangle((coarse.west - (penumbraX / 2)).round.toInt, ewY.round.toInt, ewWidth.round.toInt, ewHeight.round.toInt)
    (eastRectangle, westRectangle)
  }

  private def fineMeasure(attributeList: AttributeList): AnalysisResult = {
    val image = Phase2Util.correctBadPixels(new DicomImage(attributeList))
    // TODO should divide pixels by open field image
    val halfwayPixelValue = calcHalfwayPixelValue(image)
    val ImagePlanePixelSpacing = attributeList.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues
    val imageResolution = new Point2D.Double(ImagePlanePixelSpacing(0), ImagePlanePixelSpacing(1))

    val coarse = coarseMeasure(image, halfwayPixelValue, imageResolution)

    val penumbraX = Config.CollimatorCenteringPenumbraThickness_mm / imageResolution.getX // penumbra thickness in pixels
    val penumbraY = Config.CollimatorCenteringPenumbraThickness_mm / imageResolution.getY // penumbra thickness in pixels

    val nsX = coarse.west + (penumbraX / 2)

    val nsRect = northSouthRectangles(coarse, penumbraX, penumbraY)

    val ewRect = eastWestRectangles(coarse, penumbraX, penumbraY)

    val northEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._1).rowSums, halfwayPixelValue * nsRect._1.width) + nsRect._1.y
    val southEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._2).rowSums, halfwayPixelValue * nsRect._2.width) + nsRect._2.y
    val eastEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._1).columnSums, halfwayPixelValue * ewRect._1.height) + ewRect._1.x
    val westEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._2).columnSums, halfwayPixelValue * ewRect._2.height) + ewRect._2.x

    val pixelEdges = new MeasurementSet(northEdge, southEdge, eastEdge, westEdge)
    val scaledEdges = new MeasurementSet(northEdge * imageResolution.getY, southEdge * imageResolution.getY, eastEdge * imageResolution.getX, westEdge * imageResolution.getX)

    val bufferedImage = makeAnnotatedImage(image, pixelEdges, scaledEdges, nsRect._1, nsRect._2, ewRect._1, ewRect._2, imageResolution)

    new AnalysisResult(pixelEdges, bufferedImage)
  }

  /**
   * Allow external access for testing.
   */
  def testHook(attributeList: AttributeList): AnalysisResult = {
    fineMeasure(attributeList: AttributeList)
  }

  /**
   * Get the center of the image (regardless of collimator) in mm.
   */
  private def getImageCenter_mm(al: AttributeList): Point2D.Double = {
    val ImagePlanePixelSpacing = al.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues
    val Rows = al.get(TagFromName.Rows).getIntegerValues.head
    val Columns = al.get(TagFromName.Columns).getIntegerValues.head
    new Point2D.Double(Columns * ImagePlanePixelSpacing(0), Rows * ImagePlanePixelSpacing(1))
  }

  /**
   * Run the CollimatorCentering sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(output: Output, collimatorCenteringRunRequirements: CollimatorCenteringRunRequirements): (ProcedureStatus.Value, Elem) = {
    val outPK = output.outputPK.get

    val result090 = fineMeasure(collimatorCenteringRunRequirements.image090.attributeList.get)
    val result270 = fineMeasure(collimatorCenteringRunRequirements.image270.attributeList.get)

    val m090 = result090.measurementSet
    val m270 = result270.measurementSet
    val colCntr = new Point2D.Double((m090.center.getX + m270.center.getX) / 2, (m090.center.getY + m270.center.getY) / 2)
    val imgCntr = getImageCenter_mm(collimatorCenteringRunRequirements.image090.attributeList.get)

    val collimatorCentering = new CollimatorCentering(None, output.outputPK.get,
      colCntr.getX - imgCntr.getX, colCntr.getY - imgCntr.getY,
      colCntr.getX, colCntr.getY,
      m090.north, m090.south, m090.east, m090.west,
      m270.north, m270.south, m270.east, m270.west)

    CollimatorCenteringHTML.makeDisplay(output, result090, result270, collimatorCenteringRunRequirements)

    val pass: Boolean = false
    val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
    //val elem = CollimatorCenteringHTML.makeDisplay(output, positioningCheckRunRequirements, procedureStatus)
    //(procedureStatus, elem)
    ???
  }
}
