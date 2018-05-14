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

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object CollimatorCenteringAnalysis extends Logging {

  //private case class Measurement(location: Double, dicomImage: DicomImage)

  case class MeasurementSet(north: Double, south: Double, east: Double, west: Double)

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
  private def calcHalfwayPixelValue(image: DicomImage): Float = {
    val pixelCount = ((Config.CollimatorCenteringPenumbraPlateauSizePerMillion / 1000000.0) * image.width * image.height).round.toInt
    val min = image.minPixelValues(pixelCount).sum / pixelCount
    val max = image.maxPixelValues(pixelCount).sum / pixelCount
    ((min + max) / 2.0).toFloat
  }

  private def coarseMeasure(image: DicomImage, halfwayPixelValue: Float, imageResolution: Point2D): MeasurementSet = {
    val cntrOfMass = new Point2D.Double(ImageUtil.centerOfMass(image.columnSums), ImageUtil.centerOfMass(image.rowSums))

    val coarseNorthRectangle = getCoarseNorthRectangle(image, cntrOfMass, imageResolution)
    val coarseNorthEdge = LocateEdge.locateEdge(image.getSubimage(coarseNorthRectangle).rowSums, halfwayPixelValue)

    val coarseSouthRectangle = getCoarseSouthRectangle(image, cntrOfMass, imageResolution)
    val coarseSouthEdge = LocateEdge.locateEdge(image.getSubimage(coarseSouthRectangle).rowSums, halfwayPixelValue) + coarseSouthRectangle.getY

    val coarseEastRectangle = getCoarseEastRectangle(image, cntrOfMass, imageResolution)
    val coarseEastEdge = LocateEdge.locateEdge(image.getSubimage(coarseEastRectangle).columnSums, halfwayPixelValue)

    val coarseWestRectangle = getCoarseWestRectangle(image, cntrOfMass, imageResolution)
    val coarseWestEdge = LocateEdge.locateEdge(image.getSubimage(coarseWestRectangle).columnSums, halfwayPixelValue) + coarseWestRectangle.x

    new MeasurementSet(coarseNorthEdge, coarseSouthEdge, coarseEastEdge, coarseWestEdge)
  }

  private val imageColor = Color.green
  private val annotationColor = Color.yellow

  private def annotateNorthSouth(bufImg: BufferedImage, graphics: Graphics2D, edge: Double, rect: Rectangle, imageResolution: Point2D) = {

    val xEast = rect.getX.round.toInt
    val xWest = (rect.getX + rect.getWidth).round.toInt
    val xMid = (xEast + xWest / 2)

    val yNorth = rect.getY.round.toInt
    val ySouth = (rect.getY + rect.getHeight).round.toInt
    val yMid = edge.round.toInt

    graphics.drawLine(xEast, yMid, xWest, yMid) // horizontal line through edge that was found
    graphics.drawLine(xEast, yNorth, xEast, ySouth) // vertical line at west side of line
    graphics.drawLine(xWest, yNorth, xWest, ySouth) // vertical line at east side of line

    val text = (edge * imageResolution.getY).formatted("%7.2f")
    ImageUtil.annotatePixel(bufImg, xMid, edge.round.toInt, annotationColor, text, false)
  }

  private def annotateEastWest(bufImg: BufferedImage, graphics: Graphics2D, edge: Double, rect: Rectangle, imageResolution: Point2D) = {

    val xEast = rect.getX.round.toInt
    val xWest = (rect.getX + rect.getWidth).round.toInt
    val xMid = edge.round.toInt

    val yNorth = rect.getY.round.toInt
    val ySouth = (rect.getY + rect.getHeight).round.toInt
    val yMid = (yNorth + ySouth) / 2

    graphics.drawLine(xMid, yNorth, xMid, ySouth) // vertical line through edge that was found
    graphics.drawLine(xEast, yNorth, xWest, yNorth) // horizontal line at west side of line
    graphics.drawLine(xEast, ySouth, xWest, ySouth) // horizontal line at east side of line

    val text = (edge * imageResolution.getX).formatted("%7.2f")
    ImageUtil.annotatePixel(bufImg, edge.round.toInt, yMid, annotationColor, text, false)
  }

  /**
   * Make an annotated image that illustrates the edges.
   */
  private def makeAnnotatedImage(image: DicomImage, edges: MeasurementSet, northRect: Rectangle, southRect: Rectangle, eastRect: Rectangle, westRect: Rectangle, imageResolution: Point2D): BufferedImage = {
    val bufImg = image.toBufferedImage(imageColor)
    val graphics = ImageUtil.getGraphics(bufImg)
    graphics.setColor(annotationColor)

    annotateNorthSouth(bufImg, graphics, edges.north, northRect, imageResolution)
    annotateNorthSouth(bufImg, graphics, edges.south, southRect, imageResolution)
    annotateEastWest(bufImg, graphics, edges.east, eastRect, imageResolution)
    annotateEastWest(bufImg, graphics, edges.west, westRect, imageResolution)

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

  /** Not private to support testing. */
  def fineMeasure(attributeList: AttributeList): (MeasurementSet, BufferedImage) = {
    val image = Phase2Util.correctBadPixels(new DicomImage(attributeList))
    // TODO should divide pixels by open field image
    val halfwayPixelValue = calcHalfwayPixelValue(image)
    val pixelSpacing = attributeList.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues
    val imageResolution = new Point2D.Double(pixelSpacing(0), pixelSpacing(1))

    val coarse = coarseMeasure(image, halfwayPixelValue, imageResolution)

    val penumbraX = Config.CollimatorCenteringPenumbraThickness_mm / imageResolution.getX // penumbra thickness in pixels
    val penumbraY = Config.CollimatorCenteringPenumbraThickness_mm / imageResolution.getY // penumbra thickness in pixels

    val nsX = coarse.west + (penumbraX / 2)

    val nsRect = northSouthRectangles(coarse, penumbraX, penumbraY)

    val ewRect = eastWestRectangles(coarse, penumbraX, penumbraY)

    val northEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._1).columnSums, halfwayPixelValue * nsRect._1.height) + nsRect._1.y
    val southEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._2).columnSums, halfwayPixelValue * nsRect._2.height) + nsRect._2.y
    val eastEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._1).rowSums, halfwayPixelValue * ewRect._1.width) + ewRect._1.x
    val westEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._2).rowSums, halfwayPixelValue * ewRect._2.width) + ewRect._2.x

    val edges = new MeasurementSet(northEdge, southEdge, eastEdge, westEdge)

    val bufferedImage = makeAnnotatedImage(image, edges, nsRect._1, nsRect._2, ewRect._1, ewRect._2, imageResolution)

    (edges, bufferedImage)
  }

  /**
   * Run the CollimatorCentering sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(output: Output, collimatorCenteringRunRequirements: CollimatorCenteringRunRequirements): (ProcedureStatus.Value, Elem) = {
    val outPK = output.outputPK.get

    val edgesImage090 = fineMeasure(collimatorCenteringRunRequirements.image090.attributeList.get)
    val edgesImage270 = fineMeasure(collimatorCenteringRunRequirements.image270.attributeList.get)

    val pass: Boolean = false
    val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
    //val elem = CollimatorCenteringHTML.makeDisplay(output, positioningCheckRunRequirements, procedureStatus)
    //(procedureStatus, elem)
    ???
  }
}
