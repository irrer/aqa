/*
 * Copyright 2025 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.wl.nonCardinal

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageDisplay
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.AQALine
import org.aqa.BiCubicImage
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.wl.WLCoarseBox
import org.aqa.webrun.wl.WLMessage
import org.aqa.webrun.wl.WLPreprocessImage
import org.aqa.webrun.wl.WLRunReq

import java.awt.Color
import java.awt.geom.Point2D
import java.awt.Rectangle
import java.io.File
import javax.vecmath.Point2d

case class WLNonCardEdgeAnalysis( //
    preprocessedImage: DicomImage,
    al: AttributeList,
    wlMessage: Option[WLMessage] = None
) extends Logging {

  private val collAngle = Util.collimatorAngle(al)

  private val trans = new IsoImagePlaneTranslator(al)

  private val biCubicImage = BiCubicImage(preprocessedImage)

  /** The center of the edges as calculated by finding the center of mass.  This should be accurate to within 3 pixels. */
  private def locateCoarseCenter(): Point2d = {
    val rect = WLCoarseBox(preprocessedImage, trans, wlMsg = None).locate()
    new Point2d(rect.getCenterX, rect.getCenterY)
    new Point2d(rect.getCenterX + 5, rect.getCenterY - 8)
  }

  /**
    * Determine the maximum offset for the given line such that the edge AOI will still be within the bounds of the image.
    * @param line For this line.
    * @param direction Positive or negative 1.
    * @param width_pix With in pixel of AOI.
    * @param resolution_pix Resolution in pixels.  Determines the step size.
    * @return maximum offset in pixels.
    */
  private def maxOffset(line: AQALine, direction: Int, width_pix: Double, resolution_pix: Double): Double = {

    def isIn(pt: Point2d): Boolean = {
      val ok = {
        (pt.x >= 0) &&
        (pt.y >= 0) &&
        (pt.x < (preprocessedImage.width - 2)) &&
        (pt.y < (preprocessedImage.height - 2))
      }
      ok
    }

    def inBounds(offset: Double): Boolean = {
      val pt = line.pointOn(offset)
      val hiLine = AQALine(pt, line.perpendicularAngle)
      val hiPt = hiLine.pointOn(width_pix / 2)

      val loLine = AQALine(pt, line.perpendicularAngle)
      val loPt = loLine.pointOn(width_pix / -2)

      isIn(hiPt) && isIn(loPt)
    }

    val maxDistanceIndices = ((preprocessedImage.width + preprocessedImage.height) / resolution_pix).toInt

    val inBoundsList = (0 until maxDistanceIndices).filter(i => inBounds(i * direction * resolution_pix))

    val max = inBoundsList.last * direction * resolution_pix
    max
  }

  /**
    * Find the approximate positions of the 4 edges by projecting a band of points in each of the 4 directions
    * from the coarse center.  These are parallel and perpendicular to the coarse center.
    *
    * @return List of approximated edges.
    */
  private def approximateLocationOfEdges(coarseCenter: Point2d): WLNonCardEdgeSet = {
    // number of pixels in one mm
    val pixPerMm = trans.iso2PixDistX(1)

    // use this granularity of pixels to get initial location of edges.
    val approximateResolution = 0.5

    // Width of band to look for edges.
    val pixBandWidth = 4 * pixPerMm

    val maxLength = preprocessedImage.width + preprocessedImage.height

    val xLine = AQALine(coarseCenter, collAngle)
    val yLine = xLine.perpendicular

    val x1MaxLen = maxOffset(xLine, 1, pixBandWidth, approximateResolution)
    val x2MaxLen = maxOffset(xLine, -1, pixBandWidth, approximateResolution)
    val y1MaxLen = maxOffset(yLine, -1, pixBandWidth, approximateResolution)
    val y2MaxLen = maxOffset(yLine, 1, pixBandWidth, approximateResolution)

    val x1 = WLNonCardEdge("X1", xLine, 0, x1MaxLen, biCubicImage, pixBandWidth, approximateResolution)
    val x2 = WLNonCardEdge("X2", xLine, 0, x2MaxLen, biCubicImage, pixBandWidth, approximateResolution)
    val y1 = WLNonCardEdge("Y1", yLine, 0, y1MaxLen, biCubicImage, pixBandWidth, approximateResolution)
    val y2 = WLNonCardEdge("Y2", yLine, 0, y2MaxLen, biCubicImage, pixBandWidth, approximateResolution)

    val edgeSetApproximate: WLNonCardEdgeSet = WLNonCardEdgeSet(x1, x2, y1, y2)

    val coarseImage = WLNonCardEdgeSetImage.makeImage(edgeSetApproximate, scale = 3, al, border = 3) // TODO
    ImageDisplay.showInMSPaint(coarseImage) // TODO

    wlMessage.foreach(_.info(s"approximate center iso X: ${trans.pix2IsoCoordX(edgeSetApproximate.center.getX)}"))
    wlMessage.foreach(_.info(s"approximate center iso Y: ${trans.pix2IsoCoordY(edgeSetApproximate.center.getY)}"))

    edgeSetApproximate
  }

  case class AnnotatedEdgeAoi(line: AQALine, offsetStart: Double, offsetFinish: Double, width: Double, position: Double) {}

  /**
    * Precisely locate the four edges using a larger number of pixels and a higher sampling resolution.
    *
    * @param approximateEdgeLocations Approximate locations of edges.
    * @return
    */
  private def preciseLocationOfEdges(approximateEdgeLocations: WLNonCardEdgeSet): WLNonCardEdgeSet = {
    val xLine = AQALine(approximateEdgeLocations.center, collAngle)
    val yLine = xLine.perpendicular

    // use this granularity of pixels to get initial location of edges.
    val preciseResolution = 0.1

    val penumbra_pix = trans.iso2PixDistX(Config.PenumbraThickness_mm) / 2

    val ael = approximateEdgeLocations

    val distanceX = ael.X1.edgeCenter.distance(ael.X2.edgeCenter) + penumbra_pix
    val distanceY = ael.Y1.edgeCenter.distance(ael.Y2.edgeCenter) + penumbra_pix

    val xWidth = ael.Y1.edgeCenter.distance(ael.Y2.edgeCenter) - penumbra_pix
    val yWidth = ael.X1.edgeCenter.distance(ael.X2.edgeCenter) - penumbra_pix

    val x1 = WLNonCardEdge("X1", xLine, 0, distanceX, biCubicImage, xWidth, preciseResolution)
    val x2 = WLNonCardEdge("X2", xLine, 0, -distanceX, biCubicImage, xWidth, preciseResolution)
    val y1 = WLNonCardEdge("Y1", yLine, 0, -distanceY, biCubicImage, yWidth, preciseResolution)
    val y2 = WLNonCardEdge("Y2", yLine, 0, distanceY, biCubicImage, yWidth, preciseResolution)

    val edgeSetPrecise: WLNonCardEdgeSet = WLNonCardEdgeSet(x1, x2, y1, y2)

    wlMessage.foreach(_.info(s"precise center iso X: ${trans.pix2IsoCoordX(edgeSetPrecise.center.getX)}"))
    wlMessage.foreach(_.info(s"precise center iso Y: ${trans.pix2IsoCoordY(edgeSetPrecise.center.getY)}"))

    edgeSetPrecise
  }

  /**
    * Calculate the rectangle to enclose the region of the image that contains all the areas of interest
    * that were used for edge measurement.
    *
    * @param border_pix Number of extra pixels to serve as a border separating the AOIs from the image edge.
    * @return Bounding rectangle.
    */
  private def calcAoiBounds(border_pix: Int): Rectangle = {
    def listCoordinates(edge: WLNonCardEdge): Seq[Point2d] = {
      Seq(
        edge.loLoAoi, //
        edge.loHiAoi, //
        edge.hiLoAoi, //
        edge.hiHiAoi
      )
    }

    val coordinateList = edgeSet.edgeList.flatMap(listCoordinates)

    val minX = (coordinateList.map(_.getX).min - border_pix).round.toInt
    val maxX = (coordinateList.map(_.getX).max + border_pix).round.toInt
    val minY = (coordinateList.map(_.getY).min - border_pix).round.toInt
    val maxY = (coordinateList.map(_.getY).max + border_pix).round.toInt

    val width = maxX - minX
    val height = maxY - minY

    val boundingRectangle = new Rectangle(minX, minY, width, height)

    boundingRectangle
  }

  // main processing comprised of three steps

  // Find the coarse center using center of mass.
  private val coarseCenter: Point2d = locateCoarseCenter()

  /** Approximate position of the 4 edges.  Testing shows that this is accurate to about 0.05 pixels.  But we can do better! */
  private val approximateEdgeLocationList: WLNonCardEdgeSet = approximateLocationOfEdges(coarseCenter)

  private val preciseEdgeLocations = preciseLocationOfEdges(approximateEdgeLocationList)

  val edgeSet: WLNonCardEdgeSet = preciseEdgeLocations
}

/**
  * Quick program to show the center raw pixel values from DICOM files.
  */
object WLNonCardEdgeAnalysis {

  def main(args: Array[String]): Unit = {

    Trace.trace

    // val file = new File("""D:/tmp/wl/nonorth/1/0005.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/WLNonCardNon45_20250625_Peyton/20250625_G180C30T0.dcm""")
    val file = new File("""D:/tmp/wl/nonorth/ClinicalWinstonLutz_0.1_TB5_2025-12-12T06_34_56/RTIMAGE1.dcm""") // UM Production
    // val file = new File("""D:/tmp/wl/nonorth/1/0002.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/1/0006.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/0010.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/1/0002.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/1/0005.dcm""") // rotated 315
    // val file = new File("""D:/tmp/wl/nonorth/1/0006.dcm""") // rotated 45

    // val file = new File("""D:/tmp/wl/nonorth/1/0001.dcm""")

    // val file = new File("""D:/tmp/wl/nonorth/psm/0018.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/TB5_Aug_20/0002.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/BR1_Phase2/0014.dcm""")

    val al = new DicomFile(file).attributeList.get

    if (true) {
      val trans = new IsoImagePlaneTranslator(al)
      Trace.trace(s"5 iso == ${trans.iso2PixDistX(5.0)} pix")
      Trace.trace(s"210 pix == ${trans.pix2IsoDistX(210.0)} iso")
    }

    val colAngle = al.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head

    val dicomImage = {
      // Invert the pixels if necessary.
      WLPreprocessImage(al, None).preprocessedImage
    }

    Trace.trace()
    val runReq = WLRunReq(Seq(al), None)
    val wlMessage = WLMessage(runReq, al)
    val nonCardinal = new WLNonCardEdgeAnalysis(dicomImage, al, Some(wlMessage))
    Trace.trace()
    Trace.trace(nonCardinal)

    Trace.trace()

    val bufImg = {
      val sortedPixels = dicomImage.pixelData.flatten.sorted
      val minPixelValue = sortedPixels(10)
      val maxPixelValue = sortedPixels.dropRight(10).last
      dicomImage.toBufferedImage(ImageUtil.rgbColorMap(Color.blue), minPixelValue, maxPixelValue)
    }

    val trans = new IsoImagePlaneTranslator(al)

    val gc = ImageUtil.getGraphics(bufImg)

    gc.setColor(Color.white)

    if (true) {
      val text = "Collimator Angle: " + Util.fmtDbl(colAngle)
      ImageText.drawTextCenteredAt(gc, dicomImage.width / 2, 40, text)
    }

    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
      // Trace.trace(Util.d2i(x1) + " : " + Util.d2i(y1) + " : " + Util.d2i(x2) + " : " + Util.d2i(y2))
      gc.drawLine(Util.d2i(x1), Util.d2i(y1), Util.d2i(x2), Util.d2i(y2))
    }

    // ------------------------------------------------------------------------------------

    val j = WLNonCardEdgeSetImage.makeImage(nonCardinal.edgeSet, scale = 3, al, border = 3)
    ImageDisplay.showInMSPaint(j) // TODO rm

    Trace.trace()
    val nonCardBall = WLNonCardBall(nonCardinal.edgeSet, dicomImage, BiCubicImage(dicomImage), al)
    Trace.trace()
    nonCardBall.doIt()
    Trace.trace()

    Trace.trace("Center of four edges as pixels: " + nonCardinal.edgeSet.center)
    Trace.trace("Center of four edges as iso: " + trans.pix2IsoCoordX(nonCardinal.edgeSet.center.getX) + ", " + trans.pix2IsoCoordY(nonCardinal.edgeSet.center.getY))

    // ------------------------------------------------------------------------------------

    /**
      * Label the collimator edge
      * @param name Edge name.
      * @param point1 One end.
      * @param point2 The other end.
      */
    def labelEdge(name: String, point1: Point2D.Double, point2: Point2D.Double): Unit = {
      val centerX = (point1.getX + point2.getX) / 2
      val centerY = (point1.getY + point2.getY) / 2
      ImageText.drawTextCenteredAt(gc, centerX, centerY, name)
    }

    // ------------------------------------------------------------------------------------

    val rot = WLRotator(al)

    val X1Y1 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXLeft, rot.jawsYTop)))
    val X2Y1 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXRight, rot.jawsYTop)))

    val X1Y2 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXLeft, rot.jawsYBottom)))
    val X2Y2 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXRight, rot.jawsYBottom)))

    Trace.trace(s"jawsYTop   : ${rot.jawsYTop.round}")
    Trace.trace(s"jawsYBottom: ${rot.jawsYBottom.round}")
    Trace.trace(s"jawsXLeft  : ${rot.jawsXLeft.round}")
    Trace.trace(s"jawsXRight : ${rot.jawsXRight.round}")

    Trace.trace(s"\n    topLeft: $X1Y2\n    topRight: $X2Y2\n    bottomLeft: $X1Y1\n    bottomRight: $X2Y1")

    drawLine(X1Y2.getX, X1Y2.getY, X2Y2.getX, X2Y2.getY)
    drawLine(X1Y2.getX, X1Y2.getY, X1Y1.getX, X1Y1.getY)
    drawLine(X2Y1.getX, X2Y1.getY, X1Y1.getX, X1Y1.getY)
    drawLine(X2Y2.getX, X2Y2.getY, X2Y1.getX, X2Y1.getY)

    labelEdge("X1", X1Y2, X1Y1)
    labelEdge("X2", X2Y2, X2Y1)
    labelEdge("Y1", X1Y1, X2Y1)
    labelEdge("Y2", X1Y2, X2Y2)

    /*
    val pngFile = new File(file.getParent, file.getName.replace("dcm", "png"))
    Util.writePng(bufImg, pngFile)
    ImageDisplay.showInMSPaint(bufImg)
    Trace.trace(s"wrote $pngFile")
     */

    ImageDisplay.showInMSPaint(WLNonCardCoarseImage.makeImage(al, dicomImage))

    val txtFile = new File(file.getParent, file.getName.replace(".dcm", "_.txt"))
    Util.writeFile(txtFile, dicomImage.pixelsToText)

    Thread.sleep(2000)
    System.exit(0)
  }

}
