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
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.ScaledImage
import org.aqa.AQALine
import org.aqa.BiCubicImage
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.wl.WLCoarseBox
import org.aqa.webrun.wl.WLMessage
import org.aqa.webrun.wl.WLRunReq

import java.awt.Color
import java.awt.image.BufferedImage
import javax.vecmath.Point2d

case class WLNonCardEdgeAnalysis( //
    preprocessedImage: DicomImage,
    al: AttributeList,
    biCubicImage: BiCubicImage,
    wlRunReq: WLRunReq,
    wlMessage: Option[WLMessage],
    trans: IsoImagePlaneTranslator,
    beamCenter_mm: Point2d
) extends Logging {

  def fmt(d: Double): String = "%12.6f".format(d)

  wlMessage.foreach(_.info(s"beam center mm: ${fmt(beamCenter_mm.x)}  ${fmt(beamCenter_mm.y)}"))

  private val collAngle = Util.collimatorAngle(al)

  wlMessage.foreach(_.info(s"beamCenter pixels: ${trans.iso2Pix(beamCenter_mm)}"))
  wlMessage.foreach(_.info(s"image center pixels: ${trans.iso2Pix(0, 0)}"))

  val coarseBox: WLCoarseBox = WLCoarseBox(preprocessedImage, trans, wlMessage)

  private val coarseRectangle = coarseBox.rectangle

  /** The center of the edges as calculated by finding the center of mass.  This should be accurate to within 3 pixels. */
  val locateCoarseCenter: Point2d = new Point2d(coarseRectangle.getCenterX, coarseRectangle.getCenterY)

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
    * Make an image that represents the coarse finding of the edges.
    * @return Image showing coarse rectangle.
    */
  def coarseImage(): BufferedImage = {
    val img1 = preprocessedImage.toBufferedImage(Color.blue)

    val border = 10

    val cr = coarseRectangle

    val coarseScale = 1

    // make AOI bigger.
    val bufImg = ImageUtil.magnify(img1, coarseScale)

    val si = ScaledImage(coarseScale, 0, 0)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.yellow)
    ImageUtil.setLineThickness(gc, 2)

    val top = cr.y
    val bottom = cr.y + cr.height
    val left = cr.x
    val right = cr.x + cr.width

    si.drawLine(gc, left, top, right, top)
    si.drawLine(gc, left, top, left, bottom)
    si.drawLine(gc, right, top, right, bottom)
    si.drawLine(gc, right, bottom, left, bottom)

    bufImg
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

    val xLine = AQALine(coarseCenter, collAngle)
    val yLine = xLine.perpendicular

    val x1MaxLen = maxOffset(xLine, 1, pixBandWidth, approximateResolution)
    val x2MaxLen = maxOffset(xLine, -1, pixBandWidth, approximateResolution)
    val y1MaxLen = maxOffset(yLine, -1, pixBandWidth, approximateResolution)
    val y2MaxLen = maxOffset(yLine, 1, pixBandWidth, approximateResolution)

    val x1 = WLNonCardEdge("X1", xLine, 0, x1MaxLen, biCubicImage, al, pixBandWidth, approximateResolution)
    val x2 = WLNonCardEdge("X2", xLine, 0, x2MaxLen, biCubicImage, al, pixBandWidth, approximateResolution)
    val y1 = WLNonCardEdge("Y1", yLine, 0, y1MaxLen, biCubicImage, al, pixBandWidth, approximateResolution)
    val y2 = WLNonCardEdge("Y2", yLine, 0, y2MaxLen, biCubicImage, al, pixBandWidth, approximateResolution)

    val edgeSetApproximate: WLNonCardEdgeSet = WLNonCardEdgeSet(x1, x2, y1, y2)

    /*
    ImageDisplay.showInMSPaint(coarseImage)
    wlMessage.foreach(_.info(s"approximate center iso X: ${trans.pix2IsoCoordX(edgeSetApproximate.center_pix.getX)}"))
    wlMessage.foreach(_.info(s"approximate center iso Y: ${trans.pix2IsoCoordY(edgeSetApproximate.center_pix.getY)}"))
     */

    edgeSetApproximate
  }

  case class AnnotatedEdgeAoi(line: AQALine, offsetStart: Double, offsetFinish: Double, width: Double, position: Double) {}

  /**
    * Log statistics for this edge.
    * @param edge Show this one.
    * @param center_pix Final precise center in pixels.
    */
  private def showEdge(edge: WLNonCardEdge, center_pix: Point2d): Unit = {

    val point_mm: Point2d = {
      val line = AQALine(centerPoint = center_pix, angle_deg = edge.line.angle_deg)
      val point = line.intersection(edge.edgeLine)
      trans.pix2Iso(new Point2d(point.x, point.y))
    }

    val x = fmt(point_mm.getX)
    val y = fmt(point_mm.getY)

    val d = fmt(point_mm.distance(beamCenter_mm))
    wlMessage.foreach(_.info(s"Edge ${edge.name} edge center (mm): $x, $y    distance to center (mm): $d"))

    wlMessage.foreach(_.info(s"edgeCenter pixels: ${trans.iso2Pix(point_mm)}"))
  }

  /**
    * Precisely locate the four edges using a larger number of pixels and a higher sampling resolution.
    *
    * @param approximateEdgeLocations Approximate locations of edges.
    * @return
    */
  private def preciseLocationOfEdges(approximateEdgeLocations: WLNonCardEdgeSet): WLNonCardEdgeSet = {
    val xLine = AQALine(approximateEdgeLocations.center_pix, collAngle)
    val yLine = xLine.perpendicular

    // use this granularity of pixels to get initial location of edges.
    val preciseResolution = Config.WLNonCardEdgePixelResolution

    val penumbra_pix = trans.iso2PixDistX(Config.PenumbraThickness_mm) / 2

    val ael = approximateEdgeLocations

    val distanceX = ael.X1.edgeCenter.distance(ael.X2.edgeCenter) + penumbra_pix
    val distanceY = ael.Y1.edgeCenter.distance(ael.Y2.edgeCenter) + penumbra_pix

    val xWidth = ael.Y1.edgeCenter.distance(ael.Y2.edgeCenter) - penumbra_pix
    val yWidth = ael.X1.edgeCenter.distance(ael.X2.edgeCenter) - penumbra_pix

    val x1 = WLNonCardEdge("X1", xLine, 0, distanceX, biCubicImage, al, xWidth, preciseResolution)
    val x2 = WLNonCardEdge("X2", xLine, 0, -distanceX, biCubicImage, al, xWidth, preciseResolution)
    val y1 = WLNonCardEdge("Y1", yLine, 0, -distanceY, biCubicImage, al, yWidth, preciseResolution)
    val y2 = WLNonCardEdge("Y2", yLine, 0, distanceY, biCubicImage, al, yWidth, preciseResolution)

    val edgeSetPrecise: WLNonCardEdgeSet = WLNonCardEdgeSet(x1, x2, y1, y2)

    val center_pix = edgeSetPrecise.center_pix

    wlMessage.foreach(_.info(s"precise center of edges mm X: ${fmt(trans.pix2IsoCoordX(edgeSetPrecise.center_pix.getX))}"))
    wlMessage.foreach(_.info(s"precise center of edges mm Y: ${fmt(trans.pix2IsoCoordY(edgeSetPrecise.center_pix.getY))}"))

    wlMessage.foreach(_.info(s"precise center of edges pix X: ${fmt(edgeSetPrecise.center_pix.getX)}"))
    wlMessage.foreach(_.info(s"precise center of edges pix Y: ${fmt(edgeSetPrecise.center_pix.getY)}"))

    showEdge(x1, center_pix)
    showEdge(x2, center_pix)
    showEdge(y1, center_pix)
    showEdge(y2, center_pix)

    edgeSetPrecise
  }

  // main processing comprised of three steps

  // Find the coarse center using center of mass.
  private val coarseCenter: Point2d = locateCoarseCenter

  /** Approximate position of the 4 edges.  Testing shows that this is accurate to about 0.05 pixels.  But we can do better! */
  val approximateEdgeSet: WLNonCardEdgeSet = approximateLocationOfEdges(coarseCenter)

  private val preciseEdgeLocations = preciseLocationOfEdges(approximateEdgeSet)

  val edgeSet: WLNonCardEdgeSet = preciseEdgeLocations

}
