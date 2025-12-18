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
import edu.umro.ImageUtil.ImageDisplay
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.BiCubicImage
import org.aqa.Logging

import java.awt.Color
import javax.vecmath.Point2d

/**
  * Measure the position of the ball and also verify that it is a valid ball.
  *
  * @param edgeSet Surrounding edges.
  * @param preprocessedImage Image containing ball.
  * @param biCubicImage Interpolated version of preprocessedImage.
  * @param al Original DICOM.
  */
case class WLNonCardBall(edgeSet: WLNonCardEdgeSet, preprocessedImage: DicomImage, biCubicImage: BiCubicImage, al: AttributeList) extends Logging {

  // private val trans = new IsoImagePlaneTranslator(al)

  /**
    * Determine whether a point is within the AOI of the ball.  The AOI is defined by those points that
    * are between both X edges and Y edges.
    * @param point Check this point
    * @return True if point is in AOI.
    */
  private def pointIsInBallAoi(point: Point2d): Boolean = {
    edgeSet.X1.loLine.pointIsBetween(point, edgeSet.X2.loLine) &&
    edgeSet.Y1.loLine.pointIsBetween(point, edgeSet.Y2.loLine)
  }

  private def calculateRadiusToNearestEdge(point: Point2d): Double = {

    val radius_pix = Seq( //
      edgeSet.X1.loLine.distanceToPoint(point),
      edgeSet.X2.loLine.distanceToPoint(point),
      edgeSet.Y1.loLine.distanceToPoint(point),
      edgeSet.Y2.loLine.distanceToPoint(point)
    ).min

    radius_pix
  }

  private case class Pt(x: Double, y: Double) {
    def this(point: Point2d) = this(point.getX, point.getY)

    def point2d: Point2d = new Point2d(x, y)

    val value: Double = biCubicImage.get(x, y)
  }

  private case class PtSynthetic(x: Double, y: Double, value: Double) {}

  private def makeArray(resolution_pix: Double): Seq[PtSynthetic] = {
    val pX1Y1 = edgeSet.X1.loLine.intersection(edgeSet.Y1.loLine)
    val pX1Y2 = edgeSet.X1.loLine.intersection(edgeSet.Y2.loLine)
    val pX2Y1 = edgeSet.X2.loLine.intersection(edgeSet.Y1.loLine)
    val pX2Y2 = edgeSet.X2.loLine.intersection(edgeSet.Y2.loLine)

    val extremes = Seq( //
      pX1Y1,
      pX1Y2,
      pX2Y1,
      pX2Y2
    )

    val xMin = extremes.map(_.x).min
    val xMax = extremes.map(_.x).max
    val yMin = extremes.map(_.y).min
    val yMax = extremes.map(_.y).max

    val xDist = xMax - xMin // distance between X lines
    val yDist = yMax - yMin // distance between Y lines

    val xCount = (xDist / resolution_pix).round.toInt
    val yCount = (yDist / resolution_pix).round.toInt

    val ptListInitial = for (x <- 0 until xCount; y <- 0 until yCount) yield { Pt((x * resolution_pix) + xMin, (y * resolution_pix) + yMin) }

    val minValue = {
      val sorted = ptListInitial.map(_.value).sorted
      sorted.slice(5, 15).sum / 10
    }

    val ptListFinal = ptListInitial.map(pt => {
      val value = if (pointIsInBallAoi(pt.point2d)) pt.value else minValue

      PtSynthetic(pt.x, pt.y, value)
    })
    ptListFinal

  }

  /**
    * Get the approximate center of mass.
    * @return
    */
  private def findCenterOfMass(): Point2d = {
    val resolution_pix = 0.2

    val pointList = makeArray(resolution_pix)

    val xProfile = pointList.groupBy(_.x).values.toSeq.sortBy(_.head.x).map(group => group.map(_.value).sum / group.size)

    val yProfile = pointList.groupBy(_.y).values.toSeq.sortBy(_.head.y).map(group => group.map(_.value).sum / group.size)

    val xMinCoordinate = pointList.minBy(_.x).x
    val yMinCoordinate = pointList.minBy(_.y).y

    val x = (ImageUtil.centerOfMass(xProfile.map(_.toFloat).toIndexedSeq) * resolution_pix) + xMinCoordinate
    val y = (ImageUtil.centerOfMass(yProfile.map(_.toFloat).toIndexedSeq) * resolution_pix) + yMinCoordinate

    new Point2d(x, y)
  }

  def doIt(): Point2d = {

    Trace.trace()
    val start = System.currentTimeMillis()
    val center = findCenterOfMass()
    val elapsed = System.currentTimeMillis() - start
    Trace.trace(s"Elapsed ms for findCenterOfMass: $elapsed")
    Trace.trace(s"center: $center")
    if (false) { // TODO rm Show the ball area used for center of mass, as well as the center.
      val img = preprocessedImage.toDeepColorBufferedImage(0.01)
      img.setRGB(center.x.toInt, center.y.toInt, Color.white.getRGB)
      ImageDisplay.showInMSPaint(img)
    }

    val trans = new IsoImagePlaneTranslator(al)
    val distance_pix = center.distance(edgeSet.center_pix)
    Trace.trace(s"ball center: $center      edge center: ${edgeSet.center_pix}     distance in pix: $distance_pix distance in mm: ${trans.pix2IsoDistX(distance_pix)}")

    Trace.trace(s"right  Y2   in mm: ${trans.pix2IsoCoordY(edgeSet.Y2.line.centerY)}")
    Trace.trace(s"left   Y1   in mm: ${trans.pix2IsoCoordY(edgeSet.Y1.line.centerY)}")
    Trace.trace(s"top    X1   in mm: ${trans.pix2IsoCoordX(edgeSet.X1.line.centerX)}")
    Trace.trace(s"bottom X2   in mm: ${trans.pix2IsoCoordX(edgeSet.X2.line.centerX)}")

    Trace.trace(s"edge top    in mm+ ${trans.pix2IsoCoordX(edgeSet.Y2.line.centerX)}")
    Trace.trace(s"edge bottom in mm+ ${trans.pix2IsoCoordX(edgeSet.Y1.line.centerX)}")
    Trace.trace(s"edge left   in mm+ ${trans.pix2IsoCoordY(edgeSet.X1.line.centerY)}")
    Trace.trace(s"edge right  in mm+ ${trans.pix2IsoCoordY(edgeSet.X2.line.centerY)}")

    Trace.trace(s"edge center in mm: ${trans.pix2IsoCoordX(edgeSet.center_pix.x)}  ${trans.pix2IsoCoordY(edgeSet.center_pix.y)}")
    Trace.trace(s"ball center in mm: ${trans.pix2IsoCoordX(center.x)}  ${trans.pix2IsoCoordY(center.y)}")

    // TODO this edge calculation is correct
    val cX = (edgeSet.Y2.edgeCenter.x + edgeSet.Y1.edgeCenter.x) / 2
    val cY = (edgeSet.X2.edgeCenter.y + edgeSet.X1.edgeCenter.y) / 2
    val cPoint = new Point2d(cX, cY)
    Trace.trace(s"real edge center in pix: $cX  $cY")
    Trace.trace(s"real edge center in mm : ${trans.pix2IsoCoordX(cX)}  ${trans.pix2IsoCoordY(cY)}")
    val dist_pix = cPoint.distance(center)
    Trace.trace(s"dist_pix: $dist_pix")
    val dist_mm = trans.pix2IsoDistX(dist_pix)
    Trace.trace(s"dist_mm: $dist_mm")

    if (false) { // TODO rm
      val img = preprocessedImage.toDeepColorBufferedImage(0.01)
      img.setRGB(center.x.toInt, center.y.toInt, Color.black.getRGB)

      val gc = ImageUtil.getGraphics(img)

      gc.setColor(Color.white)
      gc.drawLine(0, edgeSet.X2.edgeLo.y.toInt, preprocessedImage.width - 1, edgeSet.X2.edgeLo.y.toInt)

      // gc.setColor(Color.black)
      // gc.drawLine(0, edgeSet.X1.line.centerY.toInt, preprocessedImage.width - 1, edgeSet.X1.line.centerY.toInt)

      // gc.setColor(Color.orange)
      // gc.drawLine(0, edgeSet.X1.line.centerY.toInt, preprocessedImage.width - 1, edgeSet.X1.line.centerY.toInt)

      ImageDisplay.showInMSPaint(img)
    }

    Thread.sleep(2000)
    center
  }
}
