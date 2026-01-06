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
import org.aqa.Config
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

  /**
    * Represent a point in the image and its value. Saving the value is more efficient because the
    * value does not have to be re-calculated.
    * @param x X coordinate.
    * @param y Y coordinate.
    */
  private case class Pt(x: Double, y: Double) {
    def this(point: Point2d) = this(point.getX, point.getY)

    def point2d: Point2d = new Point2d(x, y)

    val value: Double = biCubicImage.get(x, y)
  }

  /**
    * Represent a point in the image and its value, when the value has already been calculated.
    * @param x X coordinate.
    * @param y Y coordinate.
    */
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

    // Make a list of all points with their values.  Subtract the minimum value from each so that
    // background noise around the ball will not change the center of mass calculation.
    val ptListFinal = ptListInitial.map(pt => {
      val value: Double = if (edgeSet.pointIsInBallAoi(pt.point2d)) pt.value - minValue else 0
      PtSynthetic(pt.x, pt.y, value)
    })
    ptListFinal

  }

  private val pointList: Seq[PtSynthetic] = makeArray(Config.WLNonCardBallPixelResolution)

  val xProfile: Seq[Double] = pointList.groupBy(_.x).values.toSeq.sortBy(_.head.x).map(group => group.map(_.value).sum / group.size)

  val yProfile: Seq[Double] = pointList.groupBy(_.y).values.toSeq.sortBy(_.head.y).map(group => group.map(_.value).sum / group.size)

  /**
    * Get the approximate center of mass.
    * @return
    */
  private def findCenterOfMass(): Point2d = {

    val xMinCoordinate = pointList.minBy(_.x).x
    val yMinCoordinate = pointList.minBy(_.y).y

    val x = (ImageUtil.centerOfMass(xProfile.map(_.toFloat).toIndexedSeq) * Config.WLNonCardBallPixelResolution) + xMinCoordinate
    val y = (ImageUtil.centerOfMass(yProfile.map(_.toFloat).toIndexedSeq) * Config.WLNonCardBallPixelResolution) + yMinCoordinate

    new Point2d(x, y)
  }

  val center_pix: Point2d = findCenterOfMass()

  private case class MinMax(min: Float, max: Float) {}

  /**
    * Find the largest value in the ball area.  This is used for setting the brightness of rendered images, so high
    * precision is not required.
    *
    * @return The approximate maximum. value
    */
  private def calculateMinAndMaxMaxPixelValue(): MinMax = {
    val valueList = for (x <- 0 until preprocessedImage.width; y <- 0 until preprocessedImage.height; if edgeSet.pointIsInBallAoi(new Point2d(x, y))) yield preprocessedImage.get(x, y)
    val sorted = valueList.sorted
    val min = sorted.slice(5, 15).sum / 10
    val max = sorted.dropRight(5).takeRight(10).sum / 10
    MinMax(min, max)
  }

  private val minMaxPixelValues = calculateMinAndMaxMaxPixelValue()

  /** the approximate min pixel value of the ball. Useful for rendering images. */
  val approximateMinPixelValueOfBall: Float = minMaxPixelValues.min

  /** the approximate max pixel value of the ball. Useful for rendering images. */
  val approximateMaxPixelValueOfBall: Float = minMaxPixelValues.max

  // ------------------------------------------------------------------------------------------------------------------------------------------------------

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
