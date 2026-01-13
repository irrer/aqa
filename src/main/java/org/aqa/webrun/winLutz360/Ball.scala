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

package org.aqa.webrun.winLutz360

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.BiCubicImage
import org.aqa.Config
import org.aqa.Logging
import org.aqa.webrun.wl.WLMessage

import javax.vecmath.Point2d

/**
  * Measure the position of the ball and also verify that it is a valid ball.
  *
  * @param edgeSet Surrounding edges.
  * @param preprocessedImage Image containing ball.
  * @param biCubicImage Interpolated version of preprocessedImage.
  * @param trans For translating to and from mm / pixel.
  * @param al Original DICOM.
  * @param wlMessage Log messages here
  * @param beamCenter_mm planned center of beam in isoplane
  */
case class Ball( //
                 edgeSet: EdgeSet,
                 preprocessedImage: DicomImage,
                 biCubicImage: BiCubicImage,
                 trans: IsoImagePlaneTranslator,
                 al: AttributeList,
                 wlMessage: Option[WLMessage],
                 beamCenter_mm: Point2d
) extends Logging {

  private def fmt(d: Double): String = "%10.6f".format(d)

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

  private val pointList: Seq[PtSynthetic] = makeArray(Config.WinLutz360BallPixelResolution)

  /** Horizontal profile of ball in pixel values. */
  val xProfile: Seq[Double] = pointList.groupBy(_.x).values.toSeq.sortBy(_.head.x).map(group => group.map(_.value).sum / group.size).toIndexedSeq

  /** Vertical profile of ball in pixel values. */
  val yProfile: Seq[Double] = pointList.groupBy(_.y).values.toSeq.sortBy(_.head.y).map(group => group.map(_.value).sum / group.size).toIndexedSeq

  /**
    * Get the approximate center of mass.
    * @return
    */
  private def findCenterOfMass(): Point2d = {

    val xMinCoordinate = pointList.minBy(_.x).x
    val yMinCoordinate = pointList.minBy(_.y).y

    val x = (ImageUtil.centerOfMass(xProfile.map(_.toFloat).toIndexedSeq) * Config.WinLutz360BallPixelResolution) + xMinCoordinate
    val y = (ImageUtil.centerOfMass(yProfile.map(_.toFloat).toIndexedSeq) * Config.WinLutz360BallPixelResolution) + yMinCoordinate

    new Point2d(x, y)
  }

  /** Center of ball in pixel coordinates. */
  val center_pix: Point2d = findCenterOfMass()
  val center_iso: Point2d = trans.pix2Iso(center_pix)

  wlMessage.foreach(_.info(s"Center of ball: ${fmt(center_iso.x)}, ${fmt(center_iso.y)}  Distance to center: ${fmt(center_iso.distance(beamCenter_mm))}"))

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
  //noinspection ScalaWeakerAccess
  val approximateMinPixelValueOfBall: Float = minMaxPixelValues.min

  /** the approximate max pixel value of the ball. Useful for rendering images. */
  //noinspection ScalaWeakerAccess
  val approximateMaxPixelValueOfBall: Float = minMaxPixelValues.max

  wlMessage.foreach(
    _.info(
      s"Ball approximate min pixel value: ${fmt(approximateMinPixelValueOfBall)}  approximate max pixel value: ${fmt(approximateMaxPixelValueOfBall)}    range: ${fmt(approximateMaxPixelValueOfBall - approximateMinPixelValueOfBall)}"
    )
  )

}
