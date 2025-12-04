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

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageDisplay
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging

import java.awt.Color
import java.awt.Rectangle
import javax.vecmath.Point2d
import javax.vecmath.Point2i

case class WLNonCardBall(edgeSet: WLNonCardEdgeSet, preprocessedImage: DicomImage) extends Logging {

  private def rnd(d: Double): Int = d.round.toInt

  private def pointIsInBallAoi(point: Point2i): Boolean = {
    edgeSet.X1.loLine.pointIsBetween(point, edgeSet.X2.loLine) &&
    edgeSet.Y1.loLine.pointIsBetween(point, edgeSet.Y2.loLine)
  }

  /**
    * Construct the Ball AOI by determining which points are within the edge lines that define the minimum edge values.
    *
    * @return Ball AOI.
    */
  private def makeBallBounds(): Rectangle = {

    val pointList =
      for ( //
        x <- 0 until preprocessedImage.width; //
        y <- 0 until preprocessedImage.height //
        if pointIsInBallAoi(new Point2i(x, y))
      ) yield new Point2i(x, y)

    val x = pointList.map(_.getX).min
    val y = pointList.map(_.getY).min
    val width = pointList.map(_.getX).max - x
    val height = pointList.map(_.getY).max - y

    new Rectangle(x, y, width, height)
  }

  /**
    * Make an image that only contains the ball, with pixels outside the lo-line areas 'blacked' out.
    * @param bounds Bounds of ball AOI.
    * @return
    */
  private def makeBallAOI(bounds: Rectangle): DicomImage = {

    val minPixelValue = {
      val badPixelCount = 5 // discard this many pixels as potentially bad
      val samplePixelCount = 10 // use this many pixels to get an average of 'dark' pixels.
      val list = preprocessedImage.pixelData.flatten.sorted

      // find the average of the dark pixels.
      list.slice(badPixelCount, badPixelCount + samplePixelCount).sum / samplePixelCount
    }

    def doPoint(point: Point2i): Float = {
      if (pointIsInBallAoi(point))
        preprocessedImage.get(point.getX, point.getY)
      else
        minPixelValue
    }

    def doRow(y: Int): IndexedSeq[Float] =
      (0 until preprocessedImage.width).map(x => doPoint(new Point2i(x, y)))

    val array = (0 until preprocessedImage.height).map(doRow)
    new DicomImage(array).getSubimage(bounds)
  }

  private val looseBounds = makeBallBounds()
  private val ballAOI = makeBallAOI(looseBounds)

  if (true) {

    val bufImg = preprocessedImage.toDeepColorBufferedImage(0.01)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)

    gc.drawRect(looseBounds.x, looseBounds.y, looseBounds.width, looseBounds.height)
    ImageDisplay.showImageAndWait(bufImg)
    Trace.trace()
  }

  /** X Profile of the ball including surrounding area. */
  //noinspection ScalaWeakerAccess
  val xLooseAOIProfile: IndexedSeq[Float] = ballAOI.columnSums.toIndexedSeq.map(_ / ballAOI.height)

  /** Y Profile of the ball including surrounding area. */
  //noinspection ScalaWeakerAccess
  val yLooseAOIProfile: IndexedSeq[Float] = ballAOI.rowSums.toIndexedSeq.map(_ / ballAOI.width)

  /**
    * Use center of mass on the entire area to find the center of the ball.
    * @return Center, in absolute pixel coordinates.
    */
  private def findCoarseBallCenter(): Point2d = {
    val x = ImageUtil.centerOfMass(xLooseAOIProfile)
    val y = ImageUtil.centerOfMass(yLooseAOIProfile)
    val center = new Point2d(x + looseBounds.x, y + looseBounds.y)
    center
  }

  private case class LooseLimits(lo: Int, hi: Int) {
    val range: Int = hi - lo
  }

  /**
    * Return the first and last indices of the given profile that have points above a low
    * threshold.  These roughly determine the position of the ball.
    * @param looseProfile For this profile.
    * @return low and high indices.
    */
  private def findFindCutoffPoints(looseProfile: IndexedSeq[Float]): LooseLimits = {
    val lowPercent = 10.0

    val valueRange = looseProfile.max - looseProfile.min

    val threshold = looseProfile.min + ((valueRange * lowPercent) / 100)

    val highList = looseProfile.indices.filter(i => looseProfile(i) > threshold)

    val lo = highList.head
    val hi = highList.last

    val range: Int = hi - lo

    val borderPercent = 50.0
    val borderCount = ((range * borderPercent) / 100).round.toInt

    val loLimit = Math.max(0, lo - borderCount)
    val hiLimit = Math.min(looseProfile.size - 1, hi + borderCount)
    LooseLimits(loLimit, hiLimit)
  }

  /**
    * Define a rectangle in absolute coordinates that define an area of interest
    * that fits tightly around the ball.
    *
    * @return Tight ball AOI.
    */
  private def makeTightBounds(): Rectangle = {

    val xLimits = findFindCutoffPoints(xLooseAOIProfile)
    val yLimits = findFindCutoffPoints(yLooseAOIProfile)

    val tightBounds = new Rectangle(looseBounds.x + xLimits.lo, looseBounds.y + yLimits.lo, xLimits.range, yLimits.range)

    tightBounds
  }

  //noinspection ScalaWeakerAccess
  val tightBounds: Rectangle = makeTightBounds()

  //noinspection ScalaWeakerAccess
  val tightAoi: DicomImage = preprocessedImage.getSubimage(tightBounds)

  //noinspection ScalaWeakerAccess
  val xTightProfile: IndexedSeq[Float] = tightAoi.columnSums.map(_ / tightAoi.height)
  //noinspection ScalaWeakerAccess
  val yTightProfile: IndexedSeq[Float] = tightAoi.rowSums.map(_ / tightAoi.width)

  private def findBallCenterFirst(): Point2d = {

    val minValue: Float = {
      val sortedPixelList = tightAoi.pixelData.flatten.sorted
      // drop bad pixels and find the mean of the next 10 low pixels.  Use this as 'zero'.
      sortedPixelList.slice(5, 15).sum / 10
    }

    val xNormalizedTightProfile = xTightProfile.map(_ - minValue)
    val yNormalizedTightProfile = yTightProfile.map(_ - minValue)

    val x = ImageUtil.centerOfMass(xNormalizedTightProfile)
    val y = ImageUtil.centerOfMass(yNormalizedTightProfile)

    if (true) {
      val bufImg = tightAoi.toBufferedImage(Color.blue)
      ImageDisplay.showInMSPaint(bufImg)
    }

    val xAbsolute_pix = x + tightBounds.x
    val yAbsolute_pix = y + tightBounds.y

    new Point2d(xAbsolute_pix, yAbsolute_pix)

  }

  /**
    * Locate the ball again, this time using an AOI that is centered on a point very close to the ball's center.
    * @param ballCenterFirstTry First approximation of finding ball.
    * @return Centered ball.
    */
  private def findBallCenterSecond(ballCenterFirstTry: Point2d): Point2d = {

    val first = new Point2d(ballCenterFirstTry.x - looseBounds.x, ballCenterFirstTry.y - looseBounds.y)

    val t = first.x
    val b = looseBounds.width - first.x
    val l = first.y
    val r = looseBounds.height - first.y

    val minDistance = Seq(t, b, l, r).min

    val centeredBoundary = new Rectangle( //
      rnd(ballCenterFirstTry.x - minDistance), //
      rnd(ballCenterFirstTry.y - minDistance),
      rnd(minDistance * 2),
      rnd(minDistance * 2)
    )

    val centeredAOI = preprocessedImage.getSubimage(centeredBoundary)

    val xProfile = centeredAOI.columnSums
    val yProfile = centeredAOI.rowSums

    val relativeX = ImageUtil.centerOfMass(xProfile)
    val relativeY = ImageUtil.centerOfMass(yProfile)

    val xAbsolute_pix = relativeX + centeredBoundary.x
    val yAbsolute_pix = relativeY + centeredBoundary.y

    new Point2d(xAbsolute_pix, yAbsolute_pix)
  }

  Trace.trace("xLooseAOIProfile:\n" + xLooseAOIProfile.mkString("\n") + "\n\n\n\n")

  val ballCenterFirstTry: Point2d = findBallCenterFirst()
  Trace.trace(s"ballCenterFirstTry: $ballCenterFirstTry")

  val ballCenterSecondTry: Point2d = findBallCenterSecond(ballCenterFirstTry)
  Trace.trace(s"ballCenterSecondTry: $ballCenterSecondTry")

  Trace.trace(s" ballCenterFirstTry distance to ballCenterSecondTry: ${ballCenterSecondTry.distance(ballCenterFirstTry)}")

}
