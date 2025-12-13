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
import java.awt.Rectangle
import javax.vecmath.Point2d
import javax.vecmath.Point2i

/**
  * Measure the position of the ball and also verify that it is a valid ball.
  *
  * @param edgeSet Surrounding edges.
  * @param preprocessedImage Image containing ball.
  * @param biCubicImage Interpolated version of preprocessedImage.
  * @param al Original DICOM.
  */
case class WLNonCardBall(edgeSet: WLNonCardEdgeSet, preprocessedImage: DicomImage, biCubicImage: BiCubicImage, al: AttributeList) extends Logging {

  private val trans = new IsoImagePlaneTranslator(al)

  private def pointIsInBallAoi(point: Point2d): Boolean = {
    edgeSet.X1.loLine.pointIsBetween(point, edgeSet.X2.loLine) &&
    edgeSet.Y1.loLine.pointIsBetween(point, edgeSet.Y2.loLine)
  }

  private val pointsInBounds: Seq[Point2i] = {
    for ( //
      x <- 0 until preprocessedImage.width; //
      y <- 0 until preprocessedImage.height //
      if pointIsInBallAoi(new Point2d(x, y))
    ) yield new Point2i(x, y)
  }

  /**
    * Construct the Ball AOI by determining which points are within the edge lines that define thei
    * minimum edge values that were determined during edge measurement.
    *
    * @return Ball AOI.
    */
  private def makeBallBounds(): Rectangle = {

    val x = pointsInBounds.map(_.getX).min
    val y = pointsInBounds.map(_.getY).min
    val width = pointsInBounds.map(_.getX).max - x
    val height = pointsInBounds.map(_.getY).max - y

    new Rectangle(x, y, width, height)
  }

  def calculateRadiusToNearestEdge(point: Point2d): Double = {

    val radius_pix = Seq( //
      edgeSet.X1.loLine.distanceToPoint(point),
      edgeSet.X2.loLine.distanceToPoint(point),
      edgeSet.Y1.loLine.distanceToPoint(point),
      edgeSet.Y2.loLine.distanceToPoint(point)
    ).min

    radius_pix
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

    def doPoint(point: Point2d): Float = {
      if (pointIsInBallAoi(point))
        preprocessedImage.get(point.getX.toInt, point.getY.toInt)
      else
        minPixelValue
    }

    def doRow(y: Int): IndexedSeq[Float] =
      (0 until preprocessedImage.width).map(x => doPoint(new Point2d(x, y)))

    val array = (0 until preprocessedImage.height).map(doRow)
    new DicomImage(array).getSubimage(bounds)
  }

  private val ballAOIBounds = makeBallBounds()
  private val ballAOI = makeBallAOI(ballAOIBounds)

  if (false) { // TODO rm

    val bufImg = preprocessedImage.toDeepColorBufferedImage(0.01)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)

    gc.drawRect(ballAOIBounds.x, ballAOIBounds.y, ballAOIBounds.width, ballAOIBounds.height)
    ImageDisplay.showInMSPaint(bufImg)
    // Thread.sleep(2 * 1000) // let MS Paint start
    Trace.trace()
  }

  private case class Pt(x: Double, y: Double) {
    def this(point: Point2d) = this(point.getX, point.getY)

    def point2d: Point2d = new Point2d(x, y)

    val value: Double = biCubicImage.get(x, y)
  }

  private case class PtSynthetic(x: Double, y: Double, value: Double) {
    def point2d: Point2d = new Point2d(x, y)
  }

  /**
    * Estimate the radius of the ball.  This assumes that it has the usual approximate profile.
    *
    * This is done by finding those pixels that are outside a hard-coded number of standard
    * deviation.  This number is currently 1.0.
    *
    * If the value returned is 0 or near 0, then it does indicate that there is no ball.
    *
    * Caveat: This function is NOT a validation check, and always return a value.  That value
    * can only be trusted if the ball has been validated by other means. For example, if the
    * image is of a patient scan, then this will still return a value.
    *
    * @return Radius of the ball in pixels.  Value will generally not be a round (integer) number.
    */
  private def estimateBallRadius_pix(): Double = {
    val subImage = preprocessedImage.getSubimage(ballAOIBounds)

    // pixels that exceed the mean p
    val stdDevFactor = 1.75

    // Pixel values sorted. Drop a few from each end to discard bad pixels.
    val pixList = subImage.pixelData.flatten.sorted.drop(5).dropRight(5)

    val stdDev = ImageUtil.stdDev(pixList)
    val meanOf = pixList.sum / pixList.size

    val ballPixValueThreshold = meanOf + (stdDev * stdDevFactor)

    val ballPixList = pixList.filter(_ > ballPixValueThreshold)

    val ballArea = ballPixList.size

    // val ballRadius = Math.sqrt(ballArea / Math.PI)

    val ballRadius = trans.iso2PixDistX(5.0)
    ballRadius
  }

  private val estimatedBallRadius_pix = estimateBallRadius_pix()

  private def makePointList(radius_pix: Double, pointSpacing: Double): Seq[Point2d] = {

    def addCircle(radius: Double, pointSpacing: Double): Seq[Point2d] = {

      val diameter = Math.PI * 2 * radius

      val count = (diameter / pointSpacing).ceil.round.toInt
      val radianIncrement = (Math.PI * 2) / count

      def makePoint(i: Int): Point2d = {
        val radian = radianIncrement * i

        val x = Math.sin(radian) * radius
        val y = Math.cos(radian) * radius

        new Point2d(x, y)
      }

      (0 until count).map(makePoint)
    }
    val numCircle = (radius_pix / pointSpacing).ceil.round.toInt

    val pointList = (0 until numCircle).flatMap(c => addCircle(c * pointSpacing, pointSpacing: Double))

    logger.info(s"Point spacing: $pointSpacing     Number of points used to find ball ${pointList.size}")

    pointList
  }

  private def makePointArray1(center: Point2d, resolution_pix: Double): Seq[Point2d] = {
    val upperLimit = calculateRadiusToNearestEdge(edgeSet.center) // / 2
    val thisLimit = calculateRadiusToNearestEdge(center)

    val radius_pix = Math.min(upperLimit, thisLimit)

    val count = ((radius_pix * 2) / resolution_pix).round.toInt

    val xStart = center.getX - radius_pix
    val yStart = center.getY - radius_pix

    def indexToPoint(x: Int, y: Int): Point2d = {
      val xx = xStart + (x * resolution_pix)
      val yy = yStart + (y * resolution_pix)
      new Point2d(xx, yy)
    }

    def isWithinRadius(x: Int, y: Int): Boolean = indexToPoint(x, y).distance(center) <= radius_pix

    Trace.trace(s"center: $center   radius_pix: $radius_pix")

    val list = for (x <- 0 until count; y <- 0 until count; if isWithinRadius(x, y)) yield indexToPoint(x, y)

    if (true) { // TODO rm Show all points being used.
      val img = preprocessedImage.toDeepColorBufferedImage(0.01)

      val distinctList = list.map(p => new Point2i(p.getX.toInt, p.getY.toInt)).distinct
      distinctList.foreach(p => {
        img.setRGB(p.x, p.y, Color.white.getRGB)
      })

      ImageDisplay.showInMSPaint(img)
      // Thread.sleep(2000)
    }

    list
  }

  private def makePointArray(center: Point2d, resolution_pix: Double): Seq[Point2d] = {
    val upperLimit = calculateRadiusToNearestEdge(edgeSet.center) // / 2
    val thisLimit = calculateRadiusToNearestEdge(center)

    val radius_pix = Math.min(upperLimit, thisLimit)

    val count = ((radius_pix * 2) / resolution_pix).round.toInt

    val xStart = center.getX - radius_pix
    val yStart = center.getY - radius_pix

    def indexToPoint(x: Int, y: Int): Point2d = {
      val xx = xStart + (x * resolution_pix)
      val yy = yStart + (y * resolution_pix)
      new Point2d(xx, yy)
    }

    def isWithinRadius(x: Int, y: Int): Boolean = indexToPoint(x, y).distance(center) <= radius_pix

    Trace.trace(s"center: $center   radius_pix: $radius_pix")

    val list = for (x <- 0 until count; y <- 0 until count) yield indexToPoint(x, y)

    if (true) { // TODO rm Show all points being used.
      val img = preprocessedImage.toDeepColorBufferedImage(0.01)

      val distinctList = list.map(p => new Point2i(p.getX.toInt, p.getY.toInt)).distinct
      distinctList.foreach(p => {
        img.setRGB(p.x, p.y, Color.white.getRGB)
      })

      ImageDisplay.showInMSPaint(img)
      Thread.sleep(2000)
    }

    list
  }

  private val bounds: Rectangle = makeBallBounds()

  /**
    * Find the brightest cluster of pixels to use as the approximate center.
    *
    * radius.  The radius is defined by the shortest distance from the center to an edge of the AOI.
    * @return The center of mass.
    */
  private def calcApproximateCenter(): Point2d = {
    val resolution = 0.5

    val brightestCount = 100

    val xSize = (bounds.getWidth / resolution).round.toInt
    val ySize = (bounds.getHeight / resolution).round.toInt

    def makeRow(y: Int): Seq[Point2d] = {
      val yCoordinate = bounds.getY + (y * resolution)
      val row = (0 until xSize).map(x => new Point2d(bounds.getX + (x * resolution), yCoordinate))
      val inBounds = row.filter(pointIsInBallAoi)
      inBounds
    }

    val matrix = (0 until ySize).map(makeRow)

    val dimmest = matrix.flatten.sortBy(p => biCubicImage.get(p.x, p.y)).take(brightestCount)
    val brightest = matrix.flatten.sortBy(p => biCubicImage.get(p.x, p.y)).takeRight(brightestCount)

    val xCenter = brightest.map(_.getX).sum / brightestCount
    val yCenter = brightest.map(_.getY).sum / brightestCount

    val approximateCenter = new Point2d(xCenter, yCenter)

    if (true) {
      val img = preprocessedImage.toDeepColorBufferedImage(0.01)

      dimmest.foreach(p => {
        img.setRGB(p.getX.toInt, p.getY.toInt, Color.orange.getRGB)
      })

      brightest.foreach(p => {
        img.setRGB(p.getX.toInt, p.getY.toInt, Color.black.getRGB)
      })

      img.setRGB(xCenter.toInt, yCenter.toInt, Color.white.getRGB)
      ImageDisplay.showInMSPaint(img)
      Thread.sleep(2000)
    }

    approximateCenter
  }

  private def findCenterOfMass(pointList: Seq[Point2d]): Point2d = {
    val list = pointList.map(p => Pt(p.x, p.y))
    val totalMass = list.map(_.value).sum

    Trace.trace(s"Number of points: ${pointList.size}    mean mass: ${totalMass / list.size}")

    val xCenter = list.map(p => p.x * p.value).sum / totalMass
    val yCenter = list.map(p => p.y * p.value).sum / totalMass

    val xProfile = {
      val groupList = list.groupBy(_.x).values.toIndexedSeq.sortBy(_.head.x)
      groupList.map(g => g.map(l => l.value).sum / g.size)
    }

    Trace.trace("X profile\n" + xProfile.mkString("\n"))

    val yProfile = {
      val groupList = list.groupBy(_.y).values.toIndexedSeq.sortBy(_.head.y)
      groupList.map(g => g.map(l => l.value).sum)
    }
    Trace.trace("Y profile\n" + yProfile.mkString("\n"))

    new Point2d(xCenter, yCenter)
  }

  /** Used as the sampling rate across the biCubicImage.  A value of 0.1 means for every pixel, 100 samples will be taken. */
  private val resolution_pix_old = 0.25

  private val evalPointList = makePointList(estimatedBallRadius_pix, resolution_pix_old)

  /**
    * Evaluate how close this is to the actual center.
    *
    * @param center A guess as to where the center is.
    * @return A number indicating how close the given center actually is.  Smaller number means closer.
    */
  def evaluate(center: Point2d): Double = {
    val cX = center.getX
    val cY = center.getY
    val meanBrightness = evalPointList.map(p => biCubicImage.get(p.getX + cX, p.getY + cY)).sum / evalPointList.size
    meanBrightness
  }

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
  private def findApproximateCenterOfMass(): Point2d = {
    val resolution_pix = 0.2

    val pointList = makeArray(resolution_pix)

    val xProfile = pointList.groupBy(_.x).values.toSeq.sortBy(_.head.x).map(group => group.map(_.value).sum / group.size)
    val yProfile = pointList.groupBy(_.y).values.toSeq.sortBy(_.head.y).map(group => group.map(_.value).sum / group.size)

    if (true) {
      ImageDisplay.showChart(xProfile, title = "X Profile")
      ImageDisplay.showChart(yProfile, title = "Y Profile")

      val xSlope = xProfile.tail.indices.map(i => xProfile(i + 1) - xProfile(i))
      ImageDisplay.showChart(xSlope, title = "X Velocity")

      val ySlope = yProfile.tail.indices.map(i => yProfile(i + 1) - yProfile(i))
      ImageDisplay.showChart(ySlope, title = "Y Velocity")

      val xAcc = xSlope.tail.indices.map(i => xSlope(i + 1) - xSlope(i))
      ImageDisplay.showChart(xAcc, title = "X Acceleration")

      val yAcc = ySlope.tail.indices.map(i => ySlope(i + 1) - ySlope(i))
      ImageDisplay.showChart(yAcc, title = "Y Acceleration")

    }

    val xMin = pointList.minBy(_.x).x
    val yMin = pointList.minBy(_.y).y

    val x = (ImageUtil.centerOfMass(xProfile.map(_.toFloat).toIndexedSeq) * resolution_pix) + xMin
    val y = (ImageUtil.centerOfMass(yProfile.map(_.toFloat).toIndexedSeq) * resolution_pix) + yMin

    new Point2d(x, y)
  }

  def doIt(): Point2d = {

    // logger.info(s"Estimated radius in pixels: $searchRadius_pix     radius in mm: ${trans.pix2IsoDistX(searchRadius_pix)}")

    val center1 = calcApproximateCenter()
    val searchRadius_pix = calculateRadiusToNearestEdge(center1)

    Trace.trace(s"approximateCenter: $center1")

    Trace.trace()
    val start = System.currentTimeMillis()
    val center2 = findApproximateCenterOfMass()
    val elapsed = System.currentTimeMillis() - start
    Trace.trace(s"Elapsed ms for findApproximateCenterOfMass: $elapsed")
    Trace.trace(s"center2: $center2")

    val pointList3 = makePointArray(center2, 0.1)
    val center3 = findCenterOfMass(pointList3)
    Trace.trace(s"center3: $center3")
    Thread.sleep(2000)
    System.exit(99)

    center3

    /*
    val start = System.currentTimeMillis()
    val finder = new WLNonCardBallFinder(this, approximateCenter, initialSearchAreaSize_pix)
    val elapsed = System.currentTimeMillis() - start
    Trace.trace(s"Elapsed: $elapsed")

    val answer = finder.getMaxPoint_iso

    Trace.trace(s"answer: $answer")

    Trace.trace(s"approximate to exact distance: ${answer.p2d.distance(approximateCenter)}")

    def fmt(d: Double) = "%20.15f".format(d)
    logger.info(s"Final ball location: x: ${fmt(answer.x)}   y: ${fmt(answer.y)}   mean pixel value: ${fmt(answer.value)}   elapsed ms: $elapsed    resolution_pix: $resolution_pix")

    System.exit(99)
    answer.p2d
     */
  }
}
