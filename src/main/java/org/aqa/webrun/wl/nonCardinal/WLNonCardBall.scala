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
import org.aqa.BiCubicImage
import org.aqa.Logging

import java.awt.Color
import java.awt.Rectangle
import javax.vecmath.Point2d
import javax.vecmath.Point2i

/**
  * Measure the position of the ball and also verify that it is a valid ball.
  * @param edgeSet Surrounding edges.
  * @param preprocessedImage Image containing ball.
  * @param biCubicImage Interpolated version of preprocessedImage.
  */
case class WLNonCardBall(edgeSet: WLNonCardEdgeSet, preprocessedImage: DicomImage, biCubicImage: BiCubicImage) extends Logging {

  private def rnd(d: Double): Int = d.round.toInt

  private def pointIsInBallAoi(point: Point2i): Boolean = {
    edgeSet.X1.loLine.pointIsBetween(point, edgeSet.X2.loLine) &&
    edgeSet.Y1.loLine.pointIsBetween(point, edgeSet.Y2.loLine)
  }

  /**
    * Construct the Ball AOI by determining which points are within the edge lines that define thei
    * minimum edge values that were determined during edge measurement.
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

  private val ballAOIBounds = makeBallBounds()
  private val ballAOI = makeBallAOI(ballAOIBounds)

  if (true) { // TODO rm

    val bufImg = preprocessedImage.toDeepColorBufferedImage(0.01)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)

    gc.drawRect(ballAOIBounds.x, ballAOIBounds.y, ballAOIBounds.width, ballAOIBounds.height)
    ImageDisplay.showInMSPaint(bufImg)
    Thread.sleep(2 * 1000) // let MS Paint start
    Trace.trace()
  }

  // private val pointSpacing: Double = 0.2

  private case class Pt(x: Double, y: Double) {
    val value: Double = biCubicImage.get(x, y)
  }

  private def addCircle(radius: Double, center: Point2d, pointSpacing: Double): Seq[Pt] = {

    val diameter = Math.PI * 2 * radius

    val count = (diameter / pointSpacing).ceil.round.toInt
    val radianIncrement = (Math.PI * 2) / count

    def makePoint(i: Int): Pt = {
      val radian = radianIncrement * i

      val x = center.getX + (Math.sin(radian) * radius)
      val y = center.getY + (Math.cos(radian) * radius)

      Pt(x, y)
    }

    (0 until count).map(makePoint)
  }

  private val bounds: Rectangle = makeBallBounds()

  /**
    * Find the center of mass.  A circular area is searched, and is defined by the center point given, and a
    *
    * radius.  The radius is defined by the shortest distance from the center to an edge of the AOI.
    * @param pointSpacing The resolution. Sample the image this many pixels apart.  e.g., 0.5 would mean sampling every 1/2 pixel.
    * @param center Use this as the center of the search.  If not defined, use the center of the AOI.
    * @return The center of mass.
    */
  private def calcCenter(pointSpacing: Double, center: Point2d = new Point2d(bounds.getCenterX, bounds.getCenterY)): Point2d = {

    val radius = Math.min(bounds.width / 2.0, bounds.getHeight / 2.0) * 0.9

    // Trace.trace(s"radius: $radius")

    val numCircle = (radius / pointSpacing).ceil.round.toInt

    val pointList = (0 until numCircle).flatMap(c => addCircle(c * pointSpacing, center, pointSpacing: Double))

    if (false) {
      val img = preprocessedImage.toBufferedImage(Color.blue)
      val gc = ImageUtil.getGraphics(img)
      val color: Int = Color.white.getRGB
      pointList.foreach(pt => img.setRGB(pt.x.toInt, pt.y.toInt, color))
      ImageDisplay.showInMSPaint(img)
      Thread.sleep(2000)
    }

    val totalMass = pointList.map(_.value).sum
    val xCenter = pointList.map(pt => pt.x * pt.value).sum / totalMass
    val yCenter = pointList.map(pt => pt.y * pt.value).sum / totalMass

    new Point2d(xCenter, yCenter)
  }

  /*
  @tailrec
  private def findCenter(center: Point2d = new Point2d(bounds.getCenterX, bounds.getCenterY)): Point2d = {

    calcCenter(1.0)
  }
   */

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
    val stdDevFactor = 1.0

    // Pixel values sorted. Drop a few from each end to discard bad pixels.
    val pixList = subImage.pixelData.flatten.sorted.drop(5).dropRight(5)

    val stdDev = ImageUtil.stdDev(pixList)
    val meanOf = pixList.sum / pixList.size

    val ballPixValueThreshold = meanOf + (stdDev * stdDevFactor)

    val ballPixList = pixList.filter(_ > ballPixValueThreshold)

    val ballArea = ballPixList.size

    val ballRadius = Math.sqrt(ballArea / Math.PI)

    ballRadius
  }

  private def byRect(spacing: Double): Point2d = {
    val xCount = (bounds.getWidth / spacing).round.toInt
    val yCount = (bounds.getHeight / spacing).round.toInt

    def makeRow(r: Int): Seq[Pt] = {
      val y = r * spacing + bounds.getY
      val baseX = bounds.getX

      (0 until xCount).map(x => Pt(x * spacing + baseX, y))
    }

    val list = (0 until yCount).map(makeRow)

    val rowSums = list.map(row => row.map(_.value).sum).map(_.toFloat)

    val columnSums = {
      def doCol(x: Int): Double = (0 until yCount).map(y => list(y)(x).value).sum
      (0 until xCount).map(doCol).map(_.toFloat)
    }

    val xx = ImageUtil.centerOfMass(columnSums)
    val yy = ImageUtil.centerOfMass(rowSums)

    val x = (ImageUtil.centerOfMass(columnSums) * spacing) + bounds.getX
    val y = (ImageUtil.centerOfMass(rowSums) * spacing) + bounds.getY

    new Point2d(x, y)
  }

  def doIt(): Unit = {
    Trace.trace()
    makeBallAOI(ballAOIBounds)

    var sp = 1.0

    val ballRadius = estimateBallRadius_pix()

    estimateBallRadius_pix()
    estimateBallRadius_pix()
    estimateBallRadius_pix()

    case class Dodo(time: Long, spacing: Double, centerList: Seq[Point2d]) {}

    val list = (0 until 13).map(_ => {
      val start = System.currentTimeMillis()
      val center0 = calcCenter(sp)
      val elapsed = System.currentTimeMillis() - start

      val centerList = (0 until 10).tail.foldLeft(Seq(center0))((list, index) => list :+ calcCenter(sp, list(index - 1)))

      val distList = centerList.indices.tail.map(i => centerList(i).distance(centerList(i - 1)))

      Trace.trace(s"""spacing: $sp    ${distList.map(d => "%14.10f".format(d)).mkString("    ")}  """)

      val oldSp = sp
      sp = sp * 0.75
      Dodo(elapsed, oldSp, centerList)
    })

    /*
    val list = (0 until 16).map(i => {
      val start = System.currentTimeMillis()
      val center = byRect(sp)
      val elapsed = System.currentTimeMillis() - start

      Trace.trace(s" elapsed: $elapsed   spacing: $sp    center: $center")
      sp = sp * 0.75
      Dodo(elapsed, sp + 0, center)
    })
     */

    list.indices.foreach(i => {
      val dodo = list(i)
      val distList = dodo.centerList.indices.tail.map(i => dodo.centerList(i).distance(dodo.centerList(i - 1)))
      val centerText = dodo.centerList.map(c => "%20.15f".format(c.getX) + ", " + "%20.15f".format(c.getY)).mkString("  |  ")
      val distText = distList.map(d => "%14.10f".format(d)).mkString("    ")
      Trace.trace(s"time: ${"%10d".format(dodo.time)}    spacing: ${"%20.15f".format(dodo.spacing)}    distanceList: $distText    centerList: $centerText")
    })

    Trace.trace()
  }

}
