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

  if (false) { // TODO rm

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

  private def calcCenter(pointSpacing: Double): Point2d = {
    bounds.getCenterX

    val center = new Point2d(bounds.getCenterX, bounds.getCenterY)

    val radius = Math.min(bounds.width / 2.0, bounds.getHeight / 2.0)

    Trace.trace(s"radius: $radius")

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

  def byRect(spacing: Double): Point2d = {
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

    case class Dodo(time: Long, spacing: Double, center: Point2d) {}

    /*
    val list = (0 until 16).map(i => {
      val start = System.currentTimeMillis()
      val center = calcCenter(sp)
      val elapsed = System.currentTimeMillis() - start

      Trace.trace(s"spacing: $sp    center: $center")

      sp = sp * 0.75
      Dodo(elapsed, sp + 0, center)
    })
     */

    val list = (0 until 16).map(i => {
      val start = System.currentTimeMillis()
      val center = byRect(sp)
      val elapsed = System.currentTimeMillis() - start

      Trace.trace(s" elapsed: $elapsed   spacing: $sp    center: $center")
      sp = sp * 0.75
      Dodo(elapsed, sp + 0, center)
    })

    list.indices.foreach(i => {
      val dodo = list(i)
      val distance: Double = if (i == 0) -1 else dodo.center.distance(list(i - 1).center)
      Trace.trace(s"time: ${"%10d".format(dodo.time)}    spacing: ${"%20.15f".format(dodo.spacing)}    distance: ${"%20.15f".format(distance)}   center: ${dodo.center}")
    })

    Trace.trace()
  }

}
