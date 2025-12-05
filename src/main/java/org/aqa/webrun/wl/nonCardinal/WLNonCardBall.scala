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

  private def findCenter(bounds: Rectangle): Point2d = {

    val center = new Point2d(bounds.getX + (bounds.getWidth / 2.0), bounds.getX + (bounds.getHeight / 2.0))

    ???

  }

  def doit(): Unit = {
    Trace.trace()
    makeBallAOI(ballAOIBounds)
    Trace.trace()
  }

}
