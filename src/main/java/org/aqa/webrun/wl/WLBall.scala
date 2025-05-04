package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.MachineWL
import org.opensourcephysics.numerics.CubicSpline

import java.awt.Rectangle
import java.awt.geom.Point2D
import java.awt.image.BufferedImage

case class WLBall(aoiBounds: Rectangle, wholeImage: DicomImage, rtimage: AttributeList, machineWL: MachineWL) extends Logging {

  private val aoi = wholeImage.getSubimage(aoiBounds)

  private val trans = new IsoImagePlaneTranslator(rtimage)

  private val ResolutionX = trans.pix2IsoDistX(1)
  private val ResolutionY = trans.pix2IsoDistY(1)

  /** Scaling for drawing images. */
  val SCALE: Int = ((Config.WLScale / 0.526) * ((ResolutionX + ResolutionY) / 2.0)).round.toInt

  // The step size (in pixels) for crawling down the curve to find the
  // point where the ball height exceeds a threshold
  private val X_INCREMENT: Double = 0.001

  private val ballRadiusX_pix = trans.iso2PixDistX(machineWL.ballDiameter_mm / 2)
  private val ballRadiusY_pix = trans.iso2PixDistY(machineWL.ballDiameter_mm / 2)

  /** Expected radius of ball in (units of) number of pixels. */
  private val ballRadius_pix = Math.max(ballRadiusX_pix, ballRadiusY_pix)

  /**
    * Coarsely locate the center of the ball by creating a copy of the area expected to contain
    * the center of the ball, and then transforming it so that each pixel contains the sum of
    * the 8 neighboring pixels and itself, then find the pixel with the largest value and take that
    * as the ball's center.  This effectively says: "Find the largest sum of 3x3 pixels".
    */
  private def coarseBallLocate(): Point = {

    def sumOf(x: Int, y: Int): Float = {
      // @formatter:off
        aoi.get(x - 1, y - 1) +
        aoi.get(x - 1, y    ) +
        aoi.get(x - 1, y + 1) +
        aoi.get(x    , y - 1) +
        aoi.get(x    , y    ) +
        aoi.get(x    , y + 1) +
        aoi.get(x + 1, y - 1) +
        aoi.get(x + 1, y    ) +
        aoi.get(x + 1, y + 1)
      // @formatter:on
    }


    // make a list of coordinate pairs
    val xyList = for (x <- 1 until (aoi.width - 1); y <- 1 until (aoi.height - 1)) yield (x, y)

    val max = xyList.maxBy(xy => sumOf(xy._1, xy._2))

    new Point(max._1, max._2)
  }


  private def centerOfMass(spline: CubicSpline, len: Int): SearchRange = {
    val pointList = (0 until (len / X_INCREMENT).round.toInt).map(i => spline.evaluate(i * X_INCREMENT))
    val min = pointList.min
    val max = pointList.max

    val minAcceptable = min + ((max - min) * (1.0 - (Config.WLBallHeightPercentForeground / 100.0)))

    val lo = pointList.indexWhere(p => p >= minAcceptable)
    val hi = (pointList.size - 1) - pointList.reverse.indexWhere(p => p >= minAcceptable)

    val weighted = (lo to hi).map(i => pointList(i) * i).sum * X_INCREMENT
    val sum = pointList.slice(lo, lo + hi - lo).sum
    val centerOfMass = weighted / sum

    new SearchRange(lo * X_INCREMENT, centerOfMass, hi * X_INCREMENT)
  }


  /**
   * Determine if the spline has a single maximum by walking the spline and counting the
   * number of times it crosses the average value.  It should cross exactly twice.
   */
  def singleMax(spline: CubicSpline, values: IndexedSeq[Float]): Boolean = {
    val avg = (values.max + values.min) / 2
    val increment = 1000

    // determine if this value is over or under the average, and increment the count accordingly
    // crossCount: Number of times that the average was crossed.
    def cross(x: Int, crossCount: Int, overAvg: Boolean): (Int, Boolean) = {
      val value = spline.evaluate(x.toDouble / increment)
      0 match {
        case _ if overAvg && (value < avg) => (crossCount + 1, false)
        case _ if (!overAvg) && (value >= avg) => (crossCount + 1, true)
        case _ => (crossCount, overAvg)
      }
    }

    val state = (0 to (values.length * increment)).toList.foldLeft((0, false))((s, x) => cross(x, s._1, s._2))
    val crsCount = state._1
    if (crsCount == 2) {
      logger.info(" Ball spline verified to cross average height exactly twice")
      true
    } else {
      logger.warn(" Wrong number of times that the ball spline crossed the average value.  Should be 2 but was " + crsCount)
      false
    }
  }



  /*
   * @param x: Approximate horizontal center of ball
   * @param y: Approximate vertical center of ball
   */

  def fineBallLocate(aoiFine: IndexedSeq[IndexedSeq[Float]]): Option[Point2D.Double] = {

    val annotate = new WLAnnotate(SCALE, ballRadius_pix.round.toInt)

    val rSum = WLProcessImage.unitize(WLProcessImage.rowSum(aoiFine))
    val rSpline = WLProcessImage.toCubicSpline(rSum)
    val cSum = WLProcessImage.unitize(WLProcessImage.colSum(aoiFine))
    val cSpline = WLProcessImage.toCubicSpline(cSum)

    val fineX = centerOfMass(cSpline, cSum.length)
    val fineY = centerOfMass(rSpline, rSum.length)
    val image = annotate.saveFineLocatedImage(aoiFine, fineX, fineY)

    if (singleMax(cSpline, cSum) && singleMax(rSpline, rSum)) {
      logger.info("Ball fine location relative to area of interest in pixels: " + fineX.center + ", " + fineY.center)
      Some(new Point2D.Double(fineX.center, fineY.center))
    } else
      None
  }

  private val coarseCenter = coarseBallLocate()

  ballRadius_pix
  val aoiFine = {
    val x = (ballRadiusX_pix + coarseCenter.x).round.toInt
    val y = (ballRadiusY_pix + coarseCenter.y).round.toInt
    val width = (ballRadiusX_pix * 2).round.toInt
    val height = (ballRadiusY_pix * 2).round.toInt
    aoi.getSubimage(new Rectangle(x, y, width, height))
  }

  val point: Option[Point2D.Double] = fineBallLocate(???)

  val ballFine: BufferedImage = {
    val annotate = new WLAnnotate(SCALE, ballRadius_pix.round.toInt)
    // annotate.saveFineLocatedImage(aoiFine.pixelData, point.get.getX.round.toInt, point.get.getY)
    ???
  }


  /**
   * Make an image showing the level of background noise immediately around the ball.
   */
  def showBallBackgroundNoise(name: String): BufferedImage = {
    // val aoiWidth = areaOfInterest.head.length
    // val aoiHeight = areaOfInterest.length
    // val aoi = subSection(areaOfInterest, 0, aoiWidth, 0, aoiHeight)

    val min = aoi.minPixelValue
    val max = aoi.maxPixelValue
    val limit = ((max - min) * 0.08) + min

    def doRow(row: IndexedSeq[Float]) = row.map(v => if (v > limit) min else v)

    val newAoi = aoi.pixelData.map(doRow)

    val bufImg = WLProcessImage.toPngScaled(newAoi, SCALE)
    bufImg
  }

  val ballBackgroundNoise: BufferedImage = showBallBackgroundNoise("ball_background")


}
