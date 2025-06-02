package org.aqa.webrun.wl

import edu.umro.ImageUtil.DicomImage
import org.aqa.Config
import org.aqa.Util
import org.aqa.webrun.wl.WLProcessImage.toPngScaled
import org.aqa.Logging
import org.aqa.db.MachineWL
import org.opensourcephysics.numerics.CubicSpline

import java.awt.Graphics2D
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.io.File

case class WLBall(
    coarseAoi: DicomImage,
    ballAoi: IndexedSeq[IndexedSeq[Float]],
    subDir: File,
    SCALE: Int,
    BALL_RADIUS: Int,
    X_INCREMENT: Double,
    ResolutionX: Double,
    ResolutionY: Double,
    wlParameters: MachineWL,
    tol: Int,
    wlMsg: WLMessage
) extends Logging {

  private val tol2 = tol * 2

  /** Convert a value in mm to pixels. */
  private def toPixels(mm: Double): Int = ((mm / ResolutionX) + 0.5).toInt

  private def toPng(pix: IndexedSeq[IndexedSeq[Float]]): BufferedImage = toPngScaled(pix, SCALE)

  /**
    * Coarsely locate the center of the ball by creating a copy of the area expected to contain
    * the center of the ball, and then transforming it so that each pixel contains the sum of
    * the 8 neighboring pixels and itself, then find the pixel with the largest value and take that
    * as the ball's center.  This effectively says: "Find the largest sum of 3x3 pixels".
    */
  private def coarseBallLocate: (Int, Int) = {

    val newCenter = {
      val di = new DicomImage(ballAoi)
      val point = di.getMaxRect(3, 3)
      (point.x + 1, point.y + 1)
    }

    def saveImage(center: (Int, Int)): Unit = {
      val png = toPng(ballAoi)
      val annotate = new WLAnnotate(SCALE, BALL_RADIUS)
      annotate.drawCross(png.getGraphics.asInstanceOf[Graphics2D], center._1, center._2, 1)
      Util.writePng(png, new File(subDir, "ball_coarse.png"))
    }

    saveImage(newCenter)

    newCenter
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
  private def singleMax(spline: CubicSpline, values: IndexedSeq[Float]): Boolean = {
    val avg = (values.max + values.min) / 2
    val increment = 1000

    // determine if this value is over or under the average, and increment the count accordingly
    // crossCount: Number of times that the average was crossed.
    def cross(x: Int, crossCount: Int, overAvg: Boolean): (Int, Boolean) = {
      val value = spline.evaluate(x.toDouble / increment)
      0 match {
        case _ if overAvg && (value < avg)     => (crossCount + 1, false)
        case _ if (!overAvg) && (value >= avg) => (crossCount + 1, true)
        case _                                 => (crossCount, overAvg)
      }
    }

    val state = (0 to (values.length * increment)).toList.foldLeft((0, false))((s, x) => cross(x, s._1, s._2))
    val crsCount = state._1
    if (crsCount == 2) {
      wlMsg.info(" Ball spline verified to cross average height exactly twice")
      true
    } else {
      wlMsg.error(" Wrong number of times that the ball spline crossed the average value.  Should be 2 but was " + crsCount)
      false
    }
  }

  private def fineBallLocate(aoiFine: IndexedSeq[IndexedSeq[Float]]): Option[(Double, Double)] = {

    val annotate = new WLAnnotate(SCALE, BALL_RADIUS)

    val rSum = WLProcessImage.unitize(WLProcessImage.rowSum(aoiFine))
    val rSpline = WLProcessImage.toCubicSpline(rSum)
    val cSum = WLProcessImage.unitize(WLProcessImage.colSum(aoiFine))
    val cSpline = WLProcessImage.toCubicSpline(cSum)

    val fineX = centerOfMass(cSpline, cSum.length)
    val fineY = centerOfMass(rSpline, rSum.length)
    val image = annotate.saveFineLocatedImage(aoiFine.map(_.toIndexedSeq).toIndexedSeq, fineX, fineY)
    Util.writePng(image, new File(subDir, "ball_fine.png"))

    if (singleMax(cSpline, cSum) && singleMax(rSpline, rSum)) {
      wlMsg.info("Ball fine location relative to area of interest in pixels: " + fineX.center + ", " + fineY.center)
      Some(fineX.center, fineY.center)
    } else
      None
  }

  /**
    * Find the center of the ball within the given area.  Return the
    * coordinates relative to the area given.
    */
  def findBallCenter(): Option[(Double, Double)] = {

    val coarseCenter = coarseBallLocate
    // not sure why 0.3 works
    val radius: Int = (toPixels(wlParameters.ballDiameter_mm / 2.0) + (tol.toDouble * 0.3) + 0.5).toInt
    val leftEdge = coarseCenter._1 + tol2 - radius
    val topEdge = coarseCenter._2 + tol2 - radius
    val ballRoi = coarseAoi.getSubArray(
      new Rectangle( //
        leftEdge,
        topEdge,
        radius * 2,
        radius * 2
      )
    )

    Util.writePng(toPng(WLImageUtil.normalizeArea(ballRoi)), new File(subDir, "ballRoiNormalized.png"))
    Util.writePng(toPng(ballRoi), new File(subDir, "ballRoiRaw.png"))

    fineBallLocate(ballRoi) match {
      case Some(loc: (Double, Double)) =>
        Some(loc._1 + leftEdge, loc._2 + topEdge)
      case None =>
        None
    }
  }

}
