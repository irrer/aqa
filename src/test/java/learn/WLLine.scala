package learn

import edu.umro.ImageUtil.ImageUtil
import org.aqa.BiCubicImage
import org.aqa.Logging
import org.aqa.Util

import java.awt.geom.Point2D
import java.awt.Color
import javax.vecmath.Point2d
import scala.annotation.tailrec

case class Point(x: Double, y: Double) {
  // Auxiliary constructor: from javax.vecmath.Point2d
  def this(p: Point2d) = this(p.x, p.y)

  // Auxiliary constructor: from java.awt.geom.Point2D.Double
  def this(p: Point2D.Double) = this(p.getX, p.getY)

}

case class WLLine(centerPoint: javax.vecmath.Point2d, angle: Double) extends Logging {

  def this(centerX: Double, centerY: Double, angle: Double) = this(new javax.vecmath.Point2d(centerX, centerY), angle)

  // def this(point: Point2D.Double, angle: Double) = this(point.getX, point.getY, angle)

  def this(point: Point2D.Double, angle: Double) = this(point.getX, point.getY, angle)

  val centerX: Double = centerPoint.getX
  val centerY: Double = centerPoint.getY

  private val perpendicularAngle = Util.modulo360(angle + 90)

  /** The line perpendicular to this line, with the same center. */
  def perpendicular: WLLine = new WLLine(centerPoint, perpendicularAngle)

  private val radians: Double = Math.toRadians(Util.modulo360(angle))

  private val cos: Double = Math.cos(radians)
  private val sin: Double = Math.sin(radians)
  private val tan: Double = Math.tan(radians)

  private val m: Double = -tan

  private val b: Double = centerY - (m * centerX)

  private def x2y(x: Double): Double = (m * x) + b
  private def y2x(y: Double): Double = if (m == 0) b else (y - b) / m

  /**
    * Find a point on the line offset from the center.
    * @param offset Distance along line from center.
    * @return New point on the line.
    */
  def pointOn(offset: Double): Point2D.Double = {

    val point = angle match {
      case _ if (angle.abs > 350) || (angle.abs < 1) => new Point2D.Double(centerX + offset, centerY)
      case _ if (angle > 179) && (angle < 181)       => new Point2D.Double(centerX + offset, centerY)
      case _ if (angle > 89) && (angle < 91)         => new Point2D.Double(centerX, centerY + offset)
      case _ if (angle > 269) && (angle < 271)       => new Point2D.Double(centerX, centerY + offset)

      case _ =>
        val useX = //
          ((angle > 45) && (angle < 135)) ||
            ((angle > 225) && (angle < 315))

        if (useX) {
          val x = centerX + (offset * cos)
          val y = x2y(x)
          new Point2D.Double(x, y)
        } else {
          val y = centerY + (offset * sin)
          val x = y2x(y)

          new Point2D.Double(x, y)
        }
    }
    point
  }

  /**
    * Make profile of the sum of values along the line.
    * @param offsetStart Start of segment in pixels offset from this line's point.
    * @param offsetFinish End of segment in pixels offset from this line's point.
    * @param biCubicImage Use this to interpret values between pixels.
    * @param width Width of sample band in pixels.
    * @param resolution Distance between samples in pixels.  0.1 would make 10*10 = 100 samples per pixel.
    * @return Profile of the sampled band.
    */
  def makeGradient(offsetStart: Double, offsetFinish: Double, biCubicImage: BiCubicImage, width: Double, resolution: Double): Seq[Double] = {

    val positive = offsetFinish > offsetStart

    val increment = if (positive) resolution else -resolution

    @tailrec
    def add(offset: Double, profile: Seq[Double]): Seq[Double] = {
      if //
      (
        (positive && (offset <= offsetFinish)) || //
        ((!positive) && (offset >= offsetFinish))
      ) {
        val line = new WLLine(pointOn(offset), perpendicularAngle)

        val count = (width / resolution).round.toInt

        val pointList = (0 until count).map(i => line.pointOn((i * resolution) - (width / 2)))
        val sum =
          try {
            val mean = pointList.map(biCubicImage.get).sum / pointList.size
            Some(mean)
          } catch {
            case _: org.apache.commons.math3.exception.OutOfRangeException =>
              None // out of bounds - just use what we've got so far
          }
        if (sum.isDefined)
          add(offset + increment, profile :+ sum.get)
        else
          profile
      } else
        profile
    }

    val profile = add(offsetStart, Seq())
    profile
  }

  def makeProfile(offsetStart: Double, offsetFinish: Double, color: Color,  biCubicImage: BiCubicImage, width: Double, resolution: Double): Seq[Double] = {

    val positive = offsetFinish > offsetStart

    val increment = if (positive) resolution else -resolution

    val count = ((offsetStart - offsetFinish).abs / resolution).round.toInt

    val startPoint = pointOn(offsetStart)

    val perpendicularLineLo = new WLLine(startPoint, perpendicularAngle)
    val perpendicularLineHi = new WLLine(pointOn(offsetFinish), perpendicularAngle)

    @tailrec
    def add(offset: Double, profile: Seq[Double]): Seq[Double] = {

      if (offset <= (width / 2)) {

        val startPoint = perpendicularLineLo.pointOn(offset)
        val line = new WLLine(startPoint, angle)

        val prof = //
          (0 until count)
            .map(i => biCubicImage.get(line.pointOn(i * increment)))
            .sum / count

        add(offset + resolution, profile :+ prof)
      } else
        profile

    }

    if (true) {
      val pointLoLo = perpendicularLineLo.pointOn(-width / 2)
      val pointLoHi = perpendicularLineLo.pointOn(width / 2)

      val pointHiLo = perpendicularLineHi.pointOn(-width / 2)
      val pointHiHi = perpendicularLineHi.pointOn(width / 2)

      val bufImg = WLMeasureEdges.biCub2.get.bufImg.get

      val gc = ImageUtil.getGraphics(bufImg)
      gc.setColor(color)

      def dl(p1: Point2D.Double, p2: Point2D.Double): Unit = {
        gc.drawLine( //
          p1.getX.round.toInt,
          p1.getY.round.toInt,
          p2.getX.round.toInt,
          p2.getY.round.toInt
        )
      }

      dl(pointLoLo, pointLoHi)
      dl(pointHiLo, pointHiHi)

      dl(pointLoLo, pointHiLo)
      dl(pointLoHi, pointHiHi)

    }

    val profile = add(-width / 2, Seq())

    profile
  }

  /**
    * Find the intersection of the two lines.
    * @param other The other line.  Must not be parallel to this line.
    * @return The point where they intersect.
    */
  def intersection(other: WLLine): Point2D.Double = {
    val x = (other.b - b) / (m - other.m)
    val y = x2y(x)
    new Point2D.Double(x, y)
  }
}
