package org.aqa

import javax.vecmath.Point2d
import scala.annotation.tailrec

/**
  * Define and handle the geometry of 'traveling' along non-cardinal lines within an image.
  * @param centerPoint Point on the line.
  * @param angle_deg Angle of in degrees.
  */
case class AQALine(centerPoint: Point2d, angle_deg: Double) extends Logging {

  def this(centerX: Double, centerY: Double, angle: Double) = this(new javax.vecmath.Point2d(centerX, centerY), angle)

  val centerX: Double = centerPoint.getX
  val centerY: Double = centerPoint.getY

  val perpendicularAngle: Double = Util.modulo360(angle_deg + 90)

  /** The line perpendicular to this line, with the same center. */
  def perpendicular: AQALine = AQALine(centerPoint, perpendicularAngle)

  private val radians: Double = Math.toRadians(Util.modulo360(angle_deg))

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
  def pointOn(offset: Double): Point2d = {

    val point = angle_deg match {
      case _ if (angle_deg.abs > 359) || (angle_deg.abs < 1) => new Point2d(centerX + offset, centerY)
      case _ if (angle_deg > 179) && (angle_deg < 181)       => new Point2d(centerX + offset, centerY)
      case _ if (angle_deg > 89) && (angle_deg < 91)         => new Point2d(centerX, centerY + offset)
      case _ if (angle_deg > 269) && (angle_deg < 271)       => new Point2d(centerX, centerY + offset)

      case _ =>
        val useX = //
          ((angle_deg > 45) && (angle_deg < 135)) ||
            ((angle_deg > 225) && (angle_deg < 315))

        if (useX) {
          val x = centerX + (offset * cos)
          val y = x2y(x)
          new Point2d(x, y)
        } else {
          val y = centerY + (offset * sin)
          val x = y2x(y)

          new Point2d(x, y)
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
  def makeProfile(offsetStart: Double, offsetFinish: Double, biCubicImage: BiCubicImage, width: Double, resolution: Double): Seq[Double] = {

    val positive = offsetFinish > offsetStart

    val increment = if (positive) resolution else -resolution

    @tailrec
    def add(offset: Double, profile: Seq[Double]): Seq[Double] = {
      if //
      (
        (positive && (offset <= offsetFinish)) || //
        ((!positive) && (offset >= offsetFinish))
      ) {
        val line = new AQALine(pointOn(offset), perpendicularAngle)

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

  /**
    * Find the intersection of the two lines.
    * @param other The other line.  Must not be parallel to this line.
    * @return The point where they intersect.
    */
  def intersection(other: AQALine): Point2d = {
    val x = (other.b - b) / (m - other.m)
    val y = x2y(x)
    new Point2d(x, y)
  }
}

object AQALine extends Logging {
  def makeLine(point1: Point2d, point2: Point2d): AQALine = {
    val centerX = (point1.getX + point2.getX) / 2
    val centerY = (point1.getY + point2.getY) / 2

    val center = new Point2d(centerX, centerY)

    val slope = (point1.getY - point2.getY) / (point1.getX - point2.getX)

    val angle_radians = Math.atan(slope)
    val angle_deg = Math.toDegrees(angle_radians)

    AQALine(center, angle_deg)

  }
}
