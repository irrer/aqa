package org.aqa

import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.Trace
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolatingFunction
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolator

import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import javax.vecmath.Point2d

/**
 * Image abstraction that supports sub-pixel sampling via BiCubic mapping.
 * @param dicomImage For this DICOM image.
 * @param bufImg Optionally mark sample points in this image buffer.  Mostly for debugging.
 */
case class BiCubicImage(dicomImage: DicomImage, bufImg: Option[BufferedImage] = None) extends Logging {

  private val xCoordinateList: Array[Double] = (0 until dicomImage.width).map(_.toDouble).toArray
  private val yCoordinateList: Array[Double] = (0 until dicomImage.height).map(_.toDouble).toArray

  private val valueMatrix: Array[Array[Double]] = dicomImage.pixelData.map(row => row.map(_.toDouble).toArray).toArray

  private val interpolator = new PiecewiseBicubicSplineInterpolator()

  private val function: PiecewiseBicubicSplineInterpolatingFunction = interpolator.interpolate(yCoordinateList, xCoordinateList, valueMatrix)

  /**
    * Get the value at the given coordinates.  If this is outside the bounds of the image, then an exception will be thrown.
    * @param x X coordinate.
    * @param y Y coordinate.
    * @return Interpolated image value at given coordinates.
    */
  def get(x: Double, y: Double): Double = {
    if (bufImg.isDefined) {
      try {
        bufImg.get.setRGB(x.round.toInt, y.round.toInt, 255)
      } catch {
        case _: Throwable =>
          Trace.trace("x,y: " + x + ", " + y)
      }
    }
    function.value(y, x)
  }

  def get(point: Point2D.Double): Double = get(point.getX, point.getY)

  def get(point: Point2d): Double = get(point.getX, point.getY)

}
