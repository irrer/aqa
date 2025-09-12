package org.aqa

import edu.umro.ImageUtil.DicomImage
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolatingFunction
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolator

import java.awt.geom.Point2D
import java.awt.image.BufferedImage

case class BiCubicImage(dicomImage: DicomImage, bufImg: Option[BufferedImage] = None) extends Logging {

  private val xCoordinateList: Array[Double] = (0 until dicomImage.width).map(_.toDouble).toArray
  private val yCoordinateList: Array[Double] = (0 until dicomImage.height).map(_.toDouble).toArray

  private val valueMatrix: Array[Array[Double]] = dicomImage.pixelData.map(row => row.map(_.toDouble).toArray).toArray

  private val interpolator = new PiecewiseBicubicSplineInterpolator()

  private val function: PiecewiseBicubicSplineInterpolatingFunction = interpolator.interpolate(xCoordinateList, yCoordinateList, valueMatrix)

  /**
    * Get the value at the given coordinates.  If this is outside the bounds of the image, then an exception will be thrown.
    * @param x X coordinate.
    * @param y Y coordinate.
    * @return Interpolated image value at given coordinates.
    */
  def get(x: Double, y: Double): Double = {
    if (bufImg.isDefined)
      bufImg.get.setRGB(x.round.toInt, y.round.toInt, 0)
    function.value(y, x)
  }

  def get(point: Point2D.Double): Double = get(point.getX, point.getY)

}
