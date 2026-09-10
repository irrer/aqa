package org.aqa.webrun.psm

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolatingFunction
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolator
import org.aqa.webrun.psm.PSMUtil.centerPixelsToString
import org.aqa.Logging

import java.awt.geom.Point2D
import javax.vecmath.Point2d

/**
  * Interpolate using bi-cubic spline to determine pixels between beam center.
  * @param psmList List of
  */
class PSMInterpolator(psmList: Seq[PSMBeamAnalysisResult]) extends Logging {
  val trans: IsoImagePlaneTranslator = new IsoImagePlaneTranslator(psmList.head.rtimage)

  private val grid = PSMGrid(psmList)

  private val xCoordinateList = grid.grid.head.flatten.map(_.psmBeam.xCenter_mm).toArray
  private val yCoordinateList = grid.grid.flatMap(_.head).map(_.psmBeam.yCenter_mm).toArray

  private val interpolator = new PiecewiseBicubicSplineInterpolator()

  private val valueList: Array[Array[Double]] = {
    def doRow(row: Seq[Option[PSMBeamAnalysisResult]]): Array[Double] = {
      row.map(r => r.get.psmBeam.beamResponseNormalized.get).toArray
    }
    (0 until grid.height).map(y => doRow(grid.grid(y))).toArray
  }

  /**
    * Given x,y pixel coordinates, returns the PSM value (not normalized).
    *
    * Use: <code>val psm = function.value(x,y)</code>
    */
  private val function: PiecewiseBicubicSplineInterpolatingFunction = {
    val centerValue: Double = {
      val zero = new Point2d(0, 0)
      val c = psmList.minBy(p => p.psmBeam.center.distance(zero))
      c.psmBeam.mean_cu
    }

    val convoluted = valueList.map(row => row.map(v => centerValue / v))
    interpolator.interpolate(yCoordinateList, xCoordinateList, convoluted)
  }

  def isInBounds(x_iso: Double, y_iso: Double): Boolean = {
    val ok = { //
      (y_iso >= yCoordinateList.head) &&
      (y_iso <= yCoordinateList.last) &&
      (x_iso >= xCoordinateList.head) &&
      (x_iso <= xCoordinateList.last)
    }
    ok
  }

  def isInBounds(point_iso: Point2d): Boolean = isInBounds(point_iso.getX, point_iso.getY)

  def isInBounds(point_iso: Point2D.Double): Boolean = isInBounds(point_iso.getX, point_iso.getY)

  /**
    * Perform interpolation.
    *
    * I think it is a bug in the org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolator code, but
    * it works if the X and Y coordinates are reversed.
    *
    * @param x_iso X coordinate.
    * @param y_iso y coordinate.
    * @return value at that point.
    */
  private def interpolate(x_iso: Double, y_iso: Double): Double = function.value(y_iso, x_iso)

  def interpolate(point: Point2D.Double): Double = interpolate(point.getX, point.getY)

  /**
    * Make a normalized DICOM image.
    * @return DICOM image.
    */
  private def makeDicomImage(): DicomImage = {

    val centerBeamMean_cu = grid.centerBeam.mean_cu

    val min = {
      val pointsInBounds_pix = for (x <- 0 until trans.width; y <- 0 until trans.height; if isInBounds(trans.pix2Iso(new Point2D.Double(x, y)))) yield { trans.pix2Iso(new Point2D.Double(x, y)) }
      val pixValueList = pointsInBounds_pix.map(p => interpolate(p) / centerBeamMean_cu)
      pixValueList.min.toFloat

      // val meanList_cu = psmList.map(_.psmBeam.beamResponseNormalized.get)
      // meanList_cu.min.toFloat
    }

    def makeRow(y: Int): IndexedSeq[Float] = {
      val y_iso = trans.pix2IsoCoordY(y)
      (0 until trans.width).map(x => {
        val x_iso = trans.pix2IsoCoordX(x)
        if (isInBounds(x_iso, y_iso))
          (interpolate(x_iso, y_iso) / centerBeamMean_cu).toFloat
        else
          min
      })
    }

    val pixelArray = (0 until trans.height).map(makeRow)

    val di = new DicomImage(pixelArray)

    logger.info("PSM non-normalized center pixels: \n" + centerPixelsToString(di))

    di
  }

  val normalizedDicomImage: DicomImage = makeDicomImage()
}
