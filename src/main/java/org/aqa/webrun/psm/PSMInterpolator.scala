package org.aqa.webrun.psm

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolatingFunction
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolator
import org.aqa.webrun.psm.PSMUtil.centerPixelsToString
import org.aqa.Logging

import java.awt.geom.Point2D

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
      row.map(r => r.get.psmBeam.mean_cu).toArray
    }
    (0 until grid.height).map(y => doRow(grid.grid(y))).toArray
  }

  /**
    * Given x,y pixel coordinates, returns the PSM value (not normalized).
    *
    * Use: <code>val psm = function.value(x,y)</code>
    */
  private val function: PiecewiseBicubicSplineInterpolatingFunction = {
    interpolator.interpolate(yCoordinateList, xCoordinateList, valueList)
  }

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
    val min = psmList.map(_.psmBeam.mean_cu).min.toFloat

    def makeRow(y: Int): IndexedSeq[Float] = {
      val y_iso = trans.pix2IsoCoordY(y)
      (0 until trans.width).map(x => {
        val x_iso = trans.pix2IsoCoordX(x)
        if ( //
          (y_iso >= yCoordinateList.head) &&
          (y_iso <= yCoordinateList.last) &&
          (x_iso >= xCoordinateList.head) &&
          (x_iso <= xCoordinateList.last)
        )
          interpolate(x_iso, y_iso).toFloat
        else
          min
      })
    }

    val pixelArray = (0 until trans.height).map(makeRow)

    val di = new DicomImage(pixelArray)

    logger.info("PSM non-normalized center pixels: \n" + centerPixelsToString(di))

    di
  }

  val dicomImage: DicomImage = makeDicomImage()

  val normalizedDicomImage: DicomImage = PSMUtil.normalize(trans, dicomImage)
}
