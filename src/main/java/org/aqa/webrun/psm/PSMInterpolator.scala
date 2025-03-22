package org.aqa.webrun.psm

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolatingFunction
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolator
import org.aqa.webrun.psm.PSMUtil.centerPixelsToString
import org.aqa.Logging

class PSMInterpolator(psmList: Seq[PSMBeamAnalysisResult]) extends Logging {
  val trans: IsoImagePlaneTranslator = new IsoImagePlaneTranslator(psmList.head.rtimage)

  private val sorted = PSMUtil.layoutSpatiallyPSMResult(psmList)

  private def mean(array: Seq[Double]): Double = array.sum / array.size

  private def meanX(xIndex: Int): Double = {
    mean(sorted.map(row => row(xIndex).psmBeam.xCenter_mm))
  }

  private def meanY(yIndex: Int): Double = {
    mean(sorted(yIndex).map(psmBeam => psmBeam.psmBeam.yCenter_mm))
  }

  private val xCoordinateList = sorted.head.indices.map(meanX).map(trans.iso2PixCoordX).toArray
  private val yCoordinateList = sorted.indices.map(meanY).map(trans.iso2PixCoordY).toArray

  private val interpolator = new PiecewiseBicubicSplineInterpolator()
  private val valueList = {
    def toCol(colIndex: Int): Array[Double] = sorted.map(row => row(colIndex).psmBeam.mean_cu).toArray
    sorted.head.indices.map(toCol).toArray
  }

  /**
    * Given x,y pixel coordinates, returns the PSM value (not normalized).
    *
    * Use: <code>val psm = function.value(x,y)</code>
    */
  val function: PiecewiseBicubicSplineInterpolatingFunction = interpolator.interpolate(xCoordinateList, yCoordinateList, valueList)

  /**
    * Make a normalized DICOM image.
    * @return DICOM image.
    */
  private def makeDicomImage(): DicomImage = {
    val min = psmList.map(_.psmBeam.mean_cu).min.toFloat

    def makeRow(y: Int): IndexedSeq[Float] = {
      (0 until trans.width).map(x => {
        if ((y > yCoordinateList.head) && (y < yCoordinateList.last) && (x > xCoordinateList.head) && (x < xCoordinateList.last)) {
          function.value(x, y).toFloat
        } else
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
