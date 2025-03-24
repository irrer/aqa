package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.apache.commons.math3.analysis.interpolation.PiecewiseBicubicSplineInterpolator
import org.aqa.Config
import org.aqa.Logging

import java.awt.geom.Point2D
import java.awt.Rectangle
import javax.vecmath.Point2i
import scala.annotation.tailrec

object PSMUtil extends Logging {

  /**
    * Get a list of pixel coordinates that are within the PSM radius of the given center.
    * @param trans image plane translator
    * @param dicomImage for this DICOM image.
    * @param center_pix Center in pixel coordinates.
    * @return List of pixel coordinates.
    */
  def pixelCoordinatesWithinRadius(trans: IsoImagePlaneTranslator, dicomImage: DicomImage, center_pix: Point2D.Double): Seq[Point2i] = {

    val center_iso = trans.pix2Iso(center_pix)

    def isNear(x: Int, y: Int): Boolean = {
      val point_pix = new Point2D.Double(x, y)
      val point_iso = trans.pix2Iso(point_pix)
      center_iso.distance(point_iso) <= Config.PSMRadius_mm
    }

    val xDist_pix = trans.iso2PixDistX(Config.PSMRadius_mm) + 3
    val yDist_pix = trans.iso2PixDistY(Config.PSMRadius_mm) + 3

    val list =
      for (
        x <- (center_pix.getX - xDist_pix).toInt to (center_pix.getX + xDist_pix).toInt;
        y <- (center_pix.getY - yDist_pix).toInt to (center_pix.getY + yDist_pix).toInt
        if (x >= 0) && (y >= 0) && (x < dicomImage.width) && (y < dicomImage.height) && isNear(x, y)
      ) yield new Point2i(x, y)

    list
  }

  /**
    * Get a list of pixel coordinates that are within the PSM radius of the given center.
    * @param rtimage for this DICOM image.
    * @param center_pix Center in pixel coordinates.
    * @return List of pixel coordinates.
    */
  def pixelCoordinatesWithinRadius(rtimage: AttributeList, center_pix: Point2D.Double): Seq[Point2i] = {
    pixelCoordinatesWithinRadius(new IsoImagePlaneTranslator(rtimage), new DicomImage(rtimage), center_pix)
  }

  /**
    * Put the beams into a 2-dimensional array that is the same as their spacial layout.
    * @return Spatially sorted beams.
    */
  private def layoutSpatially(psmList: Seq[PSMBeamAnalysisResult]): Seq[Seq[PSMBeamAnalysisResult]] = {

    case class Row(beamList: Seq[PSMBeamAnalysisResult]) {
      def xSorted: Seq[PSMBeamAnalysisResult] = {
        beamList.sortBy(beam => beam.psmBeam.xCenter_mm)
      }
    }

    @tailrec
    def build(beamList: Seq[PSMBeamAnalysisResult], rowList: Seq[Row] = Seq()): Seq[Row] = {

      /** Centers (either X or Y) must be this close in mm to be considered to be in the same row or column. */
      val tolerance_mm = 5.0

      /**
        * Determine if beams are in the same column.
        * @param a one beam
        * @param b the other beam
        * @return True if they are close together in the Y axis.
        */
      def yProximal(a: PSMBeamAnalysisResult, b: PSMBeamAnalysisResult): Boolean = (a.psmBeam.yCenter_mm - b.psmBeam.yCenter_mm).abs < tolerance_mm

      if (beamList.isEmpty)
        rowList.sortBy(r => r.xSorted.head.psmBeam.yCenter_mm)
      else {
        val inOut = beamList.groupBy(b => yProximal(b, beamList.head))
        val row = Row(inOut(true).sortBy(beam => beam.psmBeam.yCenter_mm))
        val out: Seq[PSMBeamAnalysisResult] = if (inOut.contains(false)) inOut(false) else Seq()
        build(out, rowList :+ row)
      }

    }

    val rowList = build(psmList)

    val sorted = rowList.sortBy(_.xSorted.head.psmBeam.yCenter_mm)

    val list = sorted.map(row => row.xSorted)
    list
  }

  def layoutSpatiallyPSMResult(resultList: Seq[PSMBeamAnalysisResult]): Seq[Seq[PSMBeamAnalysisResult]] = {
    layoutSpatially(resultList)
  }

  /**
    * Normalize an image to it's central pixels.
    * @param trans Iso to pixel plane.
    * @param image Pixels to normalize.
    * @return
    */
  def normalize(trans: IsoImagePlaneTranslator, image: DicomImage): DicomImage = {

    val centerPixelList = pixelCoordinatesWithinRadius(trans, image, new Point2D.Double(trans.width / 2, trans.height / 2))

    val meanOfCenter = centerPixelList.map(xy => image.get(xy.getX, xy.getY)).sum / centerPixelList.size

    val j = image.pixelData.flatten.sorted.take(centerPixelList.size)
    val minMean = image.pixelData.flatten.sorted.take(centerPixelList.size).sum / centerPixelList.size
    val j1 = image.pixelData.flatten.sorted.drop(centerPixelList.size).take(centerPixelList.size).sum / centerPixelList.size

    def makeRow(y: Int): IndexedSeq[Float] = (0 until image.width).map(x => image.get(x, y) / meanOfCenter)

    val scaledPixels = (0 until image.height).map(makeRow)

    new DicomImage(scaledPixels)
  }

  /**
    * Format the center pixels of an image to text.  Mostly for debugging.
    * @param dicomImage For this image.
    * @return Human-readable text.
    */
  def centerPixelsToString(dicomImage: DicomImage): String = {
    val size = 10
    val rectangle = new Rectangle((dicomImage.width - size) / 2, (dicomImage.height - size) / 2, size, size)
    val center = dicomImage.getSubimage(rectangle)
    center.pixelsToText
  }

  /**
    * Make a DICOM image representing the normalized PSM.
    *
    * It would be nice to do the WHOLE image, but bicubic spline does not support that. It will
    * only show the points that are within the bounds of the 42 PSM centers. It's a math thing.
    *
    * @param psmList List of PSM results.
    * @return
    */
  def XmakePSMImage(psmList: Seq[PSMBeamAnalysisResult]): DicomImage = {

    val trans = new IsoImagePlaneTranslator(psmList.head.rtimage)

    val sorted = PSMUtil.layoutSpatiallyPSMResult(psmList)

    def mean(array: Seq[Double]): Double = array.sum / array.size

    def meanX(xIndex: Int): Double = {
      mean(sorted.map(row => row(xIndex).psmBeam.xCenter_mm))
    }

    def meanY(yIndex: Int): Double = {
      mean(sorted(yIndex).map(psmBeam => psmBeam.psmBeam.yCenter_mm))
    }

    val xCoordinateList = sorted.head.indices.map(meanX).map(trans.iso2PixCoordX).toArray
    val yCoordinateList = sorted.indices.map(meanY).map(trans.iso2PixCoordY).toArray

    val interpolator = new PiecewiseBicubicSplineInterpolator()

    val valueList = {
      def toCol(colIndex: Int): Array[Double] = sorted.map(row => row(colIndex).psmBeam.mean_cu).toArray
      sorted.head.indices.map(toCol).toArray
    }

    val function = interpolator.interpolate(xCoordinateList, yCoordinateList, valueList)

    val Rows = psmList.head.rtimage.get(TagByName.Rows).getIntegerValues.head
    val Columns = psmList.head.rtimage.get(TagByName.Columns).getIntegerValues.head

    val min = psmList.map(_.psmBeam.mean_cu).min.toFloat

    def makeRow(y: Int): IndexedSeq[Float] = {
      (0 until Columns).map(x => {
        if ((y > yCoordinateList.head) && (y < yCoordinateList.last) && (x > xCoordinateList.head) && (x < xCoordinateList.last)) {
          function.value(x, y).toFloat
        } else
          min
      })
    }

    val pixelArray = (0 until Rows).map(makeRow)

    val di = new DicomImage(pixelArray)

    logger.info("PSM original center pixels: \n" + centerPixelsToString(di))

    val diNormalized = PSMUtil.normalize(trans, di)

    logger.info("PSM normalized center pixels: \n" + centerPixelsToString(diNormalized))
    diNormalized
  }

}
