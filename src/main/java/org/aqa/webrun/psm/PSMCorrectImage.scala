package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.apache.commons.math3.analysis.interpolation.BicubicInterpolator
import org.apache.commons.math3.analysis.BivariateFunction
import org.aqa.db.PSMBeam
import org.aqa.Logging
import org.aqa.Util

import scala.annotation.tailrec

case class PSMCorrectImage(psmList: Seq[PSMBeam], floodField: AttributeList) extends Logging {

  /*


1.	Image orientation: Not including the pixels in the corners or around the edges of the image, I would expect that the columns(s) of dead pixels should be consistent in orientation and location between the PSM image, flood field image (FF) and raw image (WD-0 with FF removed). Looking at the profiles that you sent for each orientation, I would say that orientation B looks like a Raw image (WD-0 image with FF image removed), but that orientation A looks like a Beam Response result, which is the WD-0 image with PSM removed and is what we are ultimately looking to produce. This wouldn’t make sense to me if it were just an orientation change. Can you confirm what was done with these two scenarios?

2.	Profiles: With the different images I can tell if they look correct by viewing a profile, this needs only to be the central column and central row.

3.	Calculation steps: These look correct to me. To check, let us standardise on naming:
WD = whole detector. This is the image directly from the EPID
FF =  flood field. This is the calibration image automatically applied by the linac to create the WD image.
WD*FF =Raw image. This is the EPID image with the flood field image removed.
PSM = the alternate calibration image to the flood field that we need for QA applications.
Raw image/PSM = BR (Beam Response). The BR image is the image that we require to perform our symmetry analysis on.

From each of the images above, can you please send me a central column and a central row profile named as per naming convention above? I feel that I could then confirm that each step looks t have been performed as expected. For these profiles, ignore the dead pixels and zoom in on the rest of the pixels. If you send in excel then I can do this myself.

4.	Normalisation: The PSM is effectively a relative correction across the image. As such, its centre pixel (or the average of  small group of central pixels) should be 1. Similarly, the resultant BR image should also have centre pixel of 1.

5.	BR shape. The shape of the BR should appear to be perfectly smooth, with only one point of inflection at the centre of the image. If this isn’t the case then there is either an error in the PSM image or in how it has been applied.

6.	DICOM conversion. The level of rounding required for the DICOM conversion may be too excessive for our application. Ill be able to tell if you can send through central column and central row profiles of the final DICOM image along with similar profiles to the before conversion image.



   */

  /**
    * Put the beams into a 2-dimensional array that is the same as their spacial layout.
    * @return Spatially sorted beams.
    */
  private def layoutSpatially(): Seq[Seq[PSMBeam]] = {

    case class Row(beamList: Seq[PSMBeam]) {
      def xSorted: Seq[PSMBeam] = beamList.sortBy(_.xCenter_mm)

      override def toString: String = {
        def b2S(beam: PSMBeam): String = s"""${beam.xCenter_mm.round.formatted("%3d")},${beam.yCenter_mm.round.formatted("%3d")} :: ${beam.mean_cu.formatted("%5.2f")}"""
        s"${xSorted.map(b2S).mkString("     ")}"
      }
    }

    @tailrec
    def build(beamList: Seq[PSMBeam], rowList: Seq[Row] = Seq()): Seq[Row] = {

      /** Centers (either X or Y) must be this close in mm to be considered to be in the same row or column. */
      val tolerance_mm = 5.0

      /**
        * Determine if beams are in the same column.
        * @param a one beam
        * @param b the other beam
        * @return True if they are close together in the Y axis.
        */
      def yProximal(a: PSMBeam, b: PSMBeam): Boolean = (a.yCenter_mm - b.yCenter_mm).abs < tolerance_mm

      if (beamList.isEmpty)
        rowList
      else {
        val inOut = beamList.groupBy(b => yProximal(b, beamList.head))
        val row = Row(inOut(true).sortBy(_.xCenter_mm))
        val out: Seq[PSMBeam] = if (inOut.contains(false)) inOut(false) else Seq()
        build(out, rowList :+ row)
      }

    }

    val rowList = build(psmList)

    logger.info("List of sorted values:\n" + rowList.mkString("\n"))

    val sorted = rowList.sortBy(_.beamList.head.yCenter_mm).map(_.xSorted)

    sorted
  }

  /**
    * Make the PSM image from the list of PSM images using bivariate interpolation.
    * @return PSM image.
    */
  def makePsmImage(psmBeamList: Seq[PSMBeam], trans: IsoImagePlaneTranslator): DicomImage = {

    val sorted = PSMUtil.layoutSpatiallyPSMBeam(psmBeamList)

    def mean(array: Seq[Double]): Double = array.sum / array.size

    def meanX(xIndex: Int): Double = mean(sorted.map(row => row(xIndex).xCenter_mm))

    def meanY(yIndex: Int): Double = mean(sorted(yIndex).map(psmBeam => psmBeam.yCenter_mm))

    val xCoordinateList = sorted.head.indices.map(meanX).toArray
    val yCoordinateList = sorted.indices.map(meanY).toArray

    logger.info(s"mean X coordinates: ${xCoordinateList.map(Util.fmtDbl).mkString("   ")}       mean Y coordinates: ${yCoordinateList.map(Util.fmtDbl).mkString("   ")}")

    val valueArray = sorted.map(row => row.map(_.mean_cu).toArray).toArray

    val function: BivariateFunction = new BicubicInterpolator().interpolate(xCoordinateList, yCoordinateList, valueArray)

    val pixelArray = (0 until trans.height).map(y =>
      (0 until trans.width).map(x => {
        val x_iso = trans.pix2IsoCoordX(x)
        val y_iso = trans.pix2IsoCoordY(y)
        function.value(x_iso, y_iso).toFloat
      })
    )

    val psmImage = new DicomImage(pixelArray)

    psmImage
  }

}
