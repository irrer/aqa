package org.aqa.webrun.gapSkew

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.Profile
import org.aqa.Util

/**
  * A single graph point
  * @param y_mm Y position in  mm.
  * @param cu CU at that point.
  */
case class ProfilePoint(y_mm: Double, cu: Double) {}

/**
  * Make a profile that can be graphed to show the user what the edge looks
  * like.  The results are in mm in isoplane and CU.
  */
object GapSkewProfile {

  // Number of points to put in the profile.  This was chosen as enough to show detail without
  // being overly resource intensive.
  private val numberOfPoints = 600

  // height of profiled area in mm
  private val height_mm = 30.0

  // maximum height.  Actual height may be smaller because of cropping.
  /**
    * Make a profile that can be graphed to show the user what the edge looks like.  X values of
    * Point2d are the Y position in mm, Y values are the CU.
    *
    * @param leaf Make profile for this leaf.
    * @param translator For mapping iso --> pix.
    * @param dicomImage Original image content.
    * @return List of y-position, CU pairs.
    */
  def gapSkewProfile(position_mm: Double, xPosition_mm: Double, width_mm: Double, translator: IsoImagePlaneTranslator, dicomImage: DicomImage): Seq[ProfilePoint] = {
    //private def gapSkewProfile(endPosition_pix: Double, x_pix: Double, width: Double): Seq[Point2d] = {

    val maxHeight = translator.iso2PixDistY(height_mm)
    val endPosition_pix = translator.iso2PixCoordY(position_mm)

    // Calculate dimensions of rectangle that encloses profiling area.
    val y_pix = Math.max(endPosition_pix - maxHeight / 2, 0.0) // use Math.max to crop as necessary
    val height_pix = {
      val bottom_pix = Math.min(maxHeight + y_pix, dicomImage.height - 1)
      bottom_pix - y_pix
    }
    val x_pix = translator.iso2PixCoordX(xPosition_mm)
    val width_pix = translator.iso2PixDistY(width_mm)

    val rectangle_pix = Util.rectD(x_pix, y_pix, width_pix, height_pix)

    val profileValueSeq = dicomImage.getSubimage(rectangle_pix).rowSums.map(_ / width_pix).map(_.toFloat)

    val spline = Profile(profileValueSeq).cubicSpline

    val increment = height_pix / numberOfPoints

    def yToProfileY(i: Int): Double = -translator.pix2IsoCoordY((i * increment) + rectangle_pix.getY)

    def yToCu(i: Int): Double = spline.evaluate(i * increment)

    def yToDerivative1(i: Int): Double = (spline.evaluate(i * increment) - spline.evaluate((i + 1) * increment)).abs
    def yToDerivative2(i: Int): Double = {
      val a = spline.evaluate((i - 1) * increment)
      val b = spline.evaluate(i * increment)
      val c = spline.evaluate((i + 1) * increment)
      (c - b) - (b - a)
    }
    // (0 until numberOfPoints).map(i => ProfilePoint(yToProfileY(i), yToDerivative1(i)))

    (0 until numberOfPoints).map(i => ProfilePoint(yToProfileY(i), yToCu(i)))  // TODO put back
  }

}
