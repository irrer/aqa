package org.aqa

import com.pixelmed.dicom.AttributeList
import java.awt.geom.Point2D
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import org.aqa.webrun.phase2.Phase2Util

/**
 * Support mapping points between the isoplane and the image plane.  Isoplane values are in mm with
 * the origin at the center of the image.   Pixel plane values are in pixels with the origin in the
 * upper left corner of the image.
 */

class IsoImagePlaneTranslator(al: AttributeList) {
  private def dblOf(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head
  private def intOf(tag: AttributeTag): Int = al.get(tag).getIntegerValues.head

  private val ImagePlanePixelSpacing = Phase2Util.getImagePlanePixelSpacing(al)
  val width = intOf(TagFromName.Columns)
  val height = intOf(TagFromName.Rows)

  // Image center in pixels
  val imageCenter = new Point2D.Double(((width - 1.0) / 2), ((height - 1.0) / 2))

  val beamExpansionRatio = dblOf(TagFromName.RTImageSID) / dblOf(TagFromName.RadiationMachineSAD)

  // Multiply this value by a measurement in mm in the isoplane to get the corresponding value in pixels in the image plane.
  private val expansionFactorX = beamExpansionRatio / ImagePlanePixelSpacing.getX
  private val expansionFactorY = beamExpansionRatio / ImagePlanePixelSpacing.getY

  def iso2PixX(x: Double) = (x * expansionFactorX) + imageCenter.getX
  def iso2PixY(y: Double) = (y * expansionFactorY) + imageCenter.getY

  def iso2Pix(x: Double, y: Double): Point2D.Double = new Point2D.Double(iso2PixX(x), iso2PixY(y))
  def iso2Pix(isoPoint: Point2D.Double): Point2D.Double = iso2Pix(isoPoint.getX, isoPoint.getY)

  def pix2IsoX(x: Double) = (x - imageCenter.getX) / expansionFactorX
  def pix2IsoY(y: Double) = (y - imageCenter.getY) / expansionFactorY

  def pix2Iso(x: Double, y: Double): Point2D.Double = new Point2D.Double(pix2IsoX(x), pix2IsoY(y))
  def pix2Iso(isoPoint: Point2D.Double): Point2D.Double = pix2Iso(isoPoint.getX, isoPoint.getY)

  def equalTo(other: IsoImagePlaneTranslator) = {
    (height == other.height) &&
      (width == other.width) &&
      (beamExpansionRatio == other.beamExpansionRatio) &&
      (ImagePlanePixelSpacing.getX == other.ImagePlanePixelSpacing.getX) &&
      (ImagePlanePixelSpacing.getY == other.ImagePlanePixelSpacing.getY)
  }

  override def toString = {
    "SID/SAD" + beamExpansionRatio.formatted("%12.8f") +
      "    SID: " + dblOf(TagFromName.RTImageSID).formatted("%7.5f") + "    SAD: " + dblOf(TagFromName.RadiationMachineSAD).formatted("%7.5f") +
      "    height: " + height + ", " + "    width: " + width + ", " +
      "    ImagePlanePixelSpacing: " + ImagePlanePixelSpacing.getX.formatted("%7.5f") + ", " + ImagePlanePixelSpacing.getY.formatted("%7.5f")
  }

  def circleRadiusInPixels = {
    val radius_mm = Config.SymmetryAndFlatnessDiameter_mm / 2
    val imagePlaneCenterInPixels = iso2Pix(0, 0)
    val radiusInPixels = iso2Pix(radius_mm, radius_mm).distance(imagePlaneCenterInPixels)
    radiusInPixels
  }

}

