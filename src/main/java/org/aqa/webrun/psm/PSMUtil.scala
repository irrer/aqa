package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.util.UMROGUID
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging

import java.awt.geom.Point2D
import java.awt.Rectangle
import javax.vecmath.Point2i

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
    * Normalize an image to it's central pixels.
    * @param trans Iso to pixel plane.
    * @param image Pixels to normalize.
    * @return
    */
  def normalize(trans: IsoImagePlaneTranslator, image: DicomImage): DicomImage = {

    val centerPixelList = pixelCoordinatesWithinRadius(trans, image, new Point2D.Double(trans.width / 2, trans.height / 2))

    val meanOfCenter = centerPixelList.map(xy => image.get(xy.getX, xy.getY)).sum / centerPixelList.size

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
    val size = 8
    val rectangle = new Rectangle((dicomImage.width - size) / 2, (dicomImage.height - size) / 2, size, size)
    val center = dicomImage.getSubimage(rectangle)
    center.pixelsToText
  }

  /**
    * Perform a division of pixels : a / b, accommodating divide by 0 by using 1 instead of 0.
    *
    * @param a Numerator
    * @param b Denoominator
    * @return a / b .  If b is 0, then return a
    */
  def funcDiv(a: Float, b: Float): Float = {
    b match {
      case 0 => a
      case _ => a / b
    }
  }

  /**
   * Make DICOM out of a DicomImage.
   *
   * This DICOM will have a new SOPInstanceUID and SeriesInstanceUID.  It uses 16-bit pixels.  It will have the same time and date stamps as the prototype.
   *
   * @param dicomImage PSM values.
   * @param prototype One of the original PSM images.
   * @return
   */
  def DicomImageToDicom(dicomImage: DicomImage, prototype: AttributeList, RTImageLabel: String, RTImageDescription: String): AttributeList = {

    val al = DicomUtil.clone(prototype)

    def set(tag: AttributeTag, value: Any): Unit = {
      val attr = AttributeFactory.newAttribute(tag)
      attr.removeValues()

      value match {
        case str: String => attr.addValue(str)
        case dbl: Double => attr.addValue(dbl)
        case flt: Float  => attr.addValue(flt)
        case int: Int    => attr.addValue(int)
      }

      al.put(attr)
    }

    val sopUid = UMROGUID.getUID
    set(TagByName.MediaStorageSOPInstanceUID, sopUid)
    set(TagByName.SOPInstanceUID, sopUid)
    set(TagByName.SeriesInstanceUID, UMROGUID.getUID)
    set(TagByName.RTImageLabel, RTImageLabel)
    set(TagByName.RTImageDescription, RTImageDescription)
    set(TagByName.ReferencedBeamNumber, -1)

    val hiPix = 0xffff

    val RescaleIntercept = dicomImage.minPixelValue
    val RescaleSlope = (dicomImage.maxPixelValue - dicomImage.minPixelValue) / hiPix

    set(TagByName.RescaleIntercept, RescaleIntercept)
    set(TagByName.RescaleSlope, RescaleSlope)

    def toPix(psmValue: Float): Short = {
      val i = ((psmValue - RescaleIntercept) / RescaleSlope).round
      (i & 0xffff).toShort
    }

    val newPixels = dicomImage.pixelData.flatten.map(toPix).toArray
    val otherWordAttribute = new OtherWordAttribute(TagByName.PixelData)
    otherWordAttribute.setValues(newPixels)
    al.remove(TagByName.PixelData)
    al.put(otherWordAttribute)

    al
  }



}
