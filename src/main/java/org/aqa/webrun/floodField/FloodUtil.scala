package org.aqa.webrun.floodField

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util

import java.awt.image.BufferedImage

/**
  * Utilities peculiar to flood fields.
  */
object FloodUtil {

  /**
    * True if the al is a flood field.  Must not reference a beam and all XRayImageReceptorTranslation values must be near zero.
    * @param al For this attribute list.
    * @return True if it's a flood field.
    */
  def isFloodField(al: AttributeList): Boolean = {

    /** True if al references a beam. */
    def referencesBeam: Boolean = DicomUtil.findAllSingle(al, TagByName.ReferencedBeamNumber).nonEmpty

    /** True if there is an XRayImageReceptorTranslation and all of its values are near zero.  Criteria is that
      *  it be less than one.  Normal RTIMAGES have a large value around 500 or ~1000.
      */
    def XRayImageReceptorTranslationNearZero = {
      val attrList = DicomUtil.findAllSingle(al, TagByName.XRayImageReceptorTranslation)
      attrList.nonEmpty && attrList.flatMap(_.getDoubleValues).map(_.abs < 1).reduce(_ && _)
    }
    val is = Util.isRtimage(al) && (!referencesBeam) && XRayImageReceptorTranslationNearZero
    is
  }

  /**
    * Make a high contrast image of a flood field.  Flood field images normally have very low pixel values in the corners,
    * which uses up a significant portion of the leveling range. Because the corners are not of interest, this function
    * creates an image that classifies them as 'low level' using a single color.  The remaining color range can then be
    * used on the central part of the image.
    *
    * @param rtimage DICOM RTIMAGE
    * @return buffered image with contrast only in the center portion of the image (not the corners or extreme edges).
    */
  def highContrastImage(rtimage: AttributeList): BufferedImage = {

    val extremePixelCount = 10 // number of extremely high and extremely low pixels to drop.

    val image: DicomImage = new DicomImage(rtimage)
    val trans = new IsoImagePlaneTranslator(rtimage)

    val outsideCircleDistance_pix = 10 // make the circle this many pixels from the edge of the EPID.
    val minCoordinates = trans.pix2Iso(outsideCircleDistance_pix, outsideCircleDistance_pix)

    // The maximum distance from the center that will be used to determine whether a pixel
    // is 'near the center'.
    //
    // Do this by defining a circle that just fits inside the image by a few pixels.
    //
    // For square EPID panels the X and Y are the same, but if the EPID is not square then use
    // the smaller one. This defines a circle
    val maxIsoDistance = Math.min(minCoordinates.getX.abs, minCoordinates.getY.abs)

    /**
      * Return true if the given coordinates are inside a circle.
      * @param x X pixel coordinate.
      * @param y Y pixel coordinate.
      * @return True if in the circle.
      */
    def isInsideCircle(x: Int, y: Int): Boolean = {
      val xy = trans.pix2Iso(x, y)
      val d = Math.sqrt((xy.getX * xy.getX) + (xy.getY * xy.getY)) // distance from center
      d < maxIsoDistance
    }

    // values of pixels that are inside the circle.
    val pixelList = for (x <- 0 until image.width; y <- 0 until image.height; if isInsideCircle(x, y)) yield image.get(x, y)

    // Drop pixels that have either extremely low or high values so that they do not skew the color range.  These are often defective pixels.
    val sorted = pixelList.sorted.drop(extremePixelCount).dropRight(extremePixelCount)

    val min = sorted.head
    val max = sorted.last

    // process one row of pixels
    def doRow(row: IndexedSeq[Float]): IndexedSeq[Float] = {
      def limit(pix: Float): Float = {
        if (pix < min)
          min
        else if (pix > max)
          max
        else
          pix
      }

      row.map(limit)
    }

    // Create a new pixel array.  The pixel range is established using the pixels inside the circle.  All other
    // pixels outside the circle with values outside the range will have their values set to either the min
    // or max of the pixel range. process each row of pixels.
    val pix2 = image.pixelData.map(doRow)

    val image2 = new DicomImage(pix2)

    val bufferedImageHighContrast = image2.toDeepColorBufferedImage(0.0)

    bufferedImageHighContrast
  }

}
