package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage

/**
  * If necessary, invert the pixels so that the majority of them are 'bright'.  Also detect,
  * track, and correct any bad pixels.
  *
  * @param rtimage Raw image
  * @param imageName Name of image for logging purposes.
  */
case class WLPreprocessImage(rtimage: AttributeList, imageName: String) {

  /**
    * Get the raw pixels.  Ensure that the majority of the pixels are large.  If they are
    * not, then invert pixels so that the small become large and the large become small.
    *
    * @return Pixel array.
    */
  private def fetchPixels(): IndexedSeq[IndexedSeq[Float]] = {

    val di = new DicomImage(rtimage)

    // Assume that this is the maximum number of bad pixels that have values unreasonably high and
    // unreasonably low.  This is used to establish the general range of pixel values.
    val count = 20

    // Drop some high and low pixels to make sure that a few bad pixels do not skew the mean.
    val sorted = di.pixelData.flatten.sorted.drop(count).dropRight(count)

    val mean = sorted.sum / sorted.size

    val belowMeanPixelCount = sorted.indexWhere(_ > mean)

    val minPlusMax = di.minPixelValue + di.maxPixelValue

    val pixelData: IndexedSeq[IndexedSeq[Float]] = {
      if (belowMeanPixelCount > (sorted.size / 2)) {
        def invert(pix: Float): Float = minPlusMax - pix

        val invertedDicomImage = di.fun1(invert)
        invertedDicomImage.pixelData.map(_.toIndexedSeq).toIndexedSeq
      } else
        di.pixelData.map(_.toIndexedSeq).toIndexedSeq
    }

    pixelData
  }

  private val imageWithBadPixels: DicomImage = new DicomImage(fetchPixels())

  /** Bad pixel information. */
  val badPixels: WLBadPixels = WLBadPixels(imageWithBadPixels, imageName)

  /** Image that should be analyzed. */
  val preprocessedImage: DicomImage = new DicomImage(badPixels.correctedImage)

}
