package org.aqa.webrun.stakitt.leafBoundaries

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging

import java.awt.Rectangle

/**
 * Make DICOM images used for finding leaf boundaries.
 *
 * All calculations are done in pixels (as opposed to mm / isoplane).
 *
 * @param rtimage     Stakitt image.
 * @param xBorderList List of X coordinates for AOIs.
 */
case class LBDicomImages(rtimage: AttributeList, xBorderList: Seq[Double]) extends Logging {

  // set the span to be 1/2 the width of the narrowest known leaf.
  val span: Int = {
    val trans = new IsoImagePlaneTranslator(rtimage)
    trans.iso2PixDistX(2.5).round.toInt
  }

  private val dicomBorderLo = xBorderList.head.round.toInt - span

  private val dicomBorderHi = xBorderList.last.round.toInt + span

  private val rawDicomImage: DicomImage = new DicomImage(rtimage)

  /**
   * Make a DicomImage, fixing any bad pixels that are extremely high or low.
   */
  private val dicomImage: DicomImage = {

    // fix up to this many pixels
    val pixelCount = 5

    val sorted = rawDicomImage.pixelData.flatten.sorted

    // establish the low limit by taking the next to lowest pixels and test to see if the lowest pixels are much lower.
    val loLimit: Float = {
      val testThese = sorted.take(pixelCount)

      val goodLimit = (sorted.slice(pixelCount, pixelCount * 2).sum / pixelCount) * 0.9

      if (testThese.exists(p => p < goodLimit))
        goodLimit.toFloat
      else
        sorted.head
    }

    // establish the high limit by taking the next to highest pixels and test to see if the highest pixels are much higher.
    val hiLimit: Float = {
      val testThese = sorted.takeRight(pixelCount)

      val goodLimit = (sorted.dropRight(pixelCount).takeRight(pixelCount).sum / pixelCount) * 1.1

      if (testThese.exists(p => p > goodLimit))
        goodLimit.toFloat
      else
        sorted.last
    }

    def fixPix(pix: Float): Float = {
      0 match {
        case _ if pix > hiLimit =>
          Trace.trace(s"fixPix too hi $pix --> $hiLimit")
          hiLimit
        case _ if pix < loLimit =>
          Trace.trace(s"fixPix too lo $pix --> $loLimit")
          loLimit
        case _ => pix
      }
    }

    val fixedDicomImage = rawDicomImage.fun1(fixPix)
    fixedDicomImage
  }

  val dicomImageLo: DicomImage = dicomImage.getSubimage(new Rectangle(0, 0, dicomBorderLo, dicomImage.height))

  val dicomImageHi: DicomImage = {
    val rect = new Rectangle(dicomBorderHi, 0, dicomImage.width - dicomBorderHi, dicomImage.height)
    dicomImage.getSubimage(rect)
  }


}
