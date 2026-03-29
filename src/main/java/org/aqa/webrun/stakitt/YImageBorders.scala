package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging

import java.awt.Rectangle

case class YImageBorders(rtimage: AttributeList, xBorderList: Seq[Double]) extends Logging {

  private val dicomBorderLo = xBorderList(1).round.toInt
  private val dicomBorderHi = xBorderList.reverse(1).round.toInt

  private val dicomImage: DicomImage = new DicomImage(rtimage)

  private val dicomImageLo = dicomImage.getSubimage(new Rectangle(0, 0, dicomBorderLo, dicomImage.height))

  private val dicomImageHi = {
    val rect = new Rectangle(dicomBorderHi, 0, dicomImage.width - dicomBorderHi, dicomImage.height)
    dicomImage.getSubimage(rect)
  }

  private val dicomImageMid = dicomImage.getSubimage(new Rectangle(dicomBorderLo, 0, dicomBorderHi - dicomBorderLo, dicomImage.height))

  // set the span to be 1/2 the width of the narrowest possible leaf.
  val span: Int = {
    val trans = new IsoImagePlaneTranslator(rtimage)
    trans.iso2PixDistX(2.5).round.toInt
  }

  /**
    * Find the indices of the crossing points to the accuracy of one pixel.
    * @return List of points where the profile is closest to the mean.
    */
  private def inflectionPointsPix(profile: Seq[Float]): Seq[Int] = {

    def isInflectionPoint(index: Int): Boolean = {

      if (index > span) {
        def slopeOf(i: Int) = (profile(i) - profile(i - 1)).abs

        val slopeList = (index - span until index + span + 1).map(slopeOf)

        val isInflection = slopeList.max == slopeList(span)

        isInflection
      } else
        false
    }

    val cutoff = {
      val range = profile.max - profile.min
      profile.min + (range * 0.50)
    }

    val firstIndex = {
      val i = profile.indices.indexWhere(i => profile(i) > cutoff) - (span * 5)
      Math.max(i, span)
    }

    val lastIndex = {
      val i = profile.indices.lastIndexWhere(i => profile(i) > cutoff) + (span * 5)
      Math.min(i, dicomImage.height - (span + 1))
    }

    Trace.trace(s"================== Y indices: firstIndex: $firstIndex      lastIndex: $lastIndex") // TODO rm

    val inflectionList = profile.indices.slice(firstIndex, lastIndex).filter(isInflectionPoint)
    // val inflectionList = profile.indices.drop(span * 2).dropRight(span * 2).filter(isInflectionPoint)

    inflectionList
  }

  /**
    * Find inflection points in the profile.
    * @return List of inflection points.
    */
  private def inflectionPointsSubPix(profile: Seq[Float], name: String): Seq[Double] = {

    val list = inflectionPointsPix(profile).map(_.toDouble) // TODO

    if (true) { // TODO rm
      val scale = profile.max / 10.0
      val j = profile.map(v => v / scale)
      Trace.showChart(j, name)
    }

    list
  }

  private val profileLo = dicomImageLo.rowSums
  private val profileHi = dicomImageHi.rowSums
  private val profileMid = dicomImageMid.rowSums
  private val profileAll = new DicomImage(rtimage).rowSums

  if (false) { // TODO rm
    val drv1 = profileLo.indices.tail.map(i => profileLo(i) - profileLo(i - 1).toDouble)
    Trace.showChart(drv1, "Derivative 1")
  }

  /** Inflection points on the left */
  val yPointListLo: Seq[Double] = inflectionPointsSubPix(profileLo, "Lo") // TODO

  /** Inflection points on the right */
  val yPointListHi: Seq[Double] = inflectionPointsSubPix(profileHi, "Hi")

  /** Inflection points on the right */
  val yPointListMid: Seq[Double] = inflectionPointsSubPix(profileMid, "Mid")

  /** Inflection points on the right */
  val yPointListAll: Seq[Double] = inflectionPointsSubPix(profileAll, "All")

  if (false) { // TODO rm
    val b = dicomImageHi.toDeepColorBufferedImage(0.01)
    Trace.showInMSPaint(b)
  }
  val yPointList: Seq[Double] = yPointListAll

}
