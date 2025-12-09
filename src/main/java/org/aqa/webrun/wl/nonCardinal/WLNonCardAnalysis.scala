package org.aqa.webrun.wl.nonCardinal

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLPreprocessImage
import org.aqa.BiCubicImage
import org.aqa.Util

case class WLNonCardAnalysis(extendedData: ExtendedData, al: AttributeList, wlRunReq: WLRunReq) {

  // Invert the pixels if necessary.
  private val preprocessedImage = WLPreprocessImage(al, None).preprocessedImage

  private val nonCardinal = new WLNonCardEdgeAnalysis(preprocessedImage, al)
  private val biCubicImage = BiCubicImage(preprocessedImage)
  private val wlNonCardBall = WLNonCardBall(nonCardinal.edgeSet: WLNonCardEdgeSet, preprocessedImage: DicomImage, biCubicImage: BiCubicImage)

  /** Establish a threshold for the min-to-max pixel range.  An edge must have at least this amount of change in pixel value to be considered valid. */
  private val wholeImagePixelValueRangeThreshold_cu: Double = {
    val sorted = preprocessedImage.pixelData.flatten.sorted

    // assume that up to one entire row or column of the image has bad pixels.
    val badPixelCount = Math.max(preprocessedImage.width, preprocessedImage.height) + 5
    val sampleCount = 10

    val min = sorted.slice(badPixelCount, badPixelCount + sampleCount).sum / sampleCount
    val max = sorted.dropRight(badPixelCount).takeRight(sampleCount).sum / sampleCount

    val t = (max - min) * 0.9
    t
  }

  /**
    * Determine if one edges is valid in the sense that it is yielding a genuine measurement that can be
    * compared against pass/fail limits.  Reject if it has insufficient contrast.
    *
    * @return Empty list on success, error message on failure.
    */
  private def validateEdge(edge: WLNonCardEdge): Seq[String] = {
    if (edge.range >= wholeImagePixelValueRangeThreshold_cu)
      Seq()
    else {
      val msg = s"${edge.name} has insufficient contrast of . ${Util.fmtDbl(edge.range)} when it should be at least ${Util.fmtDbl(wholeImagePixelValueRangeThreshold_cu)}"
      Seq(msg)
    }
  }

  /**
    * Determine if the edges are valid in the sense that they are yielding genuine measurements that can be
    * compared against pass/fail limits.  Reject edges that have insufficient contrast.
    *
    * @return Empty list on success, error message on failure.
    */
  private def edgesHaveSufficientContrast(): Seq[String] = {
    val list = nonCardinal.edgeSet.edgeList.flatMap(validateEdge)
    list
  }

  /**
    * Determine ball validity in that the X profile is the same as the Y profile.
    *
    * @return None on success, error message on failure.
    */
  private def ballIsSymmetrical(): Seq[String] = {
    ???
  }

  /**
    * Determine ball validity in that it is sufficiently large so to be not mistaken for noise.
    *
    * @return None on success, error message on failure.
    */
  private def ballIsSufficientlyLarge(): Seq[String] = {
    ???
  }

  /**
    * Make a list of error messages
    *
    * @return List of errors.  If empty, then everything is ok.
    */
  private def makeErrorList(): Seq[String] = {
    val list = Seq( //
      edgesHaveSufficientContrast(),
      ballIsSufficientlyLarge(),
      ballIsSymmetrical()
    ).flatten
    list
  }

  private val errorList = makeErrorList()

  if (errorList.isEmpty) {
    // TODO make result
  }
  else {

  }

}
