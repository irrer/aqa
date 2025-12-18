package org.aqa.webrun.wl.nonCardinal

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageDisplay
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.wl.WLMessage
import org.aqa.Config
import org.aqa.Util

import java.awt.Color

case class WLNonCardValidate( //
    // extendedData: ExtendedData,
    // al: AttributeList,
    preprocessedImage: DicomImage,
    nonCardEdge: WLNonCardEdgeAnalysis,
    nonCardBall: WLNonCardBall,
    // wlRunReq: WLRunReq,
    wlMessage: Option[WLMessage]
) {

  /** Boundary containing ball. */
  private val ballAOI: DicomImage = {
    val img = WLNonCardBallAOIBounds.makeBallAOI(nonCardEdge.edgeSet, preprocessedImage)

    val sorted = img.pixelData.flatten.sorted
    val drop = 10
    val take = 10
    val minPixel = sorted.slice(drop, drop + take).sum / take
    val maxPixel = sorted.dropRight(drop).takeRight(take).sum / take
    val range = maxPixel - minPixel
    val normalizedImg = new DicomImage(img.transform(f => (f - minPixel) / range))
    normalizedImg
  }

  if (true) { // TODO rm
    ImageDisplay.showInMSPaint(ballAOI.toBufferedImage(Color.blue))
  }

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

    if (edge.range >= wholeImagePixelValueRangeThreshold_cu) {
      val msg = s"Edge for ${edge.name} has sufficient contrast of ${Util.fmtDbl(edge.range)} . Threshold: ${Util.fmtDbl(wholeImagePixelValueRangeThreshold_cu)}"
      wlMessage.foreach(_.warn(msg))
      Seq()
    } else {
      val msg = s"Edge for ${edge.name} has insufficient contrast of ${Util.fmtDbl(edge.range)} when it should be at least ${Util.fmtDbl(wholeImagePixelValueRangeThreshold_cu)}"
      wlMessage.foreach(_.warn(msg))
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
    val list = nonCardEdge.edgeSet.edgeList.flatMap(validateEdge)
    list
  }

  /**
    * Determine ball validity in that the X profile is the same as the Y profile.
    *
    * @return None on success, error message on failure.
    */
  private def ballIsSymmetrical(): Seq[String] = {

    val xProfile = ballAOI.columnSums
    val yProfile = ballAOI.rowSums

    val maxXIndex = xProfile.indexOf(xProfile.max)
    val maxYIndex = yProfile.indexOf(yProfile.max)

    val xLeft = xProfile.take(maxXIndex + 1).reverse
    val xRight = xProfile.drop(maxXIndex)
    val yTop = yProfile.take(maxYIndex + 1).reverse
    val yBottom = yProfile.drop(maxYIndex)

    if (false) { // TODO rm

      ImageDisplay.showChart(xProfile.map(_.toDouble), title = "xProfile")
      ImageDisplay.showChart(yProfile.map(_.toDouble), title = "yProfile")

      ImageDisplay.showChart(xLeft.map(_.toDouble), title = "xLeft")
      ImageDisplay.showChart(xRight.map(_.toDouble), title = "xRight")
      ImageDisplay.showChart(yTop.map(_.toDouble), title = "yTop")
      ImageDisplay.showChart(yBottom.map(_.toDouble), title = "yBottom")
    }

    // use the minimum
    val ballRadius = Seq(xLeft, xRight, yTop, yBottom).map(_.size).min

    /**
      * Find the difference of two half profiles.  Pair values from each argument and take the
      * absolute value of the difference of each.
      * @param a One half profile to compare.
      * @param b The other half profile to compare.
      * @return A value indicating how similar they are.  A smaller value means more similar.
      */
    def diff(a: Seq[Float], b: Seq[Float]): Float = {
      // note that dividing by the ball radius makes the size of the ball and the field irrelevant.
      (0 until ballRadius).map(i => (a(i) - b(i)).abs).sum / ballRadius
    }

    // Compare all combinations of profile halves.  They should be fairly close.
    val totalDiff = Seq(
      diff(xLeft, xRight),
      diff(xLeft, yTop),
      diff(xLeft, yBottom),
      diff(xRight, yTop),
      diff(xRight, yBottom),
      diff(yTop, yBottom)
    ).sum

    Trace.trace(s"profile totalDiff: $totalDiff")

    Seq() // TODO compare against configured value
  }

  /**
    * Determine ball validity in that it is sufficiently large so to be not mistaken for noise.
    *
    * @return None on success, error message on failure.
    */
  private def ballIsSufficientlyLarge(): Seq[String] = {
    val pixelValueList = ballAOI.pixelData.flatten

    val stdDev = ImageUtil.stdDev(pixelValueList)

    Trace.trace(s"stdDev: $stdDev")

    if (stdDev < Config.WLNonCardMaxStdDev) {
      val msg = s"Ball are has a standard deviation of $stdDev, which is below the required ${Config.WLNonCardMaxStdDev}.  Probably due to no phantom."
      wlMessage.foreach(_.warn(msg))
      Seq(msg)
    } else
      Seq()

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

  /** List of errors found.  Empty means no errors. */
  val errorList: Seq[String] = makeErrorList()
}
