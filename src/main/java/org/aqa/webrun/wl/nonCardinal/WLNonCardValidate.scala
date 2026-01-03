package org.aqa.webrun.wl.nonCardinal

import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageDisplay
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.wl.WLMessage
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.webrun.wl.WLImageStatus

case class WLNonCardValidate( //
    nonCardEdge: WLNonCardEdgeAnalysis,
    nonCardBall: WLNonCardBall,
    machineWL: MachineWL,
    wlMessage: Option[WLMessage]
) {

  val preprocessedImage: DicomImage = nonCardEdge.preprocessedImage

  private case class WLError(status: WLImageStatus.Value, msg: String) {}

  private var wlNonCardStatus: Option[WLError] = None

  private def setError(sts: WLImageStatus.Value, msg: String): Unit = {
    if (sts.toString.equals(WLImageStatus.Passed.toString))
      wlMessage.foreach(_.info(msg))
    else
      wlMessage.foreach(_.warn(msg))

    wlNonCardStatus.synchronized {
      if (wlNonCardStatus.isEmpty)
        wlNonCardStatus = Some(WLError(sts, msg))
    }
  }

  def getStatus(): Option[WLImageStatus.Value] =
    wlNonCardStatus.synchronized {
      wlNonCardStatus.map(_.status)
    }

  def getErrorMessage(): Option[String] =
    wlNonCardStatus.synchronized {
      wlNonCardStatus.map(_.msg)
    }

  private val ballAOI: DicomImage = {
    val ball = WLNonCardBallAOIBounds.makeBallAOI(nonCardEdge.edgeSet, preprocessedImage)
    val normalized = ball.normalize(0.001)
    normalized
  }

  // ImageDisplay.showInMSPaint(ballAOI.toBufferedImage(Color.blue))

  /** Establish a threshold for the min-to-max pixel range.  An edge must have at least this amount of change in pixel value to be considered valid. */
  private val wholeImagePixelValueRangeThreshold_cu: Double = {
    val sorted = preprocessedImage.pixelData.flatten.sorted

    // assume that up to one entire row or column of the image has bad pixels.
    val badPixelCount = Math.max(preprocessedImage.width, preprocessedImage.height) + 5
    val sampleCount = 10

    val min = sorted.slice(badPixelCount, badPixelCount + sampleCount).sum / sampleCount
    val max = sorted.dropRight(badPixelCount).takeRight(sampleCount).sum / sampleCount
    val range = max - min

    val threshold = range * (Config.WLNonCardEdgePercentChange / 100)
    wlMessage.foreach(_.info(s"Min and max image brightness: $min   $max.    Range: $range.    Threshold = ${Config.WLNonCardEdgePercentChange / 100} * $range = $threshold"))
    threshold
  }

  /**
    * Determine if one edges is valid in the sense that it is yielding a genuine measurement that can be
    * compared against pass/fail limits.  Reject if it has insufficient contrast.
    *
    * @return Empty list on success, error message on failure.
    */
  private def validateEdge(edge: WLNonCardEdge): Seq[String] = {

    val measuredPctText: String = {
      val max = wholeImagePixelValueRangeThreshold_cu / (Config.WLNonCardEdgePercentChange / 100)
      val pct = (edge.range / max) * 100
      Util.fmtDbl(pct) + "%"
    }

    if (edge.range >= wholeImagePixelValueRangeThreshold_cu) {
      val msg = s"Edge for ${edge.name} has sufficient contrast of ${Util.fmtDbl(edge.range)} $measuredPctText . Threshold: ${Util.fmtDbl(wholeImagePixelValueRangeThreshold_cu)} (${Config.WLNonCardEdgePercentChange}%)"
      wlMessage.foreach(_.info(msg))
      Seq()
    } else {
      val msg =
        s"Edge for ${edge.name} has insufficient contrast of ${Util.fmtDbl(edge.range)} ($measuredPctText)  when it should be at least ${Util.fmtDbl(wholeImagePixelValueRangeThreshold_cu)} (${Config.WLNonCardEdgePercentChange}%)"
      setError(WLImageStatus.BoxNotFound, msg)
      Seq(msg)
    }
  }

  private def beamEnergyIsHighEnough(): Seq[String] = {
    val kvp = DicomUtil.findAllSingle(nonCardEdge.al, TagByName.KVP).head.getDoubleValues.head
    if (kvp >= Config.WLNonCardKVPLimit)
      Seq()
    else {
      val msg = s"DICOM file delivered with (insufficient) $kvp energy, when it should be at least (${Config.WLNonCardKVPLimit})"
      setError(WLImageStatus.LowEnergy, msg)
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

    val error = if (totalDiff < Config.WLNonCardSymmetryLimit) {
      val msg = s"profile difference in symmetry: $totalDiff is within the valid limit, indicating that the object found is spherical," +
        s" and therefore a valid phantom.  It must be lower than ${Config.WLNonCardSymmetryLimit} to be valid."
      wlMessage.foreach(_.info(msg))
      Seq()
    } else {
      val msg = s"profile difference in symmetry: $totalDiff is too large, indicating that the object found is non-spherical," +
        s" and therefor an invalid phantom.  It must be lower than ${Config.WLNonCardSymmetryLimit} to be valid."
      setError(WLImageStatus.BallMalformed, msg)
      Seq(msg)
    }

    error
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

    if (stdDev < Config.WLNonCardMinStdDev) {
      val msg = s"Ball are has a standard deviation of $stdDev, which is below the required ${Config.WLNonCardMinStdDev}.  Probably due to no phantom."
      setError(WLImageStatus.BallMissing, msg)
      Seq(msg)
    } else
      Seq()

  }

  private def withinTolerance(): Unit = {

    val errX_mm = nonCardEdge.trans.pix2IsoDistX(nonCardEdge.edgeSet.center_pix.getX - nonCardBall.center_pix.getX)
    val errY_mm = nonCardEdge.trans.pix2IsoDistY(nonCardEdge.edgeSet.center_pix.getY - nonCardBall.center_pix.getY)

    val error_mm = Math.sqrt((errX_mm * errX_mm) + (errY_mm * errY_mm))

    0 match {
      case _ if (error_mm <= machineWL.passLimit_mm) && getStatus().isEmpty =>
        val msg = s"Passed.  Error: $error_mm   Pass limit: ${machineWL.passLimit_mm}"
        setError(WLImageStatus.Passed, msg)
        Seq()
      case _ if (error_mm > machineWL.passLimit_mm) && getStatus().isEmpty =>
        val msg = s"Failed.  Error: $error_mm   Pass limit: ${machineWL.passLimit_mm}"
        setError(WLImageStatus.OffsetLimitExceeded, msg)
        Seq()

      case _ =>
        Seq()
    }

  }

  edgesHaveSufficientContrast()
  beamEnergyIsHighEnough()
  ballIsSufficientlyLarge()
  ballIsSymmetrical()
  withinTolerance()
  if (getStatus().isEmpty)
    setError(WLImageStatus.UnexpectedError, "Unexpected error") // this should never happen

}
