package org.aqa.webrun.winLutz360

import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.wl.WLMessage
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.webrun.wl.WLImageStatus

object Validate {
  case class ValidationStatus(status: WLImageStatus.Value, msg: String) {}
}

case class Validate( //
                     edgeAnalysis: EdgeAnalysis,
                     ball: Ball,
                     machineWL: MachineWL,
                     wlMessage: Option[WLMessage]
                   ) {

  import Validate.ValidationStatus

  val preprocessedImage: DicomImage = edgeAnalysis.preprocessedImage

  private val ballAOI: DicomImage = {
    val ball = BallAOIBounds.makeBallAOI(edgeAnalysis.edgeSet, preprocessedImage)
    val normalized = ball.normalize(0.001)
    normalized
  }

  /** Establish a threshold for the min-to-max pixel range.  An edge must have at least this amount of change in pixel value to be considered valid. */
  private val wholeImagePixelValueRangeThreshold_cu: Double = {
    val sorted = preprocessedImage.pixelData.flatten.sorted

    // assume that up to one entire row or column of the image has bad pixels.
    val badPixelCount = Math.max(preprocessedImage.width, preprocessedImage.height) + 5
    val sampleCount = 10

    val min = sorted.slice(badPixelCount, badPixelCount + sampleCount).sum / sampleCount
    val max = sorted.dropRight(badPixelCount).takeRight(sampleCount).sum / sampleCount
    val range = max - min

    val threshold = range * (Config.WinLutz360PercentChange / 100)
    wlMessage.foreach(_.info(s"Min and max image brightness: $min   $max.    Range: $range.    Threshold = ${Config.WinLutz360PercentChange / 100} * $range = $threshold"))
    threshold
  }

  /**
   * Determine if one edges is valid in the sense that it is yielding a genuine measurement that can be
   * compared against pass/fail limits.  Reject if it has insufficient contrast.
   *
   * @return Empty list on success, error message on failure.
   */
  private def validateEdge(edge: Edge): Seq[ValidationStatus] = {

    val measuredPctText: String = {
      val max = wholeImagePixelValueRangeThreshold_cu / (Config.WinLutz360PercentChange / 100)
      val pct = (edge.pixelValueRange / max) * 100
      Util.fmtDbl(pct) + "%"
    }

    if (edge.pixelValueRange >= wholeImagePixelValueRangeThreshold_cu) {
      val msg =
        s"Edge for ${edge.name} has sufficient contrast of ${Util.fmtDbl(edge.pixelValueRange)} $measuredPctText . Threshold: ${Util.fmtDbl(wholeImagePixelValueRangeThreshold_cu)} (${Config.WinLutz360PercentChange}%)"
      wlMessage.foreach(_.info(msg))
      Seq()
    } else {
      val msg =
        s"Edge for ${edge.name} has insufficient contrast of ${
          Util
            .fmtDbl(edge.pixelValueRange)
        } ($measuredPctText)  when it should be at least ${Util.fmtDbl(wholeImagePixelValueRangeThreshold_cu)} (${Config.WinLutz360PercentChange}%)"

      Seq(ValidationStatus(WLImageStatus.BoxNotFound, msg))
    }
  }

  private def beamEnergyIsHighEnough(): Seq[ValidationStatus] = {
    val kvp = DicomUtil.findAllSingle(edgeAnalysis.al, TagByName.KVP).head.getDoubleValues.head
    if (kvp >= Config.WinLutz360KVPLimit)
      Seq()
    else {
      val msg = s"DICOM file delivered with (insufficient) $kvp energy, when it should be at least (${Config.WinLutz360KVPLimit})"
      Seq(ValidationStatus(WLImageStatus.LowEnergy, msg))
    }
  }

  /**
   * Determine if the edges are valid in the sense that they are yielding genuine measurements that can be
   * compared against pass/fail limits.  Reject edges that have insufficient contrast.
   *
   * @return Empty list on success, error message on failure.
   */
  private def edgesHaveSufficientContrast(): Seq[ValidationStatus] = {
    val list = edgeAnalysis.edgeSet.edgeList.flatMap(validateEdge)
    list
  }

  /**
   * Perform simple smoothing of given profile curve. Sum each adjacent pair of pixels.
   *
   * @param profile For this profile.
   * @return A smoothed curve.
   */
  private def smooth(profile: Seq[Float]): Seq[Float] = {
    val smoothed = profile.drop(1).zip(profile.tail).map(ab => ab._1 + ab._2)
    smoothed
  }

  private case class BallProfiles() {

    val xProfile: IndexedSeq[Float] = ballAOI.columnSums
    val yProfile: IndexedSeq[Float] = ballAOI.rowSums

    private val maxXIndex = xProfile.indexOf(xProfile.max)
    private val maxYIndex = yProfile.indexOf(yProfile.max)

    val xLeft: IndexedSeq[Float] = xProfile.take(maxXIndex + 1)
    val xRight: IndexedSeq[Float] = xProfile.drop(maxXIndex)
    val yTop: IndexedSeq[Float] = yProfile.take(maxYIndex + 1)
    val yBottom: IndexedSeq[Float] = yProfile.drop(maxYIndex)
  }

  private val ballProfiles = BallProfiles()

  /**
   * Determine ball validity in that the X profile is the same as the Y profile.
   *
   * @return None on success, error message on failure.
   */
  private def ballIsSymmetrical(): Seq[ValidationStatus] = {

    // use the minimum
    val ballRadius = Seq(ballProfiles.xLeft, ballProfiles.xRight, ballProfiles.yTop, ballProfiles.yBottom).map(_.size).min

    /**
     * Find the difference of two half profiles.  Pair values from each argument and take the
     * absolute value of the difference of each.
     *
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
      // @formatter:off
      diff(ballProfiles.xLeft.reverse, ballProfiles.xRight      ),
      diff(ballProfiles.xLeft.reverse, ballProfiles.yTop.reverse),
      diff(ballProfiles.xLeft.reverse, ballProfiles.yBottom     ),
      diff(ballProfiles.xRight       , ballProfiles.yTop.reverse),
      diff(ballProfiles.xRight       , ballProfiles.yBottom     ),
      diff(ballProfiles.yTop.reverse , ballProfiles.yBottom     )
      // @formatter:on
    ).sum

    val error = if (totalDiff < Config.WinLutz360SymmetryLimit) {
      val msg = s"profile difference in symmetry: $totalDiff is within the valid limit, indicating that the object found is spherical," +
        s" and therefore a valid phantom.  It must be lower than ${Config.WinLutz360SymmetryLimit} to be valid."
      wlMessage.foreach(_.info(msg))
      Seq()
    } else {
      val msg = s"profile difference in symmetry: $totalDiff is too large, indicating that the object found is non-spherical," +
        s" and therefor an invalid phantom.  It must be lower than ${Config.WinLutz360SymmetryLimit} to be valid."
      Seq(ValidationStatus(WLImageStatus.BallMalformed, msg))
    }

    error
  }

  /**
   * Determine ball validity in that it is sufficiently large so to be not mistaken for noise.
   *
   * @return None on success, error message on failure.
   */
  private def ballIsSufficientlyLarge(): Seq[ValidationStatus] = {
    val pixelValueList = ballAOI.pixelData.flatten

    val stdDev = ImageUtil.stdDev(pixelValueList)

    if (stdDev < Config.WinLutz360MinStdDev) {
      val msg = s"Ball standard deviation of $stdDev, which is too small (too flat).  It must be above the ${Config.WinLutz360MinStdDev} to be valid.  Probably due to no phantom."
      ValidationStatus(WLImageStatus.BallMissing, msg)
      Seq(ValidationStatus(WLImageStatus.BallMissing, msg))
    } else {
      wlMessage.foreach(_.info(s"Ball are has a sufficiently large standard deviation of $stdDev, which is above the required ${Config.WinLutz360MinStdDev}."))
      Seq()
    }

  }

  private def profilesCrossMeanTwice(profile: Seq[Float], name: String): Seq[ValidationStatus] = {
    val smoothed = smooth(profile)

    val mean = smoothed.sum / smoothed.size

    def crossesLoToHi(lo: Int): Boolean = {
      val hi = lo + 1
      ((smoothed(lo) < mean) && (smoothed(hi) > mean)) ||
        ((smoothed(lo) > mean) && (smoothed(hi) < mean))
    }

    val numberOfTimesMeanIsCrossed = smoothed.indices.dropRight(1).count(crossesLoToHi)

    val ok = numberOfTimesMeanIsCrossed == 2

    if (ok) {
      wlMessage.foreach(_.info(s"Ball $name crosses the mean exactly twice, as required."))
      Seq()
    } else {
      val timesText = if (numberOfTimesMeanIsCrossed == 1) "time" else "times"
      val msg = s"Ball $name crosses the mean of the profile $numberOfTimesMeanIsCrossed $timesText, which indicates that it does not match the expected simple rise and fall of a ball profile."
      wlMessage.foreach(_.warn(msg))
      Seq(ValidationStatus(WLImageStatus.BallProfileIsMisshapen, msg))
    }
  }

  /**
   * Check to make sure that ball profile:
   * Rises on the left side (X axis)
   * Falls on the right side (X axis)
   * Rises on the top side (Y axis)
   * Falls on the bottom side (Y axis)
   */
  private def profilesRiseAndFall(): Seq[ValidationStatus] = {
    def checkRiseFall(profile: IndexedSeq[Float], rising: Boolean, name: String): Seq[ValidationStatus] = {
      val first = profile.take(profile.size / 2).sum
      val second = profile.drop(profile.size / 2).sum
      val ok =
        ((first < second) && rising) ||
          ((first > second) && (!rising))

      val changeText = if (rising) "low to hi" else "hi to low"

      if (ok) {
        wlMessage.foreach(_.info(s"Ball profile $name changes from $changeText as expected."))
        Seq()
      } else {
        val msg = s"Ball profile $name does not change from ${changeText} as expected."
        wlMessage.foreach(_.warn(msg))
        Seq(ValidationStatus(WLImageStatus.BallProfileIsMisshapen, msg))
      }
    }

    // @formatter:off
    checkRiseFall(ballProfiles.xLeft  , rising = true , "Left X profile  ") ++
    checkRiseFall(ballProfiles.xRight , rising = false, "Right X profile ") ++
    checkRiseFall(ballProfiles.yTop   , rising = true , "Top Y profile   ") ++
    checkRiseFall(ballProfiles.yBottom, rising = false, "Bottom Y profile")
    // @formatter:on
  }

  /**
   * Determine if the final offset (distance between center of box and center of ball) is within the pass/fail limit.
   *
   * If so, set the status to Pass, otherwise set it to fail.
   */
  private def finalOffsetWithinTolerance(): Seq[ValidationStatus] = {

    val errX_mm = edgeAnalysis.trans.pix2IsoDistX(edgeAnalysis.edgeSet.center_pix.getX - ball.center_pix.getX)
    val errY_mm = edgeAnalysis.trans.pix2IsoDistY(edgeAnalysis.edgeSet.center_pix.getY - ball.center_pix.getY)

    val error_mm = Math.sqrt((errX_mm * errX_mm) + (errY_mm * errY_mm))

    if (error_mm < machineWL.passLimit_mm) {
      val msg = s"box to ball offset of $error_mm mm is less than limit of ${machineWL.passLimit_mm} mm."
      Seq(ValidationStatus(WLImageStatus.Passed, msg))
    } else {
      val msg = s"Failed.  Error: $error_mm   Pass limit: ${machineWL.passLimit_mm}"
      Seq(ValidationStatus(WLImageStatus.OffsetLimitExceeded, msg))
    }

  }

  /** Perform several validation checks. */
  private def makeStatusList(): Seq[ValidationStatus] =
    Seq( //
      edgesHaveSufficientContrast(),
      beamEnergyIsHighEnough(),
      ballIsSufficientlyLarge(),
      ballIsSymmetrical(),
      profilesCrossMeanTwice(ballProfiles.xProfile, "X Profile"),
      profilesCrossMeanTwice(ballProfiles.yProfile, "Y Profile"),
      profilesRiseAndFall(),
      finalOffsetWithinTolerance()
    ).flatten

  /** A list of error messages.  The first one should be used as the status.  If
   * all goes well, this list should contain a single status of "Passed".
   */
  val statusList: Seq[ValidationStatus] = makeStatusList()

}
