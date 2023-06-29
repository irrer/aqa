package org.aqa.webrun.wl

import org.aqa.webrun.wl

object WLImageStatus extends Enumeration {
  type ImageStatus = Value

  val Passed: wl.WLImageStatus.Value = Value("Passed")
  val BoxNotFound: wl.WLImageStatus.Value = Value("Failed: Box Not Found")
  val OffsetLimitExceeded: wl.WLImageStatus.Value = Value("Failed: Offset Limit Exceeded")
  val BallMissing: wl.WLImageStatus.Value = Value("Failed: No ball in box")
  val BallAreaNoisy: wl.WLImageStatus.Value = Value("Failed: Ball image area is noisy")
  val EdgeExtentsNotFound: wl.WLImageStatus.Value = Value("Failed: Extents of edges not found")
  val UnknownTreatmentMachine: wl.WLImageStatus.Value = Value("Failed: Unknown treatment machine")
  val UnexpectedError: wl.WLImageStatus.Value = Value("Failed: Unexpected Error")
  val BoxTooSmall: wl.WLImageStatus.Value = Value("Failed: Box too small")

  // List of statuses that indicate there was a result.
  private val passTextSet = Set(Passed, OffsetLimitExceeded).map(_.toString)

  /**
    * True if there is a pass/fail result.  False means that there is no result.
    * @param imageStatus Status from processing image.
    * @return True if processing completed.
    */
  def hasResult(imageStatus: WLImageStatus.Value): Boolean = passTextSet.contains(imageStatus.toString)
}
