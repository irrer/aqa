package org.aqa.webrun.wl

import org.aqa.Config
import org.aqa.Logging

object WLBallAreaIsFlat extends Logging {

  /**
   * Occasionally an image does not have a ball in it.  This might happen because the phantom is
   * not present or the image was not intended to be analyzed as Winston Lutz.
   *
   * Determine whether there is a ball present by comparing the range of the background pixels
   * @param boxArea Coarse box area including all four edges.
   * @param ballArea The area inside the edges where the ball is expected.
   * @return
   */
  def ballAreaIsFlat(boxArea: IndexedSeq[IndexedSeq[Float]], ballArea: IndexedSeq[IndexedSeq[Float]], wlMsg: WLMessage): Boolean = {

    // discard this many pixels as potential bad pixels or outliers
    val maxBadPixelCount = 10

    val ballPixelList = ballArea.flatten.sorted

    val boxPixelList = boxArea.flatten.sorted

    // there should be at least this many background pixels somewhere in the box area
    val backgroundPixelSampleSize = ballPixelList.size / 2

    val backgroundPixelList = boxPixelList.slice(maxBadPixelCount, maxBadPixelCount + backgroundPixelSampleSize)

    val backgroundRange = backgroundPixelList.max - backgroundPixelList.min

    val ballPixelListSorted = ballPixelList.sorted.drop(maxBadPixelCount).dropRight(maxBadPixelCount)

    val ballRange = ballPixelListSorted.max - ballPixelListSorted.min

    val ratio = ballRange / backgroundRange

    val stats = s"Ratio of ball area pixel range to background area pixel range: $ratio   ball range: $ballRange    backgroundRange: $backgroundRange "

    if (ratio < Config.WLBallAreaFlatnessRatioLowerLimit) {
      val msg = "Flatness check: Failed to find ball in box because area inside box was flat. " + stats
      wlMsg.error(msg)
      true
    } else {
      val msg = "Flatness check: The area inside the box contains a ball. " + stats
      wlMsg.error(msg)
      false
    }

  }


 }
