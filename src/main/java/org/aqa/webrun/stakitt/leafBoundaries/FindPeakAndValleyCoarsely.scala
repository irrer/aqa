package org.aqa.webrun.stakitt.leafBoundaries

import org.aqa.Logging

/**
  * Calculate the Y coordinates of the AOIs for the given Stakitt image.
  *
  * All calculations are done in pixels (as opposed to mm / isoplane).
  */
case class FindPeakAndValleyCoarsely() extends Logging {

  /**
    * Find the coarse center of each of the peaks and valleys of the given profile.
    *
    * @param profile For this profile
    * @return
    */
  def findPeaksAndValleysCoarsely(profile: Seq[Float]): Seq[PeakOrValley] = {

    // Halfway point between the highest peak and the lowest valley.
    val mean = {
      val sampleSize = 5

      def isDescending(i: Int): Boolean = {
        profile.slice(i, i + sampleSize).sum > profile.slice(i + sampleSize, i + (sampleSize * 2)).sum
      }

      val start = profile.indices.drop(sampleSize).indexWhere(isDescending)
      val end = profile.reverse.indices.dropRight(sampleSize).indexWhere(isDescending)

      val pvRegion = profile.drop(start).dropRight(end)

      val m = (pvRegion.max + pvRegion.min) / 2
      m
    }

    /**
      * Add an index to either an existing group, or, start a new group.
      *
      * @param peakOrValleyList List so far.
      * @param index            New index.
      * @return New list of peaks and valleys.
      */
    def putPv(peakOrValleyList: Seq[PeakOrValley], index: Int): Seq[PeakOrValley] = {

      val isPeak = profile(index) > mean

      val newList = if (peakOrValleyList.nonEmpty && (peakOrValleyList.last.isPeak == isPeak)) {
        peakOrValleyList.dropRight(1) :+ PeakOrValley(peakOrValleyList.last.indexes :+ index, isPeak)
      } else
        peakOrValleyList :+ PeakOrValley(Seq(index), isPeak)
      newList

    }

    // drop the first and last because they are meaningless
    val pvListUnclassified = profile.indices.foldLeft(Seq[PeakOrValley]())(putPv).drop(1).dropRight(1)

    val widthThreshold = {
      val sizeList = pvListUnclassified.map(_.indexes.size)
      (sizeList.max + sizeList.min) / 2.0
    }

    val pvList = pvListUnclassified.map(pv => PeakOrValley(pv.indexes, pv.isPeak, pv.indexes.size > widthThreshold))

    pvList
  }

}
