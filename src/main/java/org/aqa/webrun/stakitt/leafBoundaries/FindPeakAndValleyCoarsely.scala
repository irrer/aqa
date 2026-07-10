package org.aqa.webrun.stakitt.leafBoundaries

import edu.umro.ImageUtil.DicomImage
import org.aqa.Logging

/**
  * Calculate the Y coordinates of the AOIs for the given Stakitt image.
  *
  * All calculations are done in pixels (as opposed to mm / isoplane).
  */
object FindPeakAndValleyCoarsely extends Logging {

  /**
    * Find the coarse center of each of the peaks and valleys of the given profile.
    *
    * @param imageOfStaggeredLeaves Sub-image of DICOM image that contains staggered leaves.
    * @return List of coarsely located peaks and valleys.
    */
  def findPeaksAndValleysCoarsely(imageOfStaggeredLeaves: DicomImage, coarseVerticalFieldExtent: CoarseVerticalFieldExtent): Seq[PeakOrValley] = {

    val profile: Seq[Float] = imageOfStaggeredLeaves.rowSums

    // Trace.showChart(profile.map(_.toDouble), "Boundary Profile")

    val fieldIndices = coarseVerticalFieldExtent.top_pix until coarseVerticalFieldExtent.bottom_pix

    val meanValue = {
      val top = coarseVerticalFieldExtent.top_pix
      val size = coarseVerticalFieldExtent.bottom_pix - top
      profile.slice(top, top + size).sum / size
    }

    /**
      * Add an index to either an existing group, or, start a new group.
      *
      * @param peakOrValleyList List so far.
      * @param index            New index.
      * @return New list of peaks and valleys.
      */
    def putPv(peakOrValleyList: Seq[PeakOrValley], index: Int): Seq[PeakOrValley] = {

      val isPeak = profile(index) > meanValue

      val newList = if (peakOrValleyList.nonEmpty && (peakOrValleyList.last.isPeak == isPeak)) {
        peakOrValleyList.dropRight(1) :+ PeakOrValley(peakOrValleyList.last.indexes :+ index, isPeak)
      } else
        peakOrValleyList :+ PeakOrValley(Seq(index), isPeak)
      newList

    }

    // drop the first and last because they are meaningless
    val pvListUnclassified = fieldIndices.foldLeft(Seq[PeakOrValley]())(putPv).drop(1).dropRight(1)

    val widthThreshold = {
      val sizeList = pvListUnclassified.map(_.indexes.size)
      (sizeList.max + sizeList.min) / 2.0
    }

    val pvList = pvListUnclassified.map(pv => PeakOrValley(pv.indexes, pv.isPeak, pv.indexes.size > widthThreshold))

    pvList
  }

}
