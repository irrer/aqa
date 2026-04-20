package org.aqa.webrun.stakitt

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.LocateEdge
import org.aqa.Logging

/** Find the approximate leaf ends.  These are the points where X profile crosses mean. */
object LeafEnds extends Logging {

  /**
    * Find the indices of the crossing points to the accuracy of one pixel.
    * @param profile A wavy profile.
    * @return List of points where the profile is closest to the mean.
    */
  private def crossingPointsOfMeanPix(profile: Seq[Float]): Seq[Int] = {

    val mean = profile.sum / profile.size
    def isCrossingCenterPoint(index: Int): Boolean = {

      //noinspection RedundantCollectionConversion
      // Funny thing - the redundant conversion toSeq is required to make it compile.
      val isCross = (index > 0) && Seq(profile(index), profile(index - 1), mean).sorted.toSeq(1) == mean

      isCross
    }

    val list = profile.indices.filter(isCrossingCenterPoint)

    list
  }

  /**
    * Find the points in the profile where it crosses the mean.
    * @param profile profiles of image in either X or Y.
    * @return
    */
  private def crossingPointsOfMeanSubPix(profile: Seq[Float]): Seq[Double] = {

    // Trace.showChart(profile.map(_.toDouble), "X Borders")

    val pixIndexList = crossingPointsOfMeanPix(profile)

    val mean = profile.sum / profile.size
    val w = 5

    def closestToMean(index: Int): Double = {
      val aoi = profile.slice(index - w, index - w + (w * 2) + 1)
      val v = LocateEdge.locateEdge(aoi.toIndexedSeq, mean) + (index - w)
      v
    }

    val list = pixIndexList.map(closestToMean)
    list
  }

  /** Find the approximate leaf ends.  These are the points where X profile crosses mean.
    * @param dicomImage Stakitt image
    * @return List of approximate leaf edges (X coordinates).
    */
  def xPointList(dicomImage: DicomImage): Seq[Double] = {
    val list = crossingPointsOfMeanSubPix(dicomImage.columnSums)
    list.tail.dropRight(1)
  }
}
