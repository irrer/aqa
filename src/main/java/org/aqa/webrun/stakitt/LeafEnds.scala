package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.LocateEdge
import org.aqa.Logging

case class LeafEnds(rtimage: AttributeList) extends Logging {

  private val dicomImage = new DicomImage(rtimage)

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

  /**
    * Coarsely find the leaf ends of the square tooth pattern on either side (left and right) of the image.
    * @return First and last edges.
    */
  private def findSquareToothEnds(): Seq[Double] = {
    if (false) {
      val rowSums = dicomImage.rowSums
      val lo = (rowSums.take(10) ++ rowSums.takeRight(10)).sum / 20

      val oneThird = dicomImage.height / 3
      val hi = rowSums.slice(oneThird, oneThird + oneThird).sum / oneThird
      val mid = (lo + hi) / 2

      val top = rowSums.indices.indexWhere(i => rowSums(i) > mid)
      val bottom = rowSums.indices.lastIndexWhere(i => rowSums(i) > mid)
    }

    val columnSums = dicomImage.columnSums

    val mid: Float = {
      val lo = (columnSums.take(10) ++ columnSums.takeRight(10)).sum / 20
      val oneThird = dicomImage.width / 3
      val hi = columnSums.slice(oneThird, oneThird + oneThird).sum / oneThird
      (lo + hi) / 2
    }

    val left = columnSums.indices.indexWhere(i => columnSums(i) > mid)
    val right = columnSums.indices.lastIndexWhere(i => columnSums(i) > mid)

    Seq(left, right)
  }

  /** Points where X profile crosses mean. */
  val xPointList: Seq[Double] = {
    if (false) {
      // the first and last are in the middles of the 'square wave' profiles of the left and right side, and are meaningless.
      val list1 = crossingPointsOfMeanSubPix(dicomImage.columnSums)
      val list2 = list1.drop(1).tail
      val squareToothEnds = findSquareToothEnds()
      val list3 = squareToothEnds.head +: list2 :+ squareToothEnds.last
      list3
    } else {
      val list = crossingPointsOfMeanSubPix(dicomImage.columnSums)
      list.tail.dropRight(1)
    }
  }
}
