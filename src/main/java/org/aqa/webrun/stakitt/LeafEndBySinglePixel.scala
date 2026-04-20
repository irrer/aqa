package org.aqa.webrun.stakitt

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateEdge
import org.aqa.webrun.stakitt.leafBoundaries.LeafBoundaries
import org.aqa.Config
import org.aqa.Logging

/**
 * Calculate and store the leaf end value for every pixel within every leaf.
 * @param xAoiBordersList List of X borders (horizontal) that form the low and high AOI bounds for each leaf.
 * @param leafBoundaries Leaf boundaries (vertical) that form the vertical AOI bounds for each leaf.
 * @param trans For to and from mm and pix coordinates.
 * @param dicomImage Image data for calculating edges.
 */
case class LeafEndBySinglePixel(xAoiBordersList: Seq[XAoiBorders], leafBoundaries: LeafBoundaries, trans: IsoImagePlaneTranslator, dicomImage: DicomImage) extends Logging {

  private val allY = leafBoundaries.yPointListLo_pix.adjusted_pix ++ leafBoundaries.yPointListHi_pix.adjusted_pix
  private def mean(d1: Double, d2: Double) = (d1 + d2) / 2

  private val minMaxSampleSize_pix = trans.iso2PixDistX(Config.StakittHorizontalMinMaxSampleLength_mm).round.toInt

  val minY: Int = allY.min.round.toInt - 3
  val maxY: Int = allY.max.round.toInt + 3

  private type MapEntry = ((Double, Int), Double)

  /**
   * Measure the end of a leaf for one row of pixels within that leaf.
   * @param xAoiBorders Within these borders.
   * @param y_pix Measure for this row of pixels.
   * @return Position of leaf end.
   */
  private def measurePixEdge(xAoiBorders: XAoiBorders, y_pix: Int): MapEntry = {

    val list = xAoiBorders.xRange_pix.map(x => dicomImage.get(x, y_pix)) // one single row of pixes in the AOI

    val sorted = list.sorted
    val min = sorted.take(minMaxSampleSize_pix).sum / minMaxSampleSize_pix
    val max = sorted.takeRight(minMaxSampleSize_pix).sum / minMaxSampleSize_pix
    val midValue = mean(min, max)

    val e = LocateEdge.locateEdge(list, midValue)

    val xPosition_pix = xAoiBorders.lo + e

    ((xAoiBorders.lo, y_pix), xPosition_pix)
  }

  private def makeColumn(xAoiBorders: XAoiBorders): Seq[MapEntry] = {
    (minY until maxY).map(y => measurePixEdge(xAoiBorders, y))
  }

  // calculate all leaf positions.
  private val leafEndMap = xAoiBordersList.flatMap(makeColumn).toMap

  /**
   * Fetch an edge result for a single row of pixels to the right of the X AOI bound.
   * @param xLo_pix Lower X AOI bound.
   * @param y_pix Y coordinate.
   * @return Leaf edge.
   */
  def get(xLo_pix: Double, y_pix: Int): Double = leafEndMap((xLo_pix, y_pix))

}
