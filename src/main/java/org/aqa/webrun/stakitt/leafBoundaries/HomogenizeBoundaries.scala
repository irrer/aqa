package org.aqa.webrun.stakitt.leafBoundaries

import edu.umro.ImageUtil.ImageUtil
import org.aqa.Logging

case class HomogenizeBoundaries() extends Logging {}

/**
  * Adjust leaf boundaries so that they are more evenly spaced.
  *
  */

object HomogenizeBoundaries extends Logging {

  /** Use this many adjacent boundaries when adjusting the position of a given boundary. */
  private val groupSize: Int = 5

  private case class Wideness(measuredBoundaries_pix: Seq[Double]) {

    // List of all spaces between boundaries.
    private val widthList_pix: IndexedSeq[Double] = measuredBoundaries_pix.indices.tail.map(i => measuredBoundaries_pix(i) - measuredBoundaries_pix(i - 1))

    // The approximate width of a narrow leaf.
    private val approximateNarrowWidth_pix: Double = widthList_pix.sorted.take(5).sum / 5

    /**
      * Determine if a leaf is wide or narrow.
      * @param width Width of leaf.
      * @return
      */
    def isWide(width: Double): Boolean = {
      width > (approximateNarrowWidth_pix * 1.5)
    }
  }

  /**
    * If this is the lower boundary of a wide leaf, make a fake temporary boundary to be positioned after
    * this one.  Otherwise, return None.
    *
    * @param index Index of a measured boundary.
    * @return New fake temporary boundary or None.
    */
  private def makeNarrow(index: Int, wideness: Wideness): Option[Double] = {

    val width = wideness.measuredBoundaries_pix(index + 1) - wideness.measuredBoundaries_pix(index)
    if (wideness.isWide(width)) {
      val middle = (wideness.measuredBoundaries_pix(index + 1) + wideness.measuredBoundaries_pix(index)) / 2
      Some(middle)
    } else
      None
  }

  /**
    * Make a list of the indices of the fake boundaries.
    *
    * @param evenlySpaced_pix List of evenly spaced boundaries.
    * @return List of the indices of the fake boundaries.
    */
  private def makeIndicesToRemove(evenlySpaced_pix: Seq[Double], wideness: Wideness): Seq[Int] = {

    // List of all spaces between boundaries.
    val widthList_pix = wideness.measuredBoundaries_pix.indices.tail.map(i => wideness.measuredBoundaries_pix(i) - wideness.measuredBoundaries_pix(i - 1))

    val topWideCount = widthList_pix.takeWhile(w => wideness.isWide(w)).size
    val top = (0 until topWideCount).map(i => (i * 2) + 1)

    val bottomWideCount = widthList_pix.reverse.takeWhile(w => wideness.isWide(w)).size
    val bottom = (0 until bottomWideCount).map(i => evenlySpaced_pix.size - ((i + 1) * 2))
    top ++ bottom
  }

  private def adjust(measuredPlusFakeBoundaries: Seq[Double]): IndexedSeq[Double] = {

    def nudgeOne(index: Int): Double = {
      val proximalList = measuredPlusFakeBoundaries.sortBy(m => (m - measuredPlusFakeBoundaries(index)).abs).take(groupSize).sorted

      val myIndex = proximalList.indexOf(measuredPlusFakeBoundaries(index))

      val newValue = ImageUtil.evenlySpacedLeastSquares(proximalList)(myIndex)
      newValue
    }

    measuredPlusFakeBoundaries.indices.map(nudgeOne)
  }

  /**
    * Take the measured boundaries and make them more evenly spaced. The objective is to move each individual
    * boundary as little as possible. A least-squares method is used to accomplish this.
    *
    * All calculations are done in pixels.
    *
    * Some leaves are double width. To accommodate this, first insert a fake, temporary boundary in the
    * middle of each of the double-width leaves to make all boundaries nearly the same.
    *
    * Next, apply the least-squares algorithm to groups of boundaries to change from close to evenly spaced
    * to be closer to evenly spaced.
    *
    * Finally, remove the fake temporary boundaries and return the result.
    *
    * @param measuredBoundaries_pix Boundaries measured by finding peaks and valleys in the image's horizontal
    *                               profile.  This can be flawed because the profile is not symmetrical from
    *                               top to bottom, resulting in an alternating pattern of leaves that are
    *                               slightly narrow with slightly wider ones.
    *
    * @return Equivalent list as the measured parameters, but the position of each adjusted slightly so
    *         that they are evenly spaced.
    *
    */
  def homogenize(measuredBoundaries_pix: Seq[Double]): Seq[Double] = {

    val wideness = Wideness(measuredBoundaries_pix)

    // make all the fake boundaries needed
    val fakeBoundaries_pix = measuredBoundaries_pix.indices.dropRight(1).flatMap(index => makeNarrow(index, wideness))

    // A list of all measured boundaries plus the fake ones that bisect the wide leaves. Constructed by merging the fakes into the real.
    val measuredPlusFakeBoundaries_pix = (measuredBoundaries_pix ++ fakeBoundaries_pix).sorted

    val adjustedPlusFakeBoundaries_pix = adjust(measuredPlusFakeBoundaries_pix)

    val indicesToRemove = makeIndicesToRemove(adjustedPlusFakeBoundaries_pix, wideness)

    // remove the fake boundaries
    val adjusted_pix = adjustedPlusFakeBoundaries_pix.indices.filterNot(i => indicesToRemove.contains(i)).map(i => adjustedPlusFakeBoundaries_pix(i))

    HomogenizeBoundaries()

    adjusted_pix

  }
}
