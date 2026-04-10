package org.aqa.webrun.stakitt.leafBoundaries

import edu.umro.ImageUtil.ImageUtil
import org.aqa.Logging

/**
  * Adjust the leaf boundaries so that they are evenly spaced.
  *
  * @param measuredBoundaries_pix Measured boundaries via image analysis.
  */
case class EvenlySpacedBoundaries(measuredBoundaries_pix: Seq[Double]) extends Logging {

  // List of all spaces between boundaries.
  private val widthList_pix = measuredBoundaries_pix.indices.tail.map(i => measuredBoundaries_pix(i) - measuredBoundaries_pix(i - 1))

  // The approximate width of a narrow leaf.
  private val approximateNarrowWidth_pix = widthList_pix.sorted.take(5).sum / 5

  /**
    * Determine if a leaf is wide or narrow.
    * @param width Width of leaf.
    * @return
    */
  private def isWide(width: Double): Boolean = {
    width > (approximateNarrowWidth_pix * 1.5)
  }

  /**
    * If this is the lower boundary of a wide leaf, make a fake temporary boundary to be positioned after
    * this one.  Otherwise, return None.
    *
    * @param index Index of a measured boundary.
    * @return New fake temporary boundary or None.
    */
  private def makeNarrow(index: Int): Option[Double] = {
    val width = measuredBoundaries_pix(index + 1) - measuredBoundaries_pix(index)
    if (isWide(width)) {
      val middle = (measuredBoundaries_pix(index + 1) + measuredBoundaries_pix(index)) / 2
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
  private def makeIndicesToRemove(evenlySpaced_pix: Seq[Double]) = {
    val topWideCount = widthList_pix.takeWhile(w => isWide(w)).size
    val top = (0 until topWideCount).map(i => (i * 2) + 1)

    val bottomWideCount = widthList_pix.reverse.takeWhile(w => isWide(w)).size
    val bottom = (0 until bottomWideCount).map(i => evenlySpaced_pix.size - ((i + 1) * 2))
    top ++ bottom
  }

  /**
    * Take the measured boundaries and make them evenly spaced. The objective is to move each individual
    * boundary as little as possible. A least-squares method is used to accomplish this.
    *
    * All calculations are done in pixels.
    *
    * Some leaves are double width. To accommodate this, first insert a fake, temporary boundary in the
    * middle of each of the double-width leaves to make all boundaries nearly the same.
    *
    * Next, apply the least-squares algorithm to change from close to evenly spaced to be exactly evenly spaced.
    *
    * Finally, remove the fake temporary boundaries and return the result.
    *
    * @return Equivalent list as the measured parameters, but the position of each adjusted slightly so
    *         that they are evenly spaced.
    */
  def makeEvenlySpaced_pix(): Seq[Double] = {

    // make all the fake boundaries needed
    val fakeBoundaries_pix = measuredBoundaries_pix.indices.dropRight(1).flatMap(makeNarrow)

    // A list of all measured boundaries plus the fake ones that bisect the wide leaves. Constructed by merging the fakes into the real.
    val measuredPlusFakeBoundaries = (measuredBoundaries_pix ++ fakeBoundaries_pix).sorted

    // make an evenly spaced version
    val evenlySpaced = ImageUtil.evenlySpacedLeastSquares(measuredPlusFakeBoundaries)

    val indicesToRemove = makeIndicesToRemove(evenlySpaced)

    // remove the fake boundaries
    val measuredMadeEven_pix = evenlySpaced.indices.filterNot(i => indicesToRemove.contains(i)).map(i => evenlySpaced(i))

    measuredMadeEven_pix
  }

}
