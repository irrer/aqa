package org.aqa.webrun.wl

import edu.umro.ImageUtil.DicomImage
import org.aqa.Config
import org.aqa.Logging

import scala.annotation.tailrec

case class WLBadPixels(uncorrectedImage: DicomImage, imageName: String, wlMsg: WLMessage) extends Logging {

  private val uncorrectedPixels = uncorrectedImage.pixelData

  private val rawDistinctSortedList: Seq[Float] = uncorrectedPixels.flatten.distinct.sorted

  private case class UncorrectedWLBadPixel(x: Int, y: Int, rawValue: Double) {}

  /**
    * Make a list of bad pixels.
    */
  private def findWLBadPixels(pixelGapLimit: Int): Seq[UncorrectedWLBadPixel] = {
    def getLimits: (Double, Double) = {
      def isGood(a: Double, b: Double): Boolean = scala.math.abs(a - b) <= pixelGapLimit

      val s = rawDistinctSortedList.size
      val sh = s / 2 // half of size

      val lo = (1 until sh).reverse.takeWhile(i => isGood(rawDistinctSortedList(i), rawDistinctSortedList(i - 1))).last
      val hi = (sh until (s - 1)).takeWhile(i => isGood(rawDistinctSortedList(i), rawDistinctSortedList(i + 1))).last

      (rawDistinctSortedList(lo - 1), rawDistinctSortedList(hi + 1))
    }

    val limits = getLimits

    def isValid(p: Double): Boolean = (p >= limits._1) && (p <= limits._2)

    val largestGoodGap = {
      val lds = rawDistinctSortedList

      def gapOf(i: Int): Double = {
        val lo = lds(i - 1)
        val hi = lds(i)
        if (isValid(lo) && isValid(hi)) {
          scala.math.abs(hi - lo)
        } else 0
      }

      (1 until rawDistinctSortedList.size).map(i => gapOf(i)).max
    }
    wlMsg.info("Range of valid pixel values (inclusive): " + limits._1 + " - " + limits._2 + "    Largest value gap in good pixels: " + largestGoodGap)

    val badList =
      uncorrectedImage.pixelData.flatten.zipWithIndex.filter(pi => !isValid(pi._1)).map(pix => UncorrectedWLBadPixel(pix._2 % uncorrectedImage.width, pix._2 / uncorrectedImage.width, pix._1))

    badList.toList
  }

  /**
    * Convert uncorrected pixels into corrected bad pixels.
    * @param uncorrectedPixelList List of uncorrected pixels
    * @return
    */
  private def uncorrectedWLBadPixelsToWLBadPixels(uncorrectedPixelList: Seq[UncorrectedWLBadPixel]): Seq[WLBadPixel] = {

    // A pixel is good if its coordinates are valid, and it is not on the bad pixel list
    val height = uncorrectedPixels.length
    val width = uncorrectedPixels(0).length

    def isGoodPixel(x: Int, y: Int): Boolean = {
      (x >= 0) && (y >= 0) && (x < width) && (y < height) && !uncorrectedPixelList.exists(p => p.x == x && p.y == y)
    }

    val radius: Int = Config.WLBadPixelCorrectionRadius
    val radSq = radius * radius

    def isClose(bad: UncorrectedWLBadPixel, x: Int, y: Int): Boolean = {
      ((x - bad.x) * (x - bad.x)) + ((y - bad.y) * (y - bad.y)) <= radSq
    }

    def correctSinglePixel(unCor: UncorrectedWLBadPixel): WLBadPixel = {
      val list =
        for (x <- unCor.x - radius to unCor.x + radius; y <- unCor.y - radius to unCor.y + radius; if isGoodPixel(x, y) && isClose(unCor, x, y)) yield uncorrectedPixels(y)(x)
      val correctedValue = list.sum / list.size
      new WLBadPixel(unCor.x, unCor.y, unCor.rawValue.toInt, correctedValue, list.map(f => f.toInt).toList)
    }

    uncorrectedPixelList.map(bad => correctSinglePixel(bad))
  }

  /**
    * Create a new image with the new values of the corrected pixels.
    * @param badPixelList List of pixels to fix, with their new values.
    * @return A new pixel array with bad pixels fixed.
    */
  @tailrec
  private def correctWLBadPixelsPrivate(originalPixels: IndexedSeq[IndexedSeq[Float]], badPixelList: Seq[WLBadPixel]): IndexedSeq[IndexedSeq[Float]] = {
    if (badPixelList.isEmpty)
      originalPixels
    else {
      val bad = badPixelList.head

      def fixRow(r: IndexedSeq[Float]): IndexedSeq[Float] = {
        (0 until uncorrectedImage.width).map(col => if (col == bad.x) bad.correctedValue else r(col)).toArray
      }

      val o: IndexedSeq[IndexedSeq[Float]] = (0 until uncorrectedImage.height).map(row => {
        if (row == bad.y)
          fixRow(originalPixels(row))
        else
          originalPixels(row)
      })
      correctWLBadPixelsPrivate(o, badPixelList.tail)
    }
  }

  /**
    * Fix bad pixels.
    * @param badPixelList List of pixels to fix.
    * @return new pixel array with bad pixels fixed.
    */
  private def correctWLBadPixels(badPixelList: Seq[WLBadPixel]): IndexedSeq[IndexedSeq[Float]] = {
    correctWLBadPixelsPrivate(uncorrectedPixels, badPixelList)
  }

  private val badPixelListUncorrected: Seq[UncorrectedWLBadPixel] = findWLBadPixels(Config.WLBadPixelGapLimit)

  private val marginalPixelListUncorrected: Seq[UncorrectedWLBadPixel] = findWLBadPixels(Config.WLMarginalPixelGapLimit)

  //noinspection ScalaWeakerAccess
  val badPixelsCorrected: Seq[WLBadPixel] = uncorrectedWLBadPixelsToWLBadPixels(badPixelListUncorrected)

  val marginalPixelsCorrected: Seq[WLBadPixel] = uncorrectedWLBadPixelsToWLBadPixels(marginalPixelListUncorrected)

  val correctedImage: IndexedSeq[IndexedSeq[Float]] = correctWLBadPixels(badPixelsCorrected)

  wlMsg.info(s"$imageName Number of bad pixels: " + badPixelsCorrected.size + " : " + badPixelsCorrected)
  wlMsg.info(s"$imageName Number of marginal pixels: " + marginalPixelsCorrected.size + " : " + marginalPixelsCorrected)

}
