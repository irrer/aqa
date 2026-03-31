package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.opensourcephysics.numerics.CubicSpline

import java.awt.Rectangle
import scala.annotation.tailrec

/**
  * Calculate the Y coordinates of the AOIs for the given Stakitt image.
  *
  * All calculations are done in pixels (as opposed to mm / isoplane).
  *
  * @param rtimage Stakitt image.
  * @param xBorderList List of X coordinates for AOIs.
  */
case class YImageBorders(rtimage: AttributeList, xBorderList: Seq[Double]) extends Logging {

  private val dicomBorderLo = xBorderList(1).round.toInt - 60
  private val dicomBorderHi = xBorderList.reverse(1).round.toInt + 60

  private val dicomImage: DicomImage = new DicomImage(rtimage)

  private val dicomImageLo = dicomImage.getSubimage(new Rectangle(0, 0, dicomBorderLo, dicomImage.height))

  private val dicomImageHi = {
    val rect = new Rectangle(dicomBorderHi, 0, dicomImage.width - dicomBorderHi, dicomImage.height)
    dicomImage.getSubimage(rect)
  }

  private val dicomImageMid = dicomImage.getSubimage(new Rectangle(dicomBorderLo, 0, dicomBorderHi - dicomBorderLo, dicomImage.height))

  // set the span to be 1/2 the width of the narrowest possible leaf.
  private val span: Int = {
    val trans = new IsoImagePlaneTranslator(rtimage)
    trans.iso2PixDistX(2.5).round.toInt
  }

  /**
    * Find the indices of the crossing points to the accuracy of one pixel.
    * @return List of points where the profile is closest to the mean.
    */
  private def inflectionPointsPix(profile: Seq[Float]): Seq[Int] = {

    /**
      * Determine if the given point is an inflection point.  For that to be true, the slope of
      * line at that point must be greater than the slope of the line than either adjacent point.
      * @param index For this point.
      * @return True if it is an inflection point.
      */
    def isInflectionPoint(index: Int): Boolean = {

      /** The slope values before and after must continuously rise and fall, with the
        * <code>index</code> point being the largest and being in the center.  This
        * filters out erratic sequences.
        */
      val continuousSize = 3

      if (index > span) {
        def slopeOf(i: Int) = (profile(i) - profile(i - 1)).abs

        // make list of slopes of previous point, this point, and next point.
        val slopeList = (index - span until index + span + 1).map(slopeOf)

        def continuousSpan(): Boolean = {
          val lo = slopeList.take(span + 1).takeRight(continuousSize + 1)
          val hi = slopeList.slice(span, span + continuousSize + 1).reverse
          (lo == lo.sorted) && (hi == hi.sorted)
        }

        val j1 = slopeList.max == slopeList(span)
        val j2 = continuousSpan()

        // check if the slope of this point is greater than the others.
        val isInflection = (slopeList.max == slopeList(span)) // && continuousSpan()

        isInflection
      } else
        false
    }

    // calculate the level in the profile where the penumbra has started and is definitely greater than the background
    val cutoff = {
      val range = profile.max - profile.min
      profile.min + (range * 0.30)
    }

    Trace.trace()
    Trace.trace("first: " + profile.indices.indexWhere(i => profile(i) > cutoff))
    Trace.trace()

    val firstIndex = {
      val i = profile.indices.indexWhere(i => profile(i) > cutoff) - (span * 1)
      Math.max(i, span)
    }

    Trace.trace()
    val lastIndex = {
      val i = profile.indices.lastIndexWhere(i => profile(i) > cutoff) + (span * 1)
      Math.min(i, dicomImage.height - (span + 1))
    }

    Trace.trace()
    val inflectionList = profile.indices.slice(firstIndex, lastIndex).filter(isInflectionPoint)
    // val inflectionList = profile.indices.drop(span * 2).dropRight(span * 2).filter(isInflectionPoint)

    Trace.trace()
    inflectionList
  }

  /**
    * Precisely locate the inflection point that is near the given index.
    * @param cs Cubic spline for profile being used.
    * @param index Starting index in spline.
    * @return Precise location.
    */
  private def locatePrecisely(cs: CubicSpline, index: Double): Double = {
    val requiredPrecision = 1 / (1000.0 * 1000 * 1000)
    val initialRange = 3
    val startingLo = index - initialRange
    val startingHi = index + initialRange

    val partitions = 8
    val maxGeneration = 1000
    val reductionFactor = (partitions - 0.5) / partitions
    val segIndexList = 0 until partitions

    Trace.trace()
    @tailrec
    def loc(lo: Double, hi: Double, gen: Int = 0): Double = {
      Trace.trace()
      val segSize = (hi - lo) / partitions

      val xList = segIndexList.map(s => (s * segSize) + lo)

      val yList = xList.map(cs.evaluate)
      val slopeList = segIndexList.tail.map(s => (yList(s) - yList(s - 1)).abs)
      val yBest = slopeList.max
      val xBest = {
        val i = slopeList.indexWhere(_ == yBest)
        xList(i)
      }

      if ((xBest < startingLo) || (xBest > startingHi))
        logger.error("out of bounds")

      if ((segSize < requiredPrecision) || gen > maxGeneration) {
        if (gen > maxGeneration)
          logger.warn("Exceeded generation limit")
        xBest
      } else {
        val range = segSize * reductionFactor * (partitions / 2)
        val lo2 = xBest - range
        val hi2 = xBest + range
        loc(lo2, hi2, gen + 1)
      }
    }

    loc(index - 3, index + 3)
  }

  /**
    * Find inflection points in the profile.
    * @return List of inflection points.
    */
  private def inflectionPointsSubPix(profile: Seq[Float], name: String): Seq[Double] = {

    Trace.trace()
    val list = inflectionPointsPix(profile).map(_.toDouble) // TODO

    val cs = new CubicSpline(profile.indices.map(_.toDouble).toArray, profile.map(_.toDouble).toArray)

    val preciseList = list.map(i => locatePrecisely(cs, i))

    if (true) { // TODO rm
      val scale = profile.max / 10.0
      val j = profile.map(v => v / scale)
      Trace.showChart(j, name)
    }

    Trace.trace()
    preciseList
  }

  Trace.trace()
  private val profileLo = dicomImageLo.rowSums
  Trace.trace()
  private val profileHi = dicomImageHi.rowSums
  Trace.trace()
  private val profileMid = dicomImageMid.rowSums
  Trace.trace()
  private val profileAll = new DicomImage(rtimage).rowSums
  Trace.trace()

  if (false) { // TODO rm
    val drv1 = profileLo.indices.tail.map(i => profileLo(i) - profileLo(i - 1).toDouble)
    Trace.showChart(drv1, "Derivative 1")
  }

  Trace.trace("Calculating yPointListLo_pix")

  /** Inflection points on the left */
  val yPointListLo_pix: Seq[Double] = inflectionPointsSubPix(profileLo, "Lo") // TODO

  if (true) { // TODO rm
    Trace.trace()
    val loMin = profileLo.min
    val loMax = profileLo.max
    val range = loMax - loMin
    val j160 = profileLo(160)
    val j170 = profileLo(170)
    val j180 = profileLo(180)

    def pct(i: Int): String = {
      val p = (profileLo(i) - loMin) / range
      "    " + i + " : " + "%8.4f".format(p)
    }
    Trace.trace("Percents: " + pct(160) + pct(170) + pct(180))
  }

  Trace.trace("Calculating yPointListHi_pix")

  /** Inflection points on the right */
  val yPointListHi_pix: Seq[Double] = inflectionPointsSubPix(profileHi, "Hi")

  Trace.trace("Calculating yPointListMid_pix")

  /** Inflection points on the right */
  val yPointListMid_pix: Seq[Double] = inflectionPointsSubPix(profileMid, "Mid")

  Trace.trace("Calculating yPointListAll_pix")

  /** Inflection points on the right */
  val yPointListAll_pix: Seq[Double] = inflectionPointsSubPix(profileAll, "All")

  if (true) { // TODO rm

    case class Pointy(pint: Double, name: String) {}

    def toPointy(p: Seq[Double], name: String): Seq[Pointy] = p.map(pp => Pointy(pp, name))

    val points: Seq[Double] = (yPointListLo_pix ++ yPointListHi_pix).sorted // ++ toPointy(yPointListAll_pix, "All")

    @tailrec
    def addOrNew(groupList: Seq[Seq[Double]], pList: Seq[Double]): Seq[Seq[Double]] = {
      val isClose = groupList.last.exists(gm => (gm - pList.head).abs < 4)

      val newGroupList = if (isClose) {
        val g = (groupList.last :+ pList.head)
        val gg = groupList.dropRight(1) :+ g
        gg
      } else {
        groupList :+ Seq(pList.head)
      }

      if (pList.size > 1)
        addOrNew(newGroupList, pList.tail)
      else
        newGroupList

    }

    val groupList = addOrNew(Seq(Seq(points.head)), points.tail)

    def groupToText(g: Seq[Double]): String = {
      g.map(p => "%8.3f".format(p)).mkString("\n")
    }

    Trace.trace("Group List ==============")
    val fullText = groupList.map(groupToText).mkString("\n\n")
    println(fullText)
    Trace.trace("Group List ==============")

  }

  Trace.trace("==============")

  /** List of Y coordinates that mark the edges of the leaf AOIs.  These are extracted by using the profile of the rows of pixels across the entire image. */
  val yPointList_pix: Seq[Double] = yPointListAll_pix

}
