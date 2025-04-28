package org.aqa.webrun.wl

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.webrun.wl.WLProcessImage.colSum
import org.aqa.webrun.wl.WLProcessImage.rowSum
import org.aqa.webrun.wl.WLProcessImage.toCubicSpline
import org.aqa.webrun.wl.WLProcessImage.unitize
import org.aqa.Logging

import scala.annotation.tailrec

case class WLEdge(name: String, aoi: DicomImage, vertical: Boolean, wholeImage: DicomImage) extends Logging {

  // Number of binary search iterations before determining that the edge has
  // been measured to a sufficient degree.  Each iteration is approximately
  // equivalent to one bit of precision.
  private val PRECISION = 30 // TODO max benefit is at 60.  Replace with LocateEdge when refactoring is complete

  private def pixelValueRange(img: DicomImage): Double = {
    val ordered = img.pixelData.flatten.sorted
    val count = Config.WLAveragePixelsForBrightness
    val min = ordered.take(count).sum / count
    val max = ordered.takeRight(count).sum / count
    max - min
  }

  private val aoiPixelValueRange: Double = pixelValueRange(aoi)
  private val wholeImagePixelValueRange: Double = pixelValueRange(wholeImage)

  /**
    * Find an edge of the box as accurately as possible by drawing a cubic spline across
    * the edge and then finding the midpoint of that spline.  Find the midpoint using a binary
    * search.
    */
  def findEdge(): Either[WLImageStatus.Value, Double] = {
    val pixIn = aoi.pixelData
    val length = if (vertical) pixIn(0).length else pixIn.length
    val sum = if (vertical) colSum(pixIn) else rowSum(pixIn)
    val scaledSum = unitize(sum)
    if (true) { // TODO rm : Study StdDev of edges.

      val fixedName = "%-6s".format(name)

      if (true) {
        def doLine(line: IndexedSeq[Float]): Double = {
          val sorted = line.sorted
          val numPix = 5
          val lo = sorted.take(numPix).sum / numPix
          val hi = sorted.takeRight(numPix).sum / numPix
          val mean = (lo + hi) / 2
          val e = LocateEdge.locateEdge(line, mean)
          e
        }

        val list = if (vertical) {
          pixIn.map(doLine)
        } else {
          val di = new DicomImage(pixIn).rotate90
          di.pixelData.map(doLine)
        }

        val stdDev = ImageUtil.stdDev(list.map(_.toFloat))

        val orientation = if (vertical) "col" else "row"
        val msg = s"""SinglePixel $orientation Edge: $fixedName   StdDev: $stdDev   SinglePixel List: ${list.map(d => "%8.5f".format(d)).mkString(", ")}"""
        logger.info(msg)
      }

      if (true) {

        def doLineMean(line: IndexedSeq[Float]): Double = {
          line.sum / line.size
        }

        val listMean = if (vertical) {
          pixIn.map(doLineMean)
        } else {
          val di = new DicomImage(pixIn).rotate90
          di.pixelData.map(doLineMean)
        }

        val stdDevMean = ImageUtil.stdDev(listMean.map(_.toFloat))
        val msg = s"""Means Edge: $fixedName   StdDevMean: $stdDevMean   Means: ${listMean.map(d => "%8.5f".format(d)).mkString(", ")}"""
        logger.info(msg)
      }

      try {
        println("hey")
      }

    }

    val spline = toCubicSpline(scaledSum)

    /*
    saveEdgeImage(pixIn, vertical, name, spline)
    val terminalEdgeSlopes = getTerminalEdgeSlopes(spline, scaledSum.length)
    showEdgeStats(pixIn, vertical, name, terminalEdgeSlopes)
     */

    val increasing = scaledSum(0) < scaledSum(length - 1)

    @tailrec
    def center(min: Double, max: Double, depth: Int): Double = {
      Trace.trace(name + " : " + depth + "    mean:" + "%30.25f".format((min + max) / 2))
      val mid = (max + min) / 2
      val guess = spline.evaluate(mid)
      if (depth > 0) {
        if ((increasing && (guess < 0.5)) || ((!increasing) && (guess > 0.5)))
          center(mid, max, depth - 1)
        else
          center(min, mid, depth - 1)
      } else
        mid
    }

    // Ensure that the brightest and dimmest pixel in the edge are approximately
    // as dim and bright as the dimmest and brightest in the entire image

    val pct = ((wholeImagePixelValueRange - aoiPixelValueRange).abs / wholeImagePixelValueRange) * 100.0
    val brightnessMessage = "edge brightness   Max percent diff range allowed: " + Config.WLMaxAllowedBrightnessRangePercentDifference +
      "  image brightness range: " + wholeImagePixelValueRange.formatted("%7.2f") +
      name.format("%8s") + " edge brightness range: " + aoiPixelValueRange.formatted("%7.2f") + "    percent diff: " + pct.formatted("%7.3f")
    logger.info(brightnessMessage)

    if (pct >= Config.WLMaxAllowedBrightnessRangePercentDifference) {
      val errorMsg =
        "Edge " + name + " failed to meet criteria for brightness range of " + Config.WLMaxAllowedBrightnessRangePercentDifference + " percent.  " + brightnessMessage + "    Required percent" + Config.WLMaxAllowedBrightnessRangePercentDifference
      logger.error(errorMsg)
      Trace.trace()
      Left(WLImageStatus.BallAreaNoisy)
    } else {
      Trace.trace()
      val c = center(0, sum.length - 1, PRECISION)
      if (true) {
        val profile = if (vertical) aoi.columnSums else aoi.rowSums
        val mean = (profile.min + profile.max) / 2
        val c2 = LocateEdge.locateEdge(profile, mean)
        Trace.trace(s"c: $c    c2: $c2    diff: ${c - c2}")
      }
      val j = Right(c)
      Trace.trace()
      j
    }

  }

}
