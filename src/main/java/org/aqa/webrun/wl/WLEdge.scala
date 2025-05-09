package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.webrun.wl.WLProcessImage.colSum
import org.aqa.webrun.wl.WLProcessImage.rowSum
import org.aqa.webrun.wl.WLProcessImage.toCubicSpline
import org.aqa.webrun.wl.WLProcessImage.unitize
import org.aqa.Logging
import org.aqa.Util

import java.awt.Rectangle
import java.text.SimpleDateFormat
import scala.annotation.tailrec

case class WLEdge(name: String, vertical: Boolean, wholeImage: DicomImage, rtimage: AttributeList, bounds: Rectangle) extends Logging {

  val aoi: DicomImage = wholeImage.getSubimage(bounds)

  // ----------------------------------------------------------------------------------------------------------------------------------

  // Number of binary search iterations before determining that the edge has
  // been measured to a sufficient degree.  Each iteration is approximately
  // equivalent to one bit of precision.
  private val PRECISION = 30 // TODO max benefit is at 60.  Replace with LocateEdge when refactoring is complete

  val pixIn:IndexedSeq[IndexedSeq[Float]] = aoi.pixelData

  val sum: IndexedSeq[Float] = if (vertical) colSum(pixIn) else rowSum(pixIn)

  private def pixelValueRange(img: DicomImage): Double = {
    val ordered = img.pixelData.flatten.sorted
    val count = Config.WLAveragePixelsForBrightness
    val min = ordered.take(count).sum / count
    val max = ordered.takeRight(count).sum / count
    max - min
  }

  private val aoiPixelValueRange: Double = pixelValueRange(aoi)
  private val wholeImagePixelValueRange: Double = pixelValueRange(wholeImage)

  private def oldFindEdge(): Double = {

    val scaledSum = unitize(sum)
    val spline = toCubicSpline(scaledSum)

    val increasing = scaledSum(0) < scaledSum.last

    @tailrec
    def center(min: Double, max: Double, depth: Int): Double = {

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

    val c = center(0, sum.length - 1, PRECISION)
    c
  }

  // ----------------------------------------------------------------------------------------------------------------------------------

  /**
    * Ensure that the brightest and dimmest pixel in the edge are approximately as dim and
    * bright as the dimmest and brightest in the entire image.
    *
    * This is a sanity check to make sure that the image has one of the required properties
    * for Winston-Lutz analysis.  This is one of those things that should never happen, but
    * does increase confidence in results by guarding against a possible false positive.
    *
    * @return None on success, error status if there is an error.
    */
  private def verifyBrightness(): Option[WLImageStatus.Value] = {

    val pct = ((wholeImagePixelValueRange - aoiPixelValueRange).abs / wholeImagePixelValueRange) * 100.0
    val brightnessMessage = "edge brightness   Max percent diff range allowed: " + Config.WLMaxAllowedBrightnessRangePercentDifference +
      "  image brightness range: " + wholeImagePixelValueRange.formatted("%7.2f") +
      "  " + name.format("%8s") + " edge brightness range: " + aoiPixelValueRange.formatted("%7.2f") + "    percent diff: " + pct.formatted("%7.3f")
    logger.info(brightnessMessage)

    if (pct >= Config.WLMaxAllowedBrightnessRangePercentDifference) {
      val errorMsg =
        "Edge " + name + " failed to meet criteria for brightness range of " + Config.WLMaxAllowedBrightnessRangePercentDifference + " percent.  " + brightnessMessage + "    Required percent" + Config.WLMaxAllowedBrightnessRangePercentDifference
      logger.error(errorMsg)
      Trace.trace()
      Some(WLImageStatus.BallAreaNoisy)
    } else {
      None
    }
    None
  }

  // ----------------------------------------------------------------------------------------------------------------------------------

  private def learn(): Unit = {

    val pixIn = aoi.pixelData

    /**
      * Find the 50% edge position in the given pixel array.
      * @param line Single row or column of pixels.
      * @return 50% position.
      */
    def doLine1(line: IndexedSeq[Float]): Double = {
      val sorted = line.sorted
      val numPix = Math.max(line.size / 15, 3) // number of pixels to use at each end of the line to calculate min and max values.
      val lo = sorted.take(numPix).sum / numPix
      val hi = sorted.takeRight(numPix).sum / numPix
      val mean = (lo + hi) / 2
      val e = LocateEdge.locateEdge(line, mean)
      e
    }

    /**
      * Edge profile formed by locating the edge for each column (for horizontal edges) or row (for vertical edges) of pixels.
      */
    val crossProfile: IndexedSeq[Double] =
      if (vertical) {
        pixIn.map(doLine1)
      } else {
        val di = new DicomImage(pixIn).rotate90
        di.pixelData.map(doLine1)
      }

    /**
      * A measure of how straight the edge is.  If the ball is supported by a stem that has
      * poor radiation transparency, then this value will be larger.
      */
    val crossProfileStandardDeviation: Double = ImageUtil.stdDev(crossProfile.map(_.toFloat))

    val pixInLoc: IndexedSeq[IndexedSeq[Float]] = {
      val pix = aoi.pixelData
      val sorted = pix.flatten.sorted
      val count = 20
      val lo = sorted.take(count).sum / count
      val hi = sorted.takeRight(count).sum / count

      val range = hi - lo

      def doRow(row: IndexedSeq[Float]): IndexedSeq[Float] = {
        row.map(p => (p - lo) / range)
      }

      val normalized = pix.map(doRow)
      normalized
    }

    val fixedName = "%-6s".format(name)

    val fullName = {
      val g = "%3d".format(Util.angleRoundedTo90(Util.gantryAngle(rtimage)))
      val c = "%3d".format(Util.angleRoundedTo90(Util.collimatorAngle(rtimage)))
      val dateFmt = new SimpleDateFormat("HH:mm:ss")
      val t = Util.formatDate(dateFmt, DicomUtil.getTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime).get)

      s"G$g C$c t: $t $fixedName"
    }

    def doLine2(line: IndexedSeq[Float]): Double = {
      val sorted = line.sorted
      val numPix = 5
      val lo = sorted.take(numPix).sum / numPix
      val hi = sorted.takeRight(numPix).sum / numPix
      val mean = (lo + hi) / 2
      val e = LocateEdge.locateEdge(line, mean)
      e
    }

    val list = if (vertical) {
      pixInLoc.map(doLine2)
    } else {
      val di = new DicomImage(pixInLoc).rotate90
      di.pixelData.map(doLine2)
    }

    val stdDev = ImageUtil.stdDev(list.map(_.toFloat))

    val orientation = if (vertical) "col" else "row"
    val msg = s"""SinglePixel $orientation Edge: $fullName   StdDev: ${"%9.6f".format(stdDev)}   SinglePixel List: ${list.map(d => "%8.5f".format(d)).mkString(", ")}"""
    logger.info(msg)

    val StdDevThreshold = 0.15

    if (stdDev > StdDevThreshold) {
      logger.info(s"Edge with stem shadow : $fullName")

      val loSize = list.size / 2
      val hiSize = list.size - loSize
      val loList = list.take(loSize)
      val hiLIst = list.takeRight(hiSize)

      val descendingList = loList.foldLeft(Seq(loList.head))((descending, v) => if (v < descending.last) descending :+ v else descending :+ descending.last)

      val loEdge = LocateEdge.locateEdge(descendingList.map(_.toFloat).toIndexedSeq, descendingList.sum / descendingList.size)

      Trace.trace(s"""$fullName :: descendingList size: ${descendingList.size}  loEdge: $loEdge  : ${descendingList.mkString("  ")}""")
    }

  }

  /**
    * Find an edge of the box as accurately as possible by drawing a cubic spline across
    * the edge and then finding the midpoint of that spline.  Find the midpoint using a binary
    * search.
    */
  private def findEdge(): Either[WLImageStatus.Value, Double] = {

    /*
    try {
      learn()
    } catch {
      case t: Throwable =>
        logger.error(s"Unexpected error (ignored): ${fmtEx(t)}")
    }
     */

    verifyBrightness() match {
      case Some(err) =>
        Left(err)
      case _ =>
        Right(oldFindEdge())
    }

  }

  val edge: Either[WLImageStatus.Value, Double] = findEdge()

  def pos: Double = edge.right.get

  def posInt: Int = pos.round.toInt

  def posAbs: Int = (if (vertical) bounds.x else bounds.y) + posInt

  override def toString: String = s"$name: ${if (edge.isRight) pos.toString else edge.left.get.toString}"

}
