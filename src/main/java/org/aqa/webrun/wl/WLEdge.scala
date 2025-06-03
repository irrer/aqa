package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateEdge
import org.aqa.Config
import org.aqa.webrun.wl.WLProcessImage.colSum
import org.aqa.webrun.wl.WLProcessImage.rowSum
import org.aqa.webrun.wl.WLProcessImage.toCubicSpline
import org.aqa.webrun.wl.WLProcessImage.unitize
import org.aqa.Logging
import org.aqa.Util
import org.aqa.web.C3Chart

import java.awt.Rectangle
import scala.annotation.tailrec

case class WLEdge(name: String, vertical: Boolean, wholeImage: DicomImage, rtimage: AttributeList, bounds: Rectangle, wlMsg: WLMessage, trans: IsoImagePlaneTranslator) extends Logging {

  val aoi: DicomImage = wholeImage.getSubimage(bounds)

  // ----------------------------------------------------------------------------------------------------------------------------------

  // Number of binary search iterations before determining that the edge has
  // been measured to a sufficient degree.  Each iteration is approximately
  // equivalent to one bit of precision.
  private val PRECISION = 60 // max benefit is at 60 repetitions.

  private val pixIn: IndexedSeq[IndexedSeq[Float]] = aoi.pixelData

  val sum: IndexedSeq[Float] = if (vertical) colSum(pixIn) else rowSum(pixIn)

  private def simpleFindEdge(scaledSum: IndexedSeq[Float]): Double = {

    // val scaledSum = unitize(sum)
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

  /**
    * Find the 50% edge position of a single row or column of pixels.
    * @param line Single row or column of pixels.
    * @return 50% position.
    */
  private def edgeOfOneDimPixels(line: IndexedSeq[Float]): Double = {
    val sorted = line.sorted
    val numPix = Math.max(line.size / 15, 3) // number of pixels to use at each end of the line to calculate min and max values.
    val lo = sorted.take(numPix).sum / numPix
    val hi = sorted.takeRight(numPix).sum / numPix
    val mean = (lo + hi) / 2
    val e = LocateEdge.locateEdge(line, mean)
    e
  }

  /**
    * Get one line of pixels that traverses the gradient of the edge.
    * @param index for this offset into the edge aoi.  For horizontal edges this
    *              will be x, for vertical it is y.
    * @return Line of pixels
    */
  private def onePixelGradient(index: Int): IndexedSeq[Float] = {
    if (vertical)
      pixIn(index)
    else
      (0 until aoi.height).map(y => aoi.get(index, y))
  }

  /**
    * Edge profile formed by locating the edge for each column (for horizontal edges) or row (for vertical edges) of pixels.
    */
  private val crossProfile: IndexedSeq[Double] =
    if (vertical) {
      pixIn.map(edgeOfOneDimPixels)
    } else {
      val di = new DicomImage(pixIn).rotate90
      di.pixelData.map(edgeOfOneDimPixels)
    }

  /**
    * A measure of how straight the edge is.  If the ball is supported by a stem that has
    * poor radiation transparency, then this value will be larger.
    */
  val edgeProfileProfileCoefficientOfVariation: Double = {
    val mean = crossProfile.sum / crossProfile.size
    ImageUtil.stdDev(crossProfile.map(_.toFloat)) / mean
  }

  private val mean = crossProfile.sum / crossProfile.size

  /** pixel index of the lowest point in the profile's edge.  This is expected to be somewhere in
    * the middle of the trough created by the phantom's stem.
    */
  private val minIndex: Int = crossProfile.indexOf(crossProfile.min)

  private val border_pix: Int = {
    val mm = 2.0
    val pix: Double =
      if (vertical)
        trans.iso2PixDistY(mm)
      else
        trans.iso2PixDistX(mm)
    pix.round.toInt
  }

  /**
    * Pixel index of the left edge of the 'trough' created by the phantom's stem. Only valid if WLEdgeCoefficientOfVariationMax has been exceeded.
    */
  private val loIndex: Option[Int] = {
    if (edgeProfileProfileCoefficientOfVariation > Config.WLEdgeCoefficientOfVariationMax) {
      val l = crossProfile.take(minIndex).lastIndexWhere(_ > mean) - border_pix
      Some(Math.max(l, 0))
    } else
      None
  }

  /** pixel index of the right edge of the 'trough' created by the phantom's stem. */
  private val hiIndex: Option[Int] = {
    if (edgeProfileProfileCoefficientOfVariation > Config.WLEdgeCoefficientOfVariationMax) {
      val hOpt = crossProfile.indices.find(i => (i > minIndex) && (crossProfile(i) > mean))
      if (hOpt.isDefined)
        Some(Math.min(hOpt.get + border_pix, crossProfile.size - 1))
      else
        Some(crossProfile.size - 1)
    } else
      None
  }

  /**
    * Find an edge of the box as accurately as possible by drawing a cubic spline across
    * the edge and then finding the midpoint of that spline.  Find the midpoint using a binary
    * search.
    */
  private def findEdge(): Double = {

    val edgePosition = if (edgeProfileProfileCoefficientOfVariation > Config.WLEdgeCoefficientOfVariationMax) {
      wlMsg.info(
        s"$name edge has Coefficient of Variation $edgeProfileProfileCoefficientOfVariation which exceeds the configure WLEdgeCoefficientOfVariationMax of ${Config.WLEdgeCoefficientOfVariationMax}"
      )

      val validEdgeIndices = (0 to loIndex.get) ++ (hiIndex.get until crossProfile.size)

      val validEdge = new DicomImage(validEdgeIndices.map(onePixelGradient))

      val scaledSum = unitize(validEdge.columnSums)

      val usablePct = {
        val notUsed = hiIndex.get - loIndex.get
        val used = crossProfile.size - notUsed
        (used * 100.0) / crossProfile.size
      }
      wlMsg.info(s"$name edge length in pixels: ${crossProfile.size}  usable start index: $loIndex  usable finish index: $hiIndex   Usable percent: ${Util.fmtDbl(usablePct)}")
      simpleFindEdge(scaledSum)
    } else {
      val scaledSum = unitize(sum)
      simpleFindEdge(scaledSum)
    }

    logger.info(s"$name edgePosition $edgePosition vs mean: $mean : ${mean - edgePosition}")
    edgePosition
  }

  val edge: Double = findEdge()

  def pos_pix: Double = edge

  def posInt_pix: Int = pos_pix.round.toInt

  private val baseOffset_pix = if (vertical) bounds.x else bounds.y

  def posIntAbs_pix: Int = baseOffset_pix + posInt_pix

  def posAbs_pix: Double = baseOffset_pix + pos_pix

  override def toString: String = s"$name: $pos_pix"

  private val regionList: Seq[C3Chart.Region] = {
    if (loIndex.isDefined && hiIndex.isDefined)
      Seq(C3Chart.Region("50%", loIndex.get, hiIndex.get))
    else
      Seq()
  }

  val edgeProfileChart = new C3Chart( //
    xAxisLabel = "Pixel",
    xDataLabel = "50%",
    xValueList = crossProfile.indices.map(_.toDouble),
    yAxisLabels = Seq("50%"),
    yDataLabel = "50%",
    yValues = Seq(crossProfile),
    regionList = regionList
  )

}
