package org.aqa.webrun.phase2.vmat

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import org.aqa.Logging

import java.awt.geom.Rectangle2D

/**
 * Container for results of measuring CU for one rectangle.
 * @param mlcMean_cu Mean cu of MLC pixels.
 * @param openMean_cu Mean cu of Open pixels.
 * @param mlcPerOpenRatio Mean of ratios of pixels
 */
case class VMATRectangleRatio(mlcMean_cu: Double, openMean_cu: Double, mlcPerOpenRatio: Double) {
  /** This provides an estimate of the ratios, but will be slightly different from the per pixel ratio. */
  val mlcMeanPerOpenMeanRatio_cu: Double = mlcMean_cu / openMean_cu
}

/**
  * Find the pixel-by-pixel ratio of the mlc/open fields within a given rectangle.
  */

object VMATRectangleRatio extends Logging {

  /**
    * Find the pixel-by-pixel ratio of the <code>mlc/open</code> fields within a given rectangle.
    *
    * If a pixel in the Open field is 0, then it is ignored.
    *
    * The rectangle is defined by the RTPLAN, and the borders are superimposed on the image.  The
    * borders will usually cut through pixels, and when that happens the pixel will be weighted
    * accordingly.
    *
    * So for example, if 37% of a pixel is within the rectangle, then only 37% of pixel will be used.
    *
    * @param alMlc MLC RTIMAGE
    * @param alOpen Open RTIMAGE
    * @param rect_pix Rectangle in pixel coordinates.
    * @return Mean of ratios of pixels
    */
  def ratio(alMlc: AttributeList, alOpen: AttributeList, rect_pix: Rectangle2D.Double): VMATRectangleRatio = {

    val rectLeft = rect_pix.x
    val rectRight = rect_pix.x + rect_pix.width

    val rectTop = rect_pix.y
    val rectBottom = rect_pix.y + rect_pix.height

    /** For pixels that intersect the left of the rectangle, fraction of pixel that is inside the rectangle */
    val fractionLeft = {
      val fraction = rectLeft - rectLeft.floor

      if (fraction < 0.5)
        0.5 - fraction
      else
        1.5 - fraction
    }

    /** For pixels that intersect the right of the rectangle, fraction of pixel that is inside the rectangle */
    val fractionRight = {
      val fraction = rectRight - rectRight.floor
      if (fraction < 0.5)
        0.5 + fraction
      else
        fraction - 0.5
    }

    /** For pixels that intersect the top of the rectangle, fraction of pixel that is inside the rectangle */
    val fractionTop = {
      val fraction = rectTop - rectTop.floor

      if (fraction < 0.5)
        0.5 - fraction
      else
        1.5 - fraction
    }

    /** For pixels that intersect the bottom of the rectangle, fraction of pixel that is inside the rectangle */
    val fractionBottom = {
      val fraction = rectBottom - rectBottom.floor
      if (fraction < 0.5)
        0.5 + fraction
      else
        fraction - 0.5
    }

    /** MLC image scaled to CU */
    val mlc = {
      val mlcSlope = alMlc.get(TagFromName.RescaleSlope).getDoubleValues.head.toFloat
      val mlcOffset = alMlc.get(TagFromName.RescaleIntercept).getDoubleValues.head.toFloat
      new DicomImage(alMlc).scalePixels(mlcSlope, mlcOffset)
    }

    /** Open image scaled to CU */
    val open = {
      val openSlope = alOpen.get(TagFromName.RescaleSlope).getDoubleValues.head.toFloat
      val openOffset = alOpen.get(TagFromName.RescaleIntercept).getDoubleValues.head.toFloat
      new DicomImage(alOpen).scalePixels(openSlope, openOffset)
    }

    /**
      * The area of one pixel.
      * @param x X coordinate of pixel.
      * @param y Y coordinate of pixel.
      */
    case class Pixel(x: Int, y: Int) {

      /** If the pixel intersects a border, then the *Trim is the fraction of the pixel to be used.  If
        *  it intersects the rectangle's border, then this will be a fraction less than one.  If it is
        *  fully inside the rectangle, then it will be 1.0
        *
        *  This is defined for each of left, right, top, and bottom.
        *
        */

      private def leftTrim: Double = {
        if ((x - 0.5) < rectLeft)
          fractionLeft
        else
          1
      }

      private def rightTrim: Double = {
        if ((x + 0.5) > rectRight)
          fractionRight
        else
          1
      }

      private def topTrim: Double = {
        if ((y - 0.5) < rectTop)
          fractionTop
        else
          1
      }

      private def bottomTrim: Double = {
        if ((y + 0.5) > rectBottom)
          fractionBottom
        else
          1
      }

      /** Area of pixel within the rectangle.  If fully inside the rectangle, then this will be 1 * 1 * 1 * 1 = 1 */
      val area: Double = leftTrim * rightTrim * topTrim * bottomTrim

      val mlc_cu: Float = mlc.get(x, y)
      val open_cu: Float = open.get(x, y)

      /** For this pixel, the MLC/Open ratio.  If the Open value is 0, the division is impossible and the pixel will not be counted. */
      val ratio: Option[Double] = {
        open_cu match {
          case 0       => None
          case open_cu => Some(mlc_cu / open_cu)
        }
      }

      val weightedRatio: Option[Double] = ratio.map(_ * area)
      val weightedMlc_cu: Double = mlc_cu * area
      val weightedOpen_cu: Double = open_cu * area

      /** For debug only. */
      //noinspection ScalaUnusedSymbol
      def isCorner: Boolean = {
        Seq(leftTrim, rightTrim, topTrim, bottomTrim).count(_ < 1) > 1
      }

      /** Useful for debugging. */
      override def toString: String = {
        def fmt(d: Double) = "%10.8f".format(d)
        s"x, y: $x, $y  area: ${fmt(area)}    ratio: ${ratio.map(fmt)}   leftTrim: ${fmt(leftTrim)}    rightTrim: ${fmt(rightTrim)}    topTrim: ${fmt(topTrim)}    bottomTrim: ${fmt(bottomTrim)}"
      }
    }

    // Only consider all pixels that are at least partially inside the rectangle.
    val pixelList = {

      // X coordinate of first pixel that is inside the rectangle.
      val xLo = rectLeft.round.toInt
      // Y coordinate of last pixel that is inside the rectangle.
      val xHi = rectRight.round.toInt

      // Y coordinate of first pixel that is inside the rectangle.
      val yLo = rectTop.round.toInt
      // Y coordinate of last pixel that is inside the rectangle.
      val yHi = rectBottom.round.toInt

      // list of all pixels.
      val pixelList = for (x <- xLo to xHi; y <- yLo to yHi) yield Pixel(x, y)

      /** Code for debugging the nuances of calculating partial pixels.
      if (true) {

        val areaSum = pixelList.map(_.area).sum
        def show(sum: Double, expected: Double, name: String): Unit = {

          // account for double precision roundoff errors
          def isOk(d: Double) = d.abs < 0.0000001

          Trace.trace(s"$name :: sum: $sum    expected: $expected   ok: ${isOk(sum - expected)}")
        }

        val sumW = xHi - xLo - 1 + fractionLeft + fractionRight
        val sumH = yHi - yLo - 1 + fractionBottom + fractionTop

        show(sumW, rect_pix.width, " width")
        show(sumH, rect_pix.height, "height")

        Trace.trace(s"areaSum: $areaSum    sumW * sumH: ${sumW * sumH}    diff: ${areaSum - (sumW * sumH)}    w * h: ${rect_pix.width * rect_pix.height}")

        val cornerText = pixelList.filter(_.isCorner).map(a => s"  corner: $a").mkString("\n")
        Trace.trace(s"Corners: \n$cornerText")
        Trace.trace()
      }
        */
      pixelList
    }

    // Ignore pixels that can not be calculated because the Open field has a pixel value of zero.

    // Mean of ratios
    val ratioMean = {
      val definedPixelList = pixelList.filter(_.ratio.isDefined)
      // Total are to be used for mean of ratios.  For healthy EPID panels this is the same size as
      // the rectangle, but could be smaller because of dead pixels.
      val definedPixelListArea = definedPixelList.map(_.area).sum
      definedPixelList.map(_.weightedRatio.get).sum / definedPixelListArea
    }

    val totalArea = rect_pix.width * rect_pix.height

    val mlcMean_cu = pixelList.map(_.weightedMlc_cu).sum / totalArea
    val openMean_cu = pixelList.map(_.weightedOpen_cu).sum / totalArea

    VMATRectangleRatio(mlcMean_cu, openMean_cu, ratioMean)
  }

}
