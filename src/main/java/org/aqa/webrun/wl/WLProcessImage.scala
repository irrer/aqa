package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.Utility
import edu.umro.DicomDict.TagByName
import org.aqa.Config
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.opensourcephysics.numerics.CubicSpline

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.io.File
import java.io.PrintStream
import java.text.SimpleDateFormat
import java.util.Date
import scala.annotation.tailrec

class WLBadPixel(val x: Int, val y: Int, val rawValue: Int, val correctedValue: Float, val adjacentValidValueList: List[Int]) {
  override def toString: String = {
    "   x: " + x + "   y: " + y + "   rawValue: " + rawValue + "   correctedValue: " + correctedValue +
      adjacentValidValueList.sorted.reverse.foldLeft("\n            Adjacent Valid Values and difference from raw:")((t, v) => t + "\n                " + v + "  :  " + Math.abs(v - rawValue))
  }
}

class WLProcessImage(extendedData: ExtendedData, rtimage: AttributeList) extends org.aqa.Logging {

  private val trans = new IsoImagePlaneTranslator(rtimage)
  private val ResolutionX = trans.pix2IsoDistX(trans.pixelSizeX)
  private val ResolutionY = trans.pix2IsoDistY(trans.pixelSizeY)

  private val SizeX = trans.width
  private val SizeY = trans.height

  // private val gantryAngle = Util.gantryAngle(rtimage)
  // private val collimatorAngle = Util.collimatorAngle(rtimage)

  private val EMPTY_PIXEL: Int = Config.WLTextColor.getRGB() + 1
  // private  val NORMAL_SUMMARY_FILE_NAME = "normalSummary" + IMAGE_FILE_SUFFIX
  // private  val BRIGHT_SUMMARY_FILE_NAME = "brightSummary" + IMAGE_FILE_SUFFIX
  // private  val ORIGINAL_FILE_NAME = "original" + IMAGE_FILE_SUFFIX
  private val BAD_PIXEL_FILE_NAME = "badPixels" + WLgenHtml.IMAGE_FILE_SUFFIX

  def process: WLImageResult = {
    logger.info("Start constructing ProcessImage for " + Util.sopOfAl(rtimage))

    // Number of binary search iterations before determining that the edge has
    // been measured to a sufficient degree.  Each iteration is approximately
    // equivalent to one bit of precision.
    val PRECISION = 30

    /** Convert a value in mm to pixels. */
    def toPixels(mm: Double): Int = ((mm / ResolutionX) + 0.5).toInt

    // tolerance in pixels for how far the search for the fine edge of
    // box should look, given the coarse position of the edge.  Also used
    // determine where an edge of the box finishes, so as to define the
    // area to look for the ball.
    val tol: Int = toPixels(Config.WLBoxEdgeTolerance_mm)
    val tol2 = tol * 2
    val tol4 = tol * 4

    /** Scaling for drawing images. */
    val SCALE: Int = ((Config.WLScale / 0.526) * ((ResolutionX + ResolutionY) / 2.0)).round.toInt

    // The step size (in pixels) for crawling down the curve to find the
    // point where the ball height exceeds a threshold
    val X_INCREMENT: Double = 0.001

    /** Expected radius of ball in (units of) number of pixels. */
    val BALL_RADIUS = toPixels(Config.WLBallRadius)

    def fmt(d: Double): String = d.formatted("%10.5f")

    val imageName: String = {
      val timeText = {

        val fmt = new SimpleDateFormat("MM:ss")
        val ms = DicomUtil.getTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime).get.getTime
        val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
        fmt.format(new Date(elapsed_ms))
      }

      "G" + Util.angleRoundedTo90(Util.gantryAngle(rtimage)).formatted("%03d") +
        "C" + Util.angleRoundedTo90(Util.collimatorAngle(rtimage)).formatted("%03d") + " " +
        timeText
    }

    val diagnostics = new PrintStream(new File(extendedData.output.dir, WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME))

    def diag(text: String): Unit = {
      diagnostics.println(text)
      val msg =
        "G" + Util.angleRoundedTo90(Util.gantryAngle(rtimage)).formatted("%03d") +
          "C" + Util.angleRoundedTo90(Util.collimatorAngle(rtimage)).formatted("%03d") + " " + {
          val fmt = new SimpleDateFormat("MM:ss")
          val ms = DicomUtil.getTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime).get.getTime
          val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
          fmt.format(new Date(elapsed_ms))
        } +
          imageName + " Diagnostics: " + text
      logger.info(msg)
    }

    diag("\n\nMeta-Image Data from ARIA:\n" + extendedData.output.toString)

    def writeDicomAsText(attributeList: AttributeList): Unit = {
      try {
        val text = DicomUtil.attributeListToString(attributeList)
        Utility.writeFile(new File(extendedData.output.dir, Util.sopOfAl(rtimage) + ".txt"), text.getBytes)
      } catch {
        case e: Exception =>
          diag("Unable to write DICOM file as text: " + e)
      }
    }

    class UncorrectedWLBadPixel(val x: Int, val y: Int, val rawValue: Double)

    /**
      * Make a list of bad pixels.
      */
    def findWLBadPixels(rawPixels: Array[Array[Float]], pixelGapLimit: Int, rawDistinctSortedList: List[Float]): List[UncorrectedWLBadPixel] = {
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
      diag("Range of valid pixel values (inclusive): " + limits._1 + " - " + limits._2 + "    Largest value gap in good pixels: " + largestGoodGap)

      val badList = rawPixels.flatten.zipWithIndex.filter(pi => !isValid(pi._1)).map(pix => new UncorrectedWLBadPixel(pix._2 % SizeX, pix._2 / SizeX, pix._1))

      badList.toList
    }

    def uncorrectedWLBadPixelsToWLBadPixels(rawPixelData: Array[Array[Float]], badPixelList: List[UncorrectedWLBadPixel]): List[WLBadPixel] = {
      // A pixel is good if its coordinates are valid and it is not on the bad pixel list
      val height = rawPixelData.length
      val width = rawPixelData(0).length

      def isGoodPixel(x: Int, y: Int): Boolean = {
        (x >= 0) && (y >= 0) && (x < width) && (y < height) && badPixelList.filter(p => p.x == x && p.y == y).isEmpty
      }

      val radius: Int = Config.WLBadPixelCorrectionRadius
      val radSq = radius * radius

      def isClose(bad: UncorrectedWLBadPixel, x: Int, y: Int): Boolean = {
        ((x - bad.x) * (x - bad.x)) + ((y - bad.y) * (y - bad.y)) <= radSq
      }

      def correctSinglePixel(unCor: UncorrectedWLBadPixel): WLBadPixel = {
        val list =
          for (x <- unCor.x - radius to unCor.x + radius; y <- unCor.y - radius to unCor.y + radius; if isGoodPixel(x, y) && isClose(unCor, x, y)) yield rawPixelData(y)(x)
        val correctedValue = list.sum / list.size
        new WLBadPixel(unCor.x, unCor.y, unCor.rawValue.toInt, correctedValue, list.map(f => f.toInt).toList)
      }

      badPixelList.map(bad => correctSinglePixel(bad))
    }

    @tailrec
    def correctWLBadPixels(rawPixels: Array[Array[Float]], badPixelList: List[WLBadPixel]): Array[Array[Float]] = {
      if (badPixelList.isEmpty)
        rawPixels
      else {
        val bad = badPixelList.head

        def fixRow(r: Array[Float]): Array[Float] = {
          (0 until SizeX).map(col => if (col == bad.x) bad.correctedValue else r(col)).toArray
        }

        val o = (0 until SizeY).map(row => if (row == bad.y) fixRow(rawPixels(row)) else rawPixels(row)).toArray
        correctWLBadPixels(o, badPixelList.tail)
      }
    }

    def calcExtremeAveragesRange(pix: Array[Array[Float]]): Double = {
      val ordered = pix.flatten.toList.sorted
      val min = ordered.take(Config.WLAveragePixelsForBrightness).sum / Config.WLAveragePixelsForBrightness
      val max = ordered.reverse.take(Config.WLAveragePixelsForBrightness).sum / Config.WLAveragePixelsForBrightness
      max - min
    }

    def toPngScaled(pix: Array[Array[Float]], imageScale: Int): BufferedImage = {
      val height = pix.length
      val width = pix(0).length
      val min = pix.map(y => y.min).min
      val range = pix.map(y => y.max).max - min
      val imageColor = Config.WLImageColor.getRGB

      val png = new BufferedImage(width * imageScale, height * imageScale, BufferedImage.TYPE_INT_RGB)

      def doPixel(x: Int, y: Int): Unit = {
        val rgb: Int = (((pix(y)(x) - min) / range) * 255).toInt
        val boundedRgb = (if (rgb < 0) 0 else if (rgb > 255) 255 else rgb) * imageColor
        val yb = y * imageScale
        val xb = x * imageScale
        val ye = yb + imageScale
        val xe = xb + imageScale
        for (yi <- yb until ye) for (xi <- xb until xe) png.setRGB(xi, yi, boundedRgb)
      }

      def doRow(y: Int): Unit = (0 until width).foreach(x => doPixel(x, y))

      (0 until height).foreach(y => doRow(y))
      /*
            pix.zipWithIndex foreach {
                case (row: Array[Float], y: Int) => {
                    row.zipWithIndex foreach {
                        case (p: Float, x: Int) => {
                            val rgb: Int = (((p - min) / range) * 255).toInt
                            val boundedRgb = (if (rgb < 0) 0 else if (rgb > 255) 255 else rgb) * imageColor
                            for (sx <- 0 until imageScale) for (sy <- 0 until imageScale) png.setRGB((x * imageScale) + sx, (y * imageScale) + sy, boundedRgb)
                        }
                    }
                }
            }
       */
      png
    }

    def toPng(pix: Array[Array[Float]]): BufferedImage = toPngScaled(pix, SCALE)

    /**
      * Generate an image of the given size with all black pixels
      */
    def toBlackPng(pix: Array[Array[Float]]): BufferedImage = {
      val png = toPngScaled(pix, SCALE)
      for (y <- 0 until png.getHeight) for (x <- 0 until png.getWidth) png.setRGB(x, y, EMPTY_PIXEL)
      png
    }

    def makePngFile(name: String): File = {
      val pngName =
        if (name.endsWith(WLgenHtml.IMAGE_FILE_SUFFIX)) name
        else {
          name + WLgenHtml.IMAGE_FILE_SUFFIX
        }
      new File(extendedData.output.dir, pngName)
    }

    /*
        def writeImageNow(png: BufferedImage, name: String): Unit = {
            val file = makePngFile(name)
            logger.info("writing image file: " + file.getAbsolutePath + "   width: " + png.getWidth + "    height: " + png.getHeight)
            file.delete
            ImageIO.write(png, "png", file)
        }
     */

    def writeImageLater(img: BufferedImage, name: String): Unit = WLProcessImage.writeImage(img, makePngFile(name))

    def bound(x: Double, lo: Int, hi: Int) = {
      if (x < lo) lo else if (x > hi) hi else x
    }

    def boundInt(x: Int, lo: Int, hi: Int) = {
      if (x < lo) lo else if (x > hi) hi else x
    }

    def stdDev(values: Array[Float]): Double = {
      val avg = values.sum / values.length
      Math.sqrt(values.map(x => (x - avg) * (x - avg)).sum / values.length)
    }

    /**
      * Make a list of the sum of each row.
      */
    def rowSum(pix: Array[Array[Float]]): Array[Float] = pix.map(row => row.sum)

    /**
      * Make a list of the sum of each column.
      */
    def colSum(pix: Array[Array[Float]]): Array[Float] = {
      def oneColSum(c: Int) = pix.indices.map(y => pix(y)(c)).sum

      pix(0).indices.map(c => oneColSum(c)).toArray
    }

    def unitize(data: Array[Float]): Array[Float] = {
      val min = data.min
      val range = data.max - min
      data.map(x => (x - min) / range)
    }

    def indexes(length: Int): Array[Double] = (0 until length).toArray.map(x => x.toDouble)

    /*
     * Copy a 2 dimensional sub-array from the given 2 dimensional array.
     *
     * @param pixIn: array of data points
     *
     * @param x: low and high x coordinates
     *
     * @param y: low and high y coordinates
     *
     */
    def subSection(pixIn: Array[Array[Float]], x: Int, xWidth: Int, y: Int, yHeight: Int): Array[Array[Float]] = {

      def range[A](array: Array[A], start: Int, span: Int): Array[A] = {
        if ((start < 0) || (span < 0) || ((start + span) > array.length))
          throw new IllegalArgumentException("ProcessImage.range value out of bounds.  start: " + start + "  span: " + span + "   length: " + array.length)
        array.drop(start).take(span)
      }

      val width = pixIn.head.length
      val height = pixIn.length
      if (
        (x < 0) || (xWidth < 0) || ((x + xWidth) > width) ||
        (y < 0) || (yHeight < 0) || ((y + yHeight) > height)
      ) {
        throw new RuntimeException(
          "ProcessImage.subSection out of range." +
            "  x: " + x +
            "  xWidth: " + xWidth +
            "  y: " + y +
            "  yHeight: " + yHeight +
            "  y: " + y +
            "  width: " + width +
            "  height: " + height
        )
      }

      val pixY = range(pixIn, y, yHeight)
      pixY.map(row => range(row, x, xWidth))
    }

    /**
      * Convert a list to a cubic spline
      */
    def toCubicSpline(data: Array[Float]): CubicSpline = new CubicSpline(data.indices.toArray.map(s => s.toDouble), data.toArray.map(f => f.toDouble))

    /**
      * Save the given edge image, drawing the cubic spline on it to visualize the gradient of the edge.  This is just
      * a debugging tool.
      */
    def saveEdgeImage(pixIn: Array[Array[Float]], vertical: Boolean, name: String, spline: CubicSpline): Unit = {
      val length = if (vertical) pixIn(0).length else pixIn.length
      val png = toPng(pixIn)
      val width = png.getWidth
      val height = png.getHeight

      if (vertical) {
        for (x <- 0 until width) {
          val xd: Double = (x.toDouble / (width + SCALE)) * length
          val y = (spline.evaluate(xd) * height).toInt
          png.setRGB(x, bound(y, 0, height - 1).toInt, Config.WLSplineColor.getRGB)
        }
      } else {
        for (x <- 0 until height) {
          val xd: Double = (x.toDouble / (height + SCALE)) * length
          val y = (spline.evaluate(xd) * width).toInt
          png.setRGB(bound(y, 0, width - 1).toInt, x, Config.WLSplineColor.getRGB)
        }
      }

      val graphic = png.getGraphics
      graphic.setColor(Config.WLSplineColor)
      if (vertical) {
        val sum = unitize(rowSum(pixIn)).map(x => x * pixIn.head.length)
        sum.zipWithIndex.foreach(x => graphic.drawLine(x._1.toInt * SCALE + SCALE / 2, x._2 * SCALE, x._1.toInt * SCALE + SCALE / 2, x._2 * SCALE + SCALE))
      } else {
        val sum = unitize(colSum(pixIn)).map(y => y * pixIn.length)
        sum.zipWithIndex.foreach(y => graphic.drawLine(y._2 * SCALE, y._1.toInt * SCALE + SCALE / 2, y._2 * SCALE + SCALE, y._1.toInt * SCALE + SCALE / 2))
      }

      writeImageLater(png, "edge_" + name)
    }

    /**
      * Get the absolute value of the slope of the cubic spline at both ends.
      */
    def getTerminalEdgeSlopes(spline: CubicSpline, length: Int): (Double, Double) = {
      val incr = 0.005
      val yA1 = spline.evaluate(0.0)
      val yA2 = spline.evaluate(incr)
      val slopeA = ((yA2 - yA1) / incr).abs

      val yB1 = spline.evaluate(length.toDouble)
      val yB2 = spline.evaluate(length.toDouble + incr)
      val slopeB = ((yB2 - yB1) / incr).abs
      (slopeA, slopeB)
    }

    def showEdgeStats(pixIn: Array[Array[Float]], vertical: Boolean, name: String, terminalEdgeSlopes: (Double, Double)): Unit = {
      val sumRaw = if (vertical) rowSum(pixIn) else colSum(pixIn)

      val min = sumRaw.min
      val sum = sumRaw.map(x => x - min)

      val avg = sum.sum / sum.length

      val sumX = (sum.length * (sum.length + 1)) / 2.0
      val sumY = sum.sum

      val meanX = sumX / sum.length
      val meanY = sumY / sum.length

      val sumXSq = sum.indices.map(x => x * x).sum
      val sumXY = sum.zipWithIndex.map(xy => xy._1 * xy._2).sum
      val slope = (sumXY - ((sumX * sumY) / sum.length)) / (sumXSq - ((sumX * sumX) / sum.length))
      val yIntercept = meanY - (slope * meanX)

      diag(
        "Edge stats for " + name.formatted("%8s") +
          "    min: " + fmt(sum.min) +
          "    max: " + fmt(sum.max) +
          "    avg: " + fmt(avg) +
          "    stdDev: " + fmt(stdDev(sum)) +
          "    yIntercept: " + fmt(yIntercept) +
          "    slope: " + fmt(slope) +
          "    start slope: " + fmt(terminalEdgeSlopes._1) +
          "    end   slope: " + fmt(terminalEdgeSlopes._2)
      )
    }

    /**
      * Find an edge of the box as accurately as possible by drawing a cubic spline across
      * the edge and then finding the midpoint of that spline.  Find the midpoint using a binary
      * search.
      */
    def findEdge(pixIn: Array[Array[Float]], vertical: Boolean, name: String, rawExtremeAveragesRange: Double): Either[ProcedureStatus.Value, Double] = {
      val length = if (vertical) pixIn(0).length else pixIn.length
      val sum = (if (vertical) colSum(pixIn) else rowSum(pixIn))
      val min = sum.min
      val range = sum.max - min
      val scaledSum = unitize(sum)
      val indexes = (0 until length).toArray.map(x => x.toDouble)
      val spline = toCubicSpline(scaledSum)

      saveEdgeImage(pixIn, vertical, name, spline)
      diag("\n\n")
      val terminalEdgeSlopes = getTerminalEdgeSlopes(spline, scaledSum.length)
      showEdgeStats(pixIn, vertical, name, terminalEdgeSlopes)

      val increasing = scaledSum(0) < scaledSum(length - 1)

      def center(min: Double, max: Double, depth: Int): Double = {
        val mid = (max + min) / 2
        val guess = spline.evaluate(mid)
        if (depth > 0) {
          if ((increasing && (guess < 0.5)) || ((!increasing) && (guess > 0.5)))
            center(mid, max, depth - 1)
          else
            center(min, mid, depth - 1)
        } else mid
      }

      // Ensure that the brightest and dimmest pixel in the edge are approximately
      // as dim and bright as the dimmest and brightest in the entire image

      val brightnessRange = calcExtremeAveragesRange(pixIn)
      val pct = ((rawExtremeAveragesRange - brightnessRange).abs / rawExtremeAveragesRange) * 100.0
      val brightnessMessage = "edge brightness   Max percent diff range allowed: " + Config.WLMaxAllowedBrightnessRangePercentDifference +
        "  image brightness range: " + rawExtremeAveragesRange.formatted("%7.2f") +
        name.formatted("%s8") + " edge brightness range: " + brightnessRange.formatted("%7.2f") + "    percent diff: " + pct.formatted("%7.3f")
      diag(brightnessMessage)

      if (pct >= Config.WLMaxAllowedBrightnessRangePercentDifference) {
        val errorMsg =
          "Edge " + name + " failed to meet criteria for brightness range of " + Config.WLMaxAllowedBrightnessRangePercentDifference + " percent.  " + brightnessMessage + "    Required percent" + Config.WLMaxAllowedBrightnessRangePercentDifference
        logger.error(errorMsg)
        Left(ProcedureStatus.fail)
      } else Right(center(0, sum.length - 1, PRECISION))
    }

    /**
      * Get the x,y coordinates of the point with the largest value
      */
    def maxPoint(area: Array[Array[Double]]): (Int, Int) = {
      val coord = area.flatten.zipWithIndex.maxBy(_._1)._2
      (coord % area.length, coord / area.length)
    }

    /**
      * Translate a source image coordinate into a the coordinate system used to draw graphics.
      * Add the 0.5 to get to the center of the displayed pixel.
      */
    def coord(x: Double): Int = {
      ((x + 0.5) * SCALE).toInt
    }

    /**
      * Draw the horizontal and vertical lines marking the center of something.  By convention in
      * this project, it marks the center of the ball.
      */
    def drawCross(graphics: Graphics, x: Double, y: Double, markerSize: Double): Unit = {
      graphics.drawLine(coord(x - markerSize), coord(y), coord(x + markerSize), coord(y))
      graphics.drawLine(coord(x), coord(y - markerSize), coord(x), coord(y + markerSize))

    }

    def drawCircles(graphics: Graphics, x: Double, y: Double): Unit = {
      graphics.setColor(Config.WLBallColor)
      val NUM_CIRCLE = Config.WLNumberOfCircles

      def graphicDistance(d: Double): Int = (d * SCALE + .5).toInt

      for (circle <- 1 to NUM_CIRCLE) {
        val radius = (circle.toDouble / NUM_CIRCLE) * BALL_RADIUS
        graphics.drawOval(coord(x - radius), coord(y - radius), graphicDistance(radius * 2), graphicDistance(radius * 2))
        //graphics.drawOval(coord(x - radius), coord(y - radius), coord(radius * 2), coord(radius * 2))
      }
    }

    /**
      * Find the center of the ball within the given area.  Return the
      * coordinates relative to the area given.
      */
    def findBallCenter(areaOfInterest: Array[Array[Float]], approxAoi: Array[Array[Float]], fieldWidthUnscaled: Double): Option[(Double, Double)] = {

      /**
        * Coarsely locate the center of the ball by creating a copy of the area expected to contain
        * the center of the ball, and then transforming it so that each pixel contains the sum of
        * the 8 neighboring pixels and itself, then find the pixel with the largest value and take that
        * as the ball's center.  This effectively says: "Find the largest sum of 3x3 pixels".
        */
      def coarseBallLocate: (Int, Int) = {
        val aoiMax = list2Array(subSection(approxAoi, 0, approxAoi.head.length, 0, approxAoi.length))

        for (y <- 1 until (approxAoi.length - 1)) {
          for (x <- 1 until (approxAoi.head.length - 1)) {
            aoiMax(y)(x) =
              approxAoi(y - 1)(x - 1) + approxAoi(y - 1)(x) + approxAoi(y - 1)(x + 1) +
                approxAoi(y)(x - 1) + approxAoi(y)(x) + approxAoi(y)(x + 1) +
                approxAoi(y + 1)(x - 1) + approxAoi(y + 1)(x) + approxAoi(y + 1)(x + 1)
          }
        }

        def saveImage(center: (Int, Int)): Unit = {
          val png = toPng(approxAoi)
          drawCross(png.getGraphics, center._1, center._2, 1)
          writeImageLater(png, "ball_coarse")
        }

        val maxRow = aoiMax.zipWithIndex.foldLeft((aoiMax.head, 0))((c, m) => if (m._1.max > c._1.max) m else c)
        val maxCol = maxRow._1.zipWithIndex.foldLeft((aoiMax.head.head, 0))((c, m) => if (m._1 > c._1) m else c)
        val center = (maxCol._2, maxRow._2) // maxPoint(aoiMax)
        saveImage(center)
        center
      }

      class SearchRange(val lo: Double, val center: Double, val hi: Double) {}

      def saveFineLocatedImage(aoi: Array[Array[Float]], name: String, xPosn: SearchRange, yPosn: SearchRange): Unit = {
        val rSpline = toCubicSpline(unitize(rowSum(aoi)).map(x => (x * .5).toFloat))
        val cSpline = toCubicSpline(unitize(colSum(aoi)).map(x => (x * .5).toFloat))

        val width = aoi(0).length
        val height = aoi.length
        val png = toPng(aoi)

        for (x <- -SCALE until width * SCALE) {
          val y = (height * SCALE - 1 - (cSpline.evaluate(x.toDouble / SCALE) * (height * SCALE - 2))).toInt
          val xo = x + (SCALE / 2)
          if ((xo >= 0) && (xo < (width * SCALE))) png.setRGB(xo, boundInt(y, 0, height * SCALE - 1), Config.WLSplineColor.getRGB)
        }

        for (y <- -SCALE until height * SCALE) {
          val x = (1 + (rSpline.evaluate(y.toDouble / SCALE) * (width * SCALE - 2))).toInt
          val yo = y + (SCALE / 2)
          if ((yo >= 0) && (yo < (height * SCALE))) png.setRGB(boundInt(x, 0, width * SCALE - 1), yo, Config.WLSplineColor.getRGB)
        }

        val graphics = png.getGraphics
        graphics.setColor(Config.WLBallColor)

        drawCross(graphics, xPosn.center, yPosn.center, 20)

        drawCircles(graphics, xPosn.center, yPosn.center)

        graphics.setColor(Config.WLSplineColor)
        graphics.drawLine(coord(xPosn.lo), 0, coord(xPosn.lo), coord(height))
        graphics.drawLine(coord(xPosn.hi), 0, coord(xPosn.hi), coord(height))

        graphics.setColor(Config.WLSplineColor)
        graphics.drawLine(0, coord(yPosn.lo), coord(width), coord(yPosn.lo))
        graphics.drawLine(0, coord(yPosn.hi), coord(width), coord(yPosn.hi))

        writeImageLater(png, name)
      }

      def centerOfMass(spline: CubicSpline, len: Int): SearchRange = {
        val pointList = (0.0 until len.toDouble by X_INCREMENT).map(i => spline.evaluate(i))
        val min = pointList.min
        val max = pointList.max

        val minAcceptable = min + ((max - min) * (1.0 - (Config.WLBallHeightPercentForeground / 100.0)))

        val lo = pointList.indexWhere(p => p >= minAcceptable)
        val hi = (pointList.size - 1) - pointList.reverse.indexWhere(p => p >= minAcceptable)

        val weighted = (lo to hi).map(i => pointList(i) * i).sum * X_INCREMENT
        val sum = pointList.drop(lo).take(hi - lo).sum
        val cntrOfMass = weighted / sum

        new SearchRange(lo * X_INCREMENT, cntrOfMass, hi * X_INCREMENT)
      }

      /**
        * Determine if the spline has a single maximum by walking the spline and counting the
        * number of times it crosses the average value.  It should cross exactly twice.
        */
      def singleMax(spline: CubicSpline, values: Array[Float]): Boolean = {
        val avg = (values.max + values.min) / 2
        val increment = 1000

        // determine if this value is over or under the average, and increment the count accordingly
        // crossCount: Number of times that the average was crossed.
        def cross(x: Int, crossCount: Int, overAvg: Boolean): (Int, Boolean) = {
          val value = spline.evaluate(x.toDouble / increment)
          0 match {
            case _ if overAvg && (value < avg)     => (crossCount + 1, false)
            case _ if (!overAvg) && (value >= avg) => (crossCount + 1, true)
            case _                                 => (crossCount, overAvg)
          }
        }

        val state = (0 to (values.length * increment)).toList.foldLeft((0, false))((s, x) => cross(x, s._1, s._2))
        val crsCount = state._1
        if (crsCount == 2) {
          diag(" Ball spline verified to cross average height exactly twice")
          true
        } else {
          diag(" Wrong number of times that the ball spline crossed the average value.  Should be 2 but was " + crsCount)
          false
        }
      }

      /*
       * @param x: Approximate horizontal center of ball
       * @param y: Approximate vertical center of ball
       */

      def fineBallLocate(aoiFine: Array[Array[Float]]): Option[(Double, Double)] = {
        val rSum = unitize(rowSum(aoiFine))
        val rSpline = toCubicSpline(rSum)
        val cSum = unitize(colSum(aoiFine))
        val cSpline = toCubicSpline(cSum)

        val fineX = centerOfMass(cSpline, cSum.length)
        val fineY = centerOfMass(rSpline, rSum.length)
        saveFineLocatedImage(aoiFine, "ball_fine", fineX, fineY)

        if (singleMax(cSpline, cSum) && singleMax(rSpline, rSum)) {
          Some(fineX.center, fineY.center)
        } else None
      }

      val coarseCenter = coarseBallLocate
      // not sure why 0.3 works
      val radius: Int = (toPixels(Config.WLBallRadius) + (tol.toDouble * 0.3) + 0.5).toInt
      val leftEdge = coarseCenter._1 + tol2 - radius
      val topEdge = coarseCenter._2 + tol2 - radius
      val ballRoi = subSection(areaOfInterest, leftEdge, radius * 2, topEdge, radius * 2)

      writeImageLater(toPng(normalizeArea(ballRoi)), "ballRoiNormalized")
      writeImageLater(toPng(ballRoi), "ballRoiRaw")

      fineBallLocate(ballRoi) match {
        case Some(loc: (Double, Double)) => Some(loc._1 + leftEdge, loc._2 + topEdge)
        case None                        => None
      }
    }

    /**
      * draw line between center of square and center of ball
      */
    def drawBoxBallOffset(graphics: Graphics, ballCenter: (Double, Double), boxCenter: (Double, Double)): Unit = {
      graphics.setColor(Config.WLOffsetColor)

      graphics.drawLine(coord(ballCenter._1), coord(ballCenter._2), coord(boxCenter._1), coord(boxCenter._2))
      graphics.drawLine(coord(ballCenter._1) - 1, coord(ballCenter._2), coord(boxCenter._1) - 1, coord(boxCenter._2))
      graphics.drawLine(coord(ballCenter._1) + 1, coord(ballCenter._2), coord(boxCenter._1) + 1, coord(boxCenter._2))
      graphics.drawLine(coord(ballCenter._1), coord(ballCenter._2) - 1, coord(boxCenter._1), coord(boxCenter._2) - 1)
      graphics.drawLine(coord(ballCenter._1), coord(ballCenter._2) + 1, coord(boxCenter._1), coord(boxCenter._2) + 1)
    }

    def list2Array(x: Array[Array[Float]]) = x.map(x => x.toArray)

    /**
      * Make an image showing the level of background noise immediately around the ball.
      */
    def showBallBackgroundNoise(areaOfInterest: Array[Array[Float]], name: String): Unit = {
      val aoiWidth = areaOfInterest.head.length
      val aoiHeight = areaOfInterest.length
      val aoi = subSection(areaOfInterest, 0, aoiWidth, 0, aoiHeight)

      val min = aoi.flatten.min
      val max = aoi.flatten.max
      val limit = ((max - min) * 0.08) + min
      val aoiArray = list2Array(aoi)
      for (y <- 0 until aoiHeight) for (x <- 0 until aoiWidth) aoiArray(y)(x) = if (aoi(y)(x) > limit) min else aoi(y)(x)
      writeImageLater(toPng(aoi), name)
    }

    /**
      * Take the average of the darkest background pixels for
      * each row and subtract it from each pixel.
      */
    def normalizeArea(aoi: Array[Array[Float]]): Array[Array[Float]] = {
      aoi.map(row => {
        val bias = row.sorted.take(Config.WLNumBackgroundPixels).sum / Config.WLNumBackgroundPixels
        row.map(col => if (col > bias) col - bias else 0)
      })
    }

    /**
      * Coarsely locate the edges of the box by looking for the
      * low areas in the given profile of the image.  The profile will
      * be either vertical or horizontal, and is the sum of all pixels
      * in that orientation.
      */
    def coarseBoxLocate(sum: Array[Float]): (Int, Int) = {
      val lo = sum.min.toDouble
      val range = sum.max.toDouble - lo
      val dSum = sum.map(s => (s.toDouble - lo) / range)

      val hiList = sum.indices.filter(d => dSum(d) < 0.5)
      val edgeA = sum.indices.filter(d => dSum(d) < 0.5).head
      val edgeB = sum.indices.filter(d => dSum(d) < 0.5).last

      def bound(v: Int): Int = if (v < tol) tol else if (v > (sum.length - (tol + 1))) sum.length - tol else v

      (bound(hiList.head) - tol, bound(hiList.last) + tol)
    }

    /**
      * Locate the box to sub-pixel accuracy.
      */
    def fineBoxLocate(areaOfInterest: Array[Array[Float]], rawExtremeAveragesRange: Double): Either[ProcedureStatus.Value, Edges] = {
      val height = areaOfInterest.length
      val width = areaOfInterest(0).length

      val topArea = subSection(areaOfInterest, tol2, width - tol4, 0, tol2)
      val bottomArea = subSection(areaOfInterest, tol2, width - tol4, height - tol2, tol2)
      val leftArea = subSection(areaOfInterest, 0, tol2, tol2, height - tol4)
      val rightArea = subSection(areaOfInterest, width - tol2, tol2, tol2, height - tol4)

      val eTop = findEdge(topArea, vertical = false, "top", rawExtremeAveragesRange)
      val eBottom = findEdge(bottomArea, vertical = false, "bottom", rawExtremeAveragesRange)
      val eLeft = findEdge(leftArea, vertical = true, "left", rawExtremeAveragesRange)
      val eRight = findEdge(rightArea, vertical = true, "right", rawExtremeAveragesRange)

      def edgeStatus(e: Either[ProcedureStatus.Value, Double]): ProcedureStatus.Value = {
        e match {
          case Right(dbl) => ProcedureStatus.pass
          case Left(s)    => s
        }
      }

      val status: ProcedureStatus.Value = {
        val list = List(eTop, eBottom, eLeft, eRight).map(edgeStatus).distinct.filterNot(_ == ProcedureStatus.pass)
        if (list.isEmpty) ProcedureStatus.pass else list.head
      }

      if (status == ProcedureStatus.pass) {
        val edgeTop: Double = eTop.right.get
        val edgeBottom = eBottom.right.get + (height - tol2)
        val edgeLeft = eLeft.right.get
        val edgeRight = eRight.right.get + (width - tol2)

        val boxPrefix = "Box "

        val edges = new Edges(edgeTop, edgeBottom, edgeLeft, edgeRight)
        diag("\n\nUnscaled box dimensions in pixels")
        diagnostics.write(edges.toString.getBytes)
        Right(edges)
      } else
        Left(status)
    }

    /**
      * Draw the lines that show where the box has been located.
      */
    def drawBoxGraphics(png: BufferedImage, graphics: Graphics, top: Double, bottom: Double, left: Double, right: Double, color: Color, outside: Double, inside: Double): Unit = {
      graphics.setColor(color)
      graphics.drawLine(coord(left), coord(top), coord(left), coord(bottom)) // vertical line left
      graphics.drawLine(coord(right), coord(top), coord(right), coord(bottom)) // vertical line right
      graphics.drawLine(coord(left), coord(top), coord(right), coord(top)) // horizontal line top
      graphics.drawLine(coord(left), coord(bottom), coord(right), coord(bottom)) // horizontal line bottom

      {
        val m = (top - bottom) / (left - right)
        val b = top - (left * m)
        val x1 = left - outside
        val y1 = (m * x1) + b

        if (inside >= 0) {
          val x2 = left + inside
          val y2 = (m * x2) + b
          graphics.drawLine(coord(x1), coord(y1), coord(x2), coord(y2))

          val x3 = right - inside
          val y3 = m * x3 + b
          val x4 = right + outside
          val y4 = m * x4 + b
          graphics.drawLine(coord(x3), coord(y3), coord(x4), coord(y4))
        } else {
          val x2 = right
          val y2 = bottom
          graphics.drawLine(coord(x1), coord(y1), coord(x2), coord(y2))
        }
      }

      {
        val m = (top - bottom) / (right - left)
        val b = top - (right * m)
        val x1 = right + outside
        val y1 = (m * x1) + b
        if (inside >= 0) {
          val x2 = right - inside
          val y2 = (m * x2) + b
          graphics.drawLine(coord(x1), coord(y1), coord(x2), coord(y2))

          val x3 = left - outside
          val y3 = m * x3 + b
          val x4 = left + inside
          val y4 = m * x4 + b
          graphics.drawLine(coord(x3), coord(y3), coord(x4), coord(y4))
        } else {
          val x2 = left
          val y2 = bottom
          graphics.drawLine(coord(x1), coord(y1), coord(x2), coord(y2))
        }
      }
    }

    /**
      * Draw graphics to indicate where the ball is.
      * Draw outline of ball and diagonal cross-hairs at center of ball
      */
    def drawBallGraphics(graphics: Graphics, ballCenterX: Double, ballCenterY: Double): Unit = {
      graphics.setColor(Config.WLBallColor)

      drawCircles(graphics, ballCenterX, ballCenterY)
      drawCross(graphics, ballCenterX, ballCenterY, BALL_RADIUS)
    }

    def highlightWLBadPixelList(badPixelList: List[WLBadPixel], graphics: Graphics2D) = {
      val circleRadius = Config.WLBadPixelCorrectionRadius
      val dotRadius = 3.0 / SCALE
      graphics.setColor(Config.WLFailColor)

      def highlightWLBadPixel(badPixel: WLBadPixel) = {
        val hp = SCALE / 2

        def crd(x: Double): Int = ((x * SCALE) + 0.5).toInt

        val x = badPixel.x
        val y = badPixel.y

        (0 until 9).foreach(i => {
          val rx = circleRadius + ((i % 3).toFloat / SCALE)
          val ry = circleRadius + ((i / 3).toFloat / SCALE)
          graphics.drawOval(crd(x - rx) + hp, crd(y - ry) + hp, crd(rx * 2), crd(ry * 2))
        })
        (0 until 3).map(i => graphics.drawRect(crd(x) - i, crd(y) - i, SCALE + (2 * i), SCALE + (2 * i)))
      }

      badPixelList.map(b => highlightWLBadPixel(b))
    }

    /**
      * Annotate the image with words and numbers.
      */
    def annotateImage(
        png: BufferedImage,
        graphics: Graphics2D,
        errorScaledX: Double,
        errorScaledY: Double,
        errorScaledXYCombined: Double,
        badPixelList: List[WLBadPixel],
        background: Boolean
    ): ProcedureStatus.Value = {
      def fmt(d: Double) = d.formatted("%6.2f").replaceAll(" ", "")

      graphics.setColor(Config.WLTextColor)

      val font = GraphicFont.getFont
      graphics.setFont(font)
      val fontHeight = GraphicFont.getFontHeight
      graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)

      val spacer = "        "
      val text1 = "Offset in mm:    X = " + fmt(errorScaledX) + spacer + " Y = " + fmt(errorScaledY) + spacer
      val frc = GraphicFont.getFontRenderContext
      val stringRectangle1 = font.getStringBounds(text1, frc)
      val xPosn1 = (png.getWidth - stringRectangle1.getWidth) / 2
      val yPosn1 = stringRectangle1.getHeight
      graphics.drawString(text1, xPosn1.toInt, yPosn1.toInt)

      val combinedXY = "R = " + fmt(errorScaledXYCombined)
      val passed = if (errorScaledXYCombined <= Config.WLPassLimit) ImageStatus.Passed else ImageStatus.OffsetLimitExceeded
      val statusText: String = {
        if (passed == ImageStatus.Passed) "PASSED" else "FAILED"
      }

      val statusColor = if (passed == ImageStatus.Passed) Config.WLPassColor else Config.WLFailColor
      graphics.setBackground(statusColor)
      val text2 = combinedXY + spacer + statusText
      val stringRectangle2 = font.getStringBounds(text2, frc)
      val xPosn2 = (png.getWidth - font.getStringBounds(text2, frc).getWidth) / 2
      val yPosn2 = stringRectangle1.getHeight + stringRectangle2.getHeight
      val stringRectangleStatus = font.getStringBounds(statusText, frc)

      val statusWidth = stringRectangleStatus.getWidth.toInt
      val statusHeight = graphics.getFontMetrics.getMaxAscent
      val statusX = (xPosn2 + stringRectangle2.getWidth - stringRectangleStatus.getWidth).toInt
      val statusY = yPosn2 - stringRectangleStatus.getHeight + ((graphics.getFontMetrics.getHeight - graphics.getFontMetrics.getAscent) * 1.5 - 1)
      if (background) graphics.clearRect(statusX, statusY.toInt, statusWidth, statusHeight)
      graphics.drawString(text2, xPosn2.toInt, yPosn2.toInt)

      val stringRectangle3 = font.getStringBounds(imageName, frc)
      val xPosn3 = (png.getWidth - stringRectangle3.getWidth) / 2
      val yPosn3 = png.getHeight - stringRectangle3.getHeight
      graphics.drawString(imageName, xPosn3.toInt, yPosn3.toInt)

      ProcedureStatus.pass
    }

    /**
      * Construct the pixel data destined to be put in the DICOM image with graphics and annotations.  This is done by
      * taking a buffered image that is all black except for the graphics and annotations, and then mapping that onto
      * a scaled up version of the original pixels.
      */
    def constructPixelData(blackPng: BufferedImage, areaOfInterest: Array[Array[Float]]): Array[Array[Float]] = {
      val min = areaOfInterest.flatten.min
      val max = areaOfInterest.flatten.max
      val avg = (min + max) / 2
      val width = blackPng.getWidth
      val height = blackPng.getHeight
      val aoi = Array.ofDim[Float](height, width)

      for (y <- 0 until height) for (x <- 0 until width) {
        val pixelOrig = areaOfInterest(y / SCALE)(x / SCALE)
        val pixelGraphic =
          if (blackPng.getRGB(x, y) == EMPTY_PIXEL)
            pixelOrig
          else if (pixelOrig < avg)
            max
          else
            min
        aoi(y)(x) = pixelGraphic
      }

      aoi
    }

    /**
      * Do a sanity check to see of the coarsely located box is approximately at the
      * position and of the size that is expected.  If so, return true.  If not, log
      * a diagnostic message and return false.
      */
    def coarseBoxLocationIsGood(coarseX: (Int, Int), coarseY: (Int, Int)): Boolean = {
      val boxPositionVariance = Config.WLBoxPositionFactor * Config.WLBoxSize

      val xCenterBox = ((coarseX._1 + coarseX._2) / 2.0) * ResolutionX
      val yCenterBox = ((coarseY._1 + coarseY._2) / 2.0) * ResolutionY

      val xCenterImage = (SizeX / 2.0) * ResolutionX
      val yCenterImage = (SizeY / 2.0) * ResolutionY

      val boxSmallestAllowed = (1 - (Config.WLBoxSizePercent * 0.01)) * Config.WLBoxSize
      val boxLargestAllowed = (1 + (Config.WLBoxSizePercent * 0.01)) * Config.WLBoxSize

      val width = (coarseX._2 - coarseX._1) * ResolutionX
      val height = (coarseY._2 - coarseY._1) * ResolutionY

      val ok: Boolean = 0 match {
        case _ if (xCenterImage - xCenterBox) > boxPositionVariance => diag("Box located too far to left"); false
        case _ if (yCenterImage - yCenterBox) > boxPositionVariance => diag("Box located too far to top"); false
        case _ if (xCenterBox - xCenterImage) > boxPositionVariance => diag("Box located too far to right"); false
        case _ if (yCenterBox - yCenterImage) > boxPositionVariance => diag("Box located too far to bottom"); false

        case _ if width < boxSmallestAllowed  => diag("Box is too narrow"); false
        case _ if width > boxLargestAllowed   => diag("Box is too wide"); false
        case _ if height < boxSmallestAllowed => diag("Box is too short"); false
        case _ if height > boxLargestAllowed  => diag("Box is too tall"); false

        case _ => true
      }
      ok
    }

    /**
      * Determine whether the area inside the box is flat.  Do this by determine the ratio of the
      * range of the box pixels over the range of the ball pixels.  If this number is too large,
      * then the area is too flat to contain a ball.
      */
    def ballAreaIsFlat(boxArea: Array[Array[Float]], ballEdges: Edges): Boolean = {
      val boxWd = boxArea.head.length
      val boxHt = boxArea.length

      // Get the pixel that are not part of the ball
      val backgroundPixels = {
        // delineate a border that is half way between the outer edge of the ball and edge of
        // the box.  Use the pixels in this border to get a good sample of background pixels that
        // do not include the ball.
        val xMin = (ballEdges.left / 2).toInt
        val xMax = (ballEdges.right + ((boxWd - ballEdges.right) / 2)).toInt
        val yMin = (ballEdges.top / 2).toInt
        val yMax = (ballEdges.bottom + ((boxHt - ballEdges.bottom) / 2)).toInt
        for (
          x <- 0 until boxWd;
          y <- 0 until boxHt
          if (x < xMin) || (x > xMax) || (y < yMin) || (y > yMax)
        ) yield {
          boxArea(y)(x)
        }
      }

      val backgroundRange = backgroundPixels.max - backgroundPixels.min

      val boxValues = boxArea.flatten

      val boxRange = boxValues.max - boxValues.min

      val ratio = boxRange / backgroundRange

      val stats = " BallAreaFlatnessRatioLowerLimit: " + Config.WLBallAreaFlatnessRatioLowerLimit + "    measured ratio: " + ratio +
        "   total pixel range including ball and background: " + boxRange + "    background pixel range: " + backgroundRange
      if (ratio < Config.WLBallAreaFlatnessRatioLowerLimit) {
        val msg = "Flatness check: Failed to find ball in box because area inside box was flat. " + stats
        logger.error(msg)
        diag("Severe error: " + msg)
        true
      } else {
        val msg = "Flatness check: The area inside the box contains a ball. " + stats
        logger.error(msg)
        diag(msg)
        false
      }
    }

    /**
      * Indicate that something is wrong with the image.
      */
    def imageError(status: ImageStatus.Value, msg: String, badPixelList: List[WLBadPixel], marginalPixelList: List[WLBadPixel]): WLImageResult = {
      val fullMsg = "Image " + imageName + "  " + msg
      logger.error(fullMsg)
      diag(fullMsg)
      val imageResult = new WLImageResult(status, null, null, null, directory = extendedData.output.dir, attributeList = rtimage, null, null, marginalPixelList)
      diag(imageResult.toString)
      WLgenHtml.generateHtml(extendedData, imageResult)
      imageResult
    }

    def correctUnscaledEdges(edgesUnscaled: Edges): Edges = {
      def scaleX(x: Double): Double = (x * ResolutionX) / ResolutionX
      def scaleY(y: Double): Double = (y * ResolutionY) / ResolutionY
      val scaled = new Edges(scaleY(edgesUnscaled.top), scaleY(edgesUnscaled.bottom), scaleX(edgesUnscaled.left), scaleX(edgesUnscaled.right))
      scaled
    }

    /**
      * After locating the center of the box and the ball with some confidence, process the results.  The only
      * error that is expected to occur after this point is that the distance between the centers is too large.
      */
    def processLocation(
        areaOfInterest: Array[Array[Float]],
        edgesUnscaled: Edges,
        ballRelativeCenter: (Double, Double),
        ballArea: Array[Array[Float]],
        badPixelList: List[WLBadPixel],
        badPixelListShifted: List[WLBadPixel],
        marginalPixelList: List[WLBadPixel],
        attributeList: AttributeList
    ): WLImageResult = {

      val ballCenterX = ballRelativeCenter._1
      val ballCenterY = ballRelativeCenter._2

      // Limit all pixel brightnesses to the maximum ball pixel brightness.  This makes the ball stand out more in the final image.
      val maxBallPixel = ballArea.flatten.max
      val thresholdedAreaOfInterest = areaOfInterest.map(r => r.map(c => if (c > maxBallPixel) maxBallPixel else c))

      //drawGraphics()
      val normalPng = toPng(areaOfInterest)
      val brightPng = toPng(thresholdedAreaOfInterest)
      val blackPng = toBlackPng(areaOfInterest)

      // draw edge of box
      val normalGraphics = normalPng.getGraphics.asInstanceOf[Graphics2D]
      val brightGraphics = brightPng.getGraphics.asInstanceOf[Graphics2D]
      val blackGraphics = blackPng.getGraphics.asInstanceOf[Graphics2D]

      drawBallGraphics(normalGraphics, ballCenterX, ballCenterY)
      drawBallGraphics(brightGraphics, ballCenterX, ballCenterY)
      drawBallGraphics(blackGraphics, ballCenterX, ballCenterY)

      highlightWLBadPixelList(badPixelListShifted, normalGraphics)
      highlightWLBadPixelList(badPixelListShifted, brightGraphics)
      highlightWLBadPixelList(badPixelListShifted, blackGraphics)

      val bin = 6
      val bout = 6
      drawBoxGraphics(normalPng, normalGraphics, edgesUnscaled.top, edgesUnscaled.bottom, edgesUnscaled.left, edgesUnscaled.right, Config.WLBoxColor, bin, bout)
      drawBoxGraphics(brightPng, brightGraphics, edgesUnscaled.top, edgesUnscaled.bottom, edgesUnscaled.left, edgesUnscaled.right, Config.WLBoxColor, bin, bout)
      drawBoxGraphics(blackPng, blackGraphics, edgesUnscaled.top, edgesUnscaled.bottom, edgesUnscaled.left, edgesUnscaled.right, Config.WLBoxColor, bin, bout)

      val boxShrink = 5
      val boxUnscaledCorrected = correctUnscaledEdges(edgesUnscaled)
      val buc = boxUnscaledCorrected
      val inner = 24
      drawBoxGraphics(normalPng, normalGraphics, buc.top + boxShrink, buc.bottom - boxShrink, buc.left + boxShrink, buc.right - boxShrink, Config.WLBoxColorCorrected, 0, -1)
      drawBoxGraphics(brightPng, brightGraphics, buc.top + boxShrink, buc.bottom - boxShrink, buc.left + boxShrink, buc.right - boxShrink, Config.WLBoxColorCorrected, 0, -1)
      drawBoxGraphics(blackPng, blackGraphics, buc.top + boxShrink, buc.bottom - boxShrink, buc.left + boxShrink, buc.right - boxShrink, Config.WLBoxColorCorrected, 0, -1)

      drawBoxBallOffset(normalGraphics, (ballCenterX, ballCenterY), ((buc.left + buc.right) / 2, (buc.top + buc.bottom) / 2))
      drawBoxBallOffset(brightGraphics, (ballCenterX, ballCenterY), ((buc.left + buc.right) / 2, (buc.top + buc.bottom) / 2))
      drawBoxBallOffset(blackGraphics, (ballCenterX, ballCenterY), ((buc.left + buc.right) / 2, (buc.top + buc.bottom) / 2))

      val boxUnscaledCorrectedCenterX = (boxUnscaledCorrected.left + boxUnscaledCorrected.right) / 2
      val boxUnscaledCorrectedCenterY = (boxUnscaledCorrected.top + boxUnscaledCorrected.bottom) / 2

      val boxCenterScaledX = boxUnscaledCorrectedCenterX * ResolutionX
      val boxCenterScaledY = boxUnscaledCorrectedCenterY * ResolutionY
      val ballCenterScaledX = ballCenterX * ResolutionX
      val ballCenterScaledY = ballCenterY * ResolutionY

      val errorScaledX = boxCenterScaledX - ballCenterScaledX
      val errorScaledY = boxCenterScaledY - ballCenterScaledY
      val errorScaledXYCombined = Math.sqrt((errorScaledX * errorScaledX) + (errorScaledY * errorScaledY))

      val passed: ImageStatus.ImageStatus = {
        val p = annotateImage(normalPng, normalGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, badPixelListShifted, background = true)
        if (p == ImageStatus.Passed) {
          imageMetaDataGroup.treatmentMachine match {
            case Some(tm: TreatmentMachine) => p
            case None                       => ImageStatus.UnknownTreatmentMachine
          }
        } else p
      }
      annotateImage(brightPng, brightGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, badPixelListShifted, background = true)
      annotateImage(blackPng, blackGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, badPixelListShifted, background = false)

      val pixelData = constructPixelData(blackPng, areaOfInterest)

      writeImageLater(normalPng, WLgenHtml.NORMAL_SUMMARY_FILE_NAME)
      writeImageLater(brightPng, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)
      logger.info("Done constructing ProcessImage for " + imageMetaData.toString)

      val boxPoint = new Point(boxCenterScaledX, boxCenterScaledY)
      val ballPoint = new Point(ballCenterScaledX, ballCenterScaledY)
      val edgesScaled = new Edges(
        edgesUnscaled.top * ResolutionY,
        edgesUnscaled.bottom * ResolutionX,
        edgesUnscaled.left * ResolutionX,
        edgesUnscaled.right * ResolutionX
      )

      diag("\n\nScaled box dimensions in mm")
      diagnostics.write(edgesScaled.toString.getBytes)

      diag("\n\n")
      diag("ball center mm   X: " + fmt(ballCenterScaledX) + "    Y: " + fmt(ballCenterScaledY))
      diag("box  center mm   X: " + fmt(boxCenterScaledX) + "    Y: " + fmt(boxCenterScaledY))

      diag("X Offset mm " + fmt(errorScaledX))
      diag("Y Offset mm " + fmt(errorScaledY))
      diag("R mm " + fmt(errorScaledXYCombined))

      val imageResult = new WLImageResult(
        imageStatus = passed,
        boxP = boxPoint,
        ballP = ballPoint,
        boxEdgesP = edgesScaled,
        directory = extendedData.output.dir,
        // extendedData,
        attributeList = attributeList,
        pixels = pixelData,
        badPixelList = badPixelList,
        marginalPixelList = marginalPixelList
      )

      diag("Image processing Results:\n" + imageResult.toString)

      WLgenHtml.generateHtml(extendedData, imageResult)

      imageResult
    }

    def saveWLBadPixelImage(pixels: Array[Array[Float]], badPixelList: List[WLBadPixel], marginalPixelList: List[WLBadPixel]): Unit = {
      val png = toPngScaled(pixels, 1)
      val graphics = png.getGraphics

      def drawWLBadPixelList(list: List[WLBadPixel], color: Color): Unit = {
        graphics.setColor(color)

        // Put a single dot on the pixel
        list.foreach(b => png.setRGB(b.x, b.y, color.getRGB))

        val radius = Config.WLBadPixelCorrectionRadius // was 10
        // draw a circle around each dot
        list.foreach(b => graphics.drawOval(b.x - radius, b.y - radius, radius * 2, radius * 2))
      }

      drawWLBadPixelList(marginalPixelList, Color.YELLOW)
      drawWLBadPixelList(badPixelList, Config.WLFailColor)

      writeImageLater(png, BAD_PIXEL_FILE_NAME)
    }

    def badPixelIsNotOnList(badPixel: WLBadPixel, list: List[WLBadPixel]): Boolean = {
      list.filter(b => (b.x == badPixel.x) && (b.y == badPixel.y)).isEmpty
    }

    def getRawPixels(): Array[Array[Float]] = {
      val height: Int = rtimage.get(TagByName.Rows).getIntegerValues()(0)
      val width: Int = rtimage.get(TagByName.Columns).getIntegerValues()(0)
      val shorts: Array[Short] = rtimage.get(TagByName.PixelData).getShortValues
      JavaUtil.pixelDataToArray(height, width, shorts)
    }

    // ----------------------------------------------------------------------------------------
    // End of defs
    // ----------------------------------------------------------------------------------------

    try {
      val rawPixels = getRawPixels(rtimage)
      writeImageLater(toPngScaled(rawPixels, 1), "original")
      writeDicomAsText(rtimage)

      // List of raw distinct pixel values sorted by value
      val rawDistinctSortedList = rawPixels.flatten.toList.distinct.sorted

      if (rawDistinctSortedList.size < Config.WLMinimumDistinctPixelValues) {
        imageError(, "Image lacks visible features", List[WLBadPixel](), List[WLBadPixel]())
      } else {

        /* Raw pixels with bad pixels set to average pixel value */
        val badPixelListUncorrected = findWLBadPixels(rawPixels, Config.WLBadPixelGapLimit, rawDistinctSortedList)
        logger.info("Number of bad pixels: " + badPixelListUncorrected.size)

        val marginalPixelListUncorrected = findWLBadPixels(rawPixels, Config.WLMarginalPixelGapLimit, rawDistinctSortedList)
        logger.info("Number of marginal pixels: " + marginalPixelListUncorrected.size)

        val badPixelList = uncorrectedWLBadPixelsToWLBadPixels(rawPixels, badPixelListUncorrected)
        val marginalPixelList = uncorrectedWLBadPixelsToWLBadPixels(rawPixels, marginalPixelListUncorrected).filter(m => badPixelIsNotOnList(m, badPixelList))

        val pixels = correctWLBadPixels(rawPixels, badPixelList)

        val rawExtremeAveragesRange = calcExtremeAveragesRange(pixels)

        //val minRawPixel = minPixel(pixels)
        //val maxRawPixel = maxPixel(pixels)

        if ((!badPixelList.isEmpty) || marginalPixelList.nonEmpty) saveWLBadPixelImage(pixels, badPixelList, marginalPixelList)

        val coarseY = coarseBoxLocate(rowSum(pixels))
        val coarseX = coarseBoxLocate(colSum(pixels))

        // Shift the bad pixels so that they point to the proper place in the area of interest (AOI)
        val badPixelListShifted = badPixelList.map(b => new WLBadPixel(b.x - coarseX._1, b.y - coarseY._1, b.rawValue, b.correctedValue, b.adjacentValidValueList))

        diag("\n\nbox coarse boundaries of area of interest    X: " + coarseX + "    Y: " + coarseY)

        // check to see if the target has been found reasonably far from any edge.  If not, fail.
        if (!coarseBoxLocationIsGood(coarseX, coarseY)) {
          imageError(ImageStatus.BoxNotFound, "Could not locate box", badPixelList, marginalPixelList)
        } else {
          val areaOfInterest = subSection(pixels, coarseX._1, coarseX._2 - coarseX._1, coarseY._1, coarseY._2 - coarseY._1)

          fineBoxLocate(areaOfInterest, rawExtremeAveragesRange) match {
            case Right(edgesUnscaled) =>
              val ballArea = subSection(
                areaOfInterest,
                edgesUnscaled.left.toInt + tol,
                (edgesUnscaled.right - edgesUnscaled.left).toInt - tol2,
                edgesUnscaled.top.toInt + tol,
                (edgesUnscaled.bottom - edgesUnscaled.top).toInt - tol2
              )
              showBallBackgroundNoise(ballArea, "ball_background")
              writeImageLater(toPng(ballArea), "ball_before_normalization")

              showBallBackgroundNoise(normalizeArea(ballArea), "normalized_ball_background")

              if (ballAreaIsFlat(areaOfInterest, edgesUnscaled)) {
                imageError(ImageStatus.BallMissing, "No ball found.  Ball area is flat", badPixelList, marginalPixelList)
              } else {
                val fieldWidthUnscaled = edgesUnscaled.right - edgesUnscaled.left
                findBallCenter(areaOfInterest, ballArea, fieldWidthUnscaled) match {
                  case Some(ballRelativeCenter: (Double, Double)) =>
                    val brcX = ballRelativeCenter._1
                    val brcY = ballRelativeCenter._2
                    val ir = processLocation(areaOfInterest, edgesUnscaled, (brcX, brcY), ballArea, badPixelList, badPixelListShifted, marginalPixelList, rtimage)
                    ir
                  case None => imageError(ImageStatus.BallAreaNoisy, "Could not confidently locate ball.  Ball area is too noisy.", badPixelList, marginalPixelList)
                }
              }
            case Left(status) => imageError(status, "Edge extremities not found", badPixelList, marginalPixelList)
          }
        }
      }
    } catch {
      case e: Exception =>
        val msg = "ProcessImage.process Unexpected exception: " + fmtEx(e)
        logger.error(msg)
        diag(msg)

        val imageResult =
          new WLImageResult(
            imageStatus = ImageStatus.UnexpectedError,
            boxP = null,
            ballP = null,
            boxEdgesP = null,
            directory = extendedData.output.dir,
            // val imageMetaData =   ,
            attributeList = rtimage,
            pixels = null,
            badPixelList = List(),
            marginalPixelList = List()
          )

        diag(imageResult.toString)

        try {
          if (!new File(extendedData.output.dir, WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME).exists) WLgenHtml.generateHtml(extendedData, imageResult)
        } catch {
          case e: Exception => logger.error("ProcessImage.process tried to save results of failure: " + fmtEx(e))
        }
        imageResult
    } finally {
      diagnostics.close()
    }

  }
}

object WLProcessImage extends org.aqa.Logging {

  val DIAGNOSTICS_TEXT_FILE_NAME = "diagnostics.txt"
  val DIAGNOSTICS_HTML_FILE_NAME = "diagnostics.html"

  def angleRoundedTo22_5(angle: Double): Double = (((angle + 3600) / 22.5).round.toInt % 16) * 22.5 // convert to nearest multiple of 22.5 degrees

  def angleRounded(angle: Double): Double = {
    val parts = 16 // Round off to nearest multiple of this angle
    val fraction = 360.0 / parts

    //((((angle.toInt + 3600 + 45) % 360) / 90) * 90) % 360 // convert to nearest multiple of 90 degrees
    (((angle + 3600) / fraction).round % parts) * fraction
  }


  def writeImage(img: BufferedImage, file: File): Unit = {
    logger.info("writing image file: " + file.getAbsolutePath + "   width: " + img.getWidth + "    height: " + img.getHeight)
    Util.writePng(img, file)
  }

}
