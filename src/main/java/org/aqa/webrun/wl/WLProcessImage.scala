package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.Utility
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Util
import org.aqa.webrun.ExtendedData
import org.opensourcephysics.numerics.CubicSpline

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.io.File
import java.io.PrintStream
import java.text.SimpleDateFormat
import java.util.Date
import scala.annotation.tailrec

class WLBadPixel(val x: Int, val y: Int, val rawValue: Int, val correctedValue: Float, val adjacentValidValueList: Seq[Int]) {
  override def toString: String = {
    "   x: " + x + "   y: " + y + "   rawValue: " + rawValue + "   correctedValue: " + correctedValue +
      adjacentValidValueList.sorted.reverse.foldLeft("\n            Adjacent Valid Values and difference from raw:")((t, v) => t + "\n                " + v + "  :  " + Math.abs(v - rawValue))
  }
}

class WLProcessImage(extendedData: ExtendedData, rtimage: AttributeList) extends org.aqa.Logging {

  import org.aqa.webrun.wl.WLProcessImage.colSum
  import org.aqa.webrun.wl.WLProcessImage.rowSum
  import org.aqa.webrun.wl.WLProcessImage.toPngScaled
  import org.aqa.webrun.wl.WLProcessImage.unitize
  import org.aqa.webrun.wl.WLProcessImage.SearchRange
  import org.aqa.webrun.wl.WLProcessImage.toCubicSpline

  private val trans = new IsoImagePlaneTranslator(rtimage)
  private val ResolutionX = trans.pix2IsoDistX(1)
  private val ResolutionY = trans.pix2IsoDistY(1)

  private val SizeX = trans.width
  private val SizeY = trans.height

  // private val gantryAngle = Util.gantryAngle(rtimage)
  // private val collimatorAngle = Util.collimatorAngle(rtimage)

  private val EMPTY_PIXEL: Int = Config.WLTextColor.getRGB + 1
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

    val annotate = new WLAnnotate(SCALE, BALL_RADIUS)

    def fmt(d: Double): String = d.formatted("%10.5f")

    val elapsedTime_ms = {
      val ms = Util.dicomGetTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime).get.getTime
      val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
      elapsed_ms
    }

    def toPng(pix: Array[Array[Float]]): BufferedImage = toPngScaled(pix, SCALE)

    val gantryRounded_deg = Util.angleRoundedTo90(Util.gantryAngle(rtimage))
    val collimatorRounded_deg = Util.angleRoundedTo90(Util.collimatorAngle(rtimage))

    val gantryRounded_txt = "G" + gantryRounded_deg.formatted("%03d")
    val collimatorRounded_txt = "C" + collimatorRounded_deg.formatted("%03d")
    val elapsedTime_txt = new SimpleDateFormat("MM:ss").format(new Date(elapsedTime_ms))

    val imageName: String = gantryRounded_txt + collimatorRounded_txt + elapsedTime_txt

    val subDir: File = {
      val min = elapsedTime_ms / (60 * 1000)
      val sec = (elapsedTime_ms / 1000) % 60

      val name = min.formatted("%d") + "_" + sec.formatted("%02d") + "__" + gantryRounded_txt + "__" + collimatorRounded_txt

      val dir = new File(extendedData.output.dir, name)
      dir.mkdirs()
      dir
    }

    val diagnostics = new PrintStream(new File(subDir, WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME))

    def diagnosticMessage(text: String): Unit = {
      diagnostics.println(text)
      val msg =
        "G" + Util.angleRoundedTo90(Util.gantryAngle(rtimage)).formatted("%03d") +
          "C" + Util.angleRoundedTo90(Util.collimatorAngle(rtimage)).formatted("%03d") + " " + {
          val fmt = new SimpleDateFormat("MM:ss")
          val ms = Util.dicomGetTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime).get.getTime
          val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
          fmt.format(new Date(elapsed_ms))
        } +
          imageName + " Diagnostics: " + text
      logger.info(msg)
    }

    diagnosticMessage("\n\nMeta-Image Data from ARIA:\n" + extendedData.output.toString)

    def writeDicomAsText(attributeList: AttributeList): Unit = {
      try {
        val text = DicomUtil.attributeListToString(attributeList)
        Utility.writeFile(new File(subDir, Util.sopOfAl(rtimage) + ".txt"), text.getBytes)
      } catch {
        case e: Exception =>
          diagnosticMessage("Unable to write DICOM file as text: " + e)
      }
    }

    def writeDicomAsBinaryDicom(attributeList: AttributeList): Unit = {
      try {
        val file = new File(subDir, Util.sopOfAl(rtimage) + ".dcm")
        DicomUtil.writeAttributeListToFile(attributeList, file, "AQA")
      } catch {
        case e: Exception =>
          diagnosticMessage("Unable to write DICOM file as binary DICOM: " + e)
      }
    }

    class UncorrectedWLBadPixel(val x: Int, val y: Int, val rawValue: Double)

    /**
      * Make a list of bad pixels.
      */
    def findWLBadPixels(rawPixels: Array[Array[Float]], pixelGapLimit: Int, rawDistinctSortedList: Seq[Float]): Seq[UncorrectedWLBadPixel] = {
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
      diagnosticMessage("Range of valid pixel values (inclusive): " + limits._1 + " - " + limits._2 + "    Largest value gap in good pixels: " + largestGoodGap)

      val badList = rawPixels.flatten.zipWithIndex.filter(pi => !isValid(pi._1)).map(pix => new UncorrectedWLBadPixel(pix._2 % SizeX, pix._2 / SizeX, pix._1))

      badList.toList
    }

    def uncorrectedWLBadPixelsToWLBadPixels(rawPixelData: Array[Array[Float]], badPixelList: Seq[UncorrectedWLBadPixel]): Seq[WLBadPixel] = {
      // A pixel is good if its coordinates are valid and it is not on the bad pixel list
      val height = rawPixelData.length
      val width = rawPixelData(0).length

      def isGoodPixel(x: Int, y: Int): Boolean = {
        (x >= 0) && (y >= 0) && (x < width) && (y < height) && !badPixelList.exists(p => p.x == x && p.y == y)
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
    def correctWLBadPixels(rawPixels: Array[Array[Float]], badPixelList: Seq[WLBadPixel]): Array[Array[Float]] = {
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
      new File(subDir, pngName)
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

    def stdDev(values: Array[Float]): Double = {
      val avg = values.sum / values.length
      Math.sqrt(values.map(x => (x - avg) * (x - avg)).sum / values.length)
    }

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
        array.slice(start, start + span)
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
      val inc = 0.005
      val yA1 = spline.evaluate(0.0)
      val yA2 = spline.evaluate(inc)
      val slopeA = ((yA2 - yA1) / inc).abs

      val yB1 = spline.evaluate(length.toDouble)
      val yB2 = spline.evaluate(length.toDouble + inc)
      val slopeB = ((yB2 - yB1) / inc).abs
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

      diagnosticMessage(
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
    def findEdge(pixIn: Array[Array[Float]], vertical: Boolean, name: String, rawExtremeAveragesRange: Double): Either[ImageStatus.Value, Double] = {
      val length = if (vertical) pixIn(0).length else pixIn.length
      val sum = if (vertical) colSum(pixIn) else rowSum(pixIn)
      val scaledSum = unitize(sum)
      val spline = toCubicSpline(scaledSum)

      saveEdgeImage(pixIn, vertical, name, spline)
      diagnosticMessage("\n\n")
      val terminalEdgeSlopes = getTerminalEdgeSlopes(spline, scaledSum.length)
      showEdgeStats(pixIn, vertical, name, terminalEdgeSlopes)

      val increasing = scaledSum(0) < scaledSum(length - 1)

      @tailrec
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
      diagnosticMessage(brightnessMessage)

      if (pct >= Config.WLMaxAllowedBrightnessRangePercentDifference) {
        val errorMsg =
          "Edge " + name + " failed to meet criteria for brightness range of " + Config.WLMaxAllowedBrightnessRangePercentDifference + " percent.  " + brightnessMessage + "    Required percent" + Config.WLMaxAllowedBrightnessRangePercentDifference
        logger.error(errorMsg)
        Left(ImageStatus.BallAreaNoisy)
      } else Right(center(0, sum.length - 1, PRECISION))
    }

    /**
      * Get the x,y coordinates of the point with the largest value
      */
    /*
    def maxPoint(area: Array[Array[Double]]): (Int, Int) = {
      val coordinate = area.flatten.zipWithIndex.maxBy(_._1)._2
      (coordinate % area.length, coordinate / area.length)
    }
     */

    /**
      * Draw the horizontal and vertical lines marking the center of something.  By convention in
      * this project, it marks the center of the ball.
      */
    /*
    def drawCross(graphics: Graphics, x: Double, y: Double, markerSize: Double): Unit = {
      graphics.drawLine(coordinate(x - markerSize), coordinate(y), coordinate(x + markerSize), coordinate(y))
      graphics.drawLine(coordinate(x), coordinate(y - markerSize), coordinate(x), coordinate(y + markerSize))
    }
     */

    /*
    def drawCircles(graphics: Graphics, x: Double, y: Double): Unit = {
      graphics.setColor(Config.WLBallColor)
      val NUM_CIRCLE = Config.WLNumberOfCircles

      def graphicDistance(d: Double): Int = (d * SCALE + .5).toInt

      for (circle <- 1 to NUM_CIRCLE) {
        val radius = (circle.toDouble / NUM_CIRCLE) * BALL_RADIUS
        graphics.drawOval(coordinate(x - radius), coordinate(y - radius), graphicDistance(radius * 2), graphicDistance(radius * 2))
      }
    }
     */

    /**
      * Find the center of the ball within the given area.  Return the
      * coordinates relative to the area given.
      */
    def findBallCenter(areaOfInterest: Array[Array[Float]], approxAoi: Array[Array[Float]]): Option[(Double, Double)] = {

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
          val annotate = new WLAnnotate(SCALE, BALL_RADIUS)
          annotate.drawCross(png.getGraphics.asInstanceOf[Graphics2D], center._1, center._2, 1)
          writeImageLater(png, "ball_coarse")
        }

        val maxRow = aoiMax.zipWithIndex.foldLeft((aoiMax.head, 0))((c, m) => if (m._1.max > c._1.max) m else c)
        val maxCol = maxRow._1.zipWithIndex.foldLeft((aoiMax.head.head, 0))((c, m) => if (m._1 > c._1) m else c)
        val center = (maxCol._2, maxRow._2) // maxPoint(aoiMax)
        saveImage(center)
        center
      }

      def centerOfMass(spline: CubicSpline, len: Int): SearchRange = {
        val pointList = (0 until (len / X_INCREMENT).round.toInt).map(i => spline.evaluate(i * X_INCREMENT))
        val min = pointList.min
        val max = pointList.max

        val minAcceptable = min + ((max - min) * (1.0 - (Config.WLBallHeightPercentForeground / 100.0)))

        val lo = pointList.indexWhere(p => p >= minAcceptable)
        val hi = (pointList.size - 1) - pointList.reverse.indexWhere(p => p >= minAcceptable)

        val weighted = (lo to hi).map(i => pointList(i) * i).sum * X_INCREMENT
        val sum = pointList.slice(lo, lo + hi - lo).sum
        val centerOfMass = weighted / sum

        new SearchRange(lo * X_INCREMENT, centerOfMass, hi * X_INCREMENT)
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
          diagnosticMessage(" Ball spline verified to cross average height exactly twice")
          true
        } else {
          diagnosticMessage(" Wrong number of times that the ball spline crossed the average value.  Should be 2 but was " + crsCount)
          false
        }
      }

      /*
       * @param x: Approximate horizontal center of ball
       * @param y: Approximate vertical center of ball
       */

      def fineBallLocate(aoiFine: Array[Array[Float]]): Option[(Double, Double)] = {

        val annotate = new WLAnnotate(SCALE, BALL_RADIUS)

        if (Util.sopOfAl(rtimage).equals("1.3.6.1.4.1.22361.17483843774714.767955427.1674650787748.7"))
          Trace.trace("hey")
        val rSum = WLProcessImage.unitize(WLProcessImage.rowSum(aoiFine))
        val rSpline = WLProcessImage.toCubicSpline(rSum)
        val cSum = WLProcessImage.unitize(WLProcessImage.colSum(aoiFine))
        val cSpline = WLProcessImage.toCubicSpline(cSum)

        val fineX = centerOfMass(cSpline, cSum.length)
        val fineY = centerOfMass(rSpline, rSum.length)
        val image = annotate.saveFineLocatedImage(aoiFine, fineX, fineY)
        writeImageLater(image, "ball_fine")

        if (singleMax(cSpline, cSum) && singleMax(rSpline, rSum)) {
          diagnosticMessage("Ball fine location relative to box in pixels: " + fineX.center + ", " + fineY.center)
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
    /*
    def drawBoxBallOffset(graphics: Graphics, ballCenter: (Double, Double), boxCenter: (Double, Double)): Unit = {
      graphics.setColor(Config.WLOffsetColor)

      graphics.drawLine(coordinate(ballCenter._1), coordinate(ballCenter._2), coordinate(boxCenter._1), coordinate(boxCenter._2))
      graphics.drawLine(coordinate(ballCenter._1) - 1, coordinate(ballCenter._2), coordinate(boxCenter._1) - 1, coordinate(boxCenter._2))
      graphics.drawLine(coordinate(ballCenter._1) + 1, coordinate(ballCenter._2), coordinate(boxCenter._1) + 1, coordinate(boxCenter._2))
      graphics.drawLine(coordinate(ballCenter._1), coordinate(ballCenter._2) - 1, coordinate(boxCenter._1), coordinate(boxCenter._2) - 1)
      graphics.drawLine(coordinate(ballCenter._1), coordinate(ballCenter._2) + 1, coordinate(boxCenter._1), coordinate(boxCenter._2) + 1)
    }
     */

    def list2Array(x: Array[Array[Float]]) = x.map(x => x)

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
    def coarseBoxLocate(profile: Array[Float]): (Int, Int) = {
      val lo = profile.min.toDouble
      val range = profile.max.toDouble - lo
      val dSum = profile.map(s => (s.toDouble - lo) / range)

      val hiList = profile.indices.filter(d => dSum(d) < 0.5)

      def bound(v: Int): Int =
        if (v < tol)
          tol
        else if (v > (profile.length - (tol + 1)))
          profile.length - tol
        else
          v

      val pos = (bound(hiList.head) - tol, bound(hiList.last) + tol)
      pos
    }

    /**
      * Locate the box to sub-pixel accuracy.
      */
    def fineBoxLocate(areaOfInterest: Array[Array[Float]], rawExtremeAveragesRange: Double): Either[ImageStatus.Value, Edges] = {
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

      def edgeStatus(e: Either[ImageStatus.Value, Double]): ImageStatus.Value = {
        e match {
          case Right(_) => ImageStatus.Passed
          case Left(s)  => s
        }
      }

      val status: ImageStatus.Value = {
        val list = Seq(eTop, eBottom, eLeft, eRight).map(edgeStatus).distinct.filterNot(_ == ImageStatus.Passed)
        if (list.isEmpty) ImageStatus.Passed else list.head
      }

      if (status == ImageStatus.Passed) {
        val edgeTop: Double = eTop.right.get
        val edgeBottom = eBottom.right.get + (height - tol2)
        val edgeLeft = eLeft.right.get
        val edgeRight = eRight.right.get + (width - tol2)

        val edges = new Edges(edgeTop, edgeBottom, edgeLeft, edgeRight)
        diagnosticMessage("\n\nUnscaled box dimensions in pixels")
        diagnostics.write(edges.toString.getBytes)
        Right(edges)
      } else
        Left(status)
    }

    /*
    def highlightWLBadPixelList(badPixelList: Seq[WLBadPixel], graphics: Graphics2D) = {
      val circleRadius = Config.WLBadPixelCorrectionRadius
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
     */

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
    /*
    def coarseBoxLocationIsGood(coarseX: (Int, Int), coarseY: (Int, Int)): Boolean = {
      val boxPositionVariance = Config.WLBoxPositionFactor * Config.WLBoxSize

      val xCenterBox = ((coarseX._1 + coarseX._2) / 2.0) * ResolutionX
      val yCenterBox = ((coarseY._1 + coarseY._2) / 2.0) * ResolutionY

      val xCenterImage = (SizeX / 2.0) * ResolutionX
      val yCenterImage = (SizeY / 2.0) * ResolutionY

      // val boxSmallestAllowed = (1 - (Config.BoxSizePercent * 0.01)) * Config.BoxSize
      // val boxLargestAllowed = (1 + (Config.BoxSizePercent * 0.01)) * Config.BoxSize

      // val width = (coarseX._2 - coarseX._1) * ResolutionX
      // val height = (coarseY._2 - coarseY._1) * ResolutionY

      val ok: Boolean = 0 match {
        case _ if (xCenterImage - xCenterBox) > boxPositionVariance => diagnosticMessage("Box located too far to left"); false
        case _ if (yCenterImage - yCenterBox) > boxPositionVariance => diagnosticMessage("Box located too far to top"); false
        case _ if (xCenterBox - xCenterImage) > boxPositionVariance => diagnosticMessage("Box located too far to right"); false
        case _ if (yCenterBox - yCenterImage) > boxPositionVariance => diagnosticMessage("Box located too far to bottom"); false

        // case _ if width < boxSmallestAllowed  => diagnosticMessage("Box is too narrow"); false
        // case _ if width > boxLargestAllowed   => diagnosticMessage("Box is too wide"); false
        // case _ if height < boxSmallestAllowed => diagnosticMessage("Box is too short"); false
        // case _ if height > boxLargestAllowed  => diagnosticMessage("Box is too tall"); false

        case _ => true
      }
      ok
    }
     */

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
        diagnosticMessage("Severe error: " + msg)
        true
      } else {
        val msg = "Flatness check: The area inside the box contains a ball. " + stats
        logger.error(msg)
        diagnosticMessage(msg)
        false
      }
    }

    /**
      * Indicate that something is wrong with the image.
      */
    def imageError(status: ImageStatus.Value, msg: String, marginalPixelList: Seq[WLBadPixel]): WLImageResult = {
      val fullMsg = "Image " + imageName + "  " + msg
      logger.error(fullMsg)
      diagnosticMessage(fullMsg)
      val imageResult =
        new WLImageResult(status, boxP = null, ballP = null, boxEdgesP = null, directory = subDir, rtimage = rtimage, pixels = null, badPixelList = null, marginalPixelList, extendedData)
      diagnosticMessage(imageResult.toString)
      WLgenHtml.generateHtml(extendedData, subDir, imageResult)
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
        badPixelList: Seq[WLBadPixel],
        badPixelListShifted: Seq[WLBadPixel],
        marginalPixelList: Seq[WLBadPixel],
        attributeList: AttributeList
    ): WLImageResult = {

      val ballCenterX = ballRelativeCenter._1
      val ballCenterY = ballRelativeCenter._2

      // Limit all pixel brightnesses to the maximum ball pixel brightness.  This makes the ball stand out more in the final image.
      val maxBallPixel = ballArea.flatten.max
      val thresholdLimitedAreaOfInterest = areaOfInterest.map(r => r.map(c => if (c > maxBallPixel) maxBallPixel else c))

      //drawGraphics()
      val normalPng = toPng(areaOfInterest)
      val brightPng = toPng(thresholdLimitedAreaOfInterest)
      val blackPng = toBlackPng(areaOfInterest)

      // draw edge of box
      val normalGraphics = normalPng.getGraphics.asInstanceOf[Graphics2D]
      val brightGraphics = brightPng.getGraphics.asInstanceOf[Graphics2D]
      val blackGraphics = blackPng.getGraphics.asInstanceOf[Graphics2D]

      annotate.drawBallGraphics(normalGraphics, ballCenterX, ballCenterY)
      annotate.drawBallGraphics(brightGraphics, ballCenterX, ballCenterY)
      annotate.drawBallGraphics(blackGraphics, ballCenterX, ballCenterY)

      annotate.highlightWLBadPixelList(badPixelListShifted, normalGraphics)
      annotate.highlightWLBadPixelList(badPixelListShifted, brightGraphics)
      annotate.highlightWLBadPixelList(badPixelListShifted, blackGraphics)

      val bin = 6
      val bout = 6
      annotate.drawBoxGraphics(normalGraphics, edgesUnscaled.top, edgesUnscaled.bottom, edgesUnscaled.left, edgesUnscaled.right, Config.WLBoxColor, bin, bout)
      annotate.drawBoxGraphics(brightGraphics, edgesUnscaled.top, edgesUnscaled.bottom, edgesUnscaled.left, edgesUnscaled.right, Config.WLBoxColor, bin, bout)
      annotate.drawBoxGraphics(blackGraphics, edgesUnscaled.top, edgesUnscaled.bottom, edgesUnscaled.left, edgesUnscaled.right, Config.WLBoxColor, bin, bout)

      val boxShrink = 5
      val boxUnscaledCorrected = correctUnscaledEdges(edgesUnscaled)
      val buc = boxUnscaledCorrected
      // val inner = 24
      annotate.drawBoxGraphics(normalGraphics, buc.top + boxShrink, buc.bottom - boxShrink, buc.left + boxShrink, buc.right - boxShrink, Config.WLBoxColorCorrected, 0, -1)
      annotate.drawBoxGraphics(brightGraphics, buc.top + boxShrink, buc.bottom - boxShrink, buc.left + boxShrink, buc.right - boxShrink, Config.WLBoxColorCorrected, 0, -1)
      annotate.drawBoxGraphics(blackGraphics, buc.top + boxShrink, buc.bottom - boxShrink, buc.left + boxShrink, buc.right - boxShrink, Config.WLBoxColorCorrected, 0, -1)

      annotate.drawBoxBallOffset(normalGraphics, (ballCenterX, ballCenterY), ((buc.left + buc.right) / 2, (buc.top + buc.bottom) / 2))
      annotate.drawBoxBallOffset(brightGraphics, (ballCenterX, ballCenterY), ((buc.left + buc.right) / 2, (buc.top + buc.bottom) / 2))
      annotate.drawBoxBallOffset(blackGraphics, (ballCenterX, ballCenterY), ((buc.left + buc.right) / 2, (buc.top + buc.bottom) / 2))

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
        val p = annotate.annotateImage(normalPng, normalGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = true, imageName)
        p
      }
      annotate.annotateImage(brightPng, brightGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = true, imageName)
      annotate.annotateImage(blackPng, blackGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = false, imageName)

      val pixelData = constructPixelData(blackPng, areaOfInterest)

      writeImageLater(normalPng, WLgenHtml.NORMAL_SUMMARY_FILE_NAME)
      writeImageLater(brightPng, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)
      logger.info("Done constructing ProcessImage for " + imageName)

      val boxPoint = new Point(boxCenterScaledX, boxCenterScaledY)
      val ballPoint = new Point(ballCenterScaledX, ballCenterScaledY)
      val edgesScaled = new Edges(
        edgesUnscaled.top * ResolutionY,
        edgesUnscaled.bottom * ResolutionX,
        edgesUnscaled.left * ResolutionX,
        edgesUnscaled.right * ResolutionX
      )

      diagnosticMessage("\n\nScaled box dimensions in mm")
      diagnostics.write(edgesScaled.toString.getBytes)

      diagnosticMessage("\n\n")
      diagnosticMessage("ball center mm   X: " + fmt(ballCenterScaledX) + "    Y: " + fmt(ballCenterScaledY))
      diagnosticMessage("box  center mm   X: " + fmt(boxCenterScaledX) + "    Y: " + fmt(boxCenterScaledY))

      diagnosticMessage("X Offset mm " + fmt(errorScaledX))
      diagnosticMessage("Y Offset mm " + fmt(errorScaledY))
      diagnosticMessage("R mm " + fmt(errorScaledXYCombined))

      val imageResult = new WLImageResult(
        imageStatus = passed,
        boxP = boxPoint,
        ballP = ballPoint,
        boxEdgesP = edgesScaled,
        directory = subDir,
        // extendedData,
        rtimage = attributeList,
        pixels = pixelData,
        badPixelList = badPixelList,
        marginalPixelList = marginalPixelList,
        extendedData = extendedData
      )

      diagnosticMessage("Image processing Results:\n" + imageResult.toString)

      WLgenHtml.generateHtml(extendedData, subDir, imageResult)

      imageResult
    }

    def saveWLBadPixelImage(pixels: Array[Array[Float]], badPixelList: Seq[WLBadPixel], marginalPixelList: Seq[WLBadPixel]): Unit = {
      val png = toPngScaled(pixels, 1)
      val graphics = png.getGraphics

      def drawWLBadPixelList(list: Seq[WLBadPixel], color: Color): Unit = {
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

    def badPixelIsNotOnList(badPixel: WLBadPixel, list: Seq[WLBadPixel]): Boolean = {
      !list.exists(b => (b.x == badPixel.x) && (b.y == badPixel.y))
    }

    def fetchRawPixels(): Array[Array[Float]] = {
      val height: Int = rtimage.get(TagByName.Rows).getIntegerValues()(0)
      val width: Int = rtimage.get(TagByName.Columns).getIntegerValues()(0)
      val shorts: Array[Short] = rtimage.get(TagByName.PixelData).getShortValues
      JavaUtil.pixelDataToArray(height, width, shorts)
    }

    // ----------------------------------------------------------------------------------------

    try {
      val rawPixels = fetchRawPixels()
      writeImageLater(toPngScaled(rawPixels, 1), "original")
      writeDicomAsBinaryDicom(rtimage)
      writeDicomAsText(rtimage)

      // Seq of raw distinct pixel values sorted by value
      val rawDistinctSortedList = rawPixels.flatten.toList.distinct.sorted

      if (rawDistinctSortedList.size < Config.WLMinimumDistinctPixelValues) {
        imageError(ImageStatus.BoxNotFound, "Image lacks visible features", Seq[WLBadPixel]())
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

        if (badPixelList.nonEmpty || marginalPixelList.nonEmpty) saveWLBadPixelImage(pixels, badPixelList, marginalPixelList)

        val coarseY = coarseBoxLocate(rowSum(pixels))
        val coarseX = coarseBoxLocate(colSum(pixels))

        // Shift the bad pixels so that they point to the proper place in the area of interest (AOI)
        val badPixelListShifted = badPixelList.map(b => new WLBadPixel(b.x - coarseX._1, b.y - coarseY._1, b.rawValue, b.correctedValue, b.adjacentValidValueList))

        diagnosticMessage("\n\nbox coarse boundaries of area of interest    X: " + coarseX + "    Y: " + coarseY)

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
              imageError(ImageStatus.BallMissing, "No ball found.  Ball area is flat", marginalPixelList)
            } else {
              findBallCenter(areaOfInterest, ballArea) match {
                case Some(ballRelativeCenter: (Double, Double)) =>
                  val brcX = ballRelativeCenter._1
                  val brcY = ballRelativeCenter._2
                  val ir = processLocation(areaOfInterest, edgesUnscaled, (brcX, brcY), ballArea, badPixelList, badPixelListShifted, marginalPixelList, rtimage)
                  WLgenHtml.generateHtml(extendedData, subDir, imageResult = ir)
                  ir
                case None => imageError(ImageStatus.BallAreaNoisy, "Could not confidently locate ball.  Ball area is too noisy.", marginalPixelList)
              }
            }
          case Left(status) => imageError(status, "Edge extremities not found", marginalPixelList)
        }

      }
    } catch {
      case e: Exception =>
        val msg = "ProcessImage.process Unexpected exception: " + fmtEx(e)
        logger.error(msg)
        diagnosticMessage(msg)

        val imageResult =
          new WLImageResult(
            imageStatus = ImageStatus.UnexpectedError,
            boxP = null,
            ballP = null,
            boxEdgesP = null,
            directory = subDir,
            rtimage = rtimage,
            pixels = null,
            badPixelList = Seq(),
            marginalPixelList = Seq(),
            extendedData = extendedData
          )

        diagnosticMessage(imageResult.toString)

        try {
          if (!new File(subDir, WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME).exists) WLgenHtml.generateHtml(extendedData, subDir, imageResult)
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

  def boundInt(x: Int, lo: Int, hi: Int): Int = {
    if (x < lo) lo else if (x > hi) hi else x
  }

  class SearchRange(val lo: Double, val center: Double, val hi: Double) {}

  /**
    * Convert a list to a cubic spline
    */
  def toCubicSpline(data: Array[Float]): CubicSpline = new CubicSpline(data.indices.toArray.map(s => s.toDouble), data.map(f => f.toDouble))

  def unitize(data: Array[Float]): Array[Float] = {
    val min = data.min
    val range = data.max - min
    data.map(x => (x - min) / range)
  }

  def angleRoundedTo22_5(angle: Double): Double = (((angle + 3600) / 22.5).round.toInt % 16) * 22.5 // convert to nearest multiple of 22.5 degrees

  def angleRounded(angle: Double): Double = {
    val parts = 16 // Round off to nearest multiple of this angle
    val fraction = 360.0 / parts

    //((((angle.toInt + 3600 + 45) % 360) / 90) * 90) % 360 // convert to nearest multiple of 90 degrees
    (((angle + 3600) / fraction).round % parts) * fraction
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
    png
  }

  // def indexes(length: Int): Array[Double] = (0 until length).toArray.map(x => x.toDouble)
  private def writeImage(img: BufferedImage, file: File): Unit = {
    logger.info("writing image file: " + file.getAbsolutePath + "   width: " + img.getWidth + "    height: " + img.getHeight)
    Util.writePng(img, file)
  }

}
