package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.webrun.ExtendedData
import org.aqa.PlannedRectangle
import org.opensourcephysics.numerics.CubicSpline

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Rectangle
import java.io.File
import java.io.PrintStream
import java.text.SimpleDateFormat
import java.util.Date

/*
class WLBadPixel(val x: Int, val y: Int, val rawValue: Int, val correctedValue: Float, val adjacentValidValueList: Seq[Int]) {
  override def toString: String = {
    "   x: " + x + "   y: " + y + "   rawValue: " + rawValue + "   correctedValue: " + correctedValue +
      adjacentValidValueList.sorted.reverse.foldLeft("\n            Adjacent Valid Values and difference from raw:")((t, v) => t + "\n                " + v + "  :  " + Math.abs(v - rawValue))
  }
}
 */

class WLProcessImage(extendedData: ExtendedData, rtimage: AttributeList, index: Int, runReq: WLRunReq) extends org.aqa.Logging {

  import org.aqa.webrun.wl.WLProcessImage.colSum
  import org.aqa.webrun.wl.WLProcessImage.rowSum
  import org.aqa.webrun.wl.WLProcessImage.toPngScaled

  private val wlParameters = MachineWL.getMachineWLOrDefault(extendedData.machine.machinePK.get)

  private val trans = new IsoImagePlaneTranslator(rtimage)
  private val ResolutionX = trans.pix2IsoDistX(1)
  private val ResolutionY = trans.pix2IsoDistY(1)

  // private val gantryAngle = Util.gantryAngle(rtimage)
  // private val collimatorAngle = Util.collimatorAngle(rtimage)

  private val EMPTY_PIXEL: Int = Config.WLTextColor.getRGB + 1
  // private  val NORMAL_SUMMARY_FILE_NAME = "normalSummary" + IMAGE_FILE_SUFFIX
  // private  val BRIGHT_SUMMARY_FILE_NAME = "brightSummary" + IMAGE_FILE_SUFFIX
  // private  val ORIGINAL_FILE_NAME = "original" + IMAGE_FILE_SUFFIX
  private val BAD_PIXEL_FILE_NAME = "badPixels" + WLgenHtml.IMAGE_FILE_SUFFIX

  private val elapsedTime_ms = {
    val ms = Util.dicomGetTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime).get.getTime
    val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
    elapsed_ms
  }

  private val gantryRounded_deg = Util.angleRoundedTo90(Util.gantryAngle(rtimage))
  private val collimatorRounded_deg = Util.angleRoundedTo90(Util.collimatorAngle(rtimage))

  private val gantryRounded_txt = "G" + gantryRounded_deg.formatted("%03d")
  private val collimatorRounded_txt = "C" + collimatorRounded_deg.formatted("%03d")
  private val elapsedTime_txt = {
    val min = elapsedTime_ms / (60 * 1000)
    val sec = (elapsedTime_ms / 1000) % 60
    min.formatted("%d") + ":" + sec.formatted("%02d")
  }

  private val imageName: String = gantryRounded_txt + " " + collimatorRounded_txt + " " + elapsedTime_txt

  private val subDir: File = {
    val min = elapsedTime_ms / (60 * 1000)
    val sec = (elapsedTime_ms / 1000) % 60

    val name = min.formatted("%d") + "_" + sec.formatted("%02d") + "__" + gantryRounded_txt + "__" + collimatorRounded_txt + "-" + index.formatted("%02d")

    val dir = new File(extendedData.output.dir, name)
    dir.mkdirs()
    dir
  }

  private def makeFailedWLImageStatus(imageStatus: WLImageStatus.Value): WLImageResult = {
    WLImageResult(
      imageStatus = imageStatus,
      boxP = None,
      ballP = None,
      edgesUnscaled = None,
      boxEdgesP = None,
      edgeSet = None,
      directory = subDir,
      rtimage = rtimage,
      pixels = None,
      coarseX = None,
      coarseY = None,
      brcX = None,
      brcY = None,
      badPixelList = Seq(),
      marginalPixelList = Seq(),
      extendedData = extendedData,
      runReq
    )

  }

  def process: WLImageResult = {
    logger.info("Start constructing ProcessImage for " + Util.sopOfAl(rtimage))

    /** Convert a value in mm to pixels. */
    def toPixels(mm: Double): Int = ((mm / ResolutionX) + 0.5).toInt

    // tolerance in pixels for how far the search for the fine edge of
    // box should look, given the coarse position of the edge.  Also used
    // determine where an edge of the box finishes, to define the
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
    val BALL_RADIUS = toPixels(wlParameters.ballDiameter_mm / 2.0)

    val annotate = new WLAnnotate(SCALE, BALL_RADIUS)

    def fmt(d: Double): String = d.formatted("%10.5f")

    def toPng(pix: IndexedSeq[IndexedSeq[Float]]): BufferedImage = toPngScaled(pix, SCALE)

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

    //noinspection RegExpRepeatedSpace,RegExpSimplifiable
    diagnosticMessage("\nOutput :\n    " + extendedData.output.toString.replaceAll("   *", "\n    "))

    def writeDicomAsText(attributeList: AttributeList): Unit = {
      try {
        val text = DicomUtil.attributeListToString(attributeList)
        Util.writeFile(new File(subDir, Util.sopOfAl(rtimage) + ".txt"), text)
      } catch {
        case e: Exception =>
          diagnosticMessage("Unable to write DICOM file as text: " + e)
      }
    }

    def writeDicomAsBinaryDicom(attributeList: AttributeList): Unit = {
      try {
        val file = new File(subDir, Util.sopOfAl(rtimage) + ".dcm")
        Util.writeAttributeListToFile(attributeList, file)
      } catch {
        case e: Exception =>
          diagnosticMessage("Unable to write DICOM file as binary DICOM: " + e)
      }
    }

    def calcExtremeAveragesRange(pix: IndexedSeq[IndexedSeq[Float]]): Double = {
      val ordered = pix.flatten.toList.sorted
      val min = ordered.take(Config.WLAveragePixelsForBrightness).sum / Config.WLAveragePixelsForBrightness
      val max = ordered.reverse.take(Config.WLAveragePixelsForBrightness).sum / Config.WLAveragePixelsForBrightness
      max - min
    }

    /**
      * Generate an image of the given size with all black pixels
      */
    def toBlackPng(pix: IndexedSeq[IndexedSeq[Float]]): BufferedImage = {
      val png = toPngScaled(pix, SCALE)
      for (y <- 0 until png.getHeight) for (x <- 0 until png.getWidth) png.setRGB(x, y, EMPTY_PIXEL)
      png
    }

    def stdDev(values: IndexedSeq[Float]): Double = {
      val avg = values.sum / values.length
      Math.sqrt(values.map(x => (x - avg) * (x - avg)).sum / values.length)
    }

    /*
     * Copy a 2-dimensional sub-array from the given 2-dimensional array.
     *
     * @param pixIn: array of data points
     *
     * @param x: low and high x coordinates
     *
     * @param y: low and high y coordinates
     *
     */
    def subSection(pixIn: IndexedSeq[IndexedSeq[Float]], x: Int, xWidth1: Int, y: Int, yHeight1: Int): IndexedSeq[IndexedSeq[Float]] = {

      val xWidth = Math.max(xWidth1, 0)
      val yHeight = Math.max(yHeight1, 0)

      def range[A](array: IndexedSeq[A], start: Int, span: Int): IndexedSeq[A] = {
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

    def showEdgeStats(pixIn: IndexedSeq[IndexedSeq[Float]], vertical: Boolean, name: String, terminalEdgeSlopes: (Double, Double)): Unit = {
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
        "Edge stats for " + name.format("%8s") +
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
      * Find the center of the ball within the given area.  Return the
      * coordinates relative to the area given.
      */
    def findBallCenter(areaOfInterest: IndexedSeq[IndexedSeq[Float]], approxAoi: IndexedSeq[IndexedSeq[Float]]): Option[(Double, Double)] = {

      /**
        * Coarsely locate the center of the ball by creating a copy of the area expected to contain
        * the center of the ball, and then transforming it so that each pixel contains the sum of
        * the 8 neighboring pixels and itself, then find the pixel with the largest value and take that
        * as the ball's center.  This effectively says: "Find the largest sum of 3x3 pixels".
        */
      def coarseBallLocate: (Int, Int) = {
        val aoiMax = list2Array(subSection(approxAoi, 0, approxAoi.head.length, 0, approxAoi.length)).map(_.toArray).toArray

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
          Util.writePng(png, new File(subDir, "ball_coarse.png"))
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
      def singleMax(spline: CubicSpline, values: IndexedSeq[Float]): Boolean = {
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

      def fineBallLocate(aoiFine: IndexedSeq[IndexedSeq[Float]]): Option[(Double, Double)] = {

        val annotate = new WLAnnotate(SCALE, BALL_RADIUS)

        val rSum = WLProcessImage.unitize(WLProcessImage.rowSum(aoiFine))
        val rSpline = WLProcessImage.toCubicSpline(rSum)
        val cSum = WLProcessImage.unitize(WLProcessImage.colSum(aoiFine))
        val cSpline = WLProcessImage.toCubicSpline(cSum)

        val fineX = centerOfMass(cSpline, cSum.length)
        val fineY = centerOfMass(rSpline, rSum.length)
        val image = annotate.saveFineLocatedImage(aoiFine.map(_.toIndexedSeq).toIndexedSeq, fineX, fineY)
        Util.writePng(image, new File(subDir, "ball_fine.png"))

        if (singleMax(cSpline, cSum) && singleMax(rSpline, rSum)) {
          diagnosticMessage("Ball fine location relative to area of interest in pixels: " + fineX.center + ", " + fineY.center)
          Some(fineX.center, fineY.center)
        } else None
      }

      val coarseCenter = coarseBallLocate
      // not sure why 0.3 works
      val radius: Int = (toPixels(wlParameters.ballDiameter_mm / 2.0) + (tol.toDouble * 0.3) + 0.5).toInt
      val leftEdge = coarseCenter._1 + tol2 - radius
      val topEdge = coarseCenter._2 + tol2 - radius
      val ballRoi = subSection(areaOfInterest, leftEdge, radius * 2, topEdge, radius * 2)

      Util.writePng(toPng(normalizeArea(ballRoi)), new File(subDir, "ballRoiNormalized.png"))
      Util.writePng(toPng(ballRoi), new File(subDir, "ballRoiRaw.png"))

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

    def list2Array(x: IndexedSeq[IndexedSeq[Float]]) = x.map(x => x)

    /**
      * Make an image showing the level of background noise immediately around the ball.
      */
    def showBallBackgroundNoise(areaOfInterest: IndexedSeq[IndexedSeq[Float]], name: String): Unit = {
      val aoiWidth = areaOfInterest.head.length
      val aoiHeight = areaOfInterest.length
      val aoi = subSection(areaOfInterest, 0, aoiWidth, 0, aoiHeight).map(_.toArray).toArray

      val all = aoi.flatten
      val min = all.min
      val max = all.max
      val limit = ((max - min) * 0.08) + min

      def doRow(y: Int): IndexedSeq[Float] = {
        def doPix(pix: Float): Float = {
          if (pix > limit)
            min
          else
            pix
        }

        areaOfInterest(y).map(doPix)
      }

      val background = (0 until aoiHeight).map(doRow)

      Util.writePng(toPng(background), new File(subDir, name + ".png"))
    }

    /**
      * Take the average of the darkest background pixels for
      * each row and subtract it from each pixel.
      */
    def normalizeArea(aoi: IndexedSeq[IndexedSeq[Float]]): IndexedSeq[IndexedSeq[Float]] = {
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
    def coarseBoxLocate(profile: IndexedSeq[Float]): (Int, Int) = {
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
    def fineBoxLocate(
        areaOfInterest: IndexedSeq[IndexedSeq[Float]],
        rawExtremeAveragesRange: Double,
        pixels: IndexedSeq[IndexedSeq[Float]],
        aoiBounds: Rectangle
    ): Either[WLImageStatus.Value, WLEdgeSet] = {

      if ( // do sanity check to see if the box is reasonably sized.
        areaOfInterest.isEmpty ||
        areaOfInterest.head.isEmpty ||
        (0 >= (areaOfInterest(0).length - tol4)) ||
        (0 >= (areaOfInterest.length - tol4))
      )
        Left(WLImageStatus.BoxTooSmall)
      else
        try {
          val height = areaOfInterest.length
          val width = areaOfInterest(0).length

          val x = aoiBounds.x
          val y = aoiBounds.y

          // @formatter:off
          val topAOI    = new Rectangle(x + tol2        , y + 0            , width - tol4, tol2         )
          val bottomAOI = new Rectangle(x + tol2        , y + height - tol2, width - tol4, tol2         )
          val leftAOI   = new Rectangle(x + 0           , y + tol2         , tol2        , height - tol4)
          val rightAOI  = new Rectangle(x + width - tol2, y + tol2         , tol2        , height - tol4)

          val di = new DicomImage(pixels)
          val wlTop    = WLEdge("top"   , vertical = false, di, rtimage,    topAOI)
          val wlBottom = WLEdge("bottom", vertical = false, di, rtimage, bottomAOI)
          val wlLeft   = WLEdge("left"  , vertical = true , di, rtimage,   leftAOI)
          val wlRight  = WLEdge("right" , vertical = true , di, rtimage,  rightAOI)
          // @formatter:on

          val edgeSet = WLEdgeSet(wlTop, wlBottom, wlLeft, wlRight)

          WLEdgeImage.makeEdgeImage(wlTop, subDir, SCALE)
          WLEdgeImage.makeEdgeImage(wlBottom, subDir, SCALE)
          WLEdgeImage.makeEdgeImage(wlLeft, subDir, SCALE)
          WLEdgeImage.makeEdgeImage(wlRight, subDir, SCALE)

          val status: WLImageStatus.Value = {
            val list = Seq(wlTop, wlBottom, wlLeft, wlRight).filter(_.edge.isLeft)
            if (list.isEmpty) WLImageStatus.Passed else list.head.edge.left.get
          }

          if (status == WLImageStatus.Passed)
            Right(edgeSet)
          else
            Left(status)
        } catch {
          case t: Throwable =>
            logger.info(s"Unexpected: ${fmtEx(t)}")
            Left(WLImageStatus.UnexpectedError)
        }
    }

    /**
     * Construct the pixel data destined to be put in the DICOM image with graphics and annotations.  This is done by
     * taking a buffered image that is all black except for the graphics and annotations, and then mapping that onto
     * a scaled up version of the original pixels.
     */
    def constructPixelData(blackPng: BufferedImage, areaOfInterest: IndexedSeq[IndexedSeq[Float]]): Array[Array[Float]] = {
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
    def ballAreaIsFlat(boxArea: IndexedSeq[IndexedSeq[Float]], edgeSet: WLEdgeSet): Boolean = {
      val boxWd = boxArea.head.length
      val boxHt = boxArea.length

      // Get the pixel that are not part of the ball
      val backgroundPixels = {
        // delineate a border that is half-way between the outer edge of the ball and edge of
        // the box.  Use the pixels in this border to get a good sample of background pixels that
        // do not include the ball.
        val xMin = (edgeSet.left.pos / 2).toInt
        val xMax = (edgeSet.right.pos + ((boxWd - edgeSet.right.pos) / 2)).toInt
        val yMin = (edgeSet.top.pos / 2).toInt
        val yMax = (edgeSet.bottom.pos + ((boxHt - edgeSet.bottom.pos) / 2)).toInt
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

      false // TODO disables test
    }


    /**
     * After locating the center of the box and the ball with some confidence, process the results.  The only
     * error that is expected to occur after this point is that the distance between the centers is too large.
     */
    def processLocation(
                         areaOfInterest: IndexedSeq[IndexedSeq[Float]],
                         edgeSet: WLEdgeSet,
                         ballRelativeCenter: (Double, Double),
                         ballArea: IndexedSeq[IndexedSeq[Float]],
                         coarseX: (Int, Int),
                         coarseY: (Int, Int),
                         brcX: Double,
                         brcY: Double,
                         badPixelList: Seq[WLBadPixel],
                         badPixelListShifted: Seq[WLBadPixel],
                         marginalPixelList: Seq[WLBadPixel],
                         attributeList: AttributeList,
                         runReq: WLRunReq
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

      def annotateBox(gc: Graphics2D): Unit = {
        annotate.drawBoxGraphics(gc, edgeSet.unTop, edgeSet.unBottom, edgeSet.unLeft, edgeSet.unRight, Config.WLBoxColor, inside = bin, outside = bout)
      }

      annotateBox(normalGraphics)
      annotateBox(brightGraphics)
      annotateBox(blackGraphics)

      val boxShrink = 5
      annotate.drawBoxGraphics(normalGraphics, edgeSet.unTop + boxShrink, edgeSet.unBottom - boxShrink, edgeSet.unLeft + boxShrink, edgeSet.unRight - boxShrink, Config.WLBoxColorCorrected, inside = -1, outside = 0)
      annotate.drawBoxGraphics(brightGraphics, edgeSet.unTop + boxShrink, edgeSet.unBottom - boxShrink, edgeSet.unLeft + boxShrink, edgeSet.unRight - boxShrink, Config.WLBoxColorCorrected, inside = -1, outside = 0)
      annotate.drawBoxGraphics(blackGraphics, edgeSet.unTop + boxShrink, edgeSet.unBottom - boxShrink, edgeSet.unLeft + boxShrink, edgeSet.unRight - boxShrink, Config.WLBoxColorCorrected, inside = -1, outside = 0)

      annotate.drawBoxBallOffset(normalGraphics, (ballCenterX, ballCenterY), ((edgeSet.unLeft + edgeSet.unRight) / 2, (edgeSet.unTop + edgeSet.unBottom) / 2))
      annotate.drawBoxBallOffset(brightGraphics, (ballCenterX, ballCenterY), ((edgeSet.unLeft + edgeSet.unRight) / 2, (edgeSet.unTop + edgeSet.unBottom) / 2))
      annotate.drawBoxBallOffset(blackGraphics, (ballCenterX, ballCenterY), ((edgeSet.unLeft + edgeSet.unRight) / 2, (edgeSet.unTop + edgeSet.unBottom) / 2))

      val boxUnscaledCorrectedCenterX = (edgeSet.unLeft + edgeSet.unRight) / 2
      val boxUnscaledCorrectedCenterY = (edgeSet.unTop + edgeSet.unBottom) / 2

      val boxCenterScaledX = boxUnscaledCorrectedCenterX * ResolutionX
      val boxCenterScaledY = boxUnscaledCorrectedCenterY * ResolutionY
      val ballCenterScaledX = ballCenterX * ResolutionX
      val ballCenterScaledY = ballCenterY * ResolutionY

      val errorScaledX = boxCenterScaledX - ballCenterScaledX
      val errorScaledY = boxCenterScaledY - ballCenterScaledY
      val errorScaledXYCombined = Math.sqrt((errorScaledX * errorScaledX) + (errorScaledY * errorScaledY))

      val passed: WLImageStatus.ImageStatus = {
        val p = annotate.annotateImage(normalPng, normalGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = true, imageName, passLimit_mm = wlParameters.passLimit_mm)
        p
      }
      annotate.annotateImage(brightPng, brightGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = true, imageName, passLimit_mm = wlParameters.passLimit_mm)
      annotate.annotateImage(blackPng, blackGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = false, imageName, passLimit_mm = wlParameters.passLimit_mm)

      val pixelData = constructPixelData(blackPng, areaOfInterest)

      Util.writePng(normalPng, new File(subDir, WLgenHtml.NORMAL_SUMMARY_FILE_NAME))
      Util.writePng(brightPng, new File(subDir, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME))
      logger.info("Done constructing ProcessImage for " + imageName)

      val boxPoint = new Point(boxCenterScaledX, boxCenterScaledY)
      val ballPoint = new Point(ballCenterScaledX, ballCenterScaledY)
      val edgesScaled = new Edges(
        edgeSet.top.pos * ResolutionY,
        edgeSet.bottom.pos * ResolutionY,
        edgeSet.left.pos * ResolutionX,
        edgeSet.right.pos * ResolutionX
      )

      //diagnosticMessage("\n\nScaled box dimensions in mm")
      //diagnostics.write(edgesScaled.toString.getBytes)

      // diagnosticMessage("\n\n")
      // diagnosticMessage("ball center mm   X: " + fmt(ballCenterScaledX) + "    Y: " + fmt(ballCenterScaledY))
      // diagnosticMessage("box  center mm   X: " + fmt(boxCenterScaledX) + "    Y: " + fmt(boxCenterScaledY))

      diagnosticMessage("X Offset mm " + fmt(errorScaledX))
      diagnosticMessage("Y Offset mm " + fmt(errorScaledY))
      diagnosticMessage("R mm " + fmt(errorScaledXYCombined))

      if (runReq.rtplan.isDefined) {
        val expected = PlannedRectangle(runReq.rtplan.get, rtimage)
        diagnosticMessage("expected edges: " + expected)
      }

      val imageResult = WLImageResult(
        imageStatus = passed,
        boxP = Some(boxPoint),
        ballP = Some(ballPoint),
        edgesUnscaled = None,
        boxEdgesP = Some(edgesScaled),
        edgeSet = Some(edgeSet),
        directory = subDir,
        // extendedData,
        rtimage = attributeList,
        pixels = Some(pixelData),
        Some(coarseX),
        Some(coarseY),
        Some(brcX),
        Some(brcY),
        badPixelList = badPixelList,
        marginalPixelList = marginalPixelList,
        extendedData = extendedData,
        runReq
      )

      diagnosticMessage("Image processing Results:\n" + imageResult.toString)

      WLgenHtml.generateHtml(extendedData, subDir, imageResult)

      imageResult
    }

    def saveWLBadPixelImage(pixels: IndexedSeq[IndexedSeq[Float]], badPixelList: Seq[WLBadPixel], marginalPixelList: Seq[WLBadPixel]): Unit = {
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

      Util.writePng(png, new File(subDir, BAD_PIXEL_FILE_NAME))
    }


    /**
     * Get the raw pixels.  Ensure that the majority of the pixels are large.  If they are
     * not, then invert pixels so that the small become large and the large become small.
     *
     * @return Pixel array.
     */
    def fetchPixels(): IndexedSeq[IndexedSeq[Float]] = {

      val di = new DicomImage(rtimage)

      val count = 20 // maximum number of high and low bad pixels

      // Drop some high and low pixels to make sure that a few bad pixels do not skew the mean.
      val sorted = di.pixelData.flatten.sorted.drop(count).dropRight(count)

      val mean = sorted.sum / sorted.size

      val belowMeanPixelCount = sorted.indexWhere(_ > mean)

      val minPlusMax = di.minPixelValue + di.maxPixelValue

      val pixelData: IndexedSeq[IndexedSeq[Float]] = {
        if (belowMeanPixelCount > (sorted.size / 2)) {
          def invert(pix: Float): Float = minPlusMax - pix

          val invertedDicomImage = di.fun1(invert)
          invertedDicomImage.pixelData
        } else
          di.pixelData
      }

      pixelData
    }

    // ----------------------------------------------------------------------------------------

    try {
      val uncorrectedPixels = fetchPixels()
      Util.writePng(toPngScaled(uncorrectedPixels, 1), new File(subDir, "original.png"))
      writeDicomAsBinaryDicom(rtimage)
      writeDicomAsText(rtimage)

      // Seq of raw distinct pixel values sorted by value
      val rawDistinctSortedList = uncorrectedPixels.flatten.toList.distinct.sorted

      if (rawDistinctSortedList.size < Config.WLMinimumDistinctPixelValues) {
        makeFailedWLImageStatus(WLImageStatus.BoxNotFound)
      } else {

        // ------------------------------------------------------------------------------------------------------------------------

        val wlBadPixels = WLBadPixels(new DicomImage(uncorrectedPixels))

        logger.info(s"$imageName Number of bad pixels: " + wlBadPixels.badPixelsCorrected.size + " : " + wlBadPixels.badPixelsCorrected)
        logger.info(s"$imageName Number of marginal pixels: " + wlBadPixels.marginalPixelsCorrected.size + " : " + wlBadPixels.marginalPixelsCorrected)

        val pixels = wlBadPixels.correctedImage

        // ------------------------------------------------------------------------------------------------------------------------

        val rawExtremeAveragesRange = calcExtremeAveragesRange(pixels)

        //val minRawPixel = minPixel(pixels)
        //val maxRawPixel = maxPixel(pixels)

        if (wlBadPixels.badPixelsCorrected.nonEmpty || wlBadPixels.marginalPixelsCorrected.nonEmpty) saveWLBadPixelImage(pixels, wlBadPixels.badPixelsCorrected, wlBadPixels.marginalPixelsCorrected)

        val coarseX = coarseBoxLocate(colSum(pixels))
        val coarseY = coarseBoxLocate(rowSum(pixels))

        val coarseBox = WLCoarseBox(new DicomImage(pixels), trans).locate()

        // Shift the bad pixels so that they point to the proper place in the area of interest (AOI)
        val badPixelListShifted = wlBadPixels.badPixelsCorrected.map(b => new WLBadPixel(b.x - coarseX._1, b.y - coarseY._1, b.rawValue, b.correctedValue, b.adjacentValidValueList))

        diagnosticMessage("\n\nbox coarse boundaries of area of interest    X: " + coarseX + "    Y: " + coarseY)

        val areaOfInterest = subSection(pixels, coarseX._1, coarseX._2 - coarseX._1, coarseY._1, coarseY._2 - coarseY._1)

        val fineBoxLocateResult = fineBoxLocate(areaOfInterest, rawExtremeAveragesRange, pixels, coarseBox)

        val result: WLImageResult = fineBoxLocateResult match {
          case Left(status) => makeFailedWLImageStatus(status)
          case Right(edgeSet) =>
            val width = areaOfInterest.head.size
            val height = areaOfInterest.size

            val oldTop = edgeSet.top.pos
            val oldBottom = edgeSet.bottom.pos + height - tol2
            val oldLeft = edgeSet.left.pos
            val oldRight = edgeSet.right.pos + width - tol2

            val ballArea = subSection(
              areaOfInterest,
              oldLeft.toInt + tol,
              (oldRight - oldLeft).toInt - tol2,
              oldTop.toInt + tol,
              (oldBottom - oldTop).toInt - tol2
            )
            showBallBackgroundNoise(ballArea, "ball_background")
            Util.writePng(toPng(ballArea), new File(subDir, "ball_before_normalization.png"))

            showBallBackgroundNoise(normalizeArea(ballArea), "normalized_ball_background")

            if (ballAreaIsFlat(areaOfInterest, edgeSet)) {
              makeFailedWLImageStatus(WLImageStatus.BallMissing)
            } else {
              findBallCenter(areaOfInterest, ballArea) match {
                case Some(ballRelativeCenter: (Double, Double)) =>
                  val brcX = ballRelativeCenter._1
                  val brcY = ballRelativeCenter._2

                  val ir = processLocation(
                    areaOfInterest = areaOfInterest,
                    edgeSet = edgeSet,
                    ballRelativeCenter = ballRelativeCenter,
                    ballArea = ballArea,
                    coarseX = coarseX,
                    coarseY = coarseY,
                    brcX = brcX,
                    brcY = brcY,
                    badPixelList = wlBadPixels.badPixelsCorrected,
                    badPixelListShifted = badPixelListShifted,
                    marginalPixelList = wlBadPixels.marginalPixelsCorrected,
                    attributeList = rtimage,
                    runReq = runReq
                  )

                  diagnosticMessage(ir.toString)
                  WLgenHtml.generateHtml(extendedData, subDir, imageResult = ir)
                  ir
                case None => makeFailedWLImageStatus(WLImageStatus.BallAreaNoisy)
              }
            }
        }

        result

      }
    } catch {
      case e: Exception =>
        val msg = "ProcessImage.process Unexpected exception: " + fmtEx(e)
        logger.error(msg)
        diagnosticMessage(msg)

        val imageResult =
          makeFailedWLImageStatus(WLImageStatus.UnexpectedError)

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

  //  class SearchRange(val lo: Double, val center: Double, val hi: Double) {}

  /**
   * Convert a list to a cubic spline
   */
  def toCubicSpline(data: IndexedSeq[Float]): CubicSpline = {
    val cs = new CubicSpline(data.indices.map(_.toDouble).toArray, data.map(_.toDouble).toArray)
    cs
  }

  def unitize(data: IndexedSeq[Float]): IndexedSeq[Float] = {
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
  def rowSum(pix: IndexedSeq[IndexedSeq[Float]]): IndexedSeq[Float] = pix.map(row => row.sum)

  /**
   * Make a list of the sum of each column.
   */
  def colSum(pix: IndexedSeq[IndexedSeq[Float]]): IndexedSeq[Float] = {
    def oneColSum(c: Int) = pix.indices.map(y => pix(y)(c)).sum

    pix(0).indices.map(c => oneColSum(c)).toArray
  }

  def toPngScaled(pix: IndexedSeq[IndexedSeq[Float]], imageScale: Int): BufferedImage = {
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


}
