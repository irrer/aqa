package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.webrun.ExtendedData
import org.aqa.PlannedRectangle
import org.aqa.webrun.psm.PSMUtil
import org.opensourcephysics.numerics.CubicSpline

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Rectangle
import java.io.File
import java.io.PrintStream
import java.text.SimpleDateFormat
import java.util.Date
import scala.annotation.tailrec

class WLProcessImage(extendedData: ExtendedData, rtimage: AttributeList, index: Int, runReq: WLRunReq) extends org.aqa.Logging {

  private val wholeImage = new DicomImage(rtimage)

  import org.aqa.webrun.wl.WLProcessImage.toPngScaled

  private val wlParameters = MachineWL.getMachineWLOrDefault(extendedData.machine.machinePK.get)

  private val trans = new IsoImagePlaneTranslator(rtimage)
  private val ResolutionX = trans.pix2IsoDistX(1)
  private val ResolutionY = trans.pix2IsoDistY(1)

  private val SizeX = trans.width
  private val SizeY = trans.height

  private val EMPTY_PIXEL: Int = Config.WLTextColor.getRGB + 1
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
    new WLImageResult(
      imageStatus = imageStatus,
      boxP = null,
      ballP = null,
      edgesUnscaled = null,
      boxEdgesP = null,
      directory = subDir,
      rtimage = rtimage,
      pixels = null,
      aoiBounds = new Rectangle(-1, -1, -1, -1),
      brcX = -1,
      brcY = -1,
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
    // determine where an edge of the box finishes, so to define the
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

    class UncorrectedWLBadPixel(val x: Int, val y: Int, val rawValue: Double)

    /**
      * Make a list of bad pixels.
      */
    def findWLBadPixels(rawPixels: IndexedSeq[IndexedSeq[Float]], pixelGapLimit: Int, rawDistinctSortedList: Seq[Float]): Seq[UncorrectedWLBadPixel] = {
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

    def uncorrectedWLBadPixelsToWLBadPixels(rawPixelData: IndexedSeq[IndexedSeq[Float]], badPixelList: Seq[UncorrectedWLBadPixel]): Seq[WLBadPixel] = {
      // A pixel is good if its coordinates are valid and that it is not on the bad pixel list
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
    def correctWLBadPixels(rawPixels: IndexedSeq[IndexedSeq[Float]], badPixelList: Seq[WLBadPixel]): IndexedSeq[IndexedSeq[Float]] = {
      if (badPixelList.isEmpty)
        rawPixels
      else {
        val bad = badPixelList.head

        def fixRow(r: IndexedSeq[Float]): IndexedSeq[Float] = {
          (0 until SizeX).map(col => if (col == bad.x) bad.correctedValue else r(col))
        }

        val o = (0 until SizeY).map(row => if (row == bad.y) fixRow(rawPixels(row)) else rawPixels(row))
        correctWLBadPixels(o, badPixelList.tail)
      }
    }

    def calcExtremeAveragesRange(pix: IndexedSeq[IndexedSeq[Float]]): Double = {
      val ordered = pix.flatten.toList.sorted
      val count = Config.WLAveragePixelsForBrightness
      val min = ordered.take(count).sum / count
      val max = ordered.takeRight(count).sum / count
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

    def writeImage(img: BufferedImage, name: String): Unit = {
      val pngFile = new File(subDir, name)
      logger.info("writing image file: " + pngFile.getAbsolutePath + "   width: " + img.getWidth + "    height: " + img.getHeight)
      Util.writePng(img, pngFile)
    }

    def bound(x: Double, lo: Int, hi: Int) = {
      if (x < lo) lo else if (x > hi) hi else x
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
      val rect = new Rectangle(x, y, xWidth1, yHeight1)
      val di = new DicomImage(pixIn).getSubArray(rect)
      di
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

        def sumOf(x: Int, y: Int): Float = {
          // @formatter:off
            approxAoi(y - 1)(x - 1) +
            approxAoi(y - 1)(x    ) +
            approxAoi(y - 1)(x + 1) +
            approxAoi(y    )(x - 1) +
            approxAoi(y    )(x    ) +
            approxAoi(y    )(x + 1) +
            approxAoi(y + 1)(x - 1) +
            approxAoi(y + 1)(x    ) +
            approxAoi(y + 1)(x + 1)
        // @formatter:on
        }

        // make a list of coordinate pairs
        val xyList = for (y <- 1 until (approxAoi.length - 1); x <- 1 until (approxAoi.head.length - 1)) yield (x, y)

        val max = xyList.maxBy(xy => sumOf(xy._1, xy._2))

        max
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
            case _ if overAvg && (value < avg) => (crossCount + 1, false)
            case _ if (!overAvg) && (value >= avg) => (crossCount + 1, true)
            case _ => (crossCount, overAvg)
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
        val image = annotate.saveFineLocatedImage(aoiFine, fineX, fineY)
        writeImage(image, "ball_fine" + ".png")

        if (singleMax(cSpline, cSum) && singleMax(rSpline, rSum)) {
          diagnosticMessage("Ball fine location relative to area of interest in pixels: " + fineX.center + ", " + fineY.center)
          Some(fineX.center, fineY.center)
        } else
          None
      }

      val coarseCenter = coarseBallLocate
      // not sure why 0.3 works
      val radius: Int = (toPixels(wlParameters.ballDiameter_mm / 2.0) + (tol.toDouble * 0.3) + 0.5).toInt
      val leftEdge = coarseCenter._1 + tol2 - radius
      val topEdge = coarseCenter._2 + tol2 - radius
      val ballRoi = subSection(areaOfInterest, leftEdge, radius * 2, topEdge, radius * 2)

      writeImage(toPng(normalizeArea(ballRoi)), "ballRoiNormalized" + ".png")
      writeImage(toPng(ballRoi), "ballRoiRaw" + ".png")

      fineBallLocate(ballRoi) match {
        case Some(loc: (Double, Double)) =>
          Some(loc._1 + leftEdge, loc._2 + topEdge)
        case None =>
          None
      }
    }

    /**
     * Make an image showing the level of background noise immediately around the ball.
     */
    def showBallBackgroundNoise(areaOfInterest: IndexedSeq[IndexedSeq[Float]], name: String): Unit = {
      val aoiWidth = areaOfInterest.head.length
      val aoiHeight = areaOfInterest.length
      val aoi = subSection(areaOfInterest, 0, aoiWidth, 0, aoiHeight)

      val min = aoi.flatten.min
      val max = aoi.flatten.max
      val limit = ((max - min) * 0.08) + min

      def doRow(row: IndexedSeq[Float]) = row.map(v => if (v > limit) min else v)

      val newAoi = aoi.map(doRow)

      writeImage(toPng(newAoi), name + ".png")
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
     * Locate the box to sub-pixel accuracy.
     */
    def fineBoxLocate(areaOfInterest: IndexedSeq[IndexedSeq[Float]], rawExtremeAveragesRange: Double): Either[WLImageStatus.Value, Edges] = {

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

          val topArea = subSection(areaOfInterest, tol2, width - tol4, 0, tol2)
          val bottomArea = subSection(areaOfInterest, tol2, width - tol4, height - tol2, tol2)
          val leftArea = subSection(areaOfInterest, 0, tol2, tol2, height - tol4)
          val rightArea = subSection(areaOfInterest, width - tol2, tol2, tol2, height - tol4)


          // @formatter:off
          val eTop    = WLEdge("top"   , new DicomImage(topArea   ), vertical = false, wholeImage, rtimage).findEdge()
          val eBottom = WLEdge("bottom", new DicomImage(bottomArea), vertical = false, wholeImage, rtimage).findEdge()
          val eLeft   = WLEdge("left"  , new DicomImage(leftArea  ), vertical = true , wholeImage, rtimage).findEdge()
          val eRight  = WLEdge("right" , new DicomImage(rightArea ), vertical = true , wholeImage, rtimage).findEdge()
          // @formatter:on

          def edgeStatus(e: Either[WLImageStatus.Value, Double]): WLImageStatus.Value = {
            e match {
              case Right(_) => WLImageStatus.Passed
              case Left(s) => s
            }
          }

          val status: WLImageStatus.Value = {
            val list = Seq(eTop, eBottom, eLeft, eRight).map(edgeStatus).distinct.filterNot(_ == WLImageStatus.Passed)
            if (list.isEmpty) WLImageStatus.Passed else list.head
          }

          if (status == WLImageStatus.Passed) {
            val edgeTop: Double = eTop.right.get
            val edgeBottom = eBottom.right.get + (height - tol2)
            val edgeLeft = eLeft.right.get
            val edgeRight = eRight.right.get + (width - tol2)

            val edges = new Edges(edgeTop, edgeBottom, edgeLeft, edgeRight)
            //diagnosticMessage(s"\n\nUnscaled box dimensions in pixels\n$edges")
            //diagnostics.write(edges.toString.getBytes)
            Right(edges)
            // Right(edges2)
          } else
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
    def constructPixelData(blackPng: BufferedImage, areaOfInterest: IndexedSeq[IndexedSeq[Float]]): IndexedSeq[IndexedSeq[Float]] = {
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

      aoi.map(_.toIndexedSeq).toIndexedSeq
    }

    /**
     * Determine whether the area inside the box is flat.  Do this by determine the ratio of the
     * range of the box pixels over the range of the ball pixels.  If this number is too large,
     * then the area is too flat to contain a ball.
     */
    def ballAreaIsFlat(boxArea: IndexedSeq[IndexedSeq[Float]], ballEdges: Edges): Boolean = {
      val boxWd = boxArea.head.length
      val boxHt = boxArea.length

      // Get the pixel that are not part of the ball
      val backgroundPixels = {
        // delineate a border that is halfway between the outer edge of the ball and edge of
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
                         areaOfInterest: IndexedSeq[IndexedSeq[Float]],
                         edgesUnscaled: Edges,
                         ballRelativeCenter: (Double, Double),
                         ballArea: IndexedSeq[IndexedSeq[Float]],
                         aoiBounds: Rectangle,
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
      annotate.drawBoxGraphics(normalGraphics, edgesUnscaled.top, edgesUnscaled.bottom, edgesUnscaled.left, edgesUnscaled.right, Config.WLBoxColor, inside = bin, outside = bout)
      annotate.drawBoxGraphics(brightGraphics, edgesUnscaled.top, edgesUnscaled.bottom, edgesUnscaled.left, edgesUnscaled.right, Config.WLBoxColor, inside = bin, outside = bout)
      annotate.drawBoxGraphics(blackGraphics, edgesUnscaled.top, edgesUnscaled.bottom, edgesUnscaled.left, edgesUnscaled.right, Config.WLBoxColor, inside = bin, outside = bout)

      val boxShrink = 5
      val boxUnscaledCorrected = correctUnscaledEdges(edgesUnscaled)
      val buc = boxUnscaledCorrected
      // val inner = 24
      annotate.drawBoxGraphics(normalGraphics, buc.top + boxShrink, buc.bottom - boxShrink, buc.left + boxShrink, buc.right - boxShrink, Config.WLBoxColorCorrected, inside = -1, outside = 0)
      annotate.drawBoxGraphics(brightGraphics, buc.top + boxShrink, buc.bottom - boxShrink, buc.left + boxShrink, buc.right - boxShrink, Config.WLBoxColorCorrected, inside = -1, outside = 0)
      annotate.drawBoxGraphics(blackGraphics, buc.top + boxShrink, buc.bottom - boxShrink, buc.left + boxShrink, buc.right - boxShrink, Config.WLBoxColorCorrected, inside = -1, outside = 0)

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

      val passed: WLImageStatus.ImageStatus = {
        val p = annotate.annotateImage(normalPng, normalGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = true, imageName, passLimit_mm = wlParameters.passLimit_mm)
        p
      }
      annotate.annotateImage(brightPng, brightGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = true, imageName, passLimit_mm = wlParameters.passLimit_mm)
      annotate.annotateImage(blackPng, blackGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = false, imageName, passLimit_mm = wlParameters.passLimit_mm)

      val pixelData = constructPixelData(blackPng, areaOfInterest)

      writeImage(normalPng, WLgenHtml.NORMAL_SUMMARY_FILE_NAME)
      writeImage(brightPng, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)
      logger.info("Done constructing ProcessImage for " + imageName)

      val boxPoint = new Point(boxCenterScaledX, boxCenterScaledY)
      val ballPoint = new Point(ballCenterScaledX, ballCenterScaledY)
      val edgesScaled = new Edges(
        edgesUnscaled.top * ResolutionY,
        edgesUnscaled.bottom * ResolutionX,
        edgesUnscaled.left * ResolutionX,
        edgesUnscaled.right * ResolutionX
      )

      diagnosticMessage("X Offset mm " + fmt(errorScaledX))
      diagnosticMessage("Y Offset mm " + fmt(errorScaledY))
      diagnosticMessage("R mm " + fmt(errorScaledXYCombined))

      if (runReq.rtplan.isDefined) {
        val expected = PlannedRectangle(runReq.rtplan.get, rtimage)
        diagnosticMessage("expected edges: " + expected)
      }

      val imageResult = new WLImageResult(
        imageStatus = passed,
        boxP = boxPoint,
        ballP = ballPoint,
        edgesUnscaled = edgesUnscaled,
        boxEdgesP = edgesScaled,
        directory = subDir,
        // extendedData,
        rtimage = attributeList,
        pixels = pixelData,
        aoiBounds,
        brcX,
        brcY,
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

      writeImage(png, BAD_PIXEL_FILE_NAME)
    }

    def badPixelIsNotOnList(badPixel: WLBadPixel, list: Seq[WLBadPixel]): Boolean = {
      !list.exists(b => (b.x == badPixel.x) && (b.y == badPixel.y))
    }

    def fetchRawPixels(): IndexedSeq[IndexedSeq[Float]] = {
      val height: Int = rtimage.get(TagByName.Rows).getIntegerValues()(0)
      val width: Int = rtimage.get(TagByName.Columns).getIntegerValues()(0)
      val shorts: IndexedSeq[Short] = rtimage.get(TagByName.PixelData).getShortValues
      // JavaUtil.pixelDataToIndexedSeq(height, width, shorts)
      val flip = {
        val attr = rtimage.get(TagByName.PixelIntensityRelationshipSign)
        val f = 0 match {
          case _ if attr == null => true
          case _ if attr.getIntegerValues.head < 0 => false
          case _ => true
        }

        f
      }

      Trace.trace("rawPixels: " + PSMUtil.centerPixelsToString(new DicomImage(rtimage)))
      val pix = JavaUtil.pixelDataToArray(height, width, shorts.toArray, flip)
      val pixIndexSeq = pix.map(_.toIndexedSeq).toIndexedSeq
      Trace.trace("pix (flipped as needed: " + PSMUtil.centerPixelsToString(new DicomImage(pixIndexSeq)))
      pixIndexSeq
    }

    // ----------------------------------------------------------------------------------------

    try {
      val rawPixels = fetchRawPixels()
      writeImage(toPngScaled(rawPixels, 1), "original.png")
      writeDicomAsBinaryDicom(rtimage)
      writeDicomAsText(rtimage)

      // Seq of raw distinct pixel values sorted by value
      val rawDistinctSortedList = rawPixels.flatten.toList.distinct.sorted

      if (rawDistinctSortedList.size < Config.WLMinimumDistinctPixelValues) {
        new WLImageResult(
          imageStatus = WLImageStatus.BoxNotFound,
          boxP = null,
          ballP = null,
          edgesUnscaled = null,
          boxEdgesP = null,
          directory = subDir,
          rtimage = rtimage,
          pixels = null,
          aoiBounds = new Rectangle(-1, -1, -1, -1),
          brcX = -1,
          brcY = -1,
          badPixelList = Seq(),
          marginalPixelList = Seq(),
          extendedData = extendedData,
          runReq
        )
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

        if (badPixelList.nonEmpty || marginalPixelList.nonEmpty) saveWLBadPixelImage(pixels, badPixelList, marginalPixelList)

        val aoiBounds = WLCoarseBox(rtimage).locate()

        // Shift the bad pixels so that they point to the proper place in the area of interest (AOI)
        val badPixelListShifted = badPixelList.map(b => new WLBadPixel(b.x - aoiBounds.x, b.y - aoiBounds.y, b.rawValue, b.correctedValue, b.adjacentValidValueList))

        diagnosticMessage(s"\n\nbox coarse boundaries of area of interest: $aoiBounds")

        val areaOfInterest = subSection(pixels, aoiBounds.x, aoiBounds.width, aoiBounds.y, aoiBounds.height)

        val fineBoxLocateResult = fineBoxLocate(areaOfInterest, rawExtremeAveragesRange)

        val result: WLImageResult = fineBoxLocateResult match {
          case Left(status) =>
            makeFailedWLImageStatus(status)
          case Right(edgesUnscaled) =>
            val ballArea = subSection(
              areaOfInterest,
              edgesUnscaled.left.toInt + tol,
              (edgesUnscaled.right - edgesUnscaled.left).toInt - tol2,
              edgesUnscaled.top.toInt + tol,
              (edgesUnscaled.bottom - edgesUnscaled.top).toInt - tol2
            )
            showBallBackgroundNoise(ballArea, "ball_background")
            writeImage(toPng(ballArea), "ball_before_normalization" + ".png")

            showBallBackgroundNoise(normalizeArea(ballArea), "normalized_ball_background")

            if (ballAreaIsFlat(areaOfInterest, edgesUnscaled)) {
              makeFailedWLImageStatus(WLImageStatus.BallMissing)
            } else {
              findBallCenter(areaOfInterest, ballArea) match {
                case Some(ballRelativeCenter: (Double, Double)) =>
                  val brcX = ballRelativeCenter._1
                  val brcY = ballRelativeCenter._2

                  val ir = processLocation(
                    areaOfInterest = areaOfInterest,
                    edgesUnscaled = edgesUnscaled,
                    ballRelativeCenter = ballRelativeCenter,
                    ballArea = ballArea,
                    aoiBounds = aoiBounds,
                    brcX = brcX,
                    brcY = brcY,
                    badPixelList = badPixelList,
                    badPixelListShifted = badPixelListShifted,
                    marginalPixelList = marginalPixelList,
                    attributeList = rtimage,
                    runReq = runReq
                  )

                  diagnosticMessage(ir.toString)
                  WLgenHtml.generateHtml(extendedData, subDir, imageResult = ir)
                  ir
                case None =>
                  makeFailedWLImageStatus(WLImageStatus.BallAreaNoisy)
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

  /**
   * Convert a list to a cubic spline
   */
  def toCubicSpline(data: IndexedSeq[Float]): CubicSpline = new CubicSpline(data.indices.toArray.map(s => s.toDouble), data.map(f => f.toDouble).toArray)

  def unitize(data: IndexedSeq[Float]): IndexedSeq[Float] = {
    val min = data.min
    val range = data.max - min
    data.map(x => (x - min) / range)
  }

  /**
   * Make a list of the sum of each row.
   */
  def rowSum(pix: IndexedSeq[IndexedSeq[Float]]): IndexedSeq[Float] = pix.map(row => row.sum)

  /**
   * Make a list of the sum of each column.
   */
  def colSum(pix: IndexedSeq[IndexedSeq[Float]]): IndexedSeq[Float] = {
    val di = new DicomImage(pix.map(_.toIndexedSeq).toIndexedSeq)
    di.columnSums.toIndexedSeq
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
