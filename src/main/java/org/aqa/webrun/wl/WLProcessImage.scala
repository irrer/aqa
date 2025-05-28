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

  private def checkHasContrast(image: DicomImage): Option[WLImageResult] = {
    // Seq of raw distinct pixel values sorted by value
    val rawDistinctSortedList = image.pixelData.flatten.toList.distinct.sorted

    if (rawDistinctSortedList.size < Config.WLMinimumDistinctPixelValues) {
      Some(
        WLImageResult(
          WLImageStatus.BoxNotFound, //
          directory = subDir,
          rtimage = rtimage,
          badPixelList = Seq(),
          marginalPixelList = Seq(),
          extendedData = extendedData,
          runReq = runReq
        )
      )
    } else
      None
  }

  private val subDir: File = {
    val min = elapsedTime_ms / (60 * 1000)
    val sec = (elapsedTime_ms / 1000) % 60

    val name = min.formatted("%d") + "_" + sec.formatted("%02d") + "__" + gantryRounded_txt + "__" + collimatorRounded_txt + "-" + index.formatted("%02d")

    val dir = new File(extendedData.output.dir, name)
    dir.mkdirs()
    dir
  }

  private def saveWLBadPixelImage(pixels: IndexedSeq[IndexedSeq[Float]], badPixelList: Seq[WLBadPixel], marginalPixelList: Seq[WLBadPixel]): Unit = {
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

  private def writeDicomAsText(): Unit = {
    try {
      val text = DicomUtil.attributeListToString(rtimage)
      Util.writeFile(new File(subDir, Util.sopOfAl(rtimage) + ".txt"), text)
    } catch {
      case e: Exception =>
        logger.error("Unable to write DICOM file as text: " + e)
    }
  }

  private def writeDicomAsBinaryDicom(): Unit = {
    try {
      val file = new File(subDir, Util.sopOfAl(rtimage) + ".dcm")
      Util.writeAttributeListToFile(rtimage, file)
    } catch {
      case e: Exception =>
        logger.error("Unable to write DICOM file as binary DICOM: " + e)
    }
  }

  /** Scaling for drawing images. */
  private val SCALE: Int = ((Config.WLScale / 0.526) * ((ResolutionX + ResolutionY) / 2.0)).round.toInt

  private def toPng(pix: IndexedSeq[IndexedSeq[Float]]): BufferedImage = toPngScaled(pix, SCALE)

  /**
    * Make an image showing the level of background noise immediately around the ball.
    */
  private def showBallBackgroundNoise(areaOfInterest: IndexedSeq[IndexedSeq[Float]], name: String): Unit = {
    val aoiWidth = areaOfInterest.head.length
    val aoiHeight = areaOfInterest.length
    val aoi = {
      val di = new DicomImage(areaOfInterest)
      val subDi = di.getSubimage(new Rectangle(0, 0, aoiWidth, aoiHeight))
      subDi.pixelData
    }

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
    * Locate the box to sub-pixel accuracy.
    */
  private def fineBoxLocate(
      coarseAoi: DicomImage,
      pixels: IndexedSeq[IndexedSeq[Float]],
      aoiBounds: Rectangle,
      tol2: Int,
      tol4: Int
  ): Either[WLImageStatus.Value, WLEdgeSet] = {

    // do sanity check to see if the box is reasonably sized.
    if ((coarseAoi.width < tol4) || (coarseAoi.height < tol4))
      Left(WLImageStatus.BoxTooSmall)
    else
      try {
        val height = coarseAoi.height
        val width = coarseAoi.width

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

  /** Convert a value in mm to pixels.
   *
   * @param mm Value in mm.
   * @return value in pixels.
   * */
  private def toPixels(mm: Double): Int = ((mm / ResolutionX) + 0.5).toInt


  // tolerance in pixels for how far the search for the fine edge of
  // box should look, given the coarse position of the edge.  Also used
  // determine where an edge of the box finishes, to define the
  // area to look for the ball.
  private val tol: Int = toPixels(Config.WLBoxEdgeTolerance_mm)
  private val tol2 = tol * 2
  private val tol4 = tol * 4
  private val tol34 = tol // (tol * 0.75).round.toInt
  private val tol15 = tol2 // (tol * 1.5).round.toInt

  // The step size (in pixels) for crawling down the curve to find the
  // point where the ball height exceeds a threshold
  private val X_INCREMENT: Double = 0.001

  /** Expected radius of ball in (units of) number of pixels. */
  private val BALL_RADIUS = toPixels(wlParameters.ballDiameter_mm / 2.0)

  private val annotate = new WLAnnotate(SCALE, BALL_RADIUS)

  private def fmt(d: Double): String = d.formatted("%10.5f")

  private val diagnostics = new PrintStream(new File(subDir, WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME))

  private def diagnosticMessage(text: String): Unit = {
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

  /**
   * Generate an image of the given size with all black pixels
   */
  private def toBlackPng(pix: IndexedSeq[IndexedSeq[Float]]): BufferedImage = {
    val png = toPngScaled(pix, SCALE)
    for (y <- 0 until png.getHeight) for (x <- 0 until png.getWidth) png.setRGB(x, y, EMPTY_PIXEL)
    png
  }


  /**
   * Take the average of the darkest background pixels for
   * each row and subtract it from each pixel.
   */
  private def normalizeArea(aoi: IndexedSeq[IndexedSeq[Float]]): IndexedSeq[IndexedSeq[Float]] = {
    aoi.map(row => {
      val bias = row.sorted.take(Config.WLNumBackgroundPixels).sum / Config.WLNumBackgroundPixels
      row.map(col => if (col > bias) col - bias else 0)
    })
  }

  /**
   * Construct the pixel data destined to be put in the DICOM image with graphics and annotations.  This is done by
   * taking a buffered image that is all black except for the graphics and annotations, and then mapping that onto
   * a scaled up version of the original pixels.
   */
  private def constructPixelData(blackPng: BufferedImage, areaOfInterest: IndexedSeq[IndexedSeq[Float]]): Array[Array[Float]] = {
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
   * Determine whether the area inside the box is flat.  Do this by determine the ratio of the
   * range of the box pixels over the range of the ball pixels.  If this number is too large,
   * then the area is too flat to contain a ball.
   */
  private def ballAreaIsFlat(boxArea: IndexedSeq[IndexedSeq[Float]], ballArea: IndexedSeq[IndexedSeq[Float]], ballBounds: Rectangle): Boolean = {
    val boxWd = boxArea.head.length
    val boxHt = boxArea.length

    val boxPix = boxArea.flatten
    val boxMin = boxPix.min
    val boxMax = boxPix.max
    val boxRange = boxMax - boxMin

    val ballPix = ballArea.flatten
    val ballMin = ballPix.min
    val ballMax = ballPix.max
    val ballRange = ballMax - ballMin

    val rat = boxRange / ballRange

    Trace.trace(s"rat: $rat")
    Trace.trace()

    if (true) {
      val box = new DicomImage(boxArea).toBufferedImage(Color.green)
      val ball = new DicomImage(ballArea).toBufferedImage(Color.yellow)

      Util.writePng(box, new File(subDir, "theBox.png"))
      Util.writePng(ball, new File(subDir, "theBall.png"))
      Trace.trace("wrote theBox theBall " + subDir)
      Trace.trace()
    }

    // Get the pixel that are not part of the ball
    val backgroundPixels = {
      // delineate a border that is half-way between the outer edge of the ball and edge of
      // the box.  Use the pixels in this border to get a good sample of background pixels that
      // do not include the ball.
      val xMin = ballBounds.x
      val xMax = ballBounds.x + ballBounds.width
      val yMin = ballBounds.y
      val yMax = ballBounds.y + ballBounds.height
      for (
        x <- 0 until boxWd;
        y <- 0 until boxHt
        if (x < xMin) || (x > xMax) || (y < yMin) || (y > yMax)
      ) yield {
        boxArea(y)(x)
      }
    }

    val backgroundRange = backgroundPixels.max - backgroundPixels.min

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
  private def processLocation(
                               coarseAoi: IndexedSeq[IndexedSeq[Float]],
                               edgeSet: WLEdgeSet,
                               ballRelativeCenter: (Double, Double),
                               ballArea: IndexedSeq[IndexedSeq[Float]],
                               coarseAoiBounds: Rectangle,
                               brcX: Double,
                               brcY: Double,
                               badPixelList: Seq[WLBadPixel],
                               marginalPixelList: Seq[WLBadPixel],
                               attributeList: AttributeList,
                               runReq: WLRunReq
                             ): WLImageResult = {

    val ballCenterX = ballRelativeCenter._1
    val ballCenterY = ballRelativeCenter._2

    // Limit all pixel brightnesses to the maximum ball pixel brightness.  This makes the ball stand out more in the final image.
    val maxBallPixel = ballArea.flatten.max
    val thresholdLimitedAreaOfInterest = coarseAoi.map(r => r.map(c => if (c > maxBallPixel) maxBallPixel else c))

    //drawGraphics()
    val normalPng = toPng(coarseAoi)
    val brightPng = toPng(thresholdLimitedAreaOfInterest)
    val blackPng = toBlackPng(coarseAoi)

    // draw edge of box
    val normalGraphics = normalPng.getGraphics.asInstanceOf[Graphics2D]
    val brightGraphics = brightPng.getGraphics.asInstanceOf[Graphics2D]
    val blackGraphics = blackPng.getGraphics.asInstanceOf[Graphics2D]

    annotate.drawBallGraphics(normalGraphics, ballCenterX, ballCenterY)
    annotate.drawBallGraphics(brightGraphics, ballCenterX, ballCenterY)
    annotate.drawBallGraphics(blackGraphics, ballCenterX, ballCenterY)

    annotate.highlightWLBadPixelList(badPixelList, normalGraphics)
    annotate.highlightWLBadPixelList(badPixelList, brightGraphics)
    annotate.highlightWLBadPixelList(badPixelList, blackGraphics)

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

    val pixelData = constructPixelData(blackPng, coarseAoi)

    Util.writePng(normalPng, new File(subDir, WLgenHtml.NORMAL_SUMMARY_FILE_NAME))
    Util.writePng(brightPng, new File(subDir, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME))
    logger.info("Done constructing ProcessImage for " + imageName)

    val boxPoint = new Point(boxCenterScaledX, boxCenterScaledY)
    val ballPoint = new Point(ballCenterScaledX, ballCenterScaledY)
    val edgesScaled = new Edges(
      edgeSet.top.pos_pix * ResolutionY,
      edgeSet.bottom.pos_pix * ResolutionY,
      edgeSet.left.pos_pix * ResolutionX,
      edgeSet.right.pos_pix * ResolutionX
    )

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
      rtimage = attributeList,
      pixels = Some(pixelData),
      coarseAoiBounds = Some(coarseAoiBounds),
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


  def process: WLImageResult = {
    logger.info("Start constructing ProcessImage for " + Util.sopOfAl(rtimage))
    //noinspection RegExpRepeatedSpace,RegExpSimplifiable
    diagnosticMessage("\nOutput :\n    " + extendedData.output.toString.replaceAll("   *", "\n    "))

    // ----------------------------------------------------------------------------------------

    try {
      // val uncorrectedPixels = fetchPixels()
      val preprocessImage = WLPreprocessImage(rtimage, imageName)
      val pixels = preprocessImage.preprocessedImage.pixelData
      // val uncorrectedPixels = fetchPixels()
      Util.writePng(toPngScaled(preprocessImage.preprocessedImage.pixelData, 1), new File(subDir, "original.png"))
      writeDicomAsBinaryDicom()
      writeDicomAsText()

      val imageHasContrast = checkHasContrast(preprocessImage.preprocessedImage)

      if (imageHasContrast.isDefined) {
        imageHasContrast.get
      } else {

        val badPixels = preprocessImage.badPixels

        if (badPixels.badPixelsCorrected.nonEmpty || badPixels.marginalPixelsCorrected.nonEmpty)
          saveWLBadPixelImage(pixels, badPixels.badPixelsCorrected, badPixels.marginalPixelsCorrected)

        val coarseAoiBounds = WLCoarseBox(new DicomImage(pixels), trans).locate()

        val coarseAoi: DicomImage = preprocessImage.preprocessedImage.getSubimage(coarseAoiBounds)

        val fineBoxLocateResult = fineBoxLocate(coarseAoi, pixels, coarseAoiBounds, tol2, tol4)

        val result: WLImageResult = fineBoxLocateResult match {
          case Left(status) =>
            WLImageResult(imageStatus = status, directory = subDir, rtimage = rtimage, badPixelList = Seq(), marginalPixelList = Seq(), extendedData = extendedData, runReq = runReq)
          case Right(edgeSet) =>

            // create an area that is 3/4 of tol (penumbra width) inside each of the four measured edges
            val ballBounds: Rectangle = {
              val x = edgeSet.left.posInt_pix + tol34
              val y = edgeSet.top.posInt_pix + tol34
              val w = (edgeSet.right.posAbs_pix - edgeSet.left.posAbs_pix).round.toInt - tol15
              val h = (edgeSet.bottom.posAbs_pix - edgeSet.top.posAbs_pix).round.toInt - tol15

              new Rectangle(x, y, w, h)
            }

            val ballAoi = coarseAoi.getSubimage(ballBounds).pixelData

            showBallBackgroundNoise(ballAoi, "ball_background")
            Util.writePng(toPng(ballAoi), new File(subDir, "ball_before_normalization.png"))

            showBallBackgroundNoise(normalizeArea(ballAoi), "normalized_ball_background")


            if (ballAreaIsFlat(coarseAoi.pixelData, ballAoi, ballBounds)) {
              WLImageResult(WLImageStatus.BallMissing, directory = subDir, rtimage = rtimage, badPixelList = Seq(), marginalPixelList = Seq(), extendedData = extendedData, runReq = runReq)
            } else {
              val wlBallOld = WLBallOld(coarseAoi, ballAoi, subDir, SCALE, BALL_RADIUS, X_INCREMENT, ResolutionX, ResolutionY, wlParameters, tol)
              wlBallOld.findBallCenter() match {
                case Some(ballRelativeCenter: (Double, Double)) =>

                  if (false) { // TODO enables / disables experimental code

                    val ballGlobalBounds = new Rectangle(
                      ballBounds.x + coarseAoiBounds.x - tol34,
                      ballBounds.y + coarseAoiBounds.y - tol34,
                      ballBounds.width + tol,
                      ballBounds.height + tol
                    )
                    val wlBall = WLBall(ballGlobalBounds: Rectangle, new DicomImage(pixels): DicomImage, rtimage: AttributeList, machineWL = wlParameters, subDir)
                    val p = wlBall.center_pix
                    Trace.trace(s"new: $p    old: $ballRelativeCenter")
                  }

                  val brcX = ballRelativeCenter._1
                  val brcY = ballRelativeCenter._2

                  val ir = processLocation(
                    coarseAoi = coarseAoi.pixelData,
                    edgeSet = edgeSet,
                    ballRelativeCenter = ballRelativeCenter,
                    ballArea = ballAoi,
                    coarseAoiBounds = coarseAoiBounds,
                    brcX = brcX,
                    brcY = brcY,
                    badPixelList = badPixels.badPixelsCorrected,
                    marginalPixelList = badPixels.marginalPixelsCorrected,
                    attributeList = rtimage,
                    runReq = runReq
                  )

                  diagnosticMessage(ir.toString)
                  WLgenHtml.generateHtml(extendedData, subDir, imageResult = ir)
                  ir
                case None =>
                  WLImageResult(WLImageStatus.BallAreaNoisy, directory = subDir, rtimage = rtimage, badPixelList = Seq(), marginalPixelList = Seq(), extendedData = extendedData, runReq = runReq)
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

        val imageResult = {
          // TODO add bad and marginal pixels to the result if they are available
          WLImageResult(WLImageStatus.UnexpectedError, directory = subDir, rtimage = rtimage, badPixelList = Seq(), marginalPixelList = Seq(), extendedData = extendedData, runReq = runReq)
        }

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

  /**
   * Make a list of the sum of each row.
   */
  def rowSum(pix: IndexedSeq[IndexedSeq[Float]]): IndexedSeq[Float] = pix.map(row => row.sum)

  /**
   * Make a list of the sum of each column.
   */
  def colSum(pix: IndexedSeq[IndexedSeq[Float]]): IndexedSeq[Float] = {
    def oneColSum(c: Int) = pix.indices.map(y => pix(y)(c)).sum

    pix(0).indices.map(c => oneColSum(c))
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
