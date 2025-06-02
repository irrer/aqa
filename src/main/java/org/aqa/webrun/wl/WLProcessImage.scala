package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.opensourcephysics.numerics.CubicSpline

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle
import java.io.File

/*
class WLBadPixel(val x: Int, val y: Int, val rawValue: Int, val correctedValue: Float, val adjacentValidValueList: Seq[Int]) {
  override def toString: String = {
    "   x: " + x + "   y: " + y + "   rawValue: " + rawValue + "   correctedValue: " + correctedValue +
      adjacentValidValueList.sorted.reverse.foldLeft("\n            Adjacent Valid Values and difference from raw:")((t, v) => t + "\n                " + v + "  :  " + Math.abs(v - rawValue))
  }
}
 */

class WLProcessImage(extendedData: ExtendedData, rtimage: AttributeList, index: Int, runReq: WLRunReq) extends Logging {

  import org.aqa.webrun.wl.WLProcessImage.toPngScaled

  private val wlParameters = MachineWL.getMachineWLOrDefault(extendedData.machine.machinePK.get)

  private val trans = new IsoImagePlaneTranslator(rtimage)
  private val ResolutionX = trans.pix2IsoDistX(1)
  private val ResolutionY = trans.pix2IsoDistY(1)

  private val wlMsg = WLMessage(extendedData, rtimage)

  // private val gantryAngle = Util.gantryAngle(rtimage)
  // private val collimatorAngle = Util.collimatorAngle(rtimage)

  // private  val NORMAL_SUMMARY_FILE_NAME = "normalSummary" + IMAGE_FILE_SUFFIX
  // private  val BRIGHT_SUMMARY_FILE_NAME = "brightSummary" + IMAGE_FILE_SUFFIX
  // private  val ORIGINAL_FILE_NAME = "original" + IMAGE_FILE_SUFFIX
  private val BAD_PIXEL_FILE_NAME = "badPixels" + WLgenHtml.IMAGE_FILE_SUFFIX

  private val elapsedTime_ms = {
    val ms = Util.dicomGetTimeAndDate(rtimage, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get.getTime
    val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
    elapsed_ms
  }

  private val imageName: String = {
    val gantryRounded_deg = Util.angleRoundedTo90(Util.gantryAngle(rtimage))
    val collimatorRounded_deg = Util.angleRoundedTo90(Util.collimatorAngle(rtimage))

    val gantryRounded_txt = "G" + gantryRounded_deg.formatted("%03d")
    val collimatorRounded_txt = "C" + collimatorRounded_deg.formatted("%03d")
    val elapsedTime_txt = {
      val min = elapsedTime_ms / (60 * 1000)
      val sec = (elapsedTime_ms / 1000) % 60
      min.formatted("%d") + ":" + sec.formatted("%02d")
    }

    gantryRounded_txt + " " + collimatorRounded_txt + " " + elapsedTime_txt
  }

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
    val name = {
      val n = index.formatted("%02d") + "-" + wlMsg.imageName
      FileUtil.replaceInvalidFileNameCharacters(n, '_').replaceAllLiterally(" ", "_")
    }

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
        wlMsg.error("Unable to write DICOM file as text: " + e)
    }
  }

  private def writeDicomAsBinaryDicom(): Unit = {
    try {
      val file = new File(subDir, Util.sopOfAl(rtimage) + ".dcm")
      Util.writeAttributeListToFile(rtimage, file)
    } catch {
      case e: Exception =>
        wlMsg.error("Unable to write DICOM file as binary DICOM: " + e)
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
        val wlTop    = WLEdge("top"   , vertical = false, di, rtimage,    topAOI, wlMsg)
        val wlBottom = WLEdge("bottom", vertical = false, di, rtimage, bottomAOI, wlMsg)
        val wlLeft   = WLEdge("left"  , vertical = true , di, rtimage,   leftAOI, wlMsg)
        val wlRight  = WLEdge("right" , vertical = true , di, rtimage,  rightAOI, wlMsg)
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
          wlMsg.info(s"Unexpected: ${fmtEx(t)}")
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
  private val BALL_RADIUS: Int = toPixels((wlParameters.ballDiameter_mm * 1.5) / 2.0)


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


  def process: WLImageResult = {
    wlMsg.info("Start constructing ProcessImage for " + Util.sopOfAl(rtimage))
    //noinspection RegExpRepeatedSpace,RegExpSimplifiable
    wlMsg.info("\nOutput :\n    " + extendedData.output.toString.replaceAll("   *", "\n    "))

    // ----------------------------------------------------------------------------------------

    try {
      // val uncorrectedPixels = fetchPixels()
      val preprocessImage = WLPreprocessImage(rtimage, imageName, wlMsg)
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

        val coarseAoiBounds = WLCoarseBox(new DicomImage(pixels), trans, wlMsg).locate()

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

            val ballBoundsAbs = new Rectangle(
              coarseAoiBounds.x + ballBounds.x, //
              coarseAoiBounds.y + ballBounds.y, //
              ballBounds.width,
              ballBounds.height
            )

            if (true) {
              val bAbs = preprocessImage.preprocessedImage.getSubimage(ballBoundsAbs)
              val buf = bAbs.toBufferedImage(Color.orange)
              val file = new File(subDir, "bAbs.png")
              Util.writePng(buf, file)
            }

            showBallBackgroundNoise(ballAoi, "ball_background")
            Util.writePng(toPng(ballAoi), new File(subDir, "ball_before_normalization.png"))

            showBallBackgroundNoise(normalizeArea(ballAoi), "normalized_ball_background")


            if (WLBallAreaIsFlat.ballAreaIsFlat(coarseAoi.pixelData, ballAoi, wlMsg)) {
              WLImageResult(WLImageStatus.BallMissing, directory = subDir, rtimage = rtimage, badPixelList = Seq(), marginalPixelList = Seq(), extendedData = extendedData, runReq = runReq)
            } else {
              val wlBall = WLBall(coarseAoi, ballAoi, subDir, SCALE, BALL_RADIUS, X_INCREMENT, ResolutionX, ResolutionY, wlParameters, tol, wlMsg)
              wlBall.findBallCenter() match {
                case Some(ballRelativeCenter: (Double, Double)) =>

                  val brcX = ballRelativeCenter._1
                  val brcY = ballRelativeCenter._2


                  val wlMakeImages = WLMakeImages( //
                    rtimage,
                    imageName,
                    extendedData,
                    wlParameters,
                    SCALE,
                    BALL_RADIUS,
                    ResolutionX,
                    ResolutionY,
                    subDir,
                    wlMsg)

                  val ir = wlMakeImages.makeImages( //
                    coarseAoi.pixelData,
                    edgeSet,
                    ballRelativeCenter,
                    ballArea = ballAoi,
                    coarseAoiBounds,
                    brcX,
                    brcY,
                    badPixelList = badPixels.badPixelsCorrected,
                    marginalPixelList = badPixels.marginalPixelsCorrected,
                    attributeList = rtimage,
                    runReq
                  )


                  wlMsg.info(ir.toString)
                  wlMsg.save(subDir)
                  WLgenHtml.generateHtml(extendedData, subDir, imageResult = ir, wlMsg)
                  ir
                case None =>
                  WLImageResult(WLImageStatus.BallAreaNoisy, directory = subDir, rtimage = rtimage, badPixelList = Seq(), marginalPixelList = Seq(), extendedData = extendedData, runReq = runReq)
              }
            }
        }

        wlMsg.save(subDir)
        result

      }
    } catch {
      case e: Exception =>
        val msg = "ProcessImage.process Unexpected exception: " + fmtEx(e)
        wlMsg.error(msg)
        wlMsg.info(msg)

        val imageResult = {
          WLImageResult(WLImageStatus.UnexpectedError, directory = subDir, rtimage = rtimage, badPixelList = Seq(), marginalPixelList = Seq(), extendedData = extendedData, runReq = runReq)
        }

        wlMsg.info(imageResult.toString)

        try {
          if (!new File(subDir, WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME).exists) WLgenHtml.generateHtml(extendedData, subDir, imageResult, wlMsg)
        } catch {
          case e: Exception => wlMsg.error("ProcessImage.process tried to save results of failure: " + fmtEx(e))
        }
        wlMsg.save(subDir)
        imageResult
    } finally {
      wlMsg.save(subDir)
    }
  }
}

object WLProcessImage extends Logging {

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
