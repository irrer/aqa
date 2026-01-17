package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.ImageUtil
import org.aqa.Config
import org.aqa.PlannedRectangle
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.webrun.wl.WLProcessImage.toPngScaled
import org.aqa.webrun.ExtendedData

import java.awt.Graphics2D
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.io.File
import javax.vecmath.Point2d

case class WLMakeImages(
    rtimage: AttributeList,
    imageName: String,
    extendedData: ExtendedData,
    wlParameters: MachineWL,
    SCALE: Int,
    BALL_RADIUS: Int,
    ResolutionX: Double,
    ResolutionY: Double,
    subDir: File,
    wlMsg: WLMessage
) {

  private val EMPTY_PIXEL: Int = Config.WLTextColor.getRGB + 1

  private val annotate = new WLAnnotate(SCALE, BALL_RADIUS)

  private def fmt(d: Double): String = "%10.5f".format(d)

  private def toPng(pix: IndexedSeq[IndexedSeq[Float]]): BufferedImage = toPngScaled(pix, SCALE)

  /**
    * Generate an image of the given size with all black pixels
    */
  private def toBlackPng(pix: IndexedSeq[IndexedSeq[Float]]): BufferedImage = {
    val png = toPngScaled(pix, SCALE)
    for (y <- 0 until png.getHeight) for (x <- 0 until png.getWidth) png.setRGB(x, y, EMPTY_PIXEL)
    png
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
    * After locating the center of the box and the ball with some confidence, process the results.  The only
    * error that is expected to occur after this point is that the distance between the centers is too large.
    */
  def makeImages(
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

    // draw edges of box

    val normalGraphics = ImageUtil.getGraphics(normalPng)
    val brightGraphics = ImageUtil.getGraphics(brightPng)
    val blackGraphics = ImageUtil.getGraphics(blackPng)

    annotate.drawBallGraphics(normalGraphics, ballCenterX, ballCenterY)
    annotate.drawBallGraphics(brightGraphics, ballCenterX, ballCenterY)
    annotate.drawBallGraphics(blackGraphics, ballCenterX, ballCenterY)

    annotate.highlightWLBadPixelList(badPixelList, normalGraphics)
    annotate.highlightWLBadPixelList(badPixelList, brightGraphics)
    annotate.highlightWLBadPixelList(badPixelList, blackGraphics)

    def annotateBox(gc: Graphics2D): Unit = {
      annotate.drawBoxGraphics(gc, edgeSet.unTop, edgeSet.unBottom, edgeSet.unLeft, edgeSet.unRight)
    }

    annotateBox(normalGraphics)
    annotateBox(brightGraphics)
    annotateBox(blackGraphics)

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

    wlMsg.info(s"box  center X mm: $boxCenterScaledX")
    wlMsg.info(s"box  center Y mm: $boxCenterScaledY")

    wlMsg.info(s"ball center X mm: $ballCenterScaledX")
    wlMsg.info(s"ball center Y mm: $ballCenterScaledY")

    wlMsg.info(s"error X mm: $errorScaledX")
    wlMsg.info(s"error Y mm: $errorScaledY")
    wlMsg.info(s"error XY mm: $errorScaledXYCombined")

    val passed: WLImageStatus.ImageStatus = {
      val p = annotate.annotateImage(normalPng, normalGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = true, imageName, passLimit_mm = wlParameters.passLimit_mm)
      p
    }
    annotate.annotateImage(brightPng, brightGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = true, imageName, passLimit_mm = wlParameters.passLimit_mm)
    annotate.annotateImage(blackPng, blackGraphics, errorScaledX, errorScaledY, errorScaledXYCombined, background = false, imageName, passLimit_mm = wlParameters.passLimit_mm)

    val pixelData = constructPixelData(blackPng, coarseAoi)

    Util.writePng(normalPng, new File(subDir, WLgenHtml.NORMAL_SUMMARY_FILE_NAME))
    Util.writePng(brightPng, new File(subDir, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME))
    wlMsg.info("Done constructing ProcessImage for " + imageName)

    val boxPoint = new Point2d(boxCenterScaledX, boxCenterScaledY)
    val ballPoint = new Point2d(ballCenterScaledX, ballCenterScaledY)
    val edgesScaled = new Edges(
      edgeSet.top.pos_pix * ResolutionY,
      edgeSet.bottom.pos_pix * ResolutionY,
      edgeSet.left.pos_pix * ResolutionX,
      edgeSet.right.pos_pix * ResolutionX
    )

    wlMsg.info("X Offset mm " + fmt(errorScaledX))
    wlMsg.info("Y Offset mm " + fmt(errorScaledY))
    wlMsg.info("R mm " + fmt(errorScaledXYCombined))

    if (runReq.rtplan.isDefined) {
      val expected = PlannedRectangle(runReq.rtplan.get, rtimage)
      wlMsg.info("expected edges: " + expected)
    }

    val imageResult = WLImageResult(
      imageStatus = passed,
      boxRelativeToBounds_mm = Some(boxPoint),
      ballRelativeToBounds_mm = Some(ballPoint),
      edgesUnscaled = None,
      boxEdgesP = Some(edgesScaled),
      edgeSet = Some(edgeSet),
      directory = subDir,
      rtimage = attributeList,
      pixels = Some(pixelData),
      coarseAoiBounds_pix = Some(coarseAoiBounds),
      Some(brcX),
      Some(brcY),
      badPixelList = badPixelList,
      marginalPixelList = marginalPixelList,
      extendedData = extendedData,
      runReq
    )

    wlMsg.info("Image processing Results:\n" + imageResult.toString)

    val wl = imageResult.toWinstonLutz
    wlMsg.info(s"Box  X center iso mm: ${wl.boxCenterX_mm}")
    wlMsg.info(s"Box  Y center iso mm: ${wl.boxCenterY_mm}")
    wlMsg.info(s"Ball X center iso mm: ${wl.ballCenterX_mm}")
    wlMsg.info(s"Ball Y center iso mm: ${wl.ballCenterY_mm}")
    wlMsg.info(s"Box top       iso mm: ${wl.topEdge_mm}")
    wlMsg.info(s"Box bottom    iso mm: ${wl.bottomEdge_mm}")
    wlMsg.info(s"Box left      iso mm: ${wl.leftEdge_mm}")
    wlMsg.info(s"Box right     iso mm: ${wl.rightEdge_mm}}")

    imageResult
  }

}
