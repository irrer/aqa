package org.aqa.webrun.wl

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.awt.Color
import java.awt.Rectangle
import java.io.File
import javax.vecmath.Point2i

case class WLBallValidation(ballAoi: DicomImage, ball: DicomImage, ballBounds: Rectangle, subDir: File) {

  private def outsideBall(point: Point2i): Boolean = {
    (point.x < ballBounds.x) ||
    (point.y < ballBounds.y) ||
    (point.x > (ballBounds.x + ballBounds.width)) ||
    (point.y > (ballBounds.y + ballBounds.height))
  }

  private val allCoordinates = for (x <- 0 until ballAoi.width; y <- 0 until ballAoi.height) yield new Point2i(x, y)

  private val nonBallPixels = allCoordinates.filter(outsideBall).map(p => ballAoi.get(p.x, p.y))

  if (true) {
    val ballImg = ballAoi.toBufferedImage(Color.green)
    val fileWith = new File(subDir, "ballAoiWithBall.png")
    Util.writePng(ballImg, fileWith)

    allCoordinates.filterNot(outsideBall).foreach(p => ballImg.setRGB(p.x, p.y, 0xff0000))

    val file = new File(subDir, "ballAoiMinusBall.png")
    Util.writePng(ballImg, file)

    if (false) {
      val bi = ball.toBufferedImage(Color.magenta) // TODO throws exception
      val fileBi = new File(subDir, "ballOnly.png")
      Util.writePng(bi, fileBi)
    }
  }

  private val backgroundStdDev = ImageUtil.stdDev(nonBallPixels)

  private val ballStdDev = ImageUtil.stdDev(ball.pixelData.flatten)

  private val backgroundMean = nonBallPixels.sum / nonBallPixels.size

  private val ballMean = ball.sum / (ball.width * ball.height)

  Trace.trace(
    s"\nCompare background to ball:\n" +
      s"\nballStdDev: $ballStdDev" +
      s"\nbackgroundStdDev: $backgroundStdDev" +
      s"\nballMean: $ballMean" +
      s"\nbackgroundMean: $backgroundMean" +
      s"\nballStdDev / backgroundStdDev: ${ballStdDev / backgroundStdDev}" +
      s"\nballMean / backgroundMean: ${ballMean / backgroundMean}"
  )

  def validate: Option[WLImageStatus.Value] = None

}
