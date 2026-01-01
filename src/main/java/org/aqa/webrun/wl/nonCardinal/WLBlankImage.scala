package org.aqa.webrun.wl.nonCardinal

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil

import java.awt.image.BufferedImage
import java.awt.Color
import javax.vecmath.Point2d

/**
  * Utility for making a blank image with variable brightness.  The brightness varies such
  * that the ball is clearly visible and the edges have the expected contrast.
  */
object WLBlankImage {

  private case class MinMax(min: Float, max: Float) {}

  /**
    * Find the min and max values in the ball area.  This is used for setting the brightness of rendered images, so high
    * precision is not required.
    *
    * @return The approximate min and max pixel values.
    */
  private def calculateMinAndMaxMaxBall(preprocessedImage: DicomImage, edgeSet: WLNonCardEdgeSet): MinMax = {
    val valueList = for (x <- 0 until preprocessedImage.width; y <- 0 until preprocessedImage.height; if edgeSet.pointIsInBallAoi(new Point2d(x, y))) yield preprocessedImage.get(x, y)
    val sorted = valueList.sorted
    val min = sorted.slice(5, 15).sum / 10
    val max = sorted.dropRight(5).takeRight(10).sum / 10
    MinMax(min, max)
  }

  /**
    * Find the min and max pixel values in the whole image.
    * @param preprocessedImage For this image.
    * @return Min and max pixel values.
    */
  private def calculateMinAndMaxMaxWholeImage(preprocessedImage: DicomImage): MinMax = {
    val sortedPixelValueList = preprocessedImage.pixelData.flatten.sorted

    val minImageValue = sortedPixelValueList.slice(5, 15).sum / 10
    val maxImageValue = sortedPixelValueList.dropRight(5).takeRight(10).sum / 10

    MinMax(minImageValue, maxImageValue)
  }

  /**
    * Make an image with no annotations, with the ball area brightened to better show the
    * ball, and the edge area set to its original brightness.
    *
    * @param preprocessedImage Create from this image.
    * @param edgeSet Needed to determine ball area.
    * @return A new image with user-friendly brightness but no annotation or magnification.
    */
  def make(preprocessedImage: DicomImage, edgeSet: WLNonCardEdgeSet): BufferedImage = {

    val minMaxWhole = calculateMinAndMaxMaxWholeImage(preprocessedImage)

    val minMaxBall = calculateMinAndMaxMaxBall(preprocessedImage = preprocessedImage, edgeSet = edgeSet)

    val wholeImage = preprocessedImage.toBufferedImage(ImageUtil.rgbColorMap(Color.blue), minMaxWhole.min, minMaxWhole.max)

    val ballImage = preprocessedImage.toBufferedImage(ImageUtil.rgbColorMap(Color.blue), minMaxBall.min, minMaxBall.max)

    for (x <- 0 until preprocessedImage.width) {
      for (y <- 0 until preprocessedImage.height) {
        if (edgeSet.pointIsInBallAoi(new Point2d(x, y))) {
          wholeImage.setRGB(x, y, ballImage.getRGB(x, y))
        }
      }
    }

    wholeImage

  }
}
