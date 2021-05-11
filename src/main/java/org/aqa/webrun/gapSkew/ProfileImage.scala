package org.aqa.webrun.gapSkew

import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Util

import java.awt.Color
import java.awt.Font
import java.awt.image.BufferedImage
import javax.vecmath.Point2i

/**
  * Create an image showing the profile of intensity over the bounding box.
  * @param profileAverages: List of average CUs across the edge.
  * @return Profile image to show user.
  */
class ProfileImage(profileAverages: Seq[Float], translator: IsoImagePlaneTranslator, isHorizontal: Boolean) {

  if (isHorizontal)
    throw new RuntimeException("ProfileImage: Not implemented for horizontally positioned collimator.")

  private val profileImageTargetWidth_pix = 700
  private val textPointSize = 14
  private val fontName = "SansSerif"
  private val font = new Font(fontName, Font.PLAIN, textPointSize)

  private def makeImage(width: Int, height: Int) = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

  private val tmpGraphics = ImageUtil.getGraphics(makeImage(1, 1))

  /**
    * Establish size of text for rendering.    Use the letter 'Q' for spacing because it
    * will render to the largest possible size, and include the possible descender.
    * @return Size of a single letter in pixels.
    */
  private val textDim: Point2i = {
    val rectangle = ImageText.getTextDimensions(tmpGraphics, text = "QQQQ\nQQQQ\nQQQQ\nQQQQ")
    val dimensions = new Point2i((rectangle.getWidth / 4).ceil.toInt, (rectangle.getHeight / 4).ceil.toInt)
    dimensions
  }

  // establish borders (aka whitespace) based on text size.
  private val borderLeft_pix = textDim.getX * 9
  private val borderRight_pix = textDim.getX * 3
  private val borderTop_pix = textDim.getY * 2
  private val borderBottom_pix = textDim.getY * 3

  // Horizontal scale of graph.  Will be a multiple of the edge thickness.
  private val horizontalRatio = (profileImageTargetWidth_pix / profileAverages.size).toInt

  /**
    * Make the image upon which the profile will be drawn.  Base the size on the size of the penumbra.
    *
    * @return A blank image.
    */
  private def makeBufferedImage(): BufferedImage = {
    val backgroundColor = Color.black

    val width_pix = horizontalRatio * profileAverages.size + borderLeft_pix + borderRight_pix

    val height_pix = (width_pix / Util.goldenRatio).round.toInt + borderTop_pix + borderBottom_pix

    val profileImage = makeImage(height_pix, width_pix)

    val graphics = ImageUtil.getGraphics(profileImage)
    graphics.setColor(backgroundColor)
    graphics.clearRect(0, 0, width_pix, height_pix)
    profileImage
  }

  val minYGraticule = (profileAverages.min / 1000).floor.toInt
  val maxYGraticule = (profileAverages.min / 1000).ceil.toInt



  val profileImage = ???

}





















