package org.aqa

import edu.umro.ImageUtil.Watermark
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

/**
 * Mark images with a watermark that show patient orientation.
 */
object OrientationWatermark extends Enumeration with Logging {

  val Frontal = Value
  val Posterior = Value
  val SagittalLeft = Value
  val SagittalRight = Value
  val Transversal = Value

  // percent width
  private val percent = Map(
    (Frontal, 10.0),
    (Posterior, 10.0),
    (SagittalLeft, 5.0),
    (SagittalRight, 5.0),
    (Transversal, 10.0))

  /** Internal buffer to reuse watermarks to improve performance. */
  private val watermarkList = scala.collection.mutable.Map[OrientationWatermark.Value, Watermark]()

  private def add(ow: OrientationWatermark.Value): Unit = {
    val fileName = "Human" + ow.toString + ".png"
    val file = new File(Config.imageDirFile, fileName)
    try {
      if (file.canRead) {
        val legend = ImageIO.read(file)
        val wm = new Watermark(legend, false, true, percent(ow), 50.0)
        watermarkList.put(ow, wm)
      }
    } catch {
      case t: Throwable => logger.warn("Unexpected error with orientation watermark file " + file.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  this.values.map(ow => add(ow))

  /**
   * Apply a watermark to the given image.
   */
  def mark(ow: OrientationWatermark.Value, image: BufferedImage): Unit = if (watermarkList.contains(ow)) watermarkList(ow).mark(image)

}