package learn

import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.vecmath.Point2i

/**
  * View image characteristics.  Review coordinate system.
  */
object Orbit {
  def main(args: Array[String]): Unit = {
    val inFile = new File("""D:/tmp/isocheckBBPath/BB.png""")

    // val inImage = ImageUtil.lightenImage(ImageIO.read(inFile), 50.0)
    val inImage = ImageIO.read(inFile)
    val pix0 = inImage.getRGB(0, 0)
    val pixList = {
      for (x <- 0 until inImage.getWidth; y <- 0 until inImage.getHeight(); if (inImage.getRGB(x, y) != pix0)) yield new Point2i(x, y)
    }

    val outFile = new File("""D:/tmp/isocheckBBPath/Orbit.png""")
    outFile.delete()

    val width = 1000
    val height = width

    val outImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for (x <- 0 until width; y <- 0 until height) yield outImage.setRGB(x, y, 0xffffff)

    def putImage(index: Int): Unit = {
      val baseX = index + 100
      val baseY = (outImage.getHeight - inImage.getHeight()) / 2

      pixList.foreach(point => {
        val rgb = inImage.getRGB(point.getX, point.getY)
        outImage.setRGB(baseX + point.getX, baseY + point.getY, rgb)
      })

    }

    val outCenter = new Point2i(outImage.getWidth / 2, outImage.getHeight / 2)

    def putRevolution(deg: Int): Unit = {

      val rad = Math.toRadians(-((deg + 180) % 360))

      val sin = Math.sin(rad)
      val cos = Math.cos(rad)

      val baseX = (sin * 100).round.toInt + outCenter.getX
      val baseY = (cos * 100).round.toInt + outCenter.getY

      val brightness = {
        val a = -50.0 / 270
        val b = 50

        val br = (deg * a) + b
        Trace.trace(br)
        br
      }

      val img = ImageUtil.lightenImage(ImageIO.read(inFile), brightness)

      pixList.foreach(point => {
        val rgb = img.getRGB(point.getX, point.getY)
        outImage.setRGB(baseX + point.getX, baseY + point.getY, rgb)
      })

    }

    if (false) (200 until 500 by 40).foreach(putImage)

    (0 to 275 by 20).foreach(putRevolution)

    Util.writePng(outImage, outFile)
    println("Wrote " + outFile.getAbsolutePath)
  }

}
