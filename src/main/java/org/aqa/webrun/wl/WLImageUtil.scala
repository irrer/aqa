package org.aqa.webrun.wl

import org.aqa.Config

import java.awt.image.BufferedImage

object WLImageUtil {

  /**
    * Given a value, make sure that it does not exceed the given bounds.  If it does, then return the bounded value.
    * @param x value in question
    * @param lo lower bound
    * @param hi upper bound
    * @return bounded value
    */
  def bound(x: Double, lo: Int, hi: Int): Double = {
    if (x < lo) lo else if (x > hi) hi else x
  }

  /**
    * Scale and offset a list so that it has a range of 0 to 1.
    * @param data Scale this.
    * @return Data scaled.
    */
  def normalize(data: IndexedSeq[Float]): IndexedSeq[Float] = {
    val min = data.min
    val range = data.max - min
    data.map(x => (x - min) / range)
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

}
