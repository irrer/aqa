package org.aqa.webrun.wl

import org.aqa.Config
import org.aqa.Util
import org.opensourcephysics.numerics.CubicSpline

import java.io.File

object WLEdgeImage {

  /**
    * Make the given edge image, drawing the cubic spline on it to visualize the gradient of the edge.  This is just
    * a debugging tool.
    */
  def makeEdgeImage(edge: WLEdge, subDir: File, scale: Int): Unit = {

    def makeSpline(sums: IndexedSeq[Float]): CubicSpline = {
      val spline = new CubicSpline(sums.indices.map(_.toDouble).toArray, WLImageUtil.normalize(sums).map(_.toDouble).toArray)
      spline
    }

    val png = WLImageUtil.toPngScaled(edge.aoi.pixelData, scale)
    val width = png.getWidth
    val height = png.getHeight

    if (edge.vertical) {
      val spline = makeSpline(edge.aoi.columnSums)
      for (x <- 0 until width) {
        val xd: Double = (x.toDouble / (width + scale)) * edge.aoi.width
        val y = (spline.evaluate(xd) * height).round.toInt
        png.setRGB(x, WLImageUtil.bound(y, 0, height - 1).toInt, Config.WLSplineColor.getRGB)
      }
    } else {
      val spline = makeSpline(edge.aoi.rowSums)
      for (x <- 0 until height) {
        val xd: Double = (x.toDouble / (height + scale)) * edge.aoi.height
        val y = (spline.evaluate(xd) * width).round.toInt
        png.setRGB(WLImageUtil.bound(y, 0, width - 1).toInt, x, Config.WLSplineColor.getRGB)
      }
    }

    Util.writePng(png, new File(subDir, s"edge_${edge.name}.png"))
  }
}
