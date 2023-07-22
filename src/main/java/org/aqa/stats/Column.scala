package org.aqa.stats

import edu.umro.ImageUtil.ImageUtil

object Column {
  private val separator = AnUtil.spaces.take(2)

  private val width = 12
  private val precision = 5
  private val shift = AnUtil.spaces.take(5)

  private val nameWidth = 30

  private def fmtName(s: String) = (s + AnUtil.spaces).take(Column.nameWidth)

  private def ft(s: String) = (s + AnUtil.spaces).take(Column.width)
  val header: String = fmtName("") + shift +
    Seq("min", "max", "mean", "range", "stdDev", "cofOfVar").map(ft).mkString(Column.separator)
}

case class Column(index: Int, name: String, data: Seq[Double]) {

  import org.aqa.stats.Column._
  val min: Double = data.min
  val max: Double = data.max
  private val mean: Double = data.sum / data.size

  val range: Double = data.max - data.min
  private val stdDev: Double = ImageUtil.stdDev(data.map(_.toFloat))
  val cofOfVar: Double = stdDev / mean

  def outNess(d: Double): Double = (d / stdDev).abs

  private def fmt(d: Double) = d.formatted(s"%$width.${precision}f")

  override def toString: String = {
    fmtName(name) + Seq(min, max, mean, range, stdDev, cofOfVar).map(fmt).mkString(Column.separator)
  }

}
