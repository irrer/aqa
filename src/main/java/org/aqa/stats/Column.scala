package org.aqa.stats

import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Stats

object Column {
  private val separator = AnUtil.spaces.take(2)

  private val width = 12
  private val precision = 5
  private val shift = AnUtil.spaces.take(5)

  private val nameWidth = 30

  private def fmtName(s: String) = (s + AnUtil.spaces).take(Column.nameWidth)

  private def ft(s: String) = (s + AnUtil.spaces).take(Column.width)
  val header: String = fmtName("") + shift +
    Seq("min", "max", "mean", "range", "stdDev", "cofOfVar", "q1", "q3", "maxOtl", "otlLevel", "URL", "Slice UID", "sorted data").map(ft).mkString(Column.separator)
}

case class Column(index: Int, name: String, data: Seq[Double], beamData: BeamData) {

  import org.aqa.stats.Column._
  val min: Double = data.min
  val max: Double = data.max
  private val mean: Double = data.sum / data.size

  val range: Double = data.max - data.min
  private val stdDev: Double = ImageUtil.stdDev(data.map(_.toFloat))
  val cofOfVar: Double = stdDev / mean

  private val q1 = Stats.quartile1(data)
  private val q3 = Stats.quartile3(data)

  private val iqr = q3 - q1

  /**
    * Calculate a number indicating the the degree to which it is an
    * outlier.  The result is always positive, and larger numbers are
    * more of an outlier.
    *
    * If the passed value is within the interquartile range, then the
    * result will be zero.
    *
    * By convention, a value > 1.5 is the threshold for whether a value is an outlier.
    *
    * @param d Value to assess.
    * @return outlier-ness
    */
  def outlierLevel(d: Double): Double = {
    0 match {
      case _ if d < q1 => (q1 - d) / iqr
      case _ if d > q3 => (d - q3) / iqr
      case _           => 0
    }
  }

  val maxOutlier: Double = data.maxBy(outlierLevel)

  val maxOutlierRow: Row = {
    beamData.rowList.maxBy(row => outlierLevel(row.columnList(index).toDouble))
  }

  val maxOutlierUrl: String = maxOutlierRow.columnList(beamData.urlColumn)

  val maxOutletSopUidList: String = {
    val sopIndexList = beamData.header.columns.indices.filter(i => beamData.header.columns(i).contains("SOPInstanceUID")).filter(i => (i >= 0) && (i < maxOutlierRow.columnList.size))
    sopIndexList.map(i => maxOutlierRow.columnList(i)).mkString("  ")
  }

  val maxOutlierLevel: Double = outlierLevel(maxOutlier)

  val sorted: Seq[Double] = {
    val s = data.sorted
    if (maxOutlier > mean) s.reverse else s
  }

  if (maxOutlierLevel > 100) // TODO rm
    if (System.currentTimeMillis() < 1000000) // TODO rm
      println(s"$maxOutlier $maxOutlierLevel $sorted")

  val isAbnormal: Boolean = (range > 0.01) && (outlierLevel(maxOutlier) > 1.5)
  private def fmt(d: Double) = d.formatted(s"%$width.${precision}f")

  override def toString: String = {
    fmtName(name) +
      Seq(min, max, mean, range, stdDev, cofOfVar, q1, q3, maxOutlier, outlierLevel(maxOutlier)).map(fmt).mkString(Column.separator) +
      " " + maxOutlierUrl +
      " " + maxOutletSopUidList +
      " :: " + sorted.map(d => ("%10.5f".format(d))).mkString("  ")
  }

}
