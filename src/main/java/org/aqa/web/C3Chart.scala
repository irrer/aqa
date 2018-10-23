package org.aqa.web

import org.aqa.Logging
import java.awt.Color

object C3Chart {

  private var id = 0

  /**
   * Get an id that guaranteed to be unique for a given web page.
   */
  def getId: Int = id.synchronized({
    id = id + 1
    id
  })

  val scriptPrefix = """\n<script>\n"""
  val scriptSuffix = """\n</script>\n"""

  /**
   * Make the C3 chart snippet to set the chart width and height if specified.
   */
  def chartSizeText(width: Option[Int], height: Option[Int]): String = {
    val w = if (width.isDefined) Some("      width: " + width.get) else None
    val h = if (height.isDefined) Some("      height: " + height.get) else None

    val wh = Seq(w, h).flatten
    if (wh.nonEmpty) {
      "\n    size: {" + wh.mkString("\n      ", ",\n      ", "\n    },")
    } else ""
  }

  /**
   * Make the C3 chart snippet to set the min and max bounds.
   */
  def rangeText(min: Double, max: Double): String = {
    val range = max - min
    if (range == 0) ""
    else {
      val margin = range * 0.05 // put 5 % space on top and bottom
      val lo = min - margin
      val hi = max + margin
      "min: " + (min - margin) + ",\n" +
        "         max: " + (max + margin) + ",\n"
    }
  }

  class Tolerance(a: Double, b: Double) {
    val min = Math.min(a, b)
    val max = Math.max(a, b)
  }

}

/**
 * Make a C3 chart.
 *
 * @param width: Optional width of chart in pixels.
 *
 * @param height: Optional height of chart in pixels.
 *
 * @param xAxisLabel: Text label for X axis
 *
 * @param xValueList: List of X values
 *
 * @param xFormat: Formatting for x values.   Examples: .3 .4g.  Reference: http://bl.ocks.org/zanarmstrong/05c1e95bf7aa16c4768e
 *
 * @param yAxisLabels
 *
 * @param yValues
 *
 * @param yFormat
 *
 * Format: .3g .4g
 */
class C3Chart(
  width: Option[Int],
  height: Option[Int],
  xAxisLabel: String, xDataLabel: String, xValueList: Seq[Double], xFormat: String,
  yAxisLabels: Seq[String], yDataLabel: String, yValues: Seq[Seq[Double]], yFormat: String, yColorList: Seq[Color]) extends Logging {

  if (yAxisLabels.size != yValues.size) throw new RuntimeException("Must be same number of Y labels as Y data sets.  yAxisLabels.size: " + yAxisLabels.size + "    yValues.size: " + yValues.size)

  private val allY = yValues.flatten
  private val minY = allY.min
  private val maxY = allY.max

  val idTag = "ChartId_" + C3Chart.getId

  private def column(label: String, valueList: Seq[Double]): String = {
    "[ '" + label + "', " + valueList.mkString(", ") + "]"
  }

  val html = { <div id={ idTag }>filler</div> }

  val javascript = {
    """      
var """ + idTag + """ = c3.generate({""" + C3Chart.chartSizeText(width, height) + """
    data: {
        x: '""" + xAxisLabel + """',
        columns: [
          """ + column(xAxisLabel, xValueList) + """,
          """ + yAxisLabels.indices.map(i => column(yAxisLabels(i), yValues(i))).mkString(",\n         ") + """ 
        ]
    },
    point: { // enlarge point on hover
        r: 0,
        focus : {
            expand: {
                r:4
            }
        }
    },
    bindto : '#""" + idTag + """',
    axis: {
        x: {
            label: '""" + xDataLabel + """',
            tick: {
                format: d3.format('""" + xFormat + """')
            }
        },
        y: {
            """ + C3Chart.rangeText(minY, maxY) + """
            label: '""" + yDataLabel + """',
            tick: {
                format: d3.format('""" + yFormat + """')
            }
        }
    },
    color : {
        pattern : [ '#6688bb' ]
    },
    padding: {
      right: 30,
      top: 10
    }
  });
""";
  }

}