/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.web

import org.aqa.Logging
import org.aqa.Util

import java.awt.Color
import scala.xml.Elem

object C3Chart {

  private var id = 0

  private val idLock = "lock"

  val idTagPrefix = "ChartId_"

  def textToChartId(text: String): String = {
    idTagPrefix + Util.textToId(text)
  }

  /**
    * Get an id that guaranteed to be unique for a given web page.
    */
  def makeUniqueChartIdTag: String =
    idLock.synchronized({
      id = id + 1
      textToChartId(id.toString)
    })

  val scriptPrefix = """\n<script>\n"""
  val scriptSuffix = """\n</script>\n"""

  def html(idTag: String): Elem = {
    <div id={idTag}>{idTag}</div>
  }

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
      "min: " + (min - margin) + ",\n" +
        "         max: " + (max + margin) + ",\n"
    }
  }

  class Tolerance(a: Double, b: Double) {
    val min: Double = Math.min(a, b)
    val max: Double = Math.max(a, b)
    override def toString: String = "min: " + min + "    max: " + max
  }

  class YRange(a: Double, b: Double) {
    val min: Double = Math.min(a, b)
    val max: Double = Math.max(a, b)
    override def toString: String = "min: " + min + "    max: " + max
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
  * @param yAxisLabels Axis label, one per set of Y values.
  *
  * @param yValues Y values to be plotted.
  *
  * @param yFormat Numeric format of Y values shown on mouseover.
  *
  * Format: .3g .4g
  */
class C3Chart(
    width: Option[Int] = None,
    height: Option[Int] = None,
    xAxisLabel: String,
    xDataLabel: String,
    xValueList: Seq[Double],
    xFormat: String = ".4g",
    yAxisLabels: Seq[String],
    yDataLabel: String,
    yValues: Seq[Seq[Double]],
    yFormat: String = ".4g",
    yColorList: Seq[Color] = Seq()
) extends Logging {

  if (yAxisLabels.size != yValues.size) throw new RuntimeException("Must be same number of Y labels as Y data sets.  yAxisLabels.size: " + yAxisLabels.size + "    yValues.size: " + yValues.size)

  private val allY = yValues.flatten
  private val minY = allY.min
  private val maxY = allY.max

  private val chartIdTag = C3Chart.makeUniqueChartIdTag

  private val yColorText = {
    val list =
      if (yColorList.isEmpty)
        Seq(Color.decode("#6688bb"))
      else
        yColorList

    def colorToText(c: Color): String = {
      def fmt(i: Int) = i.formatted("%02x")

      "'#" + fmt(c.getRed) + fmt(c.getGreen) + fmt(c.getBlue) + "'"
    }

    "pattern: [ " + list.map(colorToText).mkString(", ") + "]"
  }

  private def column(label: String, valueList: Seq[Double]): String = {
    "[ '" + label + "', " + valueList.mkString(", ") + "]"
  }

  val html: Elem = {
    <div id={chartIdTag}>
      {chartIdTag}
    </div>
  }

  val javascript: String = {
    s"""
var $chartIdTag = c3.generate({${C3Chart.chartSizeText(width, height)}
    data: {
        x: '$xAxisLabel',
        columns: [
          ${column(xAxisLabel, xValueList)},
          ${yAxisLabels.indices.map(i => column(yAxisLabels(i), yValues(i))).mkString(",\n         ")}
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
    bindto : '#$chartIdTag',
    axis: {
        x: {
            label: '$xDataLabel',
            tick: {
                format: d3.format('$xFormat')
            }
        },
        y: {
            ${C3Chart.rangeText(minY, maxY)}
            label: '$yDataLabel',
            tick: {
                format: d3.format('$yFormat')
            }
        }
    },
    color : {
        $yColorText
    },
    padding: {
      right: 30,
      top: 10
    }
  });
""".stripMargin.replace('\r', ' ')
  }

}
