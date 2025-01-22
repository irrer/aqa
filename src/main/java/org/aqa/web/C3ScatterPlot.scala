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

import scala.xml.Elem

case class C3ScatterPlotDataPoint(x: Double, y: Double) {}

case class C3ScatterPlotDataSet(name: String, data: Seq[C3ScatterPlotDataPoint], color: Option[String] = None) {

  private val nameX = s"""'${name}X'"""
  private val nameY = s"""'$name'"""

  def toXs: String = s"$nameY: $nameX"

  def toColumns: String = {
    def toRow(n: String, d: Seq[Double]): String = {
      s"[$n, ${d.mkString(", ")} ]"
    }
    val text = toRow(nameX, data.map(_.x)) + ",          \n" + toRow(nameY, data.map(_.x))
    text
  }

}

/**
  * Make a C3 scatter plot.
  *
  * @param width: Optional width of chart in pixels.
  *
  * @param height: Optional height of chart in pixels.
  *
  * @param xAxisLabel: Text label for X axis
  *
  * @param xDataLabel: Label for Y axis.
  *
  * @param dataList: Data to plot.
  *
  * @param xFormat: Formatting for x values.   Examples: .3 .4g.  Reference: http://bl.ocks.org/zanarmstrong/05c1e95bf7aa16c4768e
  *
  * @param yDataLabel Axis label, one per set of Y values.
  *
  * @param pointFormat Numeric format of point shown on mouseover.
  *
  * Format: .3g .4g
  */
class C3ScatterPlot(
    width: Option[Int] = None,
    height: Option[Int] = None,
    xAxisLabel: String,
    xDataLabel: String,
    dataList: Seq[C3ScatterPlotDataSet],
    xFormat: String = ".4g",
    yDataLabel: String,
    pointFormat: String = ".4g"
) extends Logging {

  private val chartIdTag = C3Chart.makeUniqueChartIdTag

  private val defaultColorList = Seq("green", "blue", "black", "red", "orange", "cyan", "magenta", "yellow", "pink", "gray")

  /**
    * List of colors to use. If a data set does not specify a color, then use one of the defaults.
    */
  private val pointColorText = {
    def toColor(index: Int): String = {

      val color = dataList(index).color
      val colorName =
        if (color.isDefined)
          color.get
        else
          defaultColorList(index % defaultColorList.size)

      s"'$colorName'"
    }

    dataList.indices.map(toColor)

    val list = (dataList.flatMap(_.color) :+ "#4472C4").map(c => s"'$c'").mkString(", ")
    s"pattern: [ $list ]"
  }

  /** Embed this in the HTML where the chart is to appear. */
  val html: Elem = {
    <div id={chartIdTag}>
      {chartIdTag}
    </div>
  }

  /** Use this as a run script. */
  val javascript: String = {
    s"""
         |    var $chartIdTag = c3.generate({${C3Chart.chartSizeText(width, height)}
         |    data: {
         |        xs: {
         |          ${dataList.map(_.toXs).mkString(",\n")}
         |        },
         |        columns: [
         |          ${dataList.map(_.toColumns).mkString(",\n")}
         |        ],
         |        type: 'scatter'
         |    },
         |    point: { // enlarge point on hover
         |        r: 4,
         |        focus : {
         |            expand: {
         |                r:6
         |            }
         |        }
         |    },
         |    bindto : '#$chartIdTag',
         |    axis: {
         |        x: {
         |            label: '$xDataLabel',
         |            tick: {
         |                format: d3.format('$xFormat')
         |            }
         |        },
         |        y: {
         |            label: '$yDataLabel',
         |            tick: {
         |                format: d3.format('$pointFormat')
         |            }
         |        }
         |    },
         |    color : {
         |        $pointColorText
         |    },
         |    padding: {
         |      right: 30,
         |      top: 10
         |    }
         |  });
         |
         |""".stripMargin
  }

}
