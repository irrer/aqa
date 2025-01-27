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

  private val nameX = s"""'${name}_X'"""
  private val nameY = s"""'$name'"""

  def toXs: String = s"$nameY: $nameX"

  def toColumns: String = {
    def toRow(n: String, d: Seq[Double]): String = {
      s"""
         |          [
         |            $n, ${d.mkString(", ")}
         |          ]""".stripMargin
    }
    val text = toRow(nameX, data.map(_.x)) + ",\n            " + toRow(nameY, data.map(_.y))
    text
  }

}

case class C3AxisSpec(label: String, format: String = ".2g") {}

/**
  * Make a C3 scatter plot.
  *
  * @param dataList: Data to plot.
  *
  * @param xAxisLabel: Text label for X axis
  *
  * @param yAxisLabel: Label for Y axis.
  *
  * @param width: Optional width of chart in pixels.
  *
  * @param height: Optional height of chart in pixels.
  *
  * @param xAxisFormat: Formatting for x values.   Examples: .3 .4g.  Reference: http://bl.ocks.org/zanarmstrong/05c1e95bf7aa16c4768e
  *
  * @param showPrecision On mouseover for a point on the plot, show this many digits to the right of the decimal point.
  *
  * @param showGrid If true, show a grid on the plot.
  *
  */
class C3ScatterPlot(
    dataList: Seq[C3ScatterPlotDataSet],
    xAxisLabel: String,
    yAxisLabel: String,
    width: Option[Int] = None,
    height: Option[Int] = None,
    xAxisFormat: String = ".2g",
    yAxisFormat: String = ".2g",
    xMin: Option[Double] = None,
    xMax: Option[Double] = None,
    yMin: Option[Double] = None,
    yMax: Option[Double] = None,
    showPrecision: Int = 4,
    showGrid: Boolean = true
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

  private def minMaxToText(min: Option[Double], max: Option[Double]): String = {
    val minText = min.map(d => s"min: $d")
    val maxText = max.map(d => s"max: $d")

    val text = (minText, maxText) match {
      case (Some(lo), Some(hi)) =>
        s"$lo,\n          $hi,\n"

      case (Some(lo), _) =>
        s"         $lo,"

      case (_, Some(hi)) =>
        s"         $hi,"

      case _ => ""
    }
    text
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
         |
         |    var columns$chartIdTag = [ ${dataList.map(_.toColumns).mkString(",\n")} ];
         |
         |    var $chartIdTag = c3.generate({${C3Chart.chartSizeText(width, height)}
         |    data: {
         |        xs: {
         |          ${dataList.map(_.toXs).mkString(",\n")}
         |        },
         |        columns: columns$chartIdTag,
         |        type: 'scatter'
         |    },
         |    tooltip: {
         |      format: {
         |        value: function (value, ratio, id, index) {
         |          var xColIndex = 0;
         |          for (i = 0; i < columns$chartIdTag.length; i++) {
         |            if ((columns$chartIdTag[i][0] + "_X") == id)
         |              xColIndex = i;
         |          }
         |
         |          var yColIndex = 0;
         |          for (i = 0; i < columns$chartIdTag.length; i++) {
         |            if ((columns$chartIdTag[i][0]) == id)
         |              yColIndex = i;
         |          }
         |
         |          var yIndex = 1;
         |          for (i = 0; i < columns$chartIdTag[yColIndex].length; i++) {
         |            if (columns$chartIdTag[yColIndex][i] == value)
         |              yIndex = i;
         |          }
         |
         |          var text = columns$chartIdTag[xColIndex][yIndex].toPrecision($showPrecision) + ", " + value.toPrecision($showPrecision);
         |          return text;
         |        }
         |      }
         |    },
         |
         |    point: { // enlarge point on hover
         |      r: 4,
         |      focus : {
         |        expand: {
         |          r:6
         |        }
         |      }
         |    },
         |    bindto : '#$chartIdTag',
         |        grid: {
         |      x: {
         |        lines: [ {value: 0} ],
         |        show: $showGrid
         |      },
         |      y: {
         |        lines: [ {value: 0} ],
         |        show: $showGrid
         |      }
         |    },
         |    axis: {
         |      x: {
         |        label: '$xAxisLabel',
         |        ${minMaxToText(xMin, xMax)}
         |        tick: {
         |          format: d3.format('$xAxisFormat'),
         |          fit: false
         |        }
         |      },
         |      y: {
         |        label: '$yAxisLabel',
         |        ${minMaxToText(yMin, yMax)}
         |        tick: {
         |          format: d3.format('$yAxisFormat'),
         |          fit: false
         |        }
         |      }
         |    },
         |    color : {
         |      $pointColorText
         |    },
         |    padding: {
         |      right: 30,
         |      top: 10
         |    }
         |  });
         |
         |""".stripMargin
  }.replaceAll("\r", "")

}
