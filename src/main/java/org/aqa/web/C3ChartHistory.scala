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

import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Baseline
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord

import java.awt.Color
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

/**
  * @param maintenanceList   : List of maintenance records that fall chronologically in the displayed range
  * @param width       : Optional width of chart in pixels.
  * @param height      : Optional height of chart in pixels.
  * @param xLabel      : Text label for X axis
  * @param xDateList   : List of dates
  * @param baseline    : Optional baseline value.  If given, a horizontal line
  * @param tolerance   : If given, show horizontal tolerance (see AQA.css .c3-ygrid-line.tolerance)
  *                    lines at the given values.
  * @param yRange      : If given, scale the chart so that it shows values within this range.  If values
  *                    are out of range, they will be literally 'off the chart'.
  * @param yAxisLabels : Labels for each individual Y value
  * @param yValues     : Values to be charted.
  * @param yIndex      : Index of the Y values that is new.
  * @param yFormat     : Formatting for y values.   Examples: .3g .4g  Reference: http://bl.ocks.org/zanarmstrong/05c1e95bf7aa16c4768e
  */
class C3ChartHistory(
    chartIdOpt: Option[String],
    maintenanceList: Seq[MaintenanceRecord],
    width: Option[Int],
    height: Option[Int],
    xLabel: String,
    xDateList: Seq[Date],
    baseline: Option[Baseline],
    tolerance: Option[C3Chart.Tolerance],
    yRange: Option[C3Chart.YRange],
    yAxisLabels: Seq[String],
    yDataLabel: String,
    yValues: Seq[Seq[Double]],
    yIndex: Int,
    yFormat: String,
    yColorList: Seq[Color]
) extends Logging {

  if (yAxisLabels.size != yValues.size) throw new RuntimeException("Must be same number of Y labels as Y data sets.  yAxisLabels.size: " + yAxisLabels.size + "    yValues.size: " + yValues.size)

  private val yMaxSize = yValues.map(yy => yy.size).max
  if (yMaxSize > xDateList.size) throw new RuntimeException("Size of at least one of the yValues is " + yMaxSize + " and is greater than size of xDateList " + xDateList.size)

  private val chartIdTag: String = {
    if (chartIdOpt.isDefined) chartIdOpt.get else C3Chart.makeUniqueChartIdTag
  }

  private def column(label: String, valueList: Seq[Double]): String = {
    if (valueList.isEmpty)
      "[ '" + label + "' ]"
    else
      "[ '" + label + "', " + valueList.mkString(", ") + "]"
  }

  private def textColumn(valueList: Seq[String]): String = {
    valueList.map(t => "'" + t + "'").mkString("[ ", ", ", "]")
  }

  private def dateColumn(label: String, valueList: Seq[Date]): String = {
    if (valueList.isEmpty)
      "[ '" + label + "' ]"
    else
      "[ '" + label + "', " + valueList.map(d => "'" + Util.standardDateFormat.format(d) + "'").mkString(", ") + "]"
  }

  private val yColorNameList = textColumn(yColorList.map(c => (c.getRGB & 0xffffff).formatted("#%06x")))

  private val minDate = xDateList.minBy(d => d.getTime)
  private val maxDate = xDateList.maxBy(d => d.getTime)

  private val maintenanceDateList = dateColumn("maintenanceDateList", maintenanceList.flatMap(maintenance => Seq(maintenance.creationTime, maintenance.creationTime)))

  val all: Seq[Double] = yValues.flatten

  val minY: Double = all.min
  val maxY: Double = all.max

  val yRangeY: C3Chart.YRange = {
    val minYt = if (tolerance.isDefined) Math.min(minY, tolerance.get.min) else minY
    val maxYt = if (tolerance.isDefined) Math.max(maxY, tolerance.get.max) else maxY
    (tolerance.isDefined, yRange.isDefined) match {
      case (_, false)    => new C3Chart.YRange(minYt, maxYt)
      case (false, true) => new C3Chart.YRange(Math.max(minYt, yRange.get.min), Math.min(maxYt, yRange.get.max))
      case (true, true) =>
        val lo = if (minYt < yRange.get.min) yRange.get.min else minYt
        val hi = if (maxYt > yRange.get.max) yRange.get.max else maxYt
        new C3Chart.YRange(lo, hi)
    }
  }

  private val maintenanceValueList = column("MaintenanceRecord", Seq.fill(maintenanceList.size)(Seq(yRangeY.min, yRangeY.max)).flatten)
  private val maintenanceSummaryList = textColumn(maintenanceList.map(maintenance => maintenance.summary))
  private val maintenanceColorList = textColumn(
    maintenanceList.flatMap(maintenance => {
      val c = MaintenanceCategory.findMaintenanceCategoryMatch(maintenance.category).Color
      Seq(c, c)
    })
  )
  private val maintenanceCategoryList = textColumn(maintenanceList.map(maintenance => maintenance.category))

  val html: Elem = C3ChartHistory.htmlRef(chartIdTag)

  Trace.trace(html) // TODO rm

  private val yLabels = {
    val yOnly = yAxisLabels.map(y => "'" + y + "' : '" + xLabel + "',")
    yOnly.mkString("\n        ")
  }

  private val yText = {
    val yData = yAxisLabels.indices.map(i => column(yAxisLabels(i), yValues(i)))
    yData.mkString(",\n          ")
  }

  private def gridLine(clss: String, value: Double, title: String): String = {
    "{ position: 'start', class: '" + clss + "', value:  " + value.toString + ", text: '  \\032 \\032 \\032 \\032 \\032 \\032  " + title + " " + Util.fmtDbl(value) + "' }"
  }

  private val grid = {

    val gridTolerance: Seq[String] = {
      if (tolerance.isDefined) {
        Seq(gridLine("tolerance", tolerance.get.max, "Maximum Tolerance"), gridLine("tolerance", tolerance.get.min, "Minimum Tolerance"))
      } else Seq[String]()
    }

    val gridBaseline: Seq[String] = {
      if (baseline.isDefined) {
        val fmt = new SimpleDateFormat("YYYY MMM d hh:mm")
        Seq(gridLine("baseline", baseline.get.value.toDouble, "Baseline " + fmt.format(baseline.get.acquisitionDate)))
      } else Seq[String]()
    }

    val allGrid = gridBaseline ++ gridTolerance

    if (allGrid.isEmpty) ""
    else {

      """,
    grid: {
        y: {
            lines: [
                """ + allGrid.mkString(",\n                ") +
        """
            ]
        }
    }"""
    }
  }

  val javascript: String = {
    s"""
var $chartIdTag = c3.generate({${C3Chart.chartSizeText(width, height)}
    tooltip: {
      format: {
        value: function (value, ratio, id, index) {
          var maintenanceSummaryList = $maintenanceSummaryList;
          if (id === 'MaintenanceRecord') return maintenanceSummaryList[index/2];
          return d3.format('$yFormat')(value);
          },
          name: function (name, ratio, id, index) {
            var maintenanceCategoryList = $maintenanceCategoryList;
              if (id === 'MaintenanceRecord') return maintenanceCategoryList[index/2];
              return id;
          },
          title: function (value) {
              return formatDate(value) + " &nbsp; " + formatTime(value);
          }
      }
    },
    data: {
      xs: {
        $yLabels
        'MaintenanceRecord': 'maintenanceDateList'
        },
        xFormat: standardDateFormat,
        columns: [
          ${dateColumn(xLabel, xDateList)},
          $yText,
          $maintenanceDateList,
          $maintenanceValueList
        ],
        types: {
          ${yAxisLabels.map(label => "'" + label + "' : 'line'").mkString(",\n          ")},
          'MaintenanceRecord': 'bar'
        },
        color: function (color, d) {
          var maintenanceColorList = $maintenanceColorList;
          if (d.id === 'MaintenanceRecord') return maintenanceColorList[d.index % maintenanceColorList.length];
          if (d.index === $yIndex) return 'orange';
          return color;
        }
    }$grid,
    point: {
        r: 2,
        focus : {
            expand: {
                r:4
            }
        }
    },
    bindto : '#$chartIdTag',
    axis: {
       x: {
         label: 'Date',
         type: 'timeseries',
         min: '${Util.standardDateFormat.format(minDate)}',
         max: '${Util.standardDateFormat.format(maxDate)}',
         tick: { format: function(dt) { return [ formatDate(dt) , formatTime(dt) ]; } }
       },
       y: {
         ${C3Chart.rangeText(yRangeY.min, yRangeY.max)}         label: '$yDataLabel',
         tick: {
           format: d3.format('$yFormat')
         }
       }
    },
    bar: {
      width: 3
    },
    padding: {
      right: 30,
      top: 10
    },
    color : {
      pattern : $yColorNameList
    }
  });

""" + // js to set up scroll bars
      s"""
         |var @@count = ${C3ChartHistory.initialSliderCountValue};
         |var @@start = ${C3ChartHistory.initialSliderHistoryValue};
         |
         |var @@dateList = getHistoryChartDateList(@@);
         |
         |var @@slider = document.getElementById("@@Slider");
         |
         |if (@@slider != null) { // backwards compatible with history graphs that do not have sliders.
         |  @@slider.oninput = function() {
         |    @@start = parseInt(this.value);
         |    moveHistoryChart(@@, @@dateList, @@start, @@count);
         |  }
         |
         |  var @@sliderCount = document.getElementById("@@SliderCount");
         |  @@sliderCount.oninput = function() {
         |    @@count = parseInt(this.value);
         |    moveHistoryChart(@@, @@dateList, @@start, @@count);
         |  }
         |
         |  moveHistoryChart(@@, @@dateList, @@start, @@count);
         |}
         |""".stripMargin.replace("@@", chartIdTag)
  }

}
object C3ChartHistory {

  // initial setting for history slider.  value is in percent
  private val initialSliderHistoryValue = 100
  // initial setting for count slider.  value is in percent
  private val initialSliderCountValue = 51

  def htmlRef(chartIdTag: String): Elem = {
    <div>
      <div id={chartIdTag}>
        {chartIdTag}
      </div>
      <div class="row">
        <div class="col-md-10" title={"Slide all the way to the left to show the" + WebUtil.titleNewline + " oldest, to the right for newest."}>
          <label for={chartIdTag + "Slider"}>Show older or newer entries</label>
          <input type="range" min="0" max="100" value={initialSliderHistoryValue.toString} id={chartIdTag + "Slider"}/>
        </div>
        <div class="col-md-2" style="visibility: visible;" title={"Slide to the left to show just a few, slide all" + WebUtil.titleNewline + " the way to the right to show all of them."}>
          <label for={chartIdTag + "SliderCount"}>Show fewer or more entries</label>
          <input type="range" min="0" max="100" value={initialSliderCountValue.toString} id={chartIdTag + "SliderCount"}/>
        </div>
      </div>
    </div>
  }
}
