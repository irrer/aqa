package org.aqa.web

import org.aqa.Logging
import java.util.Date
import org.aqa.Util
import org.aqa.db.MaintenanceRecord
import org.aqa.db.MaintenanceCategory
import java.awt.Color
import org.aqa.db.Baseline
import java.text.SimpleDateFormat
import edu.umro.ScalaUtil.Trace

/**
 * @param maintList: List of maintenance records that fall chronologically in the displayed range
 *
 * @param width: Optional width of chart in pixels.
 *
 * @param height: Optional height of chart in pixels.
 *
 * @param xAxisLabel: Text label for X axis
 *
 * @param xValueList: List of X values
 *
 * @param baselineSpec: Optional baseline value.  If given, a horizontal line
 *
 * @param tolerance: If given, show horizontal tolerance (see AQA.css .c3-ygrid-line.tolerance) lines at the given values.
 *
 * @param yAxisLabels: Labels for each individual Y value
 *
 * @param yValues: Values to be charted.
 *
 * @param yIndex: Index of the Y values that is new.
 *
 * @param yFormat: Formatting for y values.   Examples: .3g .4g  Reference: http://bl.ocks.org/zanarmstrong/05c1e95bf7aa16c4768e
 */
class C3ChartHistory(
  chartIdOpt: Option[String],
  maintList: Seq[MaintenanceRecord],
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
  yColorList: Seq[Color]) extends Logging {

  if (yAxisLabels.size != yValues.size) throw new RuntimeException("Must be same number of Y labels as Y data sets.  yAxisLabels.size: " + yAxisLabels.size + "    yValues.size: " + yValues.size)

  private val yMaxSize = yValues.map(yy => yy.size).max
  if (yMaxSize > xDateList.size) throw new RuntimeException("Size of at least one of the yValues is " + yMaxSize + " and is greater than size of xDateList " + xDateList.size)

  private val chartIdTag: String = {
    if (chartIdOpt.isDefined) chartIdOpt.get else C3Chart.makeUniqueChartIdTag
  }

  private def column(label: String, valueList: Seq[Double]): String = {
    "[ '" + label + "', " + valueList.mkString(", ") + "]"
  }

  private def textColumn(valueList: Seq[String]): String = valueList.map(t => "'" + t + "'").mkString("[ ", ", ", "]")

  private def dateColumn(label: String, valueList: Seq[Date]): String = {
    "[ '" + label + "', " + valueList.map(d => "'" + Util.standardDateFormat.format(d) + "'").mkString(", ") + "]"
  }

  private val maintColor = if (maintList.isEmpty) Seq[Color]() else Seq(Color.white)
  private val yColorNameList = textColumn((yColorList ++ maintColor).map(c => (c.getRGB & 0xffffff).formatted("#%06x")))

  private val minDate = xDateList.minBy(d => d.getTime)
  private val maxDate = xDateList.maxBy(d => d.getTime)

  private val maintDateList = dateColumn("maintDateList", maintList.map(maint => Seq(maint.creationTime, maint.creationTime)).flatten)

  private val allY = {
    val all = yValues.flatten
    if (tolerance.isDefined)
      all ++ Seq(tolerance.get.min, tolerance.get.max)
    else all
  }

  private val minY = if (yRange.isDefined) yRange.get.min else allY.min
  private val maxY = if (yRange.isDefined) yRange.get.max else allY.max

  private val maintValueList = column("MaintenanceRecord", Seq.fill(maintList.size)(Seq(minY, maxY)).flatten)
  private val maintSummaryList = textColumn(maintList.map(maint => maint.summary))
  private val maintColorList = textColumn(maintList.map(maint => MaintenanceCategory.findMaintenanceCategoryMatch(maint.category).Color))
  private val maintCategoryList = textColumn(maintList.map(maint => maint.category))

  val html = { <div id={ chartIdTag }>{ chartIdTag }</div> }

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
        Seq(
          gridLine("tolerance", tolerance.get.max, "Maximum Tolerance"),
          gridLine("tolerance", tolerance.get.min, "Minimum Tolerance"))
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
                """ + allGrid.mkString(",\n                ") + """
            ]
        }
    }"""
    }
  }

  val javascript = {
    """
var """ + chartIdTag + """ = c3.generate({""" + C3Chart.chartSizeText(width, height) + """
    tooltip: {
      format: {
        value: function (value, ratio, id, index) {
          var maintSummaryList = """ + maintSummaryList + """;
          if (id === 'MaintenanceRecord') return maintSummaryList[index/2];
          return d3.format('""" + yFormat + """')(value);
          },
          name: function (name, ratio, id, index) {
            var maintCategoryList = """ + maintCategoryList + """;
              if (id === 'MaintenanceRecord') return maintCategoryList[index/2];
              return id;
              },
          title: function (value) { 
              return formatDate(value) + " &nbsp; " + formatTime(value);
        }
      }
    },
    data: {
      xs: {
        """ + yLabels + """
        'MaintenanceRecord': 'maintDateList'
        },
        xFormat: standardDateFormat,
        columns: [
          """ + dateColumn(xLabel, xDateList) + """,
          """ + yText + """,
          """ + maintDateList + """,
          """ + maintValueList + """
        ],
        types: {
          """ + yAxisLabels.map(label => "'" + label + "' : 'line'").mkString(",\n          ") + """,
          'MaintenanceRecord': 'bar'
        },
        color: function (color, d) {
          var maintColorList = """ + maintColorList + """;
          if (d.id === 'MaintenanceRecord') return maintColorList[d.index % maintColorList.length];
          if (d.index == """ + yIndex + """) return 'orange';
          return color;
        }
    }""" + grid + """,
    point: {
        r: 2,
        focus : {
            expand: {
                r:4
            }
        }
    },
    bindto : '#""" + chartIdTag + """',
    axis: {
       x: {
         label: 'Date',
         type: 'timeseries',
         min: '""" + Util.standardDateFormat.format(minDate) + """',
         max: '""" + Util.standardDateFormat.format(maxDate) + """',
         tick: { format: function(dt) { return [ formatDate(dt) , formatTime(dt) ]; } }
       },
       y: {
         """ + C3Chart.rangeText(minY, maxY) + """         label: '""" + yDataLabel + """',
         tick: {
           format: d3.format('""" + yFormat + """')
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
      pattern : """ + yColorNameList + """
    }
  });

"""
  }

}

