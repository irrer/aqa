package org.aqa.web

import org.aqa.Logging
import java.util.Date
import org.aqa.Util
import org.aqa.db.PMI
import org.aqa.db.MaintenanceCategory
import java.awt.Color

/**
 * @param pmiList: List of PMI records that fall chronologically in the displayed range
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
 * @param minMax: If given, show horizontal tolerance (see AQA.css .c3-ygrid-line.tolerance) lines at the given values.
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
  pmiList: Seq[PMI],
  width: Option[Int],
  height: Option[Int],
  xLabel: String, xDateList: Seq[Date],
  baselineSpec: Option[C3ChartHistory.BaselineSpec],
  minMax: Option[(Double, Double)],
  yAxisLabels: Seq[String], yDataLabel: String, yValues: Seq[Seq[Double]], yIndex: Int, yFormat: String, yColorList: Seq[Color]) extends Logging {

  if (yAxisLabels.size != yValues.size) throw new RuntimeException("Must be same number of Y labels as Y data sets.  yAxisLabels.size: " + yAxisLabels.size + "    yValues.size: " + yValues.size)

  private val yMaxSize = yValues.map(yy => yy.size).max
  if (yMaxSize > xDateList.size) throw new RuntimeException("Size of at least one of the yValues is " + yMaxSize + " and is greater than size of xDateList " + xDateList.size)

  private val idTag = "ChartId_" + C3Chart.getId

  private def column(label: String, valueList: Seq[Double]): String = {
    "[ '" + label + "', " + valueList.mkString(", ") + "]"
  }

  private def textColumn(valueList: Seq[String]): String = valueList.map(t => "'" + t + "'").mkString("[ ", ", ", "]")

  private def dateColumn(label: String, valueList: Seq[Date]): String = {
    "[ '" + label + "', " + valueList.map(d => "'" + Util.standardDateFormat.format(d) + "'").mkString(", ") + "]"
  }

  private val baselineColor = if (baselineSpec.isDefined) Seq(baselineSpec.get.color) else Seq[Color]()
  private val pmiColor = if (pmiList.isEmpty) Seq[Color]() else Seq(Color.white)
  private val yColorNameList = textColumn((yColorList ++ baselineColor ++ pmiColor).map(c => (c.getRGB & 0xffffff).formatted("#%06x")))

  private val minDate = xDateList.minBy(d => d.getTime)
  private val maxDate = xDateList.maxBy(d => d.getTime)

  private val pmiDateList = dateColumn("pmiDateList", pmiList.map(pmi => Seq(pmi.creationTime, pmi.creationTime)).flatten)

  private val allY = {
    val all = yValues.flatten
    if (minMax.isDefined)
      all ++ Seq(minMax.get._1, minMax.get._2)
    else all
  }
  private val minY = allY.min
  private val maxY = allY.max

  private val pmiValueList = column("PMI", Seq.fill(pmiList.size)(Seq(minY, maxY)).flatten)
  private val pmiSummaryList = textColumn(pmiList.map(pmi => pmi.summary))
  private val pmiColorList = textColumn(pmiList.map(pmi => MaintenanceCategory.findMaintenanceCategoryMatch(pmi.category).Color))
  private val pmiCategoryList = textColumn(pmiList.map(pmi => pmi.category))

  val html = { <div id={ idTag }>filler</div> }

  private val yLabels = {
    val yOnly = yAxisLabels.map(y => "'" + y + "' : '" + xLabel + "',")
    val baselineLabel = if (baselineSpec.isEmpty) Seq[String]() else Seq("'Baseline' : '" + xLabel + "',")
    (yOnly ++ baselineLabel).mkString("\n        ")
  }

  private val yText = {
    val yData = yAxisLabels.indices.map(i => column(yAxisLabels(i), yValues(i)))
    val lines = if (baselineSpec.isEmpty) yData else { yData :+ column("Baseline", Seq.fill(yValues.head.size)(baselineSpec.get.value)) }
    lines.mkString(",\n          ")
  }

  private val grid = {
    minMax match {
      case Some(mm) => {
        """,
    grid: {
        y: {
            lines: [
                { position: 'start', class: 'tolerance', value:  """ + mm._1 + """, text: '\0 \0 \0 \0 \0 \0 \0 Minimum Tolerance' },
                { position: 'start', class: 'tolerance', value:  """ + mm._2 + """, text: '\0 \0 \0 \0 \0 \0 \0 Maximum Tolerance' }
            ]
        }
    }"""
      }
      case _ => ""
    }
  }

  val javascript = {
    """
var """ + idTag + """ = c3.generate({""" + C3Chart.chartSizeText(width, height) + """
    tooltip: {
      format: {
        value: function (value, ratio, id, index) {
          var pmiSummaryList = """ + pmiSummaryList + """;
          if (id === 'PMI') return pmiSummaryList[index/2];
          return d3.format('""" + yFormat + """')(value);
          },
          name: function (name, ratio, id, index) {
            var pmiCategoryList = """ + pmiCategoryList + """;
              if (id === 'PMI') return pmiCategoryList[index/2];
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
        'PMI': 'pmiDateList'
        },
        xFormat: standardDateFormat,
        columns: [
          """ + dateColumn(xLabel, xDateList) + """,
          """ + yText + """,
          """ + pmiDateList + """,
          """ + pmiValueList + """
        ],
        types: {
          """ + yAxisLabels.map(label => "'" + label + "' : 'line'").mkString(",\n          ") + """,
          'PMI': 'bar'
        },
        color: function (color, d) {
          var pmiColorList = """ + pmiColorList + """;
          if (d.id === 'PMI') return pmiColorList[d.index % pmiColorList.length];
          if (d.id === 'Baseline') return color;
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
    bindto : '#""" + idTag + """',
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

object C3ChartHistory {
  case class BaselineSpec(value: Double, color: Color)
}