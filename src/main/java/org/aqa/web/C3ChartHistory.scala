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
 * @param xFormat: Formatting for x values.   Examples: .3 .4g.  Reference: http://bl.ocks.org/zanarmstrong/05c1e95bf7aa16c4768e
 *
 * @param baselineSpec: Optional baseline value.  If given, a horizontal line
 *
 * @param yAxisLabels
 *
 * @param yValues
 *
 * @param yFormat: Formatting for y values.   Examples: .3 .4g
 */
class C3ChartHistory(
  pmiList: Seq[PMI],
  width: Option[Int],
  height: Option[Int],
  xLabel: String, xDateList: Seq[Date], xFormat: String,
  baselineSpec: Option[C3ChartHistory.BaselineSpec],
  yAxisLabels: Seq[String], yDataLabel: String, yValues: Seq[Seq[Double]], yFormat: String, yColorList: Seq[Color]) extends Logging {

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
  private val yColorNameList = textColumn((yColorList ++ baselineColor).map(c => (c.getRGB & 0xffffff).formatted("#%06x")))

  private val minDate = xDateList.minBy(d => d.getTime)
  private val maxDate = xDateList.maxBy(d => d.getTime)

  private val pmiDateList = dateColumn("pmiDateList", pmiList.map(pmi => pmi.creationTime))

  private val allY = yValues.flatten
  private val minY = allY.min
  private val maxY = allY.max

  private val pmiValue = Seq(maxY.abs, (maxY - minY).abs, minY.abs).max

  private val pmiValueList = column("pmiValueList", Seq.fill(pmiList.size)(pmiValue))
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

  val javascript = {
    """
var """ + idTag + """ = c3.generate({""" + C3Chart.chartSizeText(width, height) + """
    tooltip: {
      format: {
        value: function (value, ratio, id, index) {
          var pmiSummaryList = """ + pmiSummaryList + """;
          if (id === 'pmiValueList') return pmiSummaryList[index];
          return d3.format('""" + yFormat + """')(value);
          },
          name: function (name, ratio, id, index) {
            var pmiCategoryList = """ + pmiCategoryList + """;   // make
              if (id === 'pmiValueList') return pmiCategoryList[index];
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
        'pmiValueList': 'pmiDateList'
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
          'pmiValueList': 'bar'
        },
        color: function (color, d) {
          var pmiColorList = """ + pmiColorList + """;
          return d.id === 'pmiValueList' ? pmiColorList[d.index % pmiColorList.length] : color;
        }
    },
    point: {
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
         label: 'Date',
         type: 'timeseries',
         min: '""" + Util.standardDateFormat.format(minDate) + """',
         max: '""" + Util.standardDateFormat.format(maxDate) + """',
         tick: { format: function(dt) { return [ formatDate(dt) , formatTime(dt) ]; } }
       },
       y: {
         """ + C3Chart.rangeText(minY, maxY) + """         label: '""" + yDataLabel + """'
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