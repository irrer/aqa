package org.aqa.web

import org.aqa.Logging
import java.util.Date
import org.aqa.Util

/**
 * @param xAxisLabel: Text label for X axis
 *
 * @param xValueList: List of X values
 *
 * @param xFormat
 *
 * @param yAxisLabels
 *
 * @param yValues
 *
 * @param yFormat
 *
 * Format: .3g .4g
 */
class C3ChartHistory(xAxisLabel: String, xDataLabel: String, xDateList: Seq[Date], xFormat: String, yAxisLabels: Seq[String], yDataLabel: String, yValues: Seq[Seq[Double]], yFormat: String) extends Logging {

  if (yAxisLabels.size != yValues.size) throw new RuntimeException("Must be same number of Y labels as Y data sets.  yAxisLabels.size: " + yAxisLabels.size + "    yValues.size: " + yValues.size)

  val yMaxSize = yValues.map(yy => yy.size).max
  if (yMaxSize > xDateList.size) throw new RuntimeException("Size of at least one of the yValues is " + yMaxSize + " and is greater than size of xDateList " + xDateList.size)

  val idTag = "ChartId_" + C3Chart.getId

  private def column(label: String, valueList: Seq[Double]): String = {
    "[ '" + label + "', " + valueList.mkString(", ") + "]"
  }

  private def dateColumn(label: String, valueList: Seq[Date]): String = {
    "[ '" + label + "', " + valueList.map(d => "'" + Util.standardDateFormat.format(d) + "'").mkString(", ") + "]"
  }

  val minDate = Util.standardDateFormat.format(xDateList.minBy(d => d.getTime))
  val maxDate = Util.standardDateFormat.format(xDateList.maxBy(d => d.getTime))

  val html = { <div id={ idTag }>filler</div> }

  val javascript = {
    """      
  var """ + idTag + """ = c3.generate({
    data: {
        x: '""" + xAxisLabel + """',
        xFormat: '%Y-%m-%dT%H:%M:%S',
        columns: [
          """ + dateColumn(xAxisLabel, xDateList) + """,
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
         label: 'Date',
         type: 'timeseries',
         min: '""" + minDate + """',
         max: '""" + maxDate + """',
         tick: { format: function(dt) { return formatDate(dt); } }
       },
        y: {
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