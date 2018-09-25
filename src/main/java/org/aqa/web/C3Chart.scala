package org.aqa.web

import org.aqa.Logging
import org.aqa.db.CenterDose
import java.text.SimpleDateFormat
import org.aqa.Util
import scala.collection.Seq

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

}

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
class C3Chart(xAxisLabel: String, xDataLabel: String, xValueList: Seq[Double], xFormat: String, yAxisLabels: Seq[String], yDataLabel: String, yValues: Seq[Seq[Double]], yFormat: String) extends Logging {

  if (yAxisLabels.size != yValues.size) throw new RuntimeException("Must be same number of Y labels as Y data sets.  yAxisLabels.size: " + yAxisLabels.size + "    yValues.size: " + yValues.size)

  val idTag = "ChartId_" + C3Chart.getId

  private def column(label: String, valueList: Seq[Double]): String = {
    "[ '" + label + "', " + valueList.mkString(", ") + "]"
  }

  val html = { <div id={ idTag }>filler</div> }

  val javascript = {
    """      
  var """ + idTag + """ = c3.generate({
    data: {
        x: '""" + xAxisLabel + """',
        columns: [
          """ + column(xAxisLabel, xValueList) + """,
          """ + yAxisLabels.indices.map(i => column(yAxisLabels(i), yValues(i))).mkString(",\n         ") + """ 
        ]
    },
    point: { // show point on hover
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