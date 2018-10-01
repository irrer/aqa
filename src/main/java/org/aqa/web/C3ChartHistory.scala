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
 * Format: .3 .4g
 */
class C3ChartHistory(
  pmiList: Seq[PMI],
  xAxisLabel: String, xDataLabel: String, xDateList: Seq[Date], xFormat: String,
  yAxisLabels: Seq[String], yDataLabel: String, yValues: Seq[Seq[Double]], yFormat: String, yColorList: Seq[Color]) extends Logging {

  if (yAxisLabels.size != yValues.size) throw new RuntimeException("Must be same number of Y labels as Y data sets.  yAxisLabels.size: " + yAxisLabels.size + "    yValues.size: " + yValues.size)

  val yMaxSize = yValues.map(yy => yy.size).max
  if (yMaxSize > xDateList.size) throw new RuntimeException("Size of at least one of the yValues is " + yMaxSize + " and is greater than size of xDateList " + xDateList.size)

  val idTag = "ChartId_" + C3Chart.getId

  private def column(label: String, valueList: Seq[Double]): String = {
    "[ '" + label + "', " + valueList.mkString(", ") + "]"
  }

  private def textColumn(valueList: Seq[String]): String = valueList.map(t => "'" + t + "'").mkString("[ ", ", ", "]")

  private def dateColumn(label: String, valueList: Seq[Date]): String = {
    "[ '" + label + "', " + valueList.map(d => "'" + Util.standardDateFormat.format(d) + "'").mkString(", ") + "]"
  }

  val yColorNameList = textColumn(yColorList.map(c => (c.getRGB & 0xffffff).formatted("#%06x")))

  //val minDate = Util.standardDateFormat.format(xDateList.minBy(d => d.getTime))
  val minDate = xDateList.minBy(d => d.getTime)
  val maxDate = xDateList.maxBy(d => d.getTime)

  val pmiDateList = dateColumn("pmiDateList", pmiList.map(pmi => pmi.creationTime))
  val pmiValue = {
    val allY = yValues.flatten
    Seq(allY.max.abs, (allY.max - allY.min).abs, allY.min.abs).max
  }
  val pmiValueList = column("pmiValueList", Seq.fill(pmiList.size)(pmiValue))
  val pmiSummaryList = textColumn(pmiList.map(pmi => pmi.summary))
  val pmiColorList = textColumn(pmiList.map(pmi => MaintenanceCategory.findMaintenanceCategoryMatch(pmi.category).Color))
  val pmiCategoryList = textColumn(pmiList.map(pmi => pmi.category))

  val html = { <div id={ idTag }>filler</div> }

  val javascript = {
    """
var """ + idTag + """ = c3.generate({
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
        """ + yAxisLabels.map(y => "'" + y + "' : '" + xAxisLabel + "',").mkString("\n        ") + """
        'pmiValueList': 'pmiDateList'
        },
        xFormat: standardDateFormat,
        columns: [
          """ + dateColumn(xAxisLabel, xDateList) + """,
          """ + yAxisLabels.indices.map(i => column(yAxisLabels(i), yValues(i))).mkString(",\n          ") + """,
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
         label: 'Y Data Label'
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