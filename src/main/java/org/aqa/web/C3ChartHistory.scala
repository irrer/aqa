package org.aqa.web

import org.aqa.Logging
import java.util.Date
import org.aqa.Util
import org.aqa.db.PMI
import org.aqa.db.MaintenanceCategory

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
class C3ChartHistory(machinePK: Long, xAxisLabel: String, xDataLabel: String, xDateList: Seq[Date], xFormat: String, yAxisLabels: Seq[String], yDataLabel: String, yValues: Seq[Seq[Double]], yFormat: String) extends Logging {

  if (yAxisLabels.size != yValues.size) throw new RuntimeException("Must be same number of Y labels as Y data sets.  yAxisLabels.size: " + yAxisLabels.size + "    yValues.size: " + yValues.size)

  val yMaxSize = yValues.map(yy => yy.size).max
  if (yMaxSize > xDateList.size) throw new RuntimeException("Size of at least one of the yValues is " + yMaxSize + " and is greater than size of xDateList " + xDateList.size)

  val idTag = "ChartId_" + C3Chart.getId

  private def column(label: String, valueList: Seq[Double]): String = {
    "[ '" + label + "', " + valueList.mkString(", ") + "]"
  }

  private def colorColumn(valueList: Seq[String]): String = valueList.map(t => "'" + t + "'").mkString("[ ", ", ", "]")

  private def dateColumn(label: String, valueList: Seq[Date]): String = {
    "[ '" + label + "', " + valueList.map(d => "'" + Util.standardDateFormat.format(d) + "'").mkString(", ") + "]"
  }

  //val minDate = Util.standardDateFormat.format(xDateList.minBy(d => d.getTime))
  val minDate = xDateList.minBy(d => d.getTime)
  val maxDate = xDateList.maxBy(d => d.getTime)

  val pmiList = PMI.getRange(machinePK, minDate, maxDate)

  val pmiDateList = dateColumn("pmiDateList", pmiList.map(pmi => pmi.creationTime))
  val pmiValue = yValues.flatten.max - yValues.flatten.min
  val pmiValueList = column("pmiValueList", Seq.fill(pmiList.size)(pmiValue))
  val pmiColorList = colorColumn(pmiList.map(pmi => MaintenanceCategory.findMaintenanceCategoryMatch(pmi.category).Color))

  val html = { <div id={ idTag }>filler</div> }

  val javascript = {
    """

var """ + idTag + """ = c3.generate({
    tooltip: {
        format: {
            value: function (value, ratio, id, index) {
                var pmiDateList = """ + pmiDateList + """;
                if (id === 'pmiValue') return pmiDateList[index + 1];
                return d3.format(".3")(value);  // make
            },
            name: function (name, ratio, id, index) {
                var pmiList = [ 'Update Firmware', 'Set Baseline'];   // make
                if (id === 'pmiValue') return pmiList[index];
                return id;
                },
            title: function (value) { 
                return formatDate(value) + " &nbsp; " + formatTime(value);
            }
        }
    },
    data: {
      xs: {
        'y0': 'xA',   // make
        'y1': 'xA',   // make
        'pmiValue': 'xPmi'   // make
        },
        xFormat: standardDateFormat,
        columns: [
        columns: [
          """ + dateColumn(xAxisLabel, xDateList) + """,
          """ + yAxisLabels.indices.map(i => column(yAxisLabels(i), yValues(i))).mkString(",\n         ") + """,
          """ + pmiDateList + """,
          """ + pmiValueList + """
        ],
        types: {
          """ + yAxisLabels.map(label => label + ": line").mkString(",\n         ") + """,
          pmiDateList: 'bar'
        },
    color: function (color, d) {
        var list = [ '#ff8800', '#888888' ];   // make
        return d.id === 'pmiValue' ? list[d.index % list.length] : color;
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
            label: 'Y Data Label'   // make
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
      pattern : [ '#668800', '#7788bb', '#888888' ]   // make
    }
  });

"""
  }

  val Xjavascript = {
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