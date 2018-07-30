package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CenterDose
import java.text.SimpleDateFormat
import org.aqa.Util

class CenterDoseChart(resultList: Seq[CenterDose], history: Seq[CenterDose.CenterDoseHistory]) extends Logging {

  private val dateFormat = new SimpleDateFormat("MMM dd")

  private def sortedHistoryForBeam(beamName: String) = {
    val realHistory = history.filter(h => h.beamName.equals(beamName)).sortWith((a, b) => (a.date.getTime < b.date.getTime))
    realHistory
  }

  private def beamRefOf(index: Int): String = {
    "Chart_" + index + "_" + resultList(index).beamName.replaceAll("[^a-zA-Z0-9]", "_")
  }

  private def chart(beamName: String, beamRef: String) = {
    val sortedHistory = sortedHistoryForBeam(beamName)
    val dateList = sortedHistory.map(h => "'" + Util.standardDateFormat.format(h.date) + "'").mkString("[ 'Date', ", ", ", " ]")
    val doseList = sortedHistory.map(h => h.dose.toString).mkString("[ 'HU', ", ", ", " ]")
    val beamRefTag = "@@beamRef@@"
    val dataIndexTag = "@@dataIndex@@"
    val dataIndex: Int = {
      val centerDoseUID = resultList.find(cd => cd.beamName.equals(beamName)).get.SOPInstanceUID
      sortedHistory.indexWhere(sh => sh.SOPInstanceUID.equals(centerDoseUID))
    }

    val template = """

        var """ + beamRefTag + """ = c3.generate({
                data: {
                    x: 'Date',
                    xFormat: '%Y-%m-%dT%H:%M:%S',
                    columns: [
                         """ + dateList + """,
                         """ + doseList + """
                    ],
                    color: function (color, d) {
                        return (d.index == """ + dataIndexTag + """) ? 'orange' : color ;
                    }
                },
                bindto : '#""" + beamRefTag + """',
                axis: {
                    x: {
                        label: 'Date',
                        type: 'timeseries',
                        tick: { format: function(dt) { return formatDate(dt); } }
                    },
                    y: {
                        label: 'HU',
                        tick: {
                            format: d3.format('.4f')
                        }
                    }
                },
                color : {
                    pattern : [ '#6688bb' ]
                },
                padding: {
                  right: 30
                }
            });
"""

    template.replace(beamRefTag, beamRef).replace(dataIndexTag, dataIndex.toString)
  }

  private val beamRefMap = resultList.indices.map(i => (resultList(i).beamName, beamRefOf(i))).toMap

  def refOfBeam(beamName: String) = beamRefMap(beamName)

  private val scriptPrefix = {
    """
<script>
        var monthList = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ];

        function formatDate(dt) { 
            return (dt.getYear() + 1900) + ' ' + monthList[dt.getMonth()] + ' ' + dt.getDate();
        };
"""
  }

  private val scriptSuffix = {
    """
    </script>
"""
  }

  val chartScript = resultList.zipWithIndex.map(cdi => chart(cdi._1.beamName, beamRefOf(cdi._2))).mkString(scriptPrefix, "\n", scriptSuffix)
}