package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CenterDose
import java.text.SimpleDateFormat
import org.aqa.Util

class CenterDoseChart(resultList: Seq[CenterDose.CenterDoseHistory], history: Seq[CenterDose.CenterDoseHistory]) extends Logging {

  private val dateFormat = new SimpleDateFormat("MMM dd")

  /* List of SOPInstanceUID's for data set that was just calculated. */
  private val sopSet = resultList.map(cd => cd.SOPInstanceUID).toSet

  private def sortedHistoryForBeam(beamName: String) = {
    val realHistory = (resultList ++ history).filter(h => h.beamName.equals(beamName)).sortWith((a, b) => (a.date.getTime < b.date.getTime))
    realHistory
  }

  private def beamRefOf(index: Int): String = {
    "Chart_" + index + "_" + resultList(index).beamName.replaceAll("[^a-zA-Z0-9]", "_")
  }

  private val allDates = (resultList ++ history).map(cd => cd.date)
  val minDateText = Util.standardDateFormat.format(allDates.min)
  val maxDateText = Util.standardDateFormat.format(allDates.max)

  private def chart(beamName: String, beamRef: String) = {
    val sortedHistory = sortedHistoryForBeam(beamName)
    val dateList = sortedHistory.map(h => "'" + Util.standardDateFormat.format(h.date) + "'").mkString("[ 'Date', ", ", ", " ]")
    val doseList = sortedHistory.map(h => h.dose.toString).mkString("[ 'HU', ", ", ", " ]")
    val beamRefTag = "@@beamRef@@"
    val dataIndexTag = "@@dataIndex@@"
    val minDateTag = "@@minDate@@"
    val maxDateTag = "@@maxDate@@"
    val dataIndex: Int = {
      //val centerDoseUID = resultList.find(cd => cd.beamName.equals(beamName)).get.SOPInstanceUID
      sortedHistory.indexWhere(sh => sopSet.contains(sh.SOPInstanceUID))
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
                        min: '""" + minDateTag + """',
                        max: '""" + maxDateTag + """',
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
                  right: 30,
                  top: 10
                }
            });
"""

    val allTags = Seq(
      (beamRefTag, beamRef),
      (dataIndexTag, dataIndex.toString),
      (minDateTag, minDateText),
      (maxDateTag, maxDateText))

    val chartText = allTags.foldLeft(template)((tmpl, tc) => tmpl.replace(tc._1, tc._2))
    chartText
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