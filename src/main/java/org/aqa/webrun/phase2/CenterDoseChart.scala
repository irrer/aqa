package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CenterDose
import java.text.SimpleDateFormat

class CenterDoseChart(resultList: Seq[CenterDose], history: Seq[CenterDose.CenterDoseHistory]) extends Logging {

  private val dateFormat = new SimpleDateFormat("MMM dd")

  private def sortedHistoryForBeam(beamName: String) = {
    history.filter(h => h.beamName.equals(beamName)).sortWith((a, b) => (a.date.getTime < b.date.getTime))
  }

  private def beamRefOf(index: Int): String = {
    "Chart_" + index + "_" + resultList(index).beamName.replaceAll("[^a-zA-Z0-9]", "_")
  }

  private def chart(beamName: String, beamRef: String) = {
    val sortedHistory = sortedHistoryForBeam(beamName)
    val dateList = sortedHistory.map(h => "'" + dateFormat.format(h.date) + "'").mkString("[ ", ", ", " ]")
    val dateIndexes = sortedHistory.indices.mkString("[ 'Date', ", ", ", " ]")
    val doseList = sortedHistory.map(h => h.dose.toString).mkString("[ 'Dose', ", ", ", " ]")
    val tag = "@@BR@@"

    val template = """

        var dateList_@@BR@@ = """ + dateList + """;

        var @@BR@@ = c3.generate({
                data: {
                    x: 'Date',
                    columns: [
                         """ + dateIndexes + """,
                         """ + doseList + """
                    ]
                },
                bindto : '#@@BR@@',
                axis: {
                    x: {
                        label: 'Date',
                        tick: { format: function(dd) { return dateList_@@BR@@[dd]; } }
                    },
                    y: {
                        label: 'Dose',
                        tick: {
                            format: d3.format('.4f')
                        }
                    }
                },
                color : {
                    pattern : [ '#6688bb' ]
                }
            });
"""

    template.replace(tag, beamRef)
  }

  private val beamRefMap = resultList.indices.map(i => (resultList(i).beamName, beamRefOf(i))).toMap

  def refOfBeam(beamName: String) = beamRefMap(beamName)

  val chartScript = resultList.zipWithIndex.map(cdi => chart(cdi._1.beamName, beamRefOf(cdi._2))).mkString("<script>", "\n", "\n</script>")
}