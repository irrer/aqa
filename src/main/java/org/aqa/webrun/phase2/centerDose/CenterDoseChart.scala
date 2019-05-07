package org.aqa.webrun.phase2.centerDose

import org.aqa.Logging
import org.aqa.db.CenterDose
import java.text.SimpleDateFormat
import org.aqa.Util
import org.aqa.web.C3ChartHistory
import org.aqa.db.MaintenanceRecord
import java.awt.Color
import org.aqa.webrun.phase2.ExtendedData
import edu.umro.ScalaUtil.Trace

class CenterDoseChart(resultList: Seq[CenterDose.CenterDoseHistory], history: Seq[CenterDose.CenterDoseHistory], units: String, extendedData: ExtendedData) extends Logging {

  private val allDates = (resultList ++ history).map(cd => cd.date)

  private val maintenanceRecordList = MaintenanceRecord.getRange(extendedData.machine.machinePK.get, allDates.min, allDates.max)

  /* List of SOPInstanceUID's for data set that was just calculated. */
  private val sopSet = resultList.map(cd => cd.SOPInstanceUID).toSet

  private def sortedHistoryForBeam(beamName: String) = {
    val realHistory = (resultList ++ history).filter(h => h.beamName.equals(beamName)).sortWith((a, b) => (a.date.getTime < b.date.getTime))
    realHistory
  }

  private def beamRefOf(index: Int): String = {
    "HistoryChart_" + index + "_" + resultList(index).beamName.replaceAll("[^a-zA-Z0-9]", "_")
  }

  private def chartOfBeam(beamName: String): C3ChartHistory = {
    val sortedHistory = sortedHistoryForBeam(beamName)
    val index = sortedHistory.indexWhere(sh => sopSet.contains(sh.SOPInstanceUID))

    new C3ChartHistory(
      maintenanceRecordList,
      None, // width
      None, // height
      "Date", sortedHistory.map(h => h.date),
      None, // BaselineSpec
      None, // minMax
      Seq(units), units, Seq(sortedHistory.map(h => h.dose)), index, ".5g", Seq(new Color(102, 136, 187)))
  }

  def refOfBeam(beamName: String) = beamRefMap(beamName)

  private val scriptPrefix = {
    """
<script>
"""
  }

  private val scriptSuffix = {
    """
    </script>
"""
  }

  Trace.trace("Center Dose char data:\n    " + resultList.mkString(("\n    "))) // TODO rm

  private val chartList = resultList.map(cd => chartOfBeam(cd.beamName))

  private val beamRefMap = resultList.indices.map(i => (resultList(i).beamName, chartList(i).html)).toMap

  val chartScript = chartList.map(c => c.javascript).mkString(scriptPrefix, "\n", scriptSuffix)

}
