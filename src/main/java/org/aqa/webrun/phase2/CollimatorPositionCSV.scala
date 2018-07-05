package org.aqa.webrun.phase2

import org.aqa.db.CollimatorPosition
import org.aqa.Util
import java.io.File
import org.aqa.db.Output
import org.aqa.db.Machine
import org.aqa.db.Institution
import org.aqa.db.Procedure
import org.aqa.db.Input
import org.aqa.db.User

object CollimatorPositionCSV {

  val csvFileName = "CollimatorPosition.csv"

  def makeCsvFile(extendedData: ExtendedData, runReq: RunReq, collimatorPositionSeq: Seq[CollimatorPosition]) = {

    // format lots of meta-information for the CSV header

    val analysisDate: String = {
      val date = extendedData.output.analysisDate match {
        case Some(d) => d
        case _ => extendedData.output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

    val machineId = extendedData.machine.id
    val userId = extendedData.user.id
    val acquisitionDate = if (extendedData.output.dataDate.isDefined) Util.standardDateFormat.format(extendedData.output.dataDate.get) else "none"

    type II = CollimatorPosition

    val dblFmt = "%14.11f"
    val textFmt = "%s"
    val columns: Seq[(String, (II) => Any)] = Seq(
      ("beamName", (ii: II) => ii.beamName),
      ("SOPInstanceUID", (ii: II) => ii.SOPInstanceUID),
      ("gantryAngle_deg", (ii: II) => ii.gantryAngle_deg),
      ("collimatorAngle_deg", (ii: II) => ii.collimatorAngle_deg),
      ("north_mm", (ii: II) => ii.north_mm),
      ("south_mm", (ii: II) => ii.south_mm),
      ("east_mm", (ii: II) => ii.east_mm),
      ("west_mm", (ii: II) => ii.west_mm),
      ("northPlanMinusImage_mm", (ii: II) => ii.northPlanMinusImage_mm),
      ("southPlanMinusImage_mm", (ii: II) => ii.southPlanMinusImage_mm),
      ("eastPlanMinusImage_mm", (ii: II) => ii.eastPlanMinusImage_mm),
      ("westPlanMinusImage_mm", (ii: II) => ii.westPlanMinusImage_mm),
      ("pass", (ii: II) => ii.status))

    def collimatorPositionToCsv(ii: CollimatorPosition): String = {
      def fmt(any: Any): String = {
        any match {
          case d: Double => d.formatted("%14.11e")
          case _ => Util.textToCsv(any.toString)
        }
      }
      columns.map(c => fmt(c._2(ii))).mkString(",")
    }

    val metaData = {
      val info = Seq(
        ("Procedure", procedureDesc),
        ("Machine", machineId),
        ("Institution", extendedData.institution.name),
        ("Acquisition Date", acquisitionDate),
        ("Analysis Date", analysisDate),
        ("User", userId))

      Seq(
        info.map(s => Util.textToCsv(s._1)).mkString(","),
        info.map(s => Util.textToCsv(s._2)).mkString(","))
    }

    val header = Seq(columns.map(c => c._1).mkString(","))

    val data = collimatorPositionSeq.map(positionCheck => collimatorPositionToCsv(positionCheck))

    val text = (metaData ++ header ++ data).mkString("", "\r\n", "\r\n")
    val file = new File(extendedData.output.dir, csvFileName)
    Util.writeFile(file, text)
  }

}