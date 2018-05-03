package org.aqa.webrun.phase2

import org.aqa.db.ImageIdentification
import org.aqa.Util
import java.io.File

object CheckAnglesCSV {
  def makeCsvFile(procedureDesc: String, institutionName: String, outputDir: File, machineId: String, acquisitionDate: String, analysisDate: String, userId: String, runReq: CheckAnglesRunRequirements) = {

    type II = ImageIdentification

    val dblFmt = "%14.11f"
    val textFmt = "%s"
    val columns: Seq[(String, (II) => Any)] = Seq(
      ("beamName", (ii: II) => ii.beamName),
      ("gantryAnglePlan_deg", (ii: II) => ii.gantryAnglePlan_deg),
      ("gantryAnglePlanMinusImage_deg", (ii: II) => ii.gantryAnglePlanMinusImage_deg),
      ("collimatorAnglePlan_deg", (ii: II) => ii.collimatorAnglePlan_deg),
      ("collimatorAnglePlanMinusImage_deg", (ii: II) => ii.collimatorAnglePlanMinusImage_deg),
      ("x1JawPlan_mm", (ii: II) => ii.x1JawPlan_mm),
      ("x1JawPlanMinusImage_mm", (ii: II) => ii.x1JawPlanMinusImage_mm),
      ("x2JawPlan_mm", (ii: II) => ii.x2JawPlan_mm),
      ("x2JawPlanMinusImage_mm", (ii: II) => ii.x2JawPlanMinusImage_mm),
      ("y1JawPlan_mm", (ii: II) => ii.y1JawPlan_mm),
      ("y1JawPlanMinusImage_mm", (ii: II) => ii.y1JawPlanMinusImage_mm),
      ("y2JawPlan_mm", (ii: II) => ii.y2JawPlan_mm),
      ("y2JawPlanMinusImage_mm", (ii: II) => ii.y2JawPlanMinusImage_mm),
      ("energyPlan_kev", (ii: II) => ii.energyPlan_kev),
      ("energyPlanMinusImage_kev", (ii: II) => ii.energyPlanMinusImage_kev),
      ("flatteningFilter", (ii: II) => ii.flatteningFilter),
      ("pass", (ii: II) => ii.pass))

    def imageIdentificationToCsv(ii: ImageIdentification): String = {
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
        ("Institution", institutionName),
        ("Acquisition Date", acquisitionDate),
        ("Analysis Date", analysisDate),
        ("User", userId))

      Seq(
        info.map(s => Util.textToCsv(s._1)).mkString(","),
        info.map(s => Util.textToCsv(s._2)).mkString(","))
    }

    val header = Seq(columns.map(c => c._1).mkString(","))

    val data = runReq.imageIdFileList.map(iif => imageIdentificationToCsv(iif.imageIdentification))

    val text = (metaData ++ header ++ data).mkString("", "\r\n", "\r\n")
    val file = new File(outputDir, ImageIdentification.csvFileName)
    Util.writeFile(file, text)
  }

}