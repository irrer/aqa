package org.aqa.webrun.phase2.leafPosition

import org.aqa.Util
import java.io.File
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq
import scala.collection.Seq
import org.aqa.db.LeafPosition
import org.aqa.web.WebServer

object LeafPositionCSV {

  val csvFileName = "LeafPosition.csv"

  def makeCsvFile(extendedData: ExtendedData, runReq: RunReq, beamResultsSeq: Seq[LeafPositionAnalysis.BeamResults], subDir: File): String = {

    // format lots of meta-information for the CSV header

    val analysisDate: String = {
      val date = extendedData.output.analysisDate match {
        case Some(d) => d
        case _ => extendedData.output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

    val institutionName = extendedData.institution.name
    val machineId = extendedData.machine.id
    val userId = extendedData.user.id
    val acquisitionDate = if (extendedData.output.dataDate.isDefined) Util.standardDateFormat.format(extendedData.output.dataDate.get) else "none"

    type LP = LeafPosition

    val columns: Seq[(String, (LP) => Any)] = Seq(
      ("Beam Name", (lp: LP) => lp.beamName),
      ("SOPInstanceUID", (lp: LP) => lp.SOPInstanceUID),
      ("Leaf Index", (lp: LP) => lp.leafIndex),
      ("Leaf Position Index", (lp: LP) => lp.leafPositionIndex),
      ("Offset mm", (lp: LP) => lp.offset_mm),
      ("Pass/Fail", (lp: LP) => lp.status),
      ("Measured End Position", (lp: LP) => lp.measuredEndPosition_mm),
      ("Expected End Position", (lp: LP) => lp.expectedEndPosition_mm),
      ("Measured Minor Side", (lp: LP) => lp.measuredMinorSide_mm),
      ("Measured Major Side", (lp: LP) => lp.measuredMajorSide_mm),
      ("Machine ID", (lp: LP) => machineId),
      ("Institution", (lp: LP) => institutionName),
      ("Acquistion Date", (lp: LP) => acquisitionDate),
      ("Patient ID", (lp: LP) => runReq.patientIdOfSop(lp.SOPInstanceUID)))

    def beamResultsToCsv(lp: LeafPosition): String = {
      def fmt(any: Any): String = {
        any match {
          case d: Double => d.formatted("%14.11e")
          case _ => Util.textToCsv(any.toString)
        }
      }
      columns.map(c => fmt(c._2(lp))).mkString(",")
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

    def leafSorter(a: LeafPosition, b: LeafPosition): Boolean = {
      if (!a.beamName.equals(b.beamName))
        a.beamName.compare(b.beamName) < 0
      else {
        if (a.leafIndex != b.leafIndex) a.leafIndex.compare(b.leafIndex) < 0
        else a.leafPositionIndex.compare(b.leafPositionIndex) < 0
      }
    }

    val data = beamResultsSeq.map(_.resultList).flatten.sortWith(leafSorter).map(lp => beamResultsToCsv(lp))

    val text = (metaData ++ header ++ data).mkString("", "\r\n", "\r\n")
    val csvFile = new File(subDir, csvFileName)
    Util.writeFile(csvFile, text)
    WebServer.urlOfResultsFile(csvFile)
  }

}
