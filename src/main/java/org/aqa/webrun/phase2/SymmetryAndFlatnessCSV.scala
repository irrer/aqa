package org.aqa.webrun.phase2

import org.aqa.db.SymmetryAndFlatness
import org.aqa.Util
import java.io.File
import org.aqa.db.Output
import org.aqa.db.Machine
import org.aqa.db.Institution
import org.aqa.db.Procedure
import org.aqa.db.Input
import org.aqa.db.User

object SymmetryAndFlatnessCSV {

  val csvFileName = "SymmetryAndFlatness.csv"

  def makeCsvFile(extendedData: ExtendedData, runReq: RunReq, symmetryAndFlatnessSeq: Seq[SymmetryAndFlatnessAnalysis.BeamResultBaseline], subDir: File) = {

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

    type SF = SymmetryAndFlatnessAnalysis.BeamResultBaseline

    val columns: Seq[(String, (SF) => Any)] = Seq(
      ("beamName", (sf: SF) => sf.result.beamName),
      ("SOPInstanceUID", (sf: SF) => sf.result.SOPInstanceUID),

      ("axialSymmetry CU", (sf: SF) => sf.result.axialSymmetry),
      ("axialSymmetryBaseline CU", (sf: SF) => sf.result.axialSymmetryBaseline),
      ("axialSymmetryStatus", (sf: SF) => sf.result.axialSymmetryStatus),

      ("transverseSymmetry CU", (sf: SF) => sf.result.transverseSymmetry),
      ("transverseSymmetryBaseline CU", (sf: SF) => sf.result.transverseSymmetryBaseline),
      ("transverseSymmetryStatus", (sf: SF) => sf.result.transverseSymmetryStatus),

      ("flatness CU", (sf: SF) => sf.result.flatness),
      ("flatnessBaseline CU", (sf: SF) => sf.result.flatnessBaseline),
      ("flatnessStatus", (sf: SF) => sf.result.flatnessStatus),

      ("profileConstancy CU", (sf: SF) => sf.result.profileConstancy),
      ("profileConstancyBaseline CU", (sf: SF) => sf.result.profileConstancyBaseline),
      ("profileConstancyStatus", (sf: SF) => sf.result.profileConstancyStatus),

      ("top CU", (sf: SF) => sf.pointSet.top),
      ("bottom CU", (sf: SF) => sf.pointSet.bottom),
      ("left CU", (sf: SF) => sf.pointSet.left),
      ("right CU", (sf: SF) => sf.pointSet.right),
      ("center CU", (sf: SF) => sf.pointSet.center))

    def symmetryAndFlatnessToCsv(sf: SymmetryAndFlatnessAnalysis.BeamResultBaseline): String = {
      def fmt(any: Any): String = {
        any match {
          case d: Double => d.formatted("%14.11e")
          case _ => Util.textToCsv(any.toString)
        }
      }
      columns.map(c => fmt(c._2(sf))).mkString(",")
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

    val data = symmetryAndFlatnessSeq.map(positionCheck => symmetryAndFlatnessToCsv(positionCheck))

    val text = (metaData ++ header ++ data).mkString("", "\r\n", "\r\n")
    val file = new File(subDir, csvFileName)
    Util.writeFile(file, text)
  }

}