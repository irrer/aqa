package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.Util
import java.io.File
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq
import scala.collection.Seq

object SymmetryAndFlatnessCSV {



  val csvFileName = "SymmetryAndFlatness.csv"

  def makeCsvFile(extendedData: ExtendedData, runReq: RunReq, symmetryAndFlatnessSeq: Seq[SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult], subDir: File) = {

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

    type SF = SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult

    val columns: Seq[(String, (SF) => Any)] = Seq(
      ("beamName", (sf: SF) => sf.symmetryAndFlatness.beamName),
      ("SOPInstanceUID", (sf: SF) => sf.symmetryAndFlatness.SOPInstanceUID),

      ("axialSymmetry CU", (sf: SF) => sf.symmetryAndFlatness.axialSymmetry),
      ("axialSymmetryBaseline CU", (sf: SF) => sf.baseline.axialSymmetry),
      ("axialSymmetryStatus", (sf: SF) => sf.symmetryAndFlatness.axialSymmetryStatus),

      ("transverseSymmetry CU", (sf: SF) => sf.symmetryAndFlatness.transverseSymmetry),
      ("transverseSymmetryBaseline CU", (sf: SF) => sf.baseline.transverseSymmetry),
      ("transverseSymmetryStatus", (sf: SF) => sf.symmetryAndFlatness.transverseSymmetryStatus),

      ("flatness CU", (sf: SF) => sf.symmetryAndFlatness.flatness),
      ("flatnessBaseline CU", (sf: SF) => sf.baseline.flatness),
      ("flatnessStatus", (sf: SF) => sf.symmetryAndFlatness.flatnessStatus),

      ("profileConstancy CU", (sf: SF) => sf.symmetryAndFlatness.profileConstancy(sf.baseline)),
      ("profileConstancyBaseline CU", (sf: SF) => sf.baseline.profileConstancy(sf.baseline)),
      ("profileConstancyStatus", (sf: SF) => sf.symmetryAndFlatness.profileConstancyStatus),

      ("top CU", (sf: SF) => sf.symmetryAndFlatness.top_cu),
      ("bottom CU", (sf: SF) => sf.symmetryAndFlatness.bottom_cu),
      ("left CU", (sf: SF) => sf.symmetryAndFlatness.left_cu),
      ("right CU", (sf: SF) => sf.symmetryAndFlatness.right_cu),
      ("center CU", (sf: SF) => sf.symmetryAndFlatness.center_cu))

    def symmetryAndFlatnessToCsv(sf: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult): String = {
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
