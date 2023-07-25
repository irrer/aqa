package org.aqa.stats

import java.text.SimpleDateFormat
import scala.util.Random

object AnUtil {

  /* Column index of the machine name. */
  val IndexMachine = 1

  val spaces: String = (0 until 100).map(_ => "          ").mkString("          ")

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  val TagInstitution = "Institution"
  val TagMachine = "Machine"
  val TagAcquisition = "Acquisition"
  val TagAnalysis = "Analysis"
  val TagProcedure = "Procedure"

  val ignoreMachSet: Set[String] = Set(
    "MACH_67",
    "MACH_68",
    "MACH_69"
  )

  val ignoreColumnNameSet: Set[String] = Set(
    "inputPK",
    "outputPK",
    "Analysis",
    "Beam Name",
    "Beam Name MLC",
    "Beam Name Open",
    "Beam Number",
    "C 270 SeriesInstanceUID",
    "C 270 Software Version",
    "C 270 SOPInstanceUID",
    "C 90 Beam Number",
    "C 90 Collimator Angle",
    "C 90 Gantry Angle",
    "C 90 Operator",
    "C 90 PatientID",
    "C 90 PatientName",
    "Coll 270 Beam Name",
    "Coll 90 Beam Name",
    "Collimator Angle",
    "CollimatorAnglePlan - Image deg",
    "collimatorAnglePlan deg",
    "Created by User",
    "Description",
    "Effective Date",
    "Institution",
    "Gantry Angle",
    "Gantry Angle Plan deg",
    "gantryAnglePlan - Image deg",
    "Machine",
    "Machine Type",
    "Maintenance Category",
    "Maintenance Record Marker",
    "Open Beam Number",
    "Open Collimator Angle",
    "Open Gantry Angle",
    "Open Operator",
    "Open PatientID",
    "Open PatientName",
    "Open SeriesInstanceUID",
    "Open Software Version",
    "Open SOPInstanceUID",
    "Operator",
    "outputPK",
    "Pass",
    "PatientID",
    "PatientName",
    "Procedure",
    "SeriesInstanceUID",
    "Software Version",
    "SOPInstanceUID",
    "Status",
    "Summary",
    "Units",
    "Uploaded By",
    "URL"
  )

  // Function to give
  // index of the median
  def median(l: Int, r: Int): Int = {
    val n: Int = r - l + 1
    val n2 = (n + 1) / 2 - 1
    n2 + l
  }

  def iqr(list: Seq[Double]): Double = {
    val sorted = list.sorted
    val size = list.size

    val q1: Double = {
      val i = size / 4
      size % 4 match {
        case 0 =>
          (sorted(i) - sorted(i - 1)) * .75 + sorted(i - 1)
        case 1 =>
          sorted(i)
        case 2 =>
          (sorted(i + 1) - sorted(i)) * .25 + sorted(i)
        case 3 =>
          (sorted(i + 1) - sorted(i)) * .5 + sorted(i)
      }
    }

    val q3: Double = {
      val i = (size * 3) / 4
      size % 4 match {
        case 0 =>
          (sorted(i) - sorted(i - 1)) * .25 + sorted(i - 1)
        case 1 =>
          sorted(i)
        case 2 =>
          (sorted(i) - sorted(i - 1)) * .75 + sorted(i - 1)
        case 3 =>
          (sorted(i) - sorted(i - 1)) * .5 + sorted(i - 1)
      }
    }

    val iqr = q3 - q1
    // println(s"q1: $q1    q3: $q3   iqr: $iqr")
    iqr
  }

  def main(args: Array[String]): Unit = {

    println("from scipy import stats")

    def show(seq: Seq[Int]): Unit = {
      println
      val r = iqr(seq.map(_.toDouble))
      println(s"size: ${seq.size} : ${seq.sorted.mkString(", ")}  -->  $r")
      println(s"print(stats.iqr([${seq.mkString(", ")}]))")
      println
    }

    if (true) {
      show(Seq(1, 2, 3, 4, 7, 10, 12, 14, 17, 19, 20))
      show(Seq(1, 2, 3, 4, 7, 10, 12, 14, 17, 19, 20, 25))
      show(Seq(1, 2, 3, 4, 7, 10, 12, 14, 17, 19, 20, 25, 27))
      show(Seq(1, 2, 3, 4, 7, 10, 12, 14, 17, 19, 20, 25, 27, 33))
    }

    if (true) {
      for (i <- 0 until 10) {
        val seq = (0 to i + 8).map(_ => Random.nextInt(100))
        show(seq)
      }
    }
  }

}
