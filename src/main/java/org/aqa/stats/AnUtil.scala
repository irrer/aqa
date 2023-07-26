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
  val TagUrl = "URL"

  val ignoreMachSet: Set[String] = Set(
    "MACH_67",
    "MACH_68",
    "MACH_69"
  )

  val ignoreColumnNameSetOrig: Set[String] = Set(
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

  val ignoreColumnNameSet: Set[String] = ignoreColumnNameSetOrig.toSeq.map(_.replace(' ', '_')).toSet ++ ignoreColumnNameSetOrig

  /**
    * Calculate the median ('middle') value of a list.
    *
    * Interactive page: https://www.w3schools.com/python/trypython.asp?filename=demo_ref_stat_median
    *
    * @param list Unordered list of values.
    * @return Median value.
    */
  def median(list: Seq[Double]): Double = {
    val sorted = list.sorted
    val size = sorted.size
    val m = size match {
      case 0 => Double.NaN
      case _ if (size % 2) == 0 =>
        val s2 = size / 2
        (sorted(s2 - 1) + sorted(s2)) / 2
      case _ => sorted(size / 2)
    }
    m
  }

  /**
    * Calculate the interquartile range of list of numbers.
    * This function interpolates between consecutive values as necessary.
    *
    * The interquartile range (IQR) is the difference between the 75th and 25th percentile of
    * the data. It is a measure of the dispersion similar to standard deviation or variance
    * but is much more robust against outliers.
    *
    * Interactive page for calculating interquartile range:
    *     https://www.w3schools.com/statistics/trypython.asp?filename=demo_stat_python_iqr
    *
    * Python source reference that this was modeled on:
    *     https://github.com/scipy/scipy/blob/main/scipy/stats/_stats_py.py#L3442
    *
    * @param list Unordered list of values.
    * @return Interquartile range.
    */
  def iqr(list: Seq[Double]): Double = {
    val sorted = list.sorted
    val size = list.size

    def q1: Double = {
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

    def q3: Double = {
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

    val iqr = size match {
      case 0 => Double.NaN // handle special case of empty list
      case 1 => 0.0 // handle special case of list with size 1
      case _ => q3 - q1
    }
    // println(s"q1: $q1    q3: $q3   iqr: $iqr")
    iqr
  }

  def main(args: Array[String]): Unit = {

    println("from scipy import stats")

    def show(seq: Seq[Double]): Unit = {
      println
      val r = iqr(seq)
      println(s"size: ${seq.size} : ${seq.sorted.mkString(", ")}  -->  $r")
      println(s"print(stats.iqr([${seq.mkString(", ")}]))")
      println
    }

    if (false) {
      show(Seq())
      show(Seq(0.0))
      show(Seq(13.0))
      show(Seq(13.0, 21.0))
      show(Seq(13.0, 21.0, 85.0))
    }
    if (false) {
      show(Seq(1.0, 2.0, 3.0, 4.0, 7.0, 10.0, 12.0, 14.0, 17.0, 19.0, 20.0))
      show(Seq(1.0, 2.0, 3.0, 4.0, 7.0, 10.0, 12.0, 14.0, 17.0, 19.0, 20.0, 25.0))
      show(Seq(1.0, 2.0, 3.0, 4.0, 7.0, 10.0, 12.0, 14.0, 17.0, 19.0, 20.0, 25.0, 27.0))
      show(Seq(1.0, 2.0, 3.0, 4.0, 7.0, 10.0, 12.0, 14.0, 17.0, 19.0, 20.0, 25.0, 27.0, 33.0))
    }

    if (false) {
      for (i <- 0 until 10) {
        val seq = (0 to i + 8).map(_ => Random.nextDouble() * 100)
        show(seq)
      }
    }

    if (true) {
      def showMed(list: Seq[Double]): Unit = {
        println(s"size: ${list.size}  ::  ${list.sorted.mkString(", ")}  -->  ${median(list)}")
      }

      showMed(Seq())
      showMed(Seq(5))
      showMed(Seq(1, 3, 5, 7, 9, 11, 13))
      showMed(Seq(1, 3, 5, 7, 9, 11))
      showMed(Seq(-11, 5.5, -3.4, 7.1, -9, 22))
    }
  }

}
