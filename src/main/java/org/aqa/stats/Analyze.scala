package org.aqa.stats

import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

object Analyze {

  private val TagInstitution = "Institution"
  private val TagMachine = "Machine"
  private val TagAcquisition = "Acquisition"
  private val TagAnalysis = "Analysis"
  private val TagProcedure = "Procedure"

  // private val TagUrl = "URL"

  private val allTags = Seq(
    TagInstitution,
    TagMachine,
    TagAcquisition,
    TagAnalysis,
    TagProcedure
  )

  private val ignoreMachSet = Set(
    "MACH_67",
    "MACH_68",
    "MACH_69"
  )

  private val ignoreColumnNameSet = Set(
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

  private case class Row(text: String) {
    val columns: Seq[String] = Util.csvToText(text)

    val isHeader: Boolean = allTags.intersect(columns).size == allTags.size

    private val isEmpty: Boolean = columns.isEmpty

    val isData: Boolean = !(isHeader || isEmpty)

    /**
      * Get the column indexes for the names of the beams.
      *
      * If this is not a header row then an exception will be thrown.
      * @return
      */
    def beamNameColumns: Seq[Int] =
      if (isHeader) columns.filter(_.contains("Beam Name")).map(tag => columns.indexWhere(_.equals(tag))).sorted
      else throw new RuntimeException("beamNameColumns can only be called for a header row.")

    private def machName(machCol: Int): String = columns(machCol)

    def beamName(beamNameColumns: Seq[Int]): String = {
      beamNameColumns.map(columns).mkString(" ")
    }

    def id(machCol: Int, beamNameColumns: Seq[Int]): String = machName(machCol) + " " + beamName(beamNameColumns)
  }

  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  private case class DataGroup(header: Row, dataRowList: Seq[Row]) {

    private val inst = header.columns.indexWhere(_.equals(TagInstitution))
    private val mach = header.columns.indexWhere(_.equals(TagMachine))
    private val beam = dataRowList.head.beamName(header.beamNameColumns)
    private val acqCol = header.columns.indexWhere(_.equals(TagAcquisition))
    // private val urlCol = header.columns.indexWhere(_.equals(TagAcquisition))

    private def dateOf(row: Row): Date = Util.parseDate(dateFormat, row.columns(acqCol))

    private val minDate = dateOf(dataRowList.head)
    private val maxDate = dateOf(dataRowList.last)

    private val elapsed_day = (maxDate.getTime - minDate.getTime) / (24 * 60 * 60 * 1000.0)
    private val elapsedText_day = elapsed_day.formatted("%6.1f")

    private val dayPerAcquisition = elapsed_day / (dataRowList.size - 1)
    private val dayPerAcquisitionText = dayPerAcquisition.formatted("%6.2f")

    private val spaces = "                                                                                                                      "
    private val fmtLen = 10
    private def fmt(d: Double) = d.formatted(s"%$fmtLen.5f")

    private case class Stat(name: String, calc: Seq[Double] => Double) {
      def fmtName: String = (name + spaces).take(fmtLen)
    }

    private def mean(d: Seq[Double]) = d.sum / d.size

    private def stdDev(d: Seq[Double]) = ImageUtil.stdDev(d.map(_.toFloat))
    private def cofOfVar(d: Seq[Double]): Double = stdDev(d) / mean(d)

    def stats: String = {

      def removeOutlier(data: Seq[Double]): Seq[Double] = {
        val sd = stdDev(data)
        val after = data.sortBy(v => (v - sd).abs).dropRight(1)
        after
      }

      def colToString(col: Int, data: Seq[Double]): String = {
        val columnList = Seq(
          Stat("Min", d => d.min),
          Stat("Max", d => d.max),
          Stat("Mean", d => mean(d)),
          Stat("Range", d => d.max - d.min),
          Stat("StdDev", d => stdDev(d)),
          Stat("CofOfVar", d => cofOfVar(d)),
          Stat(
            "Outlier",
            d => {
              val sd = stdDev(d)
              val v = d.maxBy(v => (v - sd).abs)
              v
            }
          ),
          Stat(
            "Otl COV",
            d => {
              val sd = stdDev(d)
              val v = d.maxBy(v => (v - sd).abs)
              ((v - sd) / mean(d)).abs
            }
          )
        )

        if (col >= 0) {
          def calc(d: Seq[Double]): String = {
            val dataList = columnList.map(col => fmt(col.calc(d))).mkString("  ")
            dataList
          }
          val dataListNoOutlier = calc(removeOutlier(data))
          val dataList = calc(data)
          val name = header.columns(col).replace(' ', '_')
          val text = (name + spaces).take(30) + dataList + "   || " + dataListNoOutlier
          text
        } else {
          val nameList = columnList.map(col => col.fmtName).mkString("  ")
          nameList
        }
      }

      def colToNumeric(col: Int): Seq[Double] = {
        try {
          dataRowList.map(_.columns(col).toDouble)
        } catch {
          case _: Throwable => Seq() // empty list means this is not a numeric column
        }
      }

      val numericColumnList = {
        val columnList = header.columns.filterNot(c => ignoreColumnNameSet.contains(c))
        val indexList = columnList.map(name => header.columns.indexWhere(_.equals(name)))
        val data = indexList.map(col => (col, colToNumeric(col))).filter(_._2.size > 1)
        data
      }

      val machName = s"${(dataRowList.head.columns(mach) + "   ").take(8)}  "
      val beamName = s"${(beam.replace(' ', '_') + "                                       ").take(20)}  "

      val text = {

        val machBeam = machName + beamName
        val pfx = s"    Machine   Beam                  Column                            "
        val text1 = colToString(-1, Seq())
        val text2 = numericColumnList.map(nc => machBeam + colToString(nc._1, nc._2)).mkString("    ", "\n    ", "")
        pfx + text1 + "      " + text1 + "\n" + text2
      }

      val title =
        s"  ${dataRowList.head.columns(inst)}    ${dataRowList.head.columns(mach)}    $beam    Count: ${dataRowList.size}  Days: $elapsedText_day   Day/Acq: $dayPerAcquisitionText"
      Seq(title, text).mkString("\n")
    }

  }

  /**
    * Split a set of data rows into groups.  Each group contains all the rows that have the same machine and beam name
    * @param header Contains column identifiers.
    * @param data List of rows of data.
    * @return
    */
  private def split(header: Row, data: Seq[Row]): Seq[DataGroup] = {

    val machCol = header.columns.indexWhere(_.equals(TagMachine))
    val beamNameColList = header.beamNameColumns

    val acq = header.columns.indexWhere(_.equals(TagAcquisition))

    val groupList = data.groupBy(row => row.id(machCol, beamNameColList)).values.toSeq.filterNot(g => ignoreMachSet.contains(g.head.columns(machCol)))

    // make row lists into DataGroup objects and sort rows in each by acquisition time.
    groupList.map(g => DataGroup(header, dataRowList = g.sortBy(row => row.columns(acq))))
  }

  def analyze(file: File): String = {
    val rowList = Util.readTextFile(file).right.get.replaceAllLiterally("\r", "").split("\n").toSeq.map(Row)

    val header = rowList.find(_.isHeader).get

    val dataRowList = rowList.filter(_.isData)

    val groupList = split(header, dataRowList)

    val text = groupList.map(_.stats).mkString("\n")

    text
  }

  def main(args: Array[String]): Unit = {
    Trace.trace("Starting...")
    // val file = new File("""D:\aqa\outliers\SymmetryAndFlatness.csv""")

    val dir = new File("""D:\aqa\outliers""")
    val fileList = Util.listDirFiles(dir).filter(f => f.getName.endsWith(".csv")).filterNot(f => f.getName.equals("MaintenanceRecord.csv"))

    def analyzeFile(file: File): Unit = {
      val outFile = new File(file.getParentFile, file.getName.dropRight(3) + "txt")
      println(s"Making file ${outFile.getName}")
      val text = analyze(file)
      Util.writeFile(outFile, text)
      println(s"Wrote file ${outFile.getName}")
    }

    fileList.foreach(analyzeFile)
    Trace.trace("Done.")
  }

}
