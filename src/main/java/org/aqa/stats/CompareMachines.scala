package org.aqa.stats

import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.io.File

object CompareMachines {

  private def makeRow(text: String): Row = {
    Row(Util.csvToText(text).map(t => Txt.get(t)))
  }

  private def columnGroupListToString(cgl: Seq[Column]): String = {
    val tagWidth = 30

    val sorted = cgl.sortBy(_.maxOutlierLevel).reverse

    def colPrefix(col: Column) = {
      col.beamData.machText + "  " + s"%${tagWidth}s".format(col.beamData.beamNameNoBlanks.take(tagWidth)) + "  " + "%4d".format(col.data.size) + "  "
    }

    val title = s"%${colPrefix(cgl.head).length}s".format("") + Column.header

    val body = sorted.map(col => colPrefix(col) + col.toString).mkString("\n")

    s"$title\n$body"
  }

  private val ignoreMachineSet = Set("MACH_67", "MACH_68", "MACH_69")

  private def groupIsOfInterest(cgl: Seq[Column]): Boolean = {
    // (cgl.size > 1) && // must have more than one machine for comparison
    val dst = cgl.map(_.range).distinct

    val rangesDifferent = dst.size > 1 // if all the values are the same, then ... BORING!

    val hasDiversity = cgl.map(_.range).max > 0.2

    rangesDifferent && hasDiversity // && notAllCloseToOne && notAllSimilar
  }

  def analyze(file: File): String = {
    val rowList = {
      Util.readTextFile(file).right.get.replaceAllLiterally("\r", "").split("\n").toSeq.map(makeRow).filter(_.columnList.size > 1)
    }

    val header = Header(rowList.head.columnList.map(t => Txt.get(t.trim.replace(' ', '_'))))

    def ignoreMachine(row: Row): Boolean =
      ignoreMachineSet.contains(row.machine(header))

    val dataList = rowList.tail.filterNot(ignoreMachine)

    val groupList = dataList.groupBy(row => row.columnList(AnUtil.IndexMachine) + row.beamName(header)).values.map(rowList => BeamData(header, rowList))

    val columnGroupList = {
      // @formatter:off
      groupList.
        flatMap(g => g.numericColumnList). //
        groupBy(col => col.name + " %%%% " + col.beamData.rowList.head.beamName(col.beamData.header)). //
        values.map(_.toSeq). //
        toSeq. //
        filter(groupIsOfInterest)
      // @formatter:on
    }

    val text = columnGroupList.map(columnGroupListToString).mkString("\n\n")

    text
  }

  def main(args: Array[String]): Unit = {
    println("Starting...")
    val start = System.currentTimeMillis()

    val dir = new File("""D:\aqa\outliers""")
    Trace.trace()
    val fileList = Util.listDirFiles(dir).filter(f => f.getName.endsWith(".csv")).filterNot(f => f.getName.contains("MaintenanceRecord"))
    Trace.trace()

    def analyzeFile(file: File): Unit = {
      val outFile = new File(file.getParentFile, file.getName.dropRight(4) + "Sorted.txt")
      print(s"Making file ${outFile.getName} ... ")
      val text = analyze(file)
      Util.writeFile(outFile, text)
      println(s"Wrote file ${outFile.getName}")
    }

    fileList.foreach(analyzeFile)
    val elapsed = System.currentTimeMillis() - start
    Trace.trace("Done.  Elapsed: " + Util.elapsedTimeHumanFriendly(elapsed))
  }

}
