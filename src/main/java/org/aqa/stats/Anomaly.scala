package org.aqa.stats

import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.io.File

object Anomaly {

  private def makeHeader(text: String): Header = {
    Header(Util.csvToText(text).map(t => Txt.get(t.trim.replace(' ', '_'))))
  }

  private def makeRow(text: String): Row = {
    Row(Util.csvToText(text).map(t => Txt.get(t)))
  }

  def analyze(file: File): String = {
    val rowList = {
      Util.readTextFile(file).right.get.replaceAllLiterally("\r", "").split("\n").toSeq.map(makeRow).filter(_.columnList.size > 1)
    }

    val header = Header(rowList.head.columnList.map(t => Txt.get(t.trim.replace(' ', '_'))))

    val dataList = rowList.tail

    val groupList = dataList.groupBy(row => row.columnList(AnUtil.IndexMachine) + row.beamName(header)).values.map(rowList => BeamData(header, rowList))

    val text = groupList.map(_.stats).mkString("\n")

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
      val outFile = new File(file.getParentFile, file.getName.dropRight(3) + "txt")
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
