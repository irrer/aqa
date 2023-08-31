package org.aqa.stats

import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.io.File

object SourceToImage {

  private def makeRow(text: String): Row = {
    Row(Util.csvToText(text).map(t => Txt.get(t)))
  }

  private val ignoreMachineSet = Set("MACH_67", "MACH_68", "MACH_69")

  private case class SrcToDst(row: Row, header: Header) {
    private val machine: String = row.columnList(header.columns.indexWhere(_.equals("Machine")))
    private val dataDate: String = row.columnList(header.columns.indexWhere(_.equals("Acquisition")))
    private val beamNameList: Seq[String] = {
      val indexList = header.columns.zipWithIndex.filter(c => c._1.matches(".*Beam.*Name.*")).map(_._2).filter(_ < row.columnList.size)
      indexList.map(i => row.columnList(i))
    }
    val sourceToImageList_mm: Seq[Double] = {
      val indexList = header.columns.zipWithIndex.filter(c => c._1.matches(".*Source.to.Image.*")).map(_._2).filter(_ < row.columnList.size)
      indexList.map(i => row.columnList(i).toDouble)
    }
    val XRay_Origin_ZList: Seq[Double] = {
      val indexList = header.columns.zipWithIndex.filter(c => c._1.matches(".*Ray.Origin.Z.*")).map(_._2).filter(_ < row.columnList.size)
      indexList.map(i => row.columnList(i).toDouble)
    }
    private val outputPK: Long = row.columnList(header.columns.indexWhere(_.equals("outputPK"))).toLong
    private val url: String = row.columnList(header.columns.indexWhere(_.equals("URL")))

    override def toString: String =
      "%8s".format(machine) +
        "%24s".format(dataDate) +
        beamNameList.map(std => "%16s".format(std)).mkString("    ") +
        sourceToImageList_mm.map(std => "%12.4f".format(std)).mkString("    ") +
        XRay_Origin_ZList.map(std => "%12.4f".format(std)).mkString("    ") +
        "%8d".format(outputPK) +
        "%s".format(url)
  }

  def analyze(file: File): String = {
    val rowList = {
      Util.readTextFile(file).right.get.replaceAllLiterally("\r", "").split("\n").toSeq.map(makeRow).filter(_.columnList.size > 1)
    }

    val header = Header(rowList.head.columnList.map(t => Txt.get(t.trim.replace(' ', '_'))))

    def ignoreMachine(row: Row): Boolean =
      ignoreMachineSet.contains(row.machine(header))

    val dataList = rowList.tail.filterNot(ignoreMachine).map(row => SrcToDst(row, header))

    val list = dataList.filter(std => std.sourceToImageList_mm.exists(_ < 1100) || std.XRay_Origin_ZList.exists(_ > -400))

    val text = list.mkString("\n") + "\n\n"

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
      val outFile = new File(file.getParentFile, file.getName.dropRight(4) + "SourceToImage.txt")
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
