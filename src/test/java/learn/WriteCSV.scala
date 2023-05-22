package learn

import edu.umro.ScalaUtil.Trace
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVPrinter

import java.io._
import java.io.File

object WriteCSV {
  def main(args: Array[String]): Unit = {

    val fileName = """D:\tmp\jj.csv"""
    val file = new File(fileName)
    file.delete()

    val writer = new FileWriter("""D:\tmp\jj.csv""")

    val csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader("ID", "Name", "Designation", "Company"))
    csvPrinter.print("hey")
    csvPrinter.print("hi\nx\n\"hi")
    csvPrinter.print("ho")
    csvPrinter.println()
    csvPrinter.printRecord("1", "Jim\nIrrer", "The'Man  ♥ ", "Wor\"ld")
    csvPrinter.printRecord("2", "Satya Nadella", "CEO", "Microsoft")
    csvPrinter.flush()
    Trace.trace("Done.")

  }

}
