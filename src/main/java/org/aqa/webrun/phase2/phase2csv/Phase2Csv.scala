package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.Output

import java.io.File

abstract class Phase2Csv[T] {

  /**
   * Define a column in a Phase2 CSV.
   *
   * @param header Name of column.
   * @param toText Converts given data to text that will be put in the cell.
   */
  case class Col(header: String, toText: T => Any) {}

  protected def makeColList: Seq[Col]

  private val colList: Seq[Col] = makeColList

  protected def getAl(data: T): AttributeList

  protected def getOutput(data: T): Output

  private val prefixCsv = new PrefixCsv
  private val dicomCsv = new DicomCsv

  /**
   * Get the data for a particular machine.
   *
   * @param machinePK Machine to get data for.
   * @return List of data for the particular machine.
   */
  protected def getData(machinePK: Long): Seq[T]


  /**
   * Make the column header line of a CSV file.
   *
   * @return Single string of data headers.
   */
  def makeHeader(): String = {
    val dataHeaderList = colList.map(col => '"' + col.header + '"').mkString(",")
    prefixCsv.headerText + "," + dicomCsv.headerText + "," + dataHeaderList
  }


  /**
   * Make one row of a CSV file.
   *
   * @param dataSet Data to format
   * @return One line of the CSV
   */
  private def makeCsvRow(dataSet: T): String = {
    val dataText = colList.map(col => {
      // Make into a new string to avoid references to other classes.  This helps free memory.
      new String(col.toText(dataSet).toString)
    }).mkString(",")

    val prefixText = prefixCsv.prefixToText(getOutput(dataSet))
    val dicomText = dicomCsv.dicomToText(getAl(dataSet))
    prefixText + "," + dicomText + "," + dataText
  }


  /** List of all machines, sorted by institution so that all of the rows
   * for a given institution will be consecutive. */
  private val machineList = Machine.list.sortBy(_.institutionPK)
    .filter(_.machinePK.get == 27) // TODO rm


  private def machineToCsv(machine: Machine) = {
    Phase2Csv.clearAlCache()
    val dataList = getData(machine.machinePK.get)
    // make the row list for this one machine
    val machineRowList = dataList.map(d => makeCsvRow(d)).mkString(",\n")
    Trace.trace("------------\n" + machineRowList)
    Phase2Csv.clearAlCache()
    machineRowList
  }


  /**
   * Get the CSV content for all institutions.
   *
   * @return
   */
  def csvContent: String = makeHeader() + "\n" + machineList.map(machine => machineToCsv(machine)).filter(_.nonEmpty).mkString("\n")

}


object Phase2Csv {

  /** Keep a cache of recently fetched attribute lists. */
  private val alCache = {
    new scala.collection.mutable.HashMap[String, AttributeList]()
  }

  private def clearAlCache(): Unit = {
    alCache.clear()
  }

  /**
   * Get the attribute list.  First try the cache.  If not there, then get it from the
   * database, put it in the cache, and then use it.
   *
   * @param SOPInstanceUID Unique slice identifier.
   * @return The corresponding attribute list.
   */
  def getAlBySop(SOPInstanceUID: String): AttributeList = {
    if (alCache.contains(SOPInstanceUID))
      alCache(SOPInstanceUID)
    else {
      val alList = DicomSeries.getBySopInstanceUID(SOPInstanceUID).flatMap(ds => ds.attributeListList)
      Trace.trace("alList.size: " + alList.size)
      if (alList.isEmpty)
        Trace.trace("empty")
      alList.foreach(al => alCache.put(Util.sopOfAl(al), al))

      // If for some reason there is a problem getting the DICOM from the database,
      // then assume it is not available.  Create an empty attribute list which will
      // result in a bunch of NA's being displayed.  This is not ideal, but better
      // than failing.  Technically even the SOPInstanceUID is not needed, but it
      // seems like it ought to be.
      if (!alCache.contains(SOPInstanceUID)) {
        val al = new AttributeList
        val attr = AttributeFactory.newAttribute(TagByName.SOPInstanceUID)
        attr.addValue(SOPInstanceUID)
        alCache.put(SOPInstanceUID, al)
      }
      alCache(SOPInstanceUID)
    }
  }

  def main(args: Array[String]): Unit = {
    DbSetup.init
    Trace.trace()
    val start = System.currentTimeMillis()
    (0 to 20).foreach(_ => println("-------------------------------------------------------------------------------"))
    val symFlat = new CsvSymmetryAndFlatness
    val text = symFlat.csvContent
    println("\n" + text + "\n")
    // val file = new File("""D:\tmp\symflat.csv""")
    val file = new File("""symflat.csv""") // put in local dir
    Util.writeFile(file, text)
    println("Wrote to file " + file.getAbsolutePath)
    val elapsed = System.currentTimeMillis() - start
    println("Done.  Elapsed ms: " + elapsed)
  }
}