package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.Output

abstract class Phase2Csv[T] {

  /**
   * Define a column in a Phase2 CSV.
   *
   * @param header Name of column.
   * @param toText Converts given data to text that will be put in the cell.
   */
  case class Col(header: String, toText: T => String) {}

  protected def makeColList: Seq[Col]

  private val colList: Seq[Col] = makeColList

  protected def getAl(data: T): AttributeList

  protected def getOutput(data: T): Output

  private val machineCache = new MachineCache

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
    machineCache.headerText + "," + dataHeaderList
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
      new String(col.toText(dataSet))
    }).mkString(",")

    val machineText = machineCache.machineCsvMap(getOutput(dataSet).machinePK.get)
    machineText + "," + dataText
  }


  // List of all institutions.
  // val institutionList: Seq[Institution] = Institution.list

  private val machineList = Machine.list.sortBy(_.institutionPK)

  // List of all machines sorted so that institutions are grouped together.
  private val machineMap = {
    Machine.list.sortBy(_.institutionPK)
  }

  /**
   * Get the name of the institution.
   *
   * @param machine For this machine.
   * @return Anonymous name of institution.
   */
  private def institutionName(machine: Machine): String = {
    // institutionList.find(_.institutionPK.get == machine.institutionPK).head.name
    ???
  }


  // make the full list of CSV rows for all machines
  def rowList: Seq[String] = {
    Phase2Csv.clearAlCache()
    for (machine <- machineList) yield {
      val dataList = getData(machine.machinePK.get)
      // make the row list for this one machine
      val machineRowList = dataList.map(d => makeCsvRow(d))
      Phase2Csv.clearAlCache()
      machineRowList
    }.mkString("\n")
  }


  val csvContent: String = makeHeader() + "\n" + rowList

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
      alList.foreach(al => alCache.put(Util.sopOfAl(al), al))
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
    val elapsed = System.currentTimeMillis() - start
    println("Done.  Elapsed ms: " + elapsed)
  }
}