package org.aqa.webrun.phase2.phase2csv

import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.db.MaintenanceRecord

import java.io.File

/**
  * Support both the creation of a maintenance record CSV file and the insertion of
  * maintenance records within another CSV file.
  *
  * @param metadataCache Institution and machine information.
  */
class MaintenanceCsv(metadataCache: MetadataCache) {

  private case class MaintenanceCsv(header: String, toText: MaintenanceRecord => String) {
    def toTextSafe(mt: MaintenanceRecord): String = Util.textToCsv(toText(mt))
  }

  private val colList = Seq(
    MaintenanceCsv("Institution", (mt: MaintenanceRecord) => metadataCache.institutionNameMap(metadataCache.machineMap(mt.machinePK).institutionPK)),
    MaintenanceCsv("Machine", (mt: MaintenanceRecord) => metadataCache.machineMap(mt.machinePK).id),
    MaintenanceCsv("Effective Date", (mt: MaintenanceRecord) => Util.standardDateFormat.format(mt.creationTime)),
    MaintenanceCsv("Maintenance Category", (mt: MaintenanceRecord) => mt.category),
    MaintenanceCsv("Created by User", (mt: MaintenanceRecord) => metadataCache.userMap(mt.userPK)),
    MaintenanceCsv("Summary", (mt: MaintenanceRecord) => mt.summary),
    MaintenanceCsv("Description", (mt: MaintenanceRecord) => mt.description)
  )

  /**
    * Convert a maintenance record into a single CSV row
    * @param maintenanceRecord From database to convert
    * @return A single line of text
    */
  def toCsvText(maintenanceRecord: MaintenanceRecord): String = {
    colList.map(mtCsv => mtCsv.toTextSafe(maintenanceRecord)).mkString(",")
  }

  /** The CSV headers for maintenance records. */
  val headerText: String = colList.map(c => c.header).mkString(",")

  /**
    * Get all maintenance records grouping all records within an institution together, all records for
    * a machine together, and order chronologically.
    *
    * @return Grouped and ordered list.
    */
  def retrieveGroupedAndOrderedList(): Seq[MaintenanceRecord] = {

    /**
      * Make a string that will group all records within an institution together, all records for
      * a machine together, and order chronologically.
      *
      * @param maintenanceRecord Convert to comparable string.
      * @return String that will group and order records.
      */
    def compareCriteria(maintenanceRecord: MaintenanceRecord): String = {
      metadataCache.machineMap(maintenanceRecord.machinePK).institutionPK + " " + maintenanceRecord.machinePK + " " + maintenanceRecord.creationTime.getTime

    }
    MaintenanceRecord.getAllExceptBaseline().sortBy(compareCriteria)
  }

}

object MaintenanceCsv {

  /**
   * Create the MaintenanceRecord.csv file for all institutions.
   *
   * @param args Not used
   */
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    DbSetup.init
    val maintenanceCsv = new MaintenanceCsv(new MetadataCache)

    val mrList = maintenanceCsv.retrieveGroupedAndOrderedList()
    val csvContent = mrList.map(mr => maintenanceCsv.toCsvText(mr))
    val csvText = maintenanceCsv.headerText + "\n" + csvContent.mkString("\n")

    val file = new File(Phase2Csv.csvDir, "MaintenanceRecord.csv")
    Phase2Csv.csvDir.mkdirs()

    Util.writeFile(file, csvText)

    println("Elapsed time: " + Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start))
    println("Wrote to file " + file.getAbsolutePath)
  }

}