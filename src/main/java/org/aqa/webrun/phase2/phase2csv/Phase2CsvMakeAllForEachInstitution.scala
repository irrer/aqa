package org.aqa.webrun.phase2.phase2csv

import edu.umro.ScalaUtil.FileUtil
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.web.WebUtil
import org.aqa.webrun.phase2.phase2csv.Phase2Csv.zipFileName

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

/**
  * Generate all CSV files.
  */
object Phase2CsvMakeAllForEachInstitution extends Logging {

  /**
    * Generate a user friendly web page to act as an index and write it as a file.  This
    * also creates the ZIP file which contains all the CSV files.
    */
  def generateIndex(csvDir: File): Unit = {
    val csvList = Util.listDirFiles(csvDir).filter(_.getName.endsWith(".csv"))

    def fileToRow(file: File): Elem = {

      val date = {
        val dateFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm")
        dateFormat.format(new Date(file.lastModified()))
      }

      val suffixPattern = ".....................csv$"

      <tr>
        <td>
          <a href={file.getName}>{file.getName.replaceAll(suffixPattern, "")}</a>
        </td>
        <td>
          {date}
        </td>
        <td>
          <a href={"../../CSV/" + file.getName.replaceAll(suffixPattern, ".html")}>Column Definitions</a>
        </td>
      </tr>
    }

    val content = {
      <div class="col-md-8 col-md-offset-2">
        <center>
          <h2>Index of CSV Files</h2>
          <em>These files are periodically generated and contain data from all institutions.</em>
        </center>
        <table class="table table-bordered" style="margin:30px;">
          <tr>
            <th>
              Type of data.  Click to download
            </th>
            <th>
              Date generated
            </th>
            <th>
              Description of Columns
            </th>
          </tr>
          {csvList.map(fileToRow)}
        </table>
        <center>
          <a href={zipFileName}>Download zipped version of all CSV files.</a>
        </center>
        {Phase2Csv.notesTag}
      </div>
    }

    val zipFile = new File(csvDir, zipFileName)
    FileUtil.readFileTreeToZipFile(csvList, excludePatternList = Seq(), excludeFileList = Seq(), zipFile)

    val text = WebUtil.wrapBody(content, "CSV Index").replace(Phase2Csv.notesTag, Phase2Csv.readNotes())
    Phase2Csv.csvDir.mkdirs()
    val file = new File(csvDir, "index.html")
    Util.writeFile(file, text)
    logger.info("Wrote " + file.length() + " bytes to file " + file.getAbsolutePath)
  }

  private def writeInstitutionIndex(institutionPK: Long): Unit = {}

  def main(args: Array[String]): Unit = {
    DbSetup.init
    (new PopulateDicomCsv).populateAll() // Get the DICOM column data up to date.

    (new CenterDoseCsv).updateAllInstitutionFiles()
    (new CollimatorCenteringCsv).updateAllInstitutionFiles()
    (new CollimatorPositionCsv).updateAllInstitutionFiles()
    (new LeafPositionCsv).updateAllInstitutionFiles()
    // MaintenanceCsv.updateFiles() // special because it is not associate with Output or DICOM
    (new MetadataCheckCsv).updateAllInstitutionFiles()
    (new SymmetryAndFlatnessCsv).updateAllInstitutionFiles()
    (new VMAT_T2_DR_GSCsv).updateAllInstitutionFiles()
    (new VMAT_T2_DG_RSCsv).updateAllInstitutionFiles()
    (new VMAT_T3MLCSpeedCsv).updateAllInstitutionFiles()
    (new WedgePointCsv).updateAllInstitutionFiles()

    MetadataCache.metadataCache.institutionNameMap.keys.foreach(institutionPK => generateIndex(Phase2Csv.institutionCsvDir(institutionPK)))
  }

}
