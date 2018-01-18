package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import java.sql.Timestamp
import java.io.File
import org.aqa.web.WebServer
import edu.umro.ScalaUtil.FileUtil

case class Input(
  inputPK: Option[Long], // primary key
  directory: Option[String], // main directory where procedure related data is stored.  Contains input and 0 or more output directories.  Is relative to configuration value for <code>Config.DataDir</code>.
  uploadDate: Timestamp, // when data was loaded into this system
  userPK: Option[Long], // user that ran the procedure
  machinePK: Option[Long], // procedure was run on this machine.  Machine is optional to support procedures that do not correlate to a single machine.
  patientId: Option[String], // patient ID if applicable.  Often derived from associated DICOM files
  dataDate: Option[Timestamp] // earliest date when data was acquired.  Often derived from associated DICOM files.
) {

  def insert: Input = {
    val insertQuery = Input.query returning Input.query.map(_.inputPK) into ((input, inputPK) => input.copy(inputPK = Some(inputPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(Input.query.insertOrUpdate(this))

  def dir: File = WebServer.fileOfResultsPath(directory.get)

  /**
   * Update the directory and content from that directory, excluding child output directories.  Returns number of
   * records updated, which should always be one.  If it is zero then it is probably because the object is not
   * in the database.
   */
  def updateDirectory(inDir: File) = {
    val dirName = WebServer.fileToResultsPath(inDir)
    Db.run(Input.query.filter(_.inputPK === inputPK.get).map(i => (i.directory)).update((Some(dirName))))
  }

  /**
   * Update content
   */
  def putFilesInDatabase(inputDir: File): InputFiles = {
    val outputDirList = Output.listByInputPK(inputPK.get).map(o => o.dir)
    val zippedContent = FileUtil.readFileTreeToZipByteArray(Seq(inputDir), Seq[String](), outputDirList)
    InputFiles.deleteByInputPK(inputPK.get)
    (new InputFiles(inputPK.get, inputPK.get, zippedContent)).insert
  }

}

object Input extends Logging {

  class InputTable(tag: Tag) extends Table[Input](tag, "input") {

    def inputPK = column[Long]("inputPK", O.PrimaryKey, O.AutoInc)
    def directory = column[Option[String]]("directory")
    def uploadDate = column[Timestamp]("uploadDate")
    def userPK = column[Option[Long]]("userPK")
    def machinePK = column[Option[Long]]("machinePK")
    def patientId = column[Option[String]]("patientId")
    def dataDate = column[Option[Timestamp]]("dataDate")

    def * = (
      inputPK.?,
      directory,
      uploadDate,
      userPK,
      machinePK,
      patientId,
      dataDate) <> ((Input.apply _)tupled, Input.unapply _)

    def userFK = foreignKey("userPK", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def machineFK = foreignKey("machinePK", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[InputTable]

  def get(inputPK: Long): Option[Input] = {
    val action = for {
      input <- query if input.inputPK === inputPK
    } yield (input)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Given the input directory, return the text for the relative path of the directory.  This path is
   * used so that files will be independent of the configuration.
   */
  def directoryOf(inputDir: File): String = {
    val resultsDirName = Config.resultsDirFile.getAbsolutePath
    val inputDirName = inputDir.getAbsolutePath
    if (inputDirName.startsWith(resultsDirName)) inputDirName.substring(Config.resultsDirFile.getAbsolutePath.size)
    else throw new RuntimeException("Input directory should be placed in results directory " + resultsDirName + " but is " + inputDirName)
  }

  def delete(inputPK: Long): Int = {
    val q = query.filter(_.inputPK === inputPK)
    logger.info("deleting input " + inputPK)
    val action = q.delete
    Db.run(action)
  }

  /**
   * Get the files from the database and restore them to the file system.  Overwrite any files that are already there.
   *
   * It is up to the caller to determine if the files need to be restored.
   *
   */
  def getFilesFromDatabase(inputPK: Long, dir: File) = {
    // Steps are done on separate lines so that if there is an error/exception it can be precisely
    // tracked.  It is up to the caller to catch any exceptions and act accordingly.
    val inputFilesOption = InputFiles.getByInput(inputPK)
    val inputFiles = inputFilesOption.get
    FileUtil.writeByteArrayZipToFileTree(inputFiles.zippedContent, dir)
  }

  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    val input = new Input(None, Some("input_dir"), new Timestamp(System.currentTimeMillis), Some(6), Some(2), None, None)

    input.insert

    println("======== input: " + get(5))
    //println("======== input delete: " + delete(5))
  }
}
