/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.db

import Db.driver.api._
import org.aqa.Logging
import org.aqa.Config

import java.sql.Timestamp
import java.io.File
import org.aqa.web.WebServer
import edu.umro.ScalaUtil.FileUtil

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import edu.umro.ScalaUtil.Trace
import org.aqa.Util

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
    if (inputPK.isEmpty) throw new RuntimeException("Attempting to search for an input using a null inputPK: " + this)
    Db.run(Input.query.filter(_.inputPK === inputPK.get).map(i => (i.directory)).update((Some(dirName))))
  }

  /**
   * Update content
   */
  def putFilesInDatabase(inputDir: File): InputFiles = {
    val outputDirList = Output.listByInputPK(inputPK.get).map(o => o.dir)
    val zippedContent = FileUtil.readFileTreeToZipByteArray(Seq(inputDir), Seq[String](), outputDirList)
    InputFiles.deleteByInputPK(inputPK.get)
    val inputFiles = new InputFiles(inputPK.get, inputPK.get, zippedContent)
    inputFiles.insert
    inputFiles
  }

  /**
   * Update content in parallel thread.
   */
  def putFilesInDatabaseFuture(inputDir: File): Future[InputFiles] = {
    val later = Future {
      val future = putFilesInDatabase(inputDir)
      future
    }
    later
  }

}

object Input extends Logging {

  class InputTable(tag: Tag) extends Table[Input](tag, "input") {

    def inputPK = column[Long]("inputPK", O.PrimaryKey, O.AutoInc) // primary key
    def directory = column[Option[String]]("directory") // directory where files reside
    def uploadDate = column[Timestamp]("uploadDate") // when files were uploaded
    def userPK = column[Option[Long]]("userPK") // user that uploaded files
    def machinePK = column[Option[Long]]("machinePK") // associated machine
    def patientId = column[Option[String]]("patientId") // patient ID for potential tracking
    def dataDate = column[Option[Timestamp]]("dataDate") // when data was generated

    def * = (
      inputPK.?,
      directory,
      uploadDate,
      userPK,
      machinePK,
      patientId,
      dataDate) <> ((Input.apply _)tupled, Input.unapply _)

    def userFK = foreignKey("Input_userPKConstraint", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Restrict)
    def machineFK = foreignKey("Input_machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Restrict)
  }

  val query = TableQuery[InputTable]

  def get(inputPK: Long): Option[Input] = {
    val action = for {
      input <- query if input.inputPK === inputPK
    } yield (input)
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Get a list of inputs that reference the given machine.
   */
  def getByMachine(machinePK: Long): Seq[Input] = {
    val action = for {
      input <- query if input.machinePK === machinePK
    } yield (input)
    val list = Db.run(action.result)
    list
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

  /**
   * Find the input given the name of its directory.  This parameter must exactly match the <code>input.directory</code> field.
   */
  def getByDirectory(dirName: String) = {
    val action = for { input <- Input.query if input.directory === dirName } yield (input)
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Find the number of inputs that reference this user.
   */
  def getUserRefernceCount(userPK: Long): Int = {
    val action = Input.query.filter(input => input.userPK === userPK).size
    val count = Db.run(action.result)
    count
  }

  def delete(inputPK: Long): Int = {
    get(inputPK) match {
      case Some(input) =>
        logger.info("Deleting input: " + input)
      case None =>
        logger.info("inputPK " + inputPK + " does not exist but still attempting to delete it")
    }
    val q = query.filter(_.inputPK === inputPK)
    val start = System.currentTimeMillis
    val action = q.delete
    val count = Db.run(action)
    val elapsed = System.currentTimeMillis - start
    logger.info("input has been deleted: " + inputPK + "    Elapsed ms: " + elapsed)
    count
  }

  /**
   * Get the files from the database and restore them to the file system.  Overwrite any files that are already there.
   *
   * It is up to the caller to determine if the files need to be restored.
   *
   */
  def getFilesFromDatabase(inputPK: Long, dir: File): Unit = {
    // Steps are done on separate lines so that if there is an error/exception it can be precisely
    // tracked.  It is up to the caller to catch any exceptions and act accordingly.
    Util.mkdirs(dir)
    val inputFilesSeq = InputFiles.getByInputPK(inputPK)
    inputFilesSeq.map(inFiles => FileUtil.writeByteArrayZipToFileTree(inFiles.zippedContent, dir))
  }

  private def fixUp = {

    def getAllInputPK = {
      val action = for { input <- Input.query } yield (input.inputPK)
      val list = Db.run(action.result)
      list
    }

    def getAllDicomSeries = {
      val action = for { ds <- DicomSeries.query } yield (ds.dicomSeriesPK, ds.inputPK)
      val list = Db.run(action.result)
      list.filter(dsi => dsi._2.isDefined).map(dsi => (dsi._1, dsi._2.get))
    }

    val allInputPK = getAllInputPK.toSet
    val allDicomSeries = getAllDicomSeries

    val toDelete = allDicomSeries.filterNot(dsi => allInputPK.contains(dsi._2))
    println("toDelete: " + toDelete.mkString("    "))
  }

  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init

    // val input = new Input(None, Some("input_dir"), new Timestamp(System.currentTimeMillis), Some(6), Some(2), None, None)
    // input.insert
    // println("======== input: " + get(5))
    //println("======== input delete: " + delete(5))

    fixUp

  }
}
