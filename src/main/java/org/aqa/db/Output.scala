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

import edu.umro.ScalaUtil.FileUtil
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Db.driver.api._
import org.aqa.run.ProcedureStatus
import org.aqa.web.GetSeries
import org.aqa.web.WebServer

import java.io.File
import java.sql.Timestamp
import java.util.Date

case class Output(
    outputPK: Option[Long], // primary key
    inputPK: Long, // input data
    directory: String, // directory containing data
    procedurePK: Long, // procedure that created this output
    userPK: Option[Long], // user that created this output
    startDate: Timestamp, // when procedure was started
    finishDate: Option[Timestamp], // when procedure finished
    dataDate: Option[Timestamp], // optionally supplied by analysis procedure to indicate or override Input.dataDate
    analysisDate: Option[Timestamp], // optionally supplied by analysis procedure to indicate prior analysis
    machinePK: Option[Long], // optionally supplied by analysis procedure to indicate treatment machine
    status: String, // termination status
    dataValidity: String
) // whether the data is valid or otherwise
{

  /**
    * Insert into table, returning the row that was inserted.  Note that outputPK in the return value is defined.
    */
  def insert: Output = {
    val insertQuery = Output.query returning Output.query.map(_.outputPK) into ((output, outputPK) => output.copy(outputPK = Some(outputPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def getUser: Option[User] = if (userPK.isDefined) User.get(userPK.get) else None

  def insertOrUpdate(): Int = {
    Db.run(Output.query.insertOrUpdate(this))
  }

  def dir: File = WebServer.fileOfResultsPath(directory)

  /**
    * Update the status and finish date.
    */
  def updateStatusAndFinishDate(sts: String, finDate: Date): Unit = {
    val finTimestamp = new Timestamp(finDate.getTime)
    Db.run(Output.query.filter(_.outputPK === outputPK.get).map(o => (o.status, o.finishDate)).update((sts, Some(finTimestamp))))
  }

  def updateData(zippedContent: Array[Byte]): OutputFiles = {
    OutputFiles.deleteByOutputPK(outputPK.get)
    val outputFiles = new OutputFiles(outputPK.get, outputPK.get, zippedContent)
    outputFiles.insert
    outputFiles
  }

  /**
    * Get the elapsed time in ms.  If the output is still running, then get the elapsed time so far.
    */
  def elapsedTime: Long = {
    val elapsed =
      if (finishDate.isDefined)
        finishDate.get.getTime - startDate.getTime
      else System.currentTimeMillis - startDate.getTime
    elapsed.abs
  }

  override def equals(o: Any): Boolean = {
    val other = o.asInstanceOf[Output]
    outputPK.isDefined && other.outputPK.isDefined && (outputPK.get == other.outputPK.get)
  }

  /** True if the data is valid. */
  val isValid: Boolean = dataValidity.equals(DataValidity.valid.toString)

  def makeZipOfFiles: Array[Byte] = {
    FileUtil.readFileTreeToZipByteArray(Seq(dir), Seq[String](), Seq[File]())
  }

  override def toString: String = {
    "outputPK: " + outputPK +
      "    inputPK: " + inputPK +
      "    dir: " + dir +
      "    procedurePK: " + procedurePK +
      "    userPK: " + userPK +
      "    startDate: " + startDate +
      "    dataDate: " + dataDate +
      "    analysisDate: " + analysisDate +
      "    machinePK: " + machinePK +
      "    status: " + status +
      "    dataValidity: " + dataValidity
  }

  def statusOf: ProcedureStatus.Value = ProcedureStatus.stringToProcedureStatus(status).get
}

object Output extends Logging {

  class OutputTable(tag: Tag) extends Table[Output](tag, "output") {

    def outputPK = column[Long]("outputPK", O.PrimaryKey, O.AutoInc)

    def inputPK = column[Long]("inputPK")

    def directory = column[String]("directory")

    def procedurePK = column[Long]("procedurePK")

    def userPK = column[Option[Long]]("userPK")

    def startDate = column[Timestamp]("startDate")

    def finishDate = column[Option[Timestamp]]("finishDate")

    def dataDate = column[Option[Timestamp]]("dataDate")

    def analysisDate = column[Option[Timestamp]]("analysisDate")

    def machinePK = column[Option[Long]]("machinePK")

    def status = column[String]("status")

    def dataValidity = column[String]("dataValidity")

    def * = (outputPK.?, inputPK, directory, procedurePK, userPK, startDate, finishDate, dataDate, analysisDate, machinePK, status, dataValidity) <> (Output.apply _ tupled, Output.unapply)

    def inputFK = foreignKey("Output_inputPKConstraint", inputPK, Input.query)(_.inputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)

    def procedureFK = foreignKey("Output_procedurePKConstraint", procedurePK, Procedure.query)(_.procedurePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)

    def userFK = foreignKey("Output_userPKConstraint", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Restrict)
  }

  val query = TableQuery[OutputTable]

  /** Query the Output table, restricting results to those whose dataValidity is valid. */
  val valid = Output.query.filter(o => o.dataValidity === DataValidity.valid.toString)

  def get(outputPK: Long): Option[Output] = {
    val action = for { output <- Output.query if output.outputPK === outputPK } yield output
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Find the output given the name of its directory.  This parameter must exactly match the <code>output.directory</code> field.
    */
  def getByDirectory(dirName: String): Option[Output] = {
    val action = for { output <- Output.query if output.directory === dirName } yield output
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all outputs.
    */
  def list: Seq[Output] = Db.run(query.result)

  case class ExtendedValues(
      input_dataDate: Option[Timestamp],
      input_directory: Option[String],
      institutionPK: Long,
      institution_name: String,
      machine_id: String,
      output_outputPK: Long,
      output_startDate: Timestamp,
      procedure_name: String,
      procedure_version: String,
      user_id: String,
      dataValidity: DataValidity.Value,
      note: Option[String]
  ) {}

  /**
    * Convert text to a DataValidity state.  Default to 'valid' if the database contains an unknown value.
    * @param text Text in database.
    * @return
    */
  private def toDV(text: String): DataValidity.Value = DataValidity.stringToDataValidityWithDefault(text, DataValidity.valid)

  /**
    * Get an extended list of all outputs.
    */
  def extendedList(instPK: Option[Long], maxSize: Int, date: Option[Date] = None): Seq[ExtendedValues] = {

    val search = for {
      output <- Output.query.map(o => (o.outputPK, o.startDate, o.inputPK, o.procedurePK, o.userPK, o.dataValidity))
      input <- Input.query.filter(i => i.inputPK === output._3).map(i => (i.dataDate, i.directory, i.machinePK))
      mach <- Machine.query.filter(m => m.machinePK === input._3).map(m => (m.id, m.institutionPK))
      inst <- Institution.query.filter(i => i.institutionPK === mach._2).map(i => (i.institutionPK, i.name))
      proc <- Procedure.query.filter(p => p.procedurePK === output._4).map(p => (p.name, p.version))
      user <- User.query.filter(u => u.userPK === output._5).map(u => u.id)
    } yield (input._1, input._2, inst, mach._1, output._1, output._2, proc._1, proc._2, user, output._6)

    val filteredByInst = {
      if (instPK.isDefined) search.filter(c => c._3._1 === instPK.get)
      else search
    }

    val filteredByDate = {
      if (date.isDefined) {
        val minDate = new Timestamp(edu.umro.ScalaUtil.Util.roundToDate(date.get).getTime)
        val maxDate = new Timestamp(minDate.getTime + (24 * 60 * 60 * 1000))
        search.filter(c => (c._1 >= minDate) && (c._1 < maxDate))
      } else filteredByInst
    }

    val sorted = filteredByDate.sortBy(_._6.desc).take(maxSize)
    val noteMap = OutputNote.list().map(n => (n.outputPK, n.contentAsText)).toMap

    val result = Db.run(sorted.result).map(a => ExtendedValues(a._1, a._2, a._3._1, a._3._2, a._4, a._5, a._6, a._7, a._8, a._9, toDV(a._10), noteMap.get(a._5)))
    result
  }

  /**
    * Get an extended list of all outputs.
    */
  def extendedList(outputPkSet: Set[Long]): Seq[ExtendedValues] = {

    val search = for {
      output <- Output.query.filter(o => o.outputPK.inSet(outputPkSet)).map(o => (o.outputPK, o.startDate, o.inputPK, o.procedurePK, o.userPK, o.dataValidity))
      input <- Input.query.filter(i => i.inputPK === output._3).map(i => (i.dataDate, i.directory, i.machinePK))
      mach <- Machine.query.filter(m => m.machinePK === input._3).map(m => (m.id, m.institutionPK))
      inst <- Institution.query.filter(i => i.institutionPK === mach._2).map(i => (i.institutionPK, i.name))
      proc <- Procedure.query.filter(p => p.procedurePK === output._4).map(p => (p.name, p.version))
      user <- User.query.filter(u => u.userPK === output._5).map(u => u.id)
    } yield (input._1, input._2, inst, mach._1, output._1, output._2, proc._1, proc._2, user, output._6)

    val sorted = search.sortBy(_._6.desc)

    val noteMap = OutputNote.list().map(n => (n.outputPK, n.contentAsText)).toMap

    val result = Db.run(sorted.result).map(a => ExtendedValues(a._1, a._2, a._3._1, a._3._2, a._4, a._5, a._6, a._7, a._8, a._9, toDV(a._10), noteMap.get(a._5)))
    result
  }

  /**
    * Get a list of all outputs with the given status and their associated procedures.
    */
  def listWithStatus(status: ProcedureStatus.Value): Seq[(Output, Procedure)] = {
    val statusText = status.toString
    val action = for {
      output <- Output.query if output.status === statusText
      procedure <- Procedure.query if procedure.procedurePK === output.procedurePK
    } yield (output, procedure)
    val list = Db.run(action.result)
    list
  }

  def listByInputPK(inputPK: Long): Seq[Output] = {
    val q = query.filter(_.inputPK === inputPK)
    Db.run(q.result)
  }

  /**
    * Get a list of all outputs that have an input in the set.
    */
  def getByInputPKSet(inputPKSet: Set[Long]): Seq[Output] = {
    val action = for {
      output <- query if output.inputPK.inSet(inputPKSet)
    } yield output
    val list = Db.run(action.result)
    list
  }

  def delete(outputPK: Long): Int = {
    get(outputPK) match {
      case Some(output) =>
        logger.info("Deleting output: " + output)
        try {
          val machine = Machine.get(output.machinePK.get)
          if (machine.isDefined)
            GetSeries.remove(machine.get.institutionPK)
        } catch {
          case t: Throwable => logger.warn("Error while removing GetSeries cache entries: " + fmtEx(t))
        }
      case None =>
        logger.info("outputPK " + outputPK + " does not exist but still attempting to delete it")
    }

    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    val count = Db.run(action)
    count
  }

  val displayFilePrefix = "display"

  /**
    * If the given directory contains an output file, then return it.
    */
  def outputFile(directory: File): Option[File] = {
    val list = directory.listFiles().filter { f => f.canRead && f.getName.toLowerCase.startsWith(Output.displayFilePrefix) }
    list.headOption
  }

  /**
    * Ensure that the output files and related input files are in the file system.  If
    * not, then get them from the database.
    */
  def ensureInputAndOutputFilesExist(output: Output): AnyVal = {
    val inputDir = output.dir.getParentFile

    if (!inputDir.isDirectory) {
      Input.getFilesFromDatabase(output.inputPK, inputDir.getParentFile)
      Output.getFilesFromDatabase(output.outputPK.get, inputDir)
    } else {
      if (!output.dir.isDirectory) {
        Output.getFilesFromDatabase(output.outputPK.get, inputDir)
      }
    }
  }

  /**
    * Get the files from the database and restore them to the file system.  Overwrite any files that are already there.
    *
    * It is up to the caller to determine if the files need to be restored.
    *
    */
  def getFilesFromDatabase(outputPK: Long, dir: File): Boolean = {
    Util.mkdirs(dir)
    // Steps are done on separate lines so that if there is an error/exception it can be precisely tracked.
    OutputFiles.getByOutput(outputPK) match {
      case Some(outputFiles) =>
        try {
          FileUtil.writeByteArrayZipToFileTree(outputFiles.zippedContent, dir)
          true
        } catch {
          case t: Throwable =>
            logger.warn("Unable to reinstate output files for output " + outputPK + " in dir " + dir.getAbsolutePath + " exception: " + t + " : " + fmtEx(t))
            false
        }
      case _ =>
        logger.info("Unable to reinstate output files for output " + outputPK + " in dir " + dir.getAbsolutePath)
        false
    }
  }

  /**
    * Given a machine PK, get the input and output containing the latest LOC baseline files.
    */
  def getLatestLOCBaselineDir(machinePK: Long, webInterface: String): Option[(Input, Output)] = {
    val search = for {
      //machine <- Machine.query.filter(m => m.machinePK === machinePK)
      procedure <- Procedure.query.filter(p => p.webInterface === webInterface)
      output <- Output.query.filter(o => (o.procedurePK === procedure.procedurePK) && (o.status === ProcedureStatus.done.toString))
      input <- Input.query.filter(i => (i.inputPK === output.inputPK) && (i.machinePK === machinePK))
    } yield (input, output)

    val sorted = search.sortBy(_._2.startDate)

    Db.run(sorted.result).lastOption
  }

  /**
    * Get the most recently  processed output that establishes a set of LOC baseline files.
    * <p/>
    * Note that this is the most recently processed.   It does not use the dataData.  This allows
    * the user to control which baseline to use.
    * <p/>
    * @param machinePK Machine PK
    * @return Most recent or None.
    */
  def getMostRecentLOCBaselineOutput(machinePK: Long): Option[Output] = {
    val procedurePK = Procedure.ProcOfLOCBaseline.get.procedurePK.get
    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
    } yield output

    val list = Db.run(search.result)
    val sorted = list.sortBy(_.analysisDate.get.getTime)
    sorted.lastOption
  }

  /**
    * Get a list of outputs from the given institution whose data is >= begin and < end.
    */
  def getOutputByDateRange(institutionPK: Long, dataDateBegin: Timestamp, dataDateEnd: Timestamp): Seq[Output] = {
    val search = for {
      machPK <- Machine.query.filter(m => m.institutionPK === institutionPK).map(m => m.machinePK)
      output <- Output.query.filter(o => (o.machinePK === machPK) && o.dataDate.isDefined && (o.dataDate >= dataDateBegin) && (o.dataDate < dataDateEnd))
    } yield output
    val seq = Db.run(search.result)
    seq
  }

  /**
    * Get a list of outputs, restricted by the parameters.
    *
    * This is designed to support paging through a large list of outputs.
    *
    * @param institutionPK Only from this institution
    * @param date Only outputs on or before this date, or before if count is negative.
    * @param count Only this many or fewer.  If count is <= 0, then take rows before dateOnOrBefore
    * @param procedurePK Only for this procedure.
    * @return List of outputs, sorted by date, most recent first.
    */
  def getOutputChunk(institutionPK: Long, date: Timestamp, count: Int, procedurePK: Long): Seq[Output] = {

    val search = for {
      machPK <- Machine.query.filter(m => m.institutionPK === institutionPK).map(m => m.machinePK)
      output <- Output.query.filter(o =>
        (o.machinePK === machPK) &&
          o.dataDate.isDefined &&
          (o.procedurePK === procedurePK) &&
          (o.machinePK === machPK)
      )
    } yield output
    if (count > 0) {
      val chunk = search.filter(o => o.dataDate <= date).sortBy(_.dataDate.desc).take(count)
      val seq = Db.run(chunk.result)
      seq
    } else {
      val chunk = search.filter(o => o.dataDate > date).sortBy(_.dataDate.asc).take(-count)
      val seq = Db.run(chunk.result).reverse
      seq
    }
  }

  def redundantWith(output: Output): Seq[Output] = {
    val dicomSeriesUIDSet = DicomSeries.getByInputPK(output.inputPK).map(ds => ds.seriesInstanceUID).toSet

    val dicomSeriesInputPKSet: Set[Long] = {
      val search = for {
        dsList <- DicomSeries.query.filter(ds => ds.seriesInstanceUID.inSet(dicomSeriesUIDSet) && ((ds.modality === "RTIMAGE") || (ds.modality === "CT"))).map(ds => ds.inputPK)
      } yield {
        dsList
      }

      val result = Db.run(search.result).flatten.toSet
      result
    }

    val outputList = {
      val search = for {
        out <- Output.query.filter(o => o.inputPK.inSet(dicomSeriesInputPKSet) && (o.procedurePK === output.procedurePK) && (o.outputPK =!= output.outputPK))
      } yield {
        out
      }
      val result = Db.run(search.result)
      result
    }

    outputList
  }

  /**
    * Get the number of outputs from the the given institution and procedure.
    * @param institutionPK Must match this institution.
    * @param procedurePK Must match this procedure.
    * @return Number of outputs.
    */
  def getCount(institutionPK: Long, procedurePK: Long): Int = {
    val search = for {
      machPK <- Machine.query.filter(m => m.institutionPK === institutionPK).map(m => m.machinePK)
      outPK <- Output.query.filter(o => (o.machinePK === machPK) && (o.procedurePK === procedurePK)).map(_.outputPK)
    } yield outPK
    val size = Db.run(search.result).size
    size
  }

  /**
    * Get the list of outputs sorted by data data for the given machine and procedure.
    * @param machinePK   Machine to match.
    * @param procedurePK Procedure to match.
    * @return List of outputs, sorted by data date.
    */
  def getByMachineAndProcedure(machinePK: Long, procedurePK: Long): Seq[Output] = {
    val search = for {
      outPK <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
    } yield outPK

    val list = Db.run(search.result)
    list.sortBy(_.dataDate.get.getTime)
  }

  /**
    * Get a list outputs for the given machine.  The outputs should be distinct by their procedurePK.  For example,
    * if there were outputs for the given machine for Phase2 and LOC, then one of each would be returned.
    * @param machinePK Match this machine PK.
    * @return List of outputs that are distinct by procedurePK.
    */
  def machineProcedureList(machinePK: Long): Seq[Output] = {
    val search = for {
      output <- Output.query.filter(o => o.machinePK === machinePK).distinctOn(_.procedurePK)
    } yield output
    val list = Db.run(search.result)
    logger.info(s"""machineProcedureList   machinePK: $machinePK  procedurePK list: ${list.map(_.procedurePK).sorted.mkString("  ")}""")
    list
  }
}
