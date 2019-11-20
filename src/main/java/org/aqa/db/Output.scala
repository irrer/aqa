package org.aqa.db

import slick.driver.PostgresDriver.api._
import scala.concurrent.ExecutionContext.Implicits.global
import org.aqa.Logging
import org.aqa.Config
import org.aqa.run.ProcedureStatus
import java.sql.Timestamp
import java.sql.Date
import org.aqa.web.WebServer
import java.io.File
import edu.umro.ScalaUtil.FileUtil

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
  dataValidity: String) // whether the data is valid or otherwise
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

  def insertOrUpdate = Db.run(Output.query.insertOrUpdate(this))

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
    (new OutputFiles(outputPK.get, outputPK.get, zippedContent)).insert
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

  def makeZipOfFiles: Array[Byte] = {
    FileUtil.readFileTreeToZipByteArray(Seq(dir), Seq[String](), Seq[File]())
  }
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

    def * = (
      outputPK.?,
      inputPK,
      directory,
      procedurePK,
      userPK,
      startDate,
      finishDate,
      dataDate,
      analysisDate,
      machinePK,
      status,
      dataValidity) <> ((Output.apply _)tupled, Output.unapply _)

    def inputFK = foreignKey("inputPK", inputPK, Input.query)(_.inputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def procedureFK = foreignKey("procedurePK", procedurePK, Procedure.query)(_.procedurePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def userFK = foreignKey("userPK", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)

  }

  val query = TableQuery[OutputTable]
  private val query2 = TableQuery[OutputTable]

  def get(outputPK: Long): Option[Output] = {
    val action = for { output <- Output.query if output.outputPK === outputPK } yield (output)
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Find the output given the name of its directory.  This parameter must exactly match the <code>output.directory</code> field.
   */
  def getByDirectory(dirName: String) = {
    val action = for { output <- Output.query if output.directory === dirName } yield (output)
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
    user_id: String);

  /**
   * Get an extended list of all outputs.
   */
  def extendedList(procedure: Set[Procedure], machine: Set[Machine], instPK: Option[Long], maxSize: Int): Seq[ExtendedValues] = {

    val search = for {
      output <- Output.query.map(o => (o.outputPK, o.startDate, o.inputPK, o.procedurePK, o.userPK))
      input <- Input.query.filter(i => i.inputPK === output._3).map(i => (i.dataDate, i.directory, i.machinePK))
      mach <- Machine.query.filter(m => m.machinePK === input._3).map(m => (m.id, m.institutionPK))
      inst <- Institution.query.filter(i => i.institutionPK === mach._2).map(i => (i.institutionPK, i.name))
      proc <- Procedure.query.filter(p => p.procedurePK === output._4).map(p => (p.name, p.version))
      user <- User.query.filter(u => u.userPK === output._5).map(u => u.id)
    } yield ((input._1, input._2, inst, mach._1, output._1, output._2, proc._1, proc._2, user))

    val filtered = {
      if (instPK.isDefined) search.filter(c => c._3._1 === instPK.get)
      else search
    }

    val sorted = filtered.sortBy(_._1).take(maxSize)

    val result = Db.run(sorted.result).map(a => new ExtendedValues(a._1, a._2, a._3._1, a._3._2, a._4, a._5, a._6, a._7, a._8, a._9))
    result
  }

  // can pass sorting function in using this horrendous type:
  //
  //    val k: jType = null
  //    k._5.description.reverse
  //
  //    def jfun(j: jType) = {
  //        val fwd = true
  //        val j0a = (j._4.name).reverse
  //        val j0 = j._4.name
  //        val j1 = j0.reverse
  //        val rev= if (fwd) j0 else j0.reverse
  //        j0
  //    }
  //
  //    def jfunRev(j: jType) = {
  //        j._4.name.reverse
  //    }

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
      output <- query if (output.inputPK.inSet(inputPKSet))
    } yield (output)
    val list = Db.run(action.result)
    list
  }

  def delete(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  val displayFilePrefix = "display"

  /**
   * If the given directory contains an output file, then return it.
   */
  def outputFile(directory: File): Option[File] = {
    val list = (directory).listFiles().filter { f => f.canRead && f.getName.toLowerCase.startsWith(Output.displayFilePrefix) }
    if (list.isEmpty) None
    else Some(list.head)
  }

  /**
   * Given an output, find out which other outputs it is redundant with.  Two outputs are
   * redundant if they are the created by the same procedure with the same machine with data
   * that has the same acquisition date.
   */
  def redundantWith(output: Output): Seq[Output] = {
    output.dataDate match {
      case Some(dataDate) => {
        val q = query.filter(o =>
          (o.machinePK === output.machinePK) &&
            (o.procedurePK === output.procedurePK) &&
            (o.outputPK =!= output.outputPK) &&
            (o.dataDate.isDefined && (o.dataDate === dataDate)))
        sortByStartDate(Db.run(q.result))
      }
      case _ => Seq[Output]()
    }
  }

  private def sortByStartDate(outputList: Seq[Output]): Seq[Output] = {
    def cmpr(a: Output, b: Output): Boolean = a.startDate.getTime < b.startDate.getTime
    outputList.sortWith(cmpr)
  }

  /**
   * Ensure that the output files and related input files are in the file system.  If
   * not, then get them from the database.
   */
  def ensureInputAndOutputFilesExist(output: Output) = {
    val inputDir = output.dir.getParentFile

    if (!inputDir.isDirectory) {
      Input.getFilesFromDatabase(output.inputPK, inputDir.getParentFile)
    }

    if (!output.dir.isDirectory) {
      Output.getFilesFromDatabase(output.outputPK.get, inputDir)
    }

  }

  /**
   * Return the list of redundant outputs, keeping the ones that users would want. The rules are:
   *
   *     Keep the latest successful output, even if there are newer ones that were not successful.
   *
   *     If the most recent output is not successful, then keep it (useful for diagnostics) but
   *     remove all previous unsuccessful outputs.
   *
   *     An output is successful if it has a status of 'passed', 'failed', or 'done', because all
   *     of these indicate that the process performed as expected, regardless of whether the data
   *     was 'good'.
   *
   * Two outputs are redundant if they are the created by the same procedure with the same machine
   * with data that has the same acquisition date.
   *
   * @deprecate
   */
  @deprecated("Not used.  This is too kind and gentle.  Remove old outputs regardless of crash status, including fail, crash, etc.. ")
  private def listRedundant(outputListUnsorted: Seq[Output]): Seq[Output] = {
    val outputList = sortByStartDate(outputListUnsorted)

    val goodStatus = Seq(ProcedureStatus.done, ProcedureStatus.pass, ProcedureStatus.fail).map(s => s.toString)
    def isGood(output: Output): Boolean = goodStatus.map(g => g.equals(output.status)).contains(true)

    val latestGood = outputList.indexWhere(g => isGood(g))
    val latestNotGood = outputList.indexWhere(b => !isGood(b))

    // list of indexes in outputList to keep
    val listToKeep: Seq[Int] = (latestGood, latestNotGood) match {
      case (-1, -1) => {
        logger.warn("removeRedundant Unexpected error, output is neither good or notGood") // nothing?
        Seq[Int]() // remove nothing
      }
      case (g, -1) => Seq(g) // all outputs good.  Keep only the latest
      case (-1, b) => Seq(b) // all outputs bad.  Keep only the latest
      case (g, b) if (g < b) => Seq(g) // at least on each of good and bad, but good is more recent
      case (g, b) => Seq(b, g) // at least on each of good and bad, but bad is more recent
    }
    val pkToKeep = listToKeep.map(k => outputList(k).outputPK.get)

    outputList.filter { o => o.outputPK.isDefined && (!pkToKeep.contains(o.outputPK.get)) }
  }

  /**
   * Make a list of all outputs that are redundant.  Two outputs are
   * redundant if they are the created by the same procedure with the same machine with data
   * that has the same acquisition date.
   */
  def redundant: Seq[Set[Output]] = {
    val q = for {
      (a, b) <- query join query2
      if (a.machinePK === b.machinePK) &&
        (a.procedurePK === b.procedurePK) &&
        (a.outputPK < b.outputPK) &&
        (a.dataDate.isDefined && b.dataDate.isDefined && (a.dataDate === b.dataDate))
    } yield (a, b)

    Db.run(q.result).map(p => Set(p._1, p._2))
  }

  /**
   * Make a list of all outputs put into sets where each the members in each set are redundant.  Two outputs are
   * redundant if they are the created by the same procedure with the same machine with data
   * that has the same acquisition date.
   */
  def redundantReduced(all: Seq[Set[Output]]): Seq[Set[Output]] = {
    def grp(seq: Seq[Set[Output]], rem: Seq[Set[Output]]): Seq[Set[Output]] = {
      if (rem.isEmpty) seq
      else {
        val i = seq.indexWhere(s => s.intersect(rem.head).nonEmpty)
        if (i < 0) {
          grp(seq :+ rem.head, rem.tail)
        } else {
          grp(seq.updated(i, seq(i).union(rem.head)), rem.tail)
        }
      }
    }

    def grpGrp(seq: Seq[Set[Output]]): Seq[Set[Output]] = {
      val g = grp(Seq[Set[Output]](), seq)
      if (g.size == seq.size) g
      else grpGrp(g)
    }

    grpGrp(all)
  }

  /**
   * Get the files from the database and restore them to the file system.  Overwrite any files that are already there.
   *
   * It is up to the caller to determine if the files need to be restored.
   *
   */
  def getFilesFromDatabase(outputPK: Long, dir: File) = {
    dir.mkdirs
    // Steps are done on separate lines so that if there is an error/exception it can be precisely
    // tracked.  It is up to the caller to catch any exceptions and act accordingly.
    val outputFilesOption = OutputFiles.getByOutput(outputPK)
    val outputFiles = outputFilesOption.get
    FileUtil.writeByteArrayZipToFileTree(outputFiles.zippedContent, dir)
  }

  /**
   * Given a machine PK, get the input and output containing the latest LOC baseline files.
   */
  def getLatestLOCBaselineDir(machinePK: Long, webInterface: String): Option[(Input, Output)] = {
    val search = for {
      machine <- Machine.query.filter(m => m.machinePK === machinePK)
      procedure <- Procedure.query.filter(p => p.webInterface === webInterface)
      output <- Output.query.filter(o => (o.procedurePK === procedure.procedurePK) && (o.status === ProcedureStatus.done.toString))
      input <- Input.query.filter(i => (i.inputPK === output.inputPK) && (i.machinePK === machinePK))
    } yield ((input, output))

    val sorted = search.sortBy(_._2.startDate)

    Db.run(sorted.result).lastOption
  }

  def main(args: Array[String]): Unit = {
    println("Starting Output.main")
    val valid = Config.validate
    DbSetup.init

    if (false) {
      println("\n\ntesting redundant")
      val red = redundant
      red.map(r => println("  r: " + r.map(r => r.outputPK.get).toSeq.sorted.mkString(", ")))
      val redred = redundantReduced(red)
      println("\n\ntesting redundantReduced")
      println("redred size: " + redred.size)
      redred.map(r => println("  rr: " + r.map(r => r.outputPK.get).toSeq.sorted.mkString(", ")))
      System.exit(0)
    }

    if (false) {
      println("testing redundantWith")
      val redun = redundantWith(listByInputPK(73).head)
      println("redun size: " + redun.size)
      redun.map(o => println(o))
      System.exit(0)
    }

    if (false) {
      println("starting input find")
      val list = listByInputPK(70)
      println("list size: " + list.size)
      list.map(o => println(o))
      System.exit(0)
    }

    if (false) {
      val output = new Output(
        None, // primary key
        (4).toLong, // input data
        "dir", // directory containing data
        1.toLong, // procedure that created this output
        Some((6).toLong), // user that created this output
        startDate = new Timestamp(System.currentTimeMillis), // when procedure was started
        finishDate = Some(new Timestamp(System.currentTimeMillis + 1)), // when procedure finished
        dataDate = None,
        analysisDate = None,
        machinePK = None,
        status = "testing",
        dataValidity = DataValidity.valid.toString) // termination status
      println("output: " + output)
      val result = output.insert
      println("result: " + result)
    }
    // println("======== output: " + get(5))
    // println("======== output delete: " + delete(5))
  }
}
