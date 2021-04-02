package org.aqa.db

import edu.umro.ScalaUtil.FileUtil
import org.aqa.Config
import org.aqa.db.Db.driver.api._

import java.io.File
import java.sql.Date

/**
 * A quality assurance procedure.
 *
 */
case class Procedure(
                      procedurePK: Option[Long], // Primary key
                      name: String, // human readable identifier
                      version: String, // code version, should be in numeric+dot format, as in 1.2.3
                      timeout: Float, // For 'runaway' programs.  Timeout in minutes after which the procedure should be terminated
                      created: Date, // time that this record was last modified
                      supportingUserPK: Long, // id of user that supports this procedure, usually the author
                      webInterface: String, // Class name of Restlet for running procedure
                      notes: String // Additional information on usage, inputs, limitations, etc.
                    ) {

  def insert: Procedure = {
    val insertQuery = Procedure.query returning Procedure.query.map(_.procedurePK) into ((procedure, procedurePK) => procedure.copy(procedurePK = Some(procedurePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    execDir.mkdirs
    result
  }

  def insertOrUpdate = {
    // if the procedure already exists, and the execution directory is being
    // changed (via a name or version change) then rename the execution directory on disk.
    if (procedurePK.isDefined) {
      val current = Procedure.get(procedurePK.get)
      if (current.isDefined) {
        if (current.get.execDir.getAbsolutePath != execDir.getAbsolutePath) {
          current.get.execDir.renameTo(execDir)
        }
      }
    }
    Db.run(Procedure.query.insertOrUpdate(this))
  }

  def fullName = Procedure.fullName(name, version)

  def fileName = Procedure.fileName(name, version)

  def webUrl = webInterface + "_" + procedurePK.get

  def timeoutInMs = (timeout * (60 * 1000)).round.toLong

  /** Get the directory containing the executables for this procedure. */
  def execDir = new File(Config.ProcedureDir, fileName)

  override def toString = {
    "\n    procedurePK: " + procedurePK +
      "\n    name: " + name +
      "\n    version: " + version +
      "\n    timeout: " + timeout +
      "\n    created: " + created +
      "\n    supportingUserPK: " + supportingUserPK +
      "\n    webInterface: " + webInterface +
      "\n    notes: " + notes

  }

  // TODO There should be a more structured way of identifying procedure types.
  final val isBBbyCBCT = name.toLowerCase.contains("bb") && name.toLowerCase.contains("cbct")
  final val isBBbyEPID = name.toLowerCase.contains("bb") && name.toLowerCase.contains("epid")
  final val isPhase2 = name.toLowerCase.contains("phase2")
  final val isLOC = (name.toLowerCase.contains("loc") || name.toLowerCase.contains("leaf offset")) && (!name.toLowerCase.contains("base"))
  final val isLOCBaseline = name.toLowerCase.contains("loc") && name.toLowerCase.contains("base")
  final val isMlcQa = name.toLowerCase.contains("mlc") && name.toLowerCase.contains("qa")

}

object Procedure {

  class ProcedureTable(tag: Tag) extends Table[Procedure](tag, "procedure") {

    def procedurePK = column[Long]("procedurePK", O.PrimaryKey, O.AutoInc)

    def name = column[String]("name")

    def version = column[String]("version")

    def timeout = column[Float]("timeout")

    def created = column[Date]("created");

    def supportingUserPK = column[Long]("userPK")

    def webInterface = column[String]("webInterface")

    def notes = column[String]("notes")

    def * = (
      procedurePK.?,
      name,
      version,
      timeout,
      created,
      supportingUserPK,
      webInterface,
      notes) <> ((Procedure.apply _) tupled, Procedure.unapply _)

    def supportingUserFK = foreignKey("Procedure_userPKConstraint", supportingUserPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[ProcedureTable]

  def fullName(name: String, version: String): String = name + " " + version

  def fileName(name: String, version: String): String = {
    val chr: Char = '_'
    val fn = fullName(name, version)
    FileUtil.replaceInvalidFileNameCharacters(fn.trim.replace(' ', chr), chr)
  }

  def list: Seq[Procedure] = {
    Db.run(query.result).toList
  }

  case class ProcedureUser(procedure: Procedure, user: User)

  def listWithDependencies: Seq[ProcedureUser] = {
    Db.run(query.result).toList

    val action = for {
      procedure <- query
      user <- User.query if user.userPK === procedure.supportingUserPK
    } yield (procedure, user)
    val seq = Db.run(action.result)
    seq.map(pu => new ProcedureUser(pu._1, pu._2))
  }

  def get(pk: Long): Option[Procedure] = {
    val list = Db.run(query.filter(p => p.procedurePK === pk).result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(procedurePK: Long): Int = {
    val action = query.filter(_.procedurePK === procedurePK).delete
    Db.run(action)
  }

  lazy val ProcOfBBbyCBCT = list.filter(p => p.isBBbyCBCT).headOption
  lazy val ProcOfBBbyEPID = list.filter(p => p.isBBbyEPID).headOption
  lazy val ProcOfPhase2 = list.filter(p => p.isPhase2).headOption
  lazy val ProcOfLOC = list.filter(p => p.isLOC).headOption
  lazy val ProcOfLOCBaseline = list.filter(p => p.isLOCBaseline).headOption
  lazy val ProcOfMlcQa = list.filter(p => p.isMlcQa).headOption

  def main(args: Array[String]): Unit = {
    println("Starting Procedure.main")
    val valid = Config.validate
    DbSetup.init

    def show(p: Procedure) = {
      println(p.fullName + " : " + p.execDir.getAbsolutePath + " isDirectory: " + p.execDir.isDirectory)
    }

    list.map(p => show(p))
  }

}
