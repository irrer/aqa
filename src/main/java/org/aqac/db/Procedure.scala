package org.aqac.db

import slick.driver.PostgresDriver.api._
import slick.lifted.{ ProvenShape, ForeignKeyQuery }
import java.sql.Date
import org.aqac.Logging._
import edu.umro.ScalaUtil.FileUtil
import org.aqac.Config
import java.io.File

/**
 * A quality assurance procedure.
 *
 */
case class Procedure(
        procedurePK: Option[Long], // Primary key
        name: String, // human readable identifier
        version: String, // code version, should be in numeric+dot format, as in 1.2.3
        timeout: Float, // For 'runaway' programs.  Timeout in minutes after which the procedure should be terminated
        created: Date, // time that this record was created
        supportingUserPK: Long, // id of user that supports this procedure, usually the author
        webInterface: String, // Class name of Restlet for running procedure
        notes: String // Additional information on usage, inputs, limitations, etc.
        ) {

    def insert: Procedure = {
        val insertQuery = Procedure.query returning Procedure.query.map(_.procedurePK) into ((procedure, procedurePK) => procedure.copy(procedurePK = Some(procedurePK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(Procedure.query.insertOrUpdate(this))

    def fullName = Procedure.fullName(name, version)

    def fileName = Procedure.fileName(name, version)

    def webUrl = "/run/" + webInterface + "_" + procedurePK.get 

    def timeoutInMs = (timeout * (60 * 1000)).round.toLong

    /** Get the directory containing the executables for this procedure. */
    def execDir = new File(Config.ProcedureDir, fileName)
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
            notes) <> ((Procedure.apply _)tupled, Procedure.unapply _)

        def supportingUserFK = foreignKey("userPK", supportingUserPK, User.query)(_.userPK)

    }
    val query = TableQuery[ProcedureTable]

    def fullName(name: String, version: String): String = name + " " + version

    def fileName(name: String, version: String): String = {
        val chr: Char = '_'
        val fn = fullName(name, version)
        FileUtil.replaceInvalidFileNameCharacters(fn.trim.replace(' ', chr), chr)
    }
    
    //def webURL(name: String, version: String): String = fileName(name, version)     TODO remove

    def list: Seq[Procedure] = {
        Db.run(query.result).toList
    }

    type PU = (Procedure, User)

    def listWithDependencies: Seq[PU] = {
        Db.run(query.result).toList

        val action = for {
            procedure <- query
            user <- User.query if user.userPK === procedure.supportingUserPK
        } yield (procedure, user)
        val seq = Db.run(action.result)
        seq
    }

    def get(pk: Long): Option[Procedure] = {
        val list = Db.run(query.filter(p => p.procedurePK === pk).result)
        if (list.isEmpty) None else Some(list.head)
    }

    def delete(procedurePK: Long): Int = {
        val action = query.filter(_.procedurePK === procedurePK).delete
        Db.run(action)
    }

}