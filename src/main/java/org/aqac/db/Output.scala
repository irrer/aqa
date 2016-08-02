package org.aqac.db

import slick.driver.PostgresDriver.api._
import org.aqac.Logging._
import org.aqac.Config
import org.aqac.run.ProcedureStatus
import java.sql.Timestamp
import java.sql.Date
import org.aqac.web.WebServer
import java.io.File

case class Output(
        outputPK: Option[Long], // primary key
        inputPK: Long, // input data
        directory: String, // directory containing data
        procedurePK: Long, // procedure that created this output
        userPK: Option[Long], // user that created this output
        startDate: Timestamp, // when procedure was started
        finishDate: Option[Timestamp], // when procedure finished
        status: String) // termination status
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

    def dir: File = WebServer.fileOfDataPath(directory)
    
    /**
     * Update the status and finish date.  Returns number of records updated, which should always be one.  If it is zero then
     * it is probably because the object is not in the database.
     */
    def updateStatusAndFinishDate(sts: String, finDate: Date) = {
        val finTimestamp = new Timestamp(finDate.getTime)
        Db.run(Output.query.filter(_.outputPK === outputPK.get).map(o => (o.status, o.finishDate)).update((sts, Some(finTimestamp))))
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
}

object Output {
    class OutputTable(tag: Tag) extends Table[Output](tag, "output") {

        def outputPK = column[Long]("outputPK", O.PrimaryKey, O.AutoInc)
        def inputPK = column[Long]("inputPK")
        def directory = column[String]("directory")
        def procedurePK = column[Long]("procedurePK")
        def userPK = column[Option[Long]]("userPK")
        def startDate = column[Timestamp]("startDate")
        def finishDate = column[Option[Timestamp]]("finishDate")
        def status = column[String]("status")

        def * = (
            outputPK.?,
            inputPK,
            directory,
            procedurePK,
            userPK,
            startDate,
            finishDate,
            status) <> ((Output.apply _)tupled, Output.unapply _)

        def inputFK = foreignKey("inputPK", inputPK, Input.query)(_.inputPK)
        def procedureFK = foreignKey("procedurePK", procedurePK, Procedure.query)(_.procedurePK)
        def userFK = foreignKey("userPK", userPK, User.query)(_.userPK)
    }

    val query = TableQuery[OutputTable]

    def get(outputPK: Long): Option[Output] = {
        val action = for {
            output <- Output.query if output.outputPK === outputPK
        } yield (output)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all outputs.
     */
    def list: Seq[Output] = Db.run(query.result)
    
    case class ExtendedValues(val output: Output, val input: Input, val machine: Machine, val procedure: Procedure, val institution:Institution, val user: User);
    
    /**
     * Get an extended list of all outputs.
     */
    def extendedList(procedure: Option[Procedure], machine: Option[Machine], maxSize: Int): Seq[ExtendedValues] = {
        val search = for {
            output <- Output.query
            input <- Input.query if input.inputPK === output.inputPK
            machine <- Machine.query if machine.machinePK === input.machinePK
            procedure <- Procedure.query if procedure.procedurePK === output.procedurePK
            institution <- Institution.query if institution.institutionPK === machine.institutionPK
            user <- User.query if user.userPK === output.userPK
        } yield (output, input, machine, procedure, institution, user)
        val sorted = search.sortBy(_._2.dataDate).take(maxSize)
        val result = Db.run(sorted.result)
        result.map(x => new ExtendedValues(x._1, x._2, x._3, x._4, x._5, x._6))
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

    def delete(outputPK: Long): Int = {
        val q = query.filter(_.outputPK === outputPK)
        val action = q.delete
        Db.run(action)
    }

    val outputFilePrefix = "output".toLowerCase

    /**
     * If the given directory contains an output file, then return it.
     */
    def outputFile(directory: File): Option[File] = {
        val list = (directory).listFiles().filter { f => f.canRead && f.getName.toLowerCase.startsWith(Output.outputFilePrefix) }
        if (list.isEmpty) None
        else Some(list.head)
    }

    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        if (true) {
            val output = new Output(None, // primary key
                (4).toLong, // input data
                "dir", // directory containing data
                1.toLong, // procedure that created this output
                Some((6).toLong), // user that created this output
                startDate = new Timestamp(System.currentTimeMillis), // when procedure was started
                finishDate = Some(new Timestamp(System.currentTimeMillis + 1)), // when procedure finished
                status = "testing") // termination status
            println("output: " + output)
            val result = output.insert
            println("result: " + result)
        }
        println("======== output: " + get(5))
        // println("======== output delete: " + delete(5))
    }
}
