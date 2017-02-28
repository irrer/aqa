package org.aqa.db

import slick.driver.PostgresDriver.api._
import scala.concurrent.ExecutionContext.Implicits.global
import org.aqa.Logging._
import org.aqa.Config
import org.aqa.run.ProcedureStatus
import java.sql.Timestamp
import java.sql.Date
import org.aqa.web.WebServer
import java.io.File

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

    case class ExtendedValues(val output: Output, val input: Input, val machine: Machine, val procedure: Procedure, val institution: Institution, val user: User);

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
        //val sorted = search.sortBy(_._2.dataDate.desc.reverse).take(maxSize)
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

    def listByInputPK(inputPK: Long): Seq[Output] = {
        val q = query.filter(_.inputPK === inputPK)
        Db.run(q.result)
    }

    def delete(outputPK: Long): Int = {
        val q = query.filter(_.outputPK === outputPK)
        val action = q.delete
        Db.run(action)
    }

//    /**
//     * Delete the given output and all rows in other tables that reference it.  Note
//     *  that inputs referenced by the output are not deleted.
//     */
//    def deleteOutputAndReferences(outputPK: Long): Unit = {
//        val action = for {
//            _ <- CentralAxis.query.filter(_.outputPK === outputPK).delete
//            _ <- query.filter(_.outputPK === outputPK).delete
//        } yield ()
//
//        Db.run(action.transactionally)
//    }

    val displayFilePrefix = "display".toLowerCase

    /**
     * If the given directory contains an output file, then return it.
     */
    def outputFile(directory: File): Option[File] = {
        val list = (directory).listFiles().filter { f => f.canRead && f.getName.toLowerCase.startsWith(Output.displayFilePrefix) }
        if (list.isEmpty) None
        else Some(list.head)
    }

    def main(args: Array[String]): Unit = {
        println("Starting Output.main")
        val valid = Config.validate
        DbSetup.init

        if (true) {
            println("starting input find")
            val list = listByInputPK(70)
            println("list size: " + list.size)
            list.map(o => println(o))
            System.exit(0)
        }

        if (false) {
            println("starting composed query")
            val maxSize = 10
            val machPK = 2.toLong // 3 = EX2      2 = TB5 
            val procPK = 3.toLong // 1 = Winston  3 = Max Leaf Gap 
            val procPKOpt: Option[Long] = Some(3.toLong) // 1 = Winston  3 = Max Leaf Gap 

            val mmq = for {
                machine <- Machine.query if machine.machinePK === machPK
            } yield (machine)

            val search = for {
                output <- Output.query
                input <- Input.query if input.inputPK === output.inputPK
                machine <- Machine.query if machine.machinePK === input.machinePK
                procedure <- Procedure.query if procedure.procedurePK === output.procedurePK
                institution <- Institution.query if institution.institutionPK === machine.institutionPK
                user <- User.query if user.userPK === output.userPK
            } yield (output, input, machine, procedure, institution, user)

            val filtered = search
                .filter(x => (x._3.machinePK === machPK) || (machPK < 0))
                .filter(x => (x._4.procedurePK === procPK) || (procPK < 0))

            val sorted = filtered.sortBy(_._2.dataDate).take(maxSize)

            val result = Db.run(sorted.result)
            val list = result.map(x => new ExtendedValues(x._1, x._2, x._3, x._4, x._5, x._6))

            println("done.  Number of rows: " + list.size)
        }

        if (false) {
            val output = new Output(None, // primary key
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
