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

    def dir: File = WebServer.fileOfResultsPath(directory)

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

    override def equals(o: Any): Boolean = {
        val other = o.asInstanceOf[Output]
        outputPK.isDefined && other.outputPK.isDefined && (outputPK.get == other.outputPK.get)
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
    private val query2 = TableQuery[OutputTable]

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

    val displayFilePrefix = "display".toLowerCase

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
                Db.run(q.result)
            }
            case _ => Seq[Output]()
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
     */
    def listRedundant(outputList: Seq[Output]): Seq[Output] = {
        def cmpr(a: Output, b: Output): Boolean = {
            (a.dataDate, b.dataDate) match {
                case (Some(aa), Some(bb)) => aa.getTime < bb.getTime
                case (Some(aa), _) => false
                case (_, Some(bb)) => true
                case _ => true
            }
        }
        val goodStatus = Seq(ProcedureStatus.done, ProcedureStatus.pass, ProcedureStatus.fail).map(s => s.toString)
        def isGood(output: Output): Boolean = goodStatus.map(g => g.equals(output.status)).contains(true)

        val latestGood = outputList.indexWhere(g => isGood(g))
        val latestNotGood = outputList.indexWhere(b => !isGood(b))

        // list of indexes in outputList to keep
        val listToKeep: Seq[Int] = (latestGood, latestNotGood) match {
            case (-1, -1) => {
                logWarning("removeRedundant Unexpected error, output is neither good or notGood") // nothing?
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
                }
                else {
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
