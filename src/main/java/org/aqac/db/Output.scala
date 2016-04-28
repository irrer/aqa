package org.aqac.db

import slick.driver.PostgresDriver.api._
import org.aqac.Logging._
import org.aqac.Config
import java.sql.Date

case class Output(
        outputPK: Option[Long], // primary key
        finishDate: Date, // when procedure was finished
        inputPK: Long) {

    def insert = {
        logInfo("Creating output: " + this)
        Db.run(Output.query ++= Seq(this))
        logInfo("Created output: " + this)
    }

    def insertOrUpdate = Db.run(Output.query.insertOrUpdate(this))
}

object Output {
    class OutputTable(tag: Tag) extends Table[Output](tag, "output") {

        def outputPK = column[Long]("outputPK", O.PrimaryKey, O.AutoInc)
        def startDate = column[Date]("startDate")
        def inputPK = column[Long]("inputPK")

        def * = (
            outputPK.?,
            startDate,
            inputPK) <> ((Output.apply _)tupled, Output.unapply _)

        def inputFK = foreignKey("inputPK", inputPK, Input.query)(_.inputPK)
    }

    val query = TableQuery[OutputTable]

    def get(outputPK: Long): Option[Output] = {
        val action = for {
            output <- Output.query if output.outputPK === outputPK
        } yield (output)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    def delete(outputPK: Long): Int = {
        val q = query.filter(_.outputPK === outputPK)
        val action = q.delete
        Db.run(action)
    }

    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        println("======== output: " + get(5))
        println("======== output delete: " + delete(5))
    }
}
