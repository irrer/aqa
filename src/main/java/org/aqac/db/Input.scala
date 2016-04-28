package org.aqac.db

import slick.driver.PostgresDriver.api._
import org.aqac.Logging._
import org.aqac.Config
import java.sql.Date
import java.io.File

case class Input(
        inputPK: Option[Long], // primary key
        directory: String, // main directory where procedure related data is stored.  Contains an input and output directory.
        startDate: Date, // when procedure was started
        userPK: Long, // user that ran the procedure
        machinePK: Option[Long] // procedure was run on this machine.  Machine is optional to support procedures that do not correlate to a single machine.
        ) {

    def insert = {
        logInfo("Creating input: " + this)
        Db.run(Input.query ++= Seq(this))
        logInfo("Created input: " + this)
    }

    def insertOrUpdate = Db.run(Input.query.insertOrUpdate(this))
    
    def inputDir: File = new File(directory, Input.inputDirName)
}

object Input {
    
    val inputDirName = "input"
    
    class InputTable(tag: Tag) extends Table[Input](tag, "input") {

        def inputPK = column[Long]("inputPK", O.PrimaryKey, O.AutoInc)
        def directory = column[String]("directory")
        def startDate = column[Date]("startDate")
        def userPK = column[Long]("userPK")
        def machinePK = column[Long]("machinePK")

        def * = (
            inputPK.?,
            directory,
            startDate,
            userPK,
            machinePK.?) <> ((Input.apply _)tupled, Input.unapply _)

        def userFK = foreignKey("userPK", userPK, User.query)(_.userPK)
        def machineFK = foreignKey("machinePK", machinePK, Machine.query)(_.machinePK)
    }

    val query = TableQuery[InputTable]

    def get(inputPK: Long): Option[Input] = {
        val action = for {
            input <- Input.query if input.inputPK === inputPK
        } yield (input)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    def delete(inputPK: Long): Int = {
        val q = query.filter(_.inputPK === inputPK)
        val action = q.delete
        Db.run(action)
    }

    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        println("======== input: " + get(5))
        println("======== input delete: " + delete(5))
    }
}
