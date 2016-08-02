package org.aqac.db

import slick.driver.PostgresDriver.api._
import org.aqac.Logging._
import org.aqac.Config
import java.sql.Timestamp
import java.io.File
import org.aqac.web.WebServer

case class Input(
        inputPK: Option[Long], // primary key
        directory: Option[String], // main directory where procedure related data is stored.  Contains input and 0 or more output directories.
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

    def dir: File = WebServer.fileOfDataPath(directory.get)
    
    /**
     * Update the directory.  Returns number of records updated, which should always be one.  If it is zero then
     * it is probably because the object is not in the database.
     */
    def updateDirectory(dirName: String) = {
        Db.run(Input.query.filter(_.inputPK === inputPK.get).map(i => i.directory).update(Some(dirName)))
    }
}

object Input {

    class InputTable(tag: Tag) extends Table[Input](tag, "input") {

        def inputPK = column[Long]("inputPK", O.PrimaryKey, O.AutoInc)
        def directory = column[Option[String]]("directory")
        def uploadDate = column[Timestamp]("uploadDate")
        def userPK = column[Option[Long]]("userPK")
        def machinePK = column[Option[Long]]("machinePK")
        def patientId = column[Option[String]]("patientId")
        def dataDate = column[Option[Timestamp]]("dataDate")

        def * = (
            inputPK.?,
            directory,
            uploadDate,
            userPK,
            machinePK,
            patientId,
            dataDate
        ) <> ((Input.apply _)tupled, Input.unapply _)

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
        val input = new Input(None, Some("input_dir"), new Timestamp(System.currentTimeMillis), Some(6), Some(2), None, None)

        input.insert

        println("======== input: " + get(5))
        //println("======== input delete: " + delete(5))
    }
}
