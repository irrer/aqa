package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.sql.Timestamp

case class MaintenanceRecord(
        maintenanceRecordPK: Option[Long], // primary key
        machinePK: Long, // machine that was maintained
        dateTime: Timestamp, // when data was loaded into this system
        userPK: Long, // user that performed or oversaw maintenance
        summary: String, // short description of maintenance
        description: String // description of maintenance
        ) {

    def insert: MaintenanceRecord = {
        val insertQuery = MaintenanceRecord.query returning MaintenanceRecord.query.map(_.maintenanceRecordPK) into ((maintenanceRecord, maintenanceRecordPK) => maintenanceRecord.copy(maintenanceRecordPK = Some(maintenanceRecordPK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(MaintenanceRecord.query.insertOrUpdate(this))
}

object MaintenanceRecord {
    class MaintenanceRecordTable(tag: Tag) extends Table[MaintenanceRecord](tag, "maintenanceRecord") {

        def maintenanceRecordPK = column[Long]("maintenanceRecordPK", O.PrimaryKey, O.AutoInc)
        def machinePK = column[Long]("machinePK")
        def dateTime = column[Timestamp]("dateTime")
        def userPK = column[Long]("userPK")
        def summary = column[String]("summary")
        def description = column[String]("description")

        def * = (
            maintenanceRecordPK.?,
            machinePK,
            dateTime,
            userPK,
            summary,
            description) <> ((MaintenanceRecord.apply _)tupled, MaintenanceRecord.unapply _)

        def machineFK = foreignKey("machinePK", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
        def userFK = foreignKey("userPK", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    }

    val query = TableQuery[MaintenanceRecordTable]

    def get(maintenanceRecordPK: Long): Option[MaintenanceRecord] = {
        val action = for {
            inst <- MaintenanceRecord.query if inst.maintenanceRecordPK === maintenanceRecordPK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    def getByMachine(machinePK: Long): Seq[MaintenanceRecord] = {
        val action = for {
            inst <- MaintenanceRecord.query if (inst.machinePK === machinePK)
        } yield (inst)
        Db.run(action.result)
    }

    /**
     * Get a list of all maintenanceRecords.
     */
    def list = Db.run(query.result)

    def delete(maintenanceRecordPK: Long): Int = {
        val q = query.filter(_.maintenanceRecordPK === maintenanceRecordPK)
        val action = q.delete
        Db.run(action)
    }

    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
    }
}
