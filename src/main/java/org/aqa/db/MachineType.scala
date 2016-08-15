package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging._
import org.aqa.Config
import org.aqa.Util

case class MachineType(
        machineTypePK: Option[Long], // primary key
        manufacturer: String, // company name
        model: String, // manufacturer's model name
        version: String, // details if manufacturer and model are not sufficiently unique
        notes: String // any extra information
        ) {

    def insert: MachineType = {
        val insertQuery = MachineType.query returning MachineType.query.map(_.machineTypePK) into ((machineType, machineTypePK) => machineType.copy(machineTypePK = Some(machineTypePK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(MachineType.query.insertOrUpdate(this))

    def toName: String = (manufacturer + " " + model + " " + version).trim
}

object MachineType {
    class MachineTypeTable(tag: Tag) extends Table[MachineType](tag, "machineType") {

        def machineTypePK = column[Long]("machineTypePK", O.PrimaryKey, O.AutoInc)
        def manufacturer = column[String]("manufacturer")
        def model = column[String]("model")
        def version = column[String]("version")
        def notes = column[String]("notes")

        def * = (
            machineTypePK.?,
            manufacturer,
            model,
            version,
            notes) <> ((MachineType.apply _)tupled, MachineType.unapply _)
    }

    val query = TableQuery[MachineTypeTable]

    def get(machineTypePK: Long): Option[MachineType] = {
        val action = for {
            inst <- MachineType.query if inst.machineTypePK === machineTypePK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    def get(manufacturer: String, model: String, version: String): Option[MachineType] = {
        val action = for {
            inst <- MachineType.query if (inst.manufacturer.toLowerCase === manufacturer.toLowerCase) &&
                (inst.model.toLowerCase === model.toLowerCase) &&
                (inst.version.toLowerCase === version.toLowerCase)
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all machineTypes.
     */
    def list = Db.run(query.result)

    def delete(machineTypePK: Long): Int = {
        val q = query.filter(_.machineTypePK === machineTypePK)
        val action = q.delete
        Db.run(action)
    }

    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        println("======== inst: " + get(5))
        println("======== inst delete: " + delete(5))
    }
}
