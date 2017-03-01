package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging._
import org.aqa.Config
import org.aqa.Util

case class EPID(
        epidPK: Option[Long], // primary key
        manufacturer: String, // manufacturer's name
        model: String, // manufacturer's model name
        hardwareVersion: String, // version of hardware
        pixelCountX: Int, // number of columns
        pixelCountY: Int, // number of rows
        pixelSizeX_mm: Double, // size of pixels in X direction in mm
        pixelSizeY_mm: Double, // size of pixels in Y direction in mm
        notes: String // any extra information
        ) {

    def insert: EPID = {
        val insertQuery = EPID.query returning EPID.query.map(_.epidPK) into ((epid, epidPK) => epid.copy(epidPK = Some(epidPK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(EPID.query.insertOrUpdate(this))

    def toName: String = (manufacturer + " " + model + " " + hardwareVersion).trim
}

object EPID {
    class EPIDTable(tag: Tag) extends Table[EPID](tag, "epid") {

        def epidPK = column[Long]("epidPK", O.PrimaryKey, O.AutoInc)
        def manufacturer = column[String]("manufacturer")
        def model = column[String]("model")
        def hardwareVersion = column[String]("hardwareVersion")
        def pixelCountX = column[Int]("pixelCountX")
        def pixelCountY = column[Int]("pixelCountY")
        def pixelSizeX_mm = column[Double]("pixelSizeX_mm")
        def pixelSizeY_mm = column[Double]("pixelSizeY_mm")
        def notes = column[String]("notes")

        def * = (
            epidPK.?,
            manufacturer,
            model,
            hardwareVersion,
            pixelCountX,
            pixelCountY,
            pixelSizeX_mm,
            pixelSizeY_mm,
            notes) <> ((EPID.apply _)tupled, EPID.unapply _)
    }

    val query = TableQuery[EPIDTable]

    def get(epidPK: Long): Option[EPID] = {
        val action = for {
            inst <- EPID.query if inst.epidPK === epidPK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    def get(manufacturer: String, model: String, hardwareVersion: String): Option[EPID] = {
        val action = for {
            inst <- EPID.query if (inst.manufacturer.toLowerCase === manufacturer.toLowerCase) &&
                (inst.model.toLowerCase === model.toLowerCase) &&
                (inst.hardwareVersion.toLowerCase === hardwareVersion.toLowerCase)
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all epids.
     */
    def list = Db.run(query.result)

    def delete(epidPK: Long): Int = {
        val q = query.filter(_.epidPK === epidPK)
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
