package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging._
import org.aqa.Config
import org.aqa.Util

case class MultileafCollimator(
        multileafCollimatorPK: Option[Long], // primary key
        manufacturer: String, // manufacturer's name
        model: String, // manufacturer's model name
        version: String, // details if manufacturer and model are not sufficiently unique
        outerLeafWidthCount: Int, // total number of opposing outer leaf pairs
        innerLeafWidthCount: Int, // total number of opposing inner leaf pairs
        outerLeafWidth_mm: Double, // width of each outer leaf in mm
        innerLeafWidth_mm: Double, // width of each inner leaf in mm
        outerLeafRetractedPosition_mm: Double, // fully retracted position of each outer leaf in mm
        innerLeafRetractedPosition_mm: Double, // fully retracted position of each outer leaf in mm
        outerLeafExtendedPosition_mm: Double, // fully extended position of each inner leaf in mm
        innerLeafExtendedPosition_mm: Double, // fully extended position of each inner leaf in mm
        notes: String // any extra information
        ) {

    def insert: MultileafCollimator = {
        val insertQuery = MultileafCollimator.query returning MultileafCollimator.query.map(_.multileafCollimatorPK) into ((multileafCollimator, multileafCollimatorPK) => multileafCollimator.copy(multileafCollimatorPK = Some(multileafCollimatorPK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(MultileafCollimator.query.insertOrUpdate(this))

    def toName: String = (manufacturer + " " + model + " " + version).trim
}

object MultileafCollimator {
    class MultileafCollimatorTable(tag: Tag) extends Table[MultileafCollimator](tag, "multileafCollimator") {

        def multileafCollimatorPK = column[Long]("multileafCollimatorPK", O.PrimaryKey, O.AutoInc)
        def manufacturer = column[String]("manufacturer")
        def model = column[String]("model")
        def version = column[String]("version")
        def outerLeafWidthCount = column[Int]("outerLeafWidthCount")
        def innerLeafWidthCount = column[Int]("innerLeafWidthCount")
        def outerLeafWidth_mm = column[Double]("outerLeafWidth_mm")
        def innerLeafWidth_mm = column[Double]("innerLeafWidth_mm")
        def outerLeafRetractedPosition_mm = column[Double]("outerLeafRetractedPosition_mm")
        def innerLeafRetractedPosition_mm = column[Double]("innerLeafRetractedPosition_mm")
        def outerLeafExtendedPosition_mm = column[Double]("outerLeafExtendedPosition_mm")
        def innerLeafExtendedPosition_mm = column[Double]("innerLeafExtendedPosition_mm")
        def notes = column[String]("notes")

        def * = (
            multileafCollimatorPK.?,
            manufacturer,
            model,
            version,
            outerLeafWidthCount,
            innerLeafWidthCount,
            outerLeafWidth_mm,
            innerLeafWidth_mm,
            outerLeafRetractedPosition_mm,
            innerLeafRetractedPosition_mm,
            outerLeafExtendedPosition_mm,
            innerLeafExtendedPosition_mm,
            notes) <> ((MultileafCollimator.apply _)tupled, MultileafCollimator.unapply _)
    }

    val query = TableQuery[MultileafCollimatorTable]

    def get(multileafCollimatorPK: Long): Option[MultileafCollimator] = {
        val action = for {
            inst <- MultileafCollimator.query if inst.multileafCollimatorPK === multileafCollimatorPK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    def get(manufacturer: String, model: String, version: String): Option[MultileafCollimator] = {
        val action = for {
            inst <- MultileafCollimator.query if (inst.manufacturer.toLowerCase === manufacturer.toLowerCase) &&
                (inst.model.toLowerCase === model.toLowerCase) &&
                (inst.version.toLowerCase === version.toLowerCase)
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all multileafCollimators.
     */
    def list = Db.run(query.result)

    def delete(multileafCollimatorPK: Long): Int = {
        val q = query.filter(_.multileafCollimatorPK === multileafCollimatorPK)
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
