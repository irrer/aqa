package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import edu.umro.ScalaUtil.Trace

case class EPID(
  epidPK: Option[Long], // primary key
  manufacturer: String, // manufacturer's name
  model: String, // manufacturer's model name
  hardwareVersion: String, // version of hardware
  pixelCountX: Int, // number of columns
  pixelCountY: Int, // number of rows
  width_cm: Double, // width of imager in cm
  height_cm: Double, // height of imager in cm
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
    def width_cm = column[Double]("width_cm")
    def height_cm = column[Double]("height_cm")
    def notes = column[String]("notes")

    def * = (
      epidPK.?,
      manufacturer,
      model,
      hardwareVersion,
      pixelCountX,
      pixelCountY,
      width_cm,
      height_cm,
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
    Trace.trace(get(2))

    //    println("======== inst: " + get(5))
    //    println("======== inst delete: " + delete(5))
  }
}
