package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

case class CollimatorPosition(
  collimatorPositionPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  status: String, // termination status
  SOPInstanceUID: String, // UID of source image
  beamName: String, // name of beam in plan
  north_mm: Double, // north position of collimator (Y axis) in mm
  south_mm: Double, // south position of collimator (Y axis) in mm
  east_mm: Double, //  east position of collimator  (X axis) in mm
  west_mm: Double, //  west position of collimator  (X axis) in mm
  northPlanMinusImage_mm: Double, // plan minus north position of collimator (Y axis) in mm
  southPlanMinusImage_mm: Double, // plan minus south position of collimator (Y axis) in mm
  eastPlanMinusImage_mm: Double, //  plan minus east position of collimator  (X axis) in mm
  westPlanMinusImage_mm: Double, //  plan minus west position of collimator  (X axis) in mm
  gantryAngle_deg: Double, // gantry angle in degrees
  collimatorAngle_deg: Double // collimator angle in degrees
) {

  def insert: CollimatorPosition = {
    val insertQuery = CollimatorPosition.query returning CollimatorPosition.query.map(_.collimatorPositionPK) into
      ((collimatorPosition, collimatorPositionPK) => collimatorPosition.copy(collimatorPositionPK = Some(collimatorPositionPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(CollimatorPosition.query.insertOrUpdate(this))

  override def toString: String = {
    "    collimatorPositionPK: " + collimatorPositionPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    status: " + status + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    north_mm: " + north_mm + "\n" +
      "    south_mm: " + south_mm + "\n" +
      "    east_mm: " + east_mm + "\n" +
      "    west_mm: " + west_mm + "\n" +
      "    northPlanMinusImage_mm: " + northPlanMinusImage_mm + "\n" +
      "    southPlanMinusImage_mm: " + southPlanMinusImage_mm + "\n" +
      "    eastPlanMinusImage_mm: " + eastPlanMinusImage_mm + "\n" +
      "    westPlanMinusImage_mm: " + westPlanMinusImage_mm + "\n" +
      "    gantryAngle_deg: " + gantryAngle_deg + "\n" +
      "    collimatorAngle_deg: " + collimatorAngle_deg + "\n"
  }
}

object CollimatorPosition extends ProcedureOutput {
  class CollimatorPositionTable(tag: Tag) extends Table[CollimatorPosition](tag, "collimatorPosition") {

    def collimatorPositionPK = column[Long]("collimatorPositionPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def status = column[String]("status")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def north_mm = column[Double]("north_mm")
    def south_mm = column[Double]("south_mm")
    def east_mm = column[Double]("east_mm")
    def west_mm = column[Double]("west_mm")
    def northPlanMinusImage_mm = column[Double]("northPlanMinusImage_mm")
    def southPlanMinusImage_mm = column[Double]("southPlanMinusImage_mm")
    def eastPlanMinusImage_mm = column[Double]("eastPlanMinusImage_mm")
    def westPlanMinusImage_mm = column[Double]("westPlanMinusImage_mm")
    def gantryAngle_deg = column[Double]("gantryAngle_deg")
    def collimatorAngle_deg = column[Double]("collimatorAngle_deg")

    def * = (
      collimatorPositionPK.?,
      outputPK,
      status,
      SOPInstanceUID,
      beamName,
      north_mm,
      south_mm,
      east_mm,
      west_mm,
      northPlanMinusImage_mm,
      southPlanMinusImage_mm,
      eastPlanMinusImage_mm,
      westPlanMinusImage_mm,
      gantryAngle_deg,
      collimatorAngle_deg) <> ((CollimatorPosition.apply _)tupled, CollimatorPosition.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[CollimatorPositionTable]

  override val topXmlLabel = "CollimatorPosition"

  def get(collimatorPositionPK: Long): Option[CollimatorPosition] = {
    val action = for {
      inst <- CollimatorPosition.query if inst.collimatorPositionPK === collimatorPositionPK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[CollimatorPosition] = {
    val action = for {
      inst <- CollimatorPosition.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(collimatorPositionPK: Long): Int = {
    val q = query.filter(_.collimatorPositionPK === collimatorPositionPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[CollimatorPosition]) = {
    val ops = list.map { imgId => CollimatorPosition.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ??? // not implemented
  }

  def insertSeq(list: Seq[CollimatorPosition]): Unit = {
    val ops = list.map { loc => CollimatorPosition.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }
}
