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
  FloodCompensation: Boolean, // true if flood compensation was used
  X1_mm: Double, //  X1 jaw position of collimator  (X axis) in mm
  X2_mm: Double, //  X2 jaw position of collimator  (X axis) in mm
  Y1_mm: Double, //  Y1  jaw of collimator (Y axis) in mm
  Y2_mm: Double, //  Y2 jaw position of collimator (Y axis) in mm
  X1_PlanMinusImage_mm: Double, //  X1 jaw plan minus east position of collimator  (X axis) in mm
  X2_PlanMinusImage_mm: Double, //  X2 jaw plan minus west position of collimator  (X axis) in mm
  Y1_PlanMinusImage_mm: Double, //  Y1 jaw plan minus north position of collimator (Y axis) in mm
  Y2_PlanMinusImage_mm: Double, //  Y2 jaw plan minus south position of collimator (Y axis) in mm
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
      "    FloodCompensation: " + FloodCompensation + "\n" +
      "    X1_mm: " + X1_mm + "\n" +
      "    X2_mm: " + X2_mm + "\n" +
      "    Y1_mm: " + Y1_mm + "\n" +
      "    Y2_mm: " + Y2_mm + "\n" +
      "    X1_PlanMinusImage_mm: " + X1_PlanMinusImage_mm + "\n" +
      "    X2_PlanMinusImage_mm: " + X2_PlanMinusImage_mm + "\n" +
      "    Y1_PlanMinusImage_mm: " + Y1_PlanMinusImage_mm + "\n" +
      "    Y2_PlanMinusImage_mm: " + Y2_PlanMinusImage_mm + "\n" +
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
    def FloodCompensation = column[Boolean]("FloodCompensation")
    def X1_mm = column[Double]("X1_mm")
    def X2_mm = column[Double]("X2_mm")
    def Y1_mm = column[Double]("Y1_mm")
    def Y2_mm = column[Double]("Y2_mm")
    def X1_PlanMinusImage_mm = column[Double]("X1_PlanMinusImage_mm")
    def X2_PlanMinusImage_mm = column[Double]("X2_PlanMinusImage_mm")
    def Y1_PlanMinusImage_mm = column[Double]("Y1_PlanMinusImage_mm")
    def Y2_PlanMinusImage_mm = column[Double]("Y2_PlanMinusImage_mm")
    def gantryAngle_deg = column[Double]("gantryAngle_deg")
    def collimatorAngle_deg = column[Double]("collimatorAngle_deg")

    def * = (
      collimatorPositionPK.?,
      outputPK,
      status,
      SOPInstanceUID,
      beamName,
      FloodCompensation,
      X1_mm,
      X2_mm,
      Y1_mm,
      Y2_mm,
      X1_PlanMinusImage_mm,
      X2_PlanMinusImage_mm,
      Y1_PlanMinusImage_mm,
      Y2_PlanMinusImage_mm,
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
