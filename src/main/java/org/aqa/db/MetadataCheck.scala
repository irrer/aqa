package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

case class MetadataCheck(
  metadataCheckPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  beamName: String, // name of beam in plan
  gantryAnglePlan_deg: Double, // planned gantry angle in degrees
  gantryAnglePlanMinusImage_deg: Double, // difference from planned gantry angle in degrees
  collimatorAnglePlan_deg: Double, // planned collimator angle in degrees
  collimatorAnglePlanMinusImage_deg: Double, // difference from planned collimator angle in degrees
  x1JawPlan_mm: Double, // planned jaw position in mm
  x1JawPlanMinusImage_mm: Double, // difference from planned jaw position in mm
  x2JawPlan_mm: Double, // planned jaw position in mm
  x2JawPlanMinusImage_mm: Double, // difference from planned jaw position in mm
  y1JawPlan_mm: Double, // planned jaw position in mm
  y1JawPlanMinusImage_mm: Double, // difference from planned jaw position in mm
  y2JawPlan_mm: Double, // planned jaw position in mm
  y2JawPlanMinusImage_mm: Double, // difference from planned jaw position in mm
  energyPlan_kev: Double, // planned energy in kilo electron volts
  energyPlanMinusImage_kev: Double, // difference from planned energy in kilo electron volts
  flatteningFilter: Boolean, // true if a flattening filter was present
  pass: Boolean // true if all values were within tolerances
) {

  def insert: MetadataCheck = {
    val insertQuery = MetadataCheck.query returning MetadataCheck.query.map(_.metadataCheckPK) into
      ((metadataCheck, metadataCheckPK) => metadataCheck.copy(metadataCheckPK = Some(metadataCheckPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(MetadataCheck.query.insertOrUpdate(this))

  override def toString: String = {
    "    metadataCheckPK: " + metadataCheckPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    beamName: " + beamName + "\n" +
      "    gantryAnglePlan_deg: " + gantryAnglePlan_deg + "\n" +
      "    gantryAnglePlanMinusImage_deg: " + gantryAnglePlanMinusImage_deg + "\n" +
      "    collimatorAnglePlan_deg: " + collimatorAnglePlan_deg + "\n" +
      "    collimatorAnglePlanMinusImage_deg: " + collimatorAnglePlanMinusImage_deg + "\n" +
      "    x1JawPlan_mm: " + x1JawPlan_mm + "\n" +
      "    x1JawPlanMinusImage_mm: " + x1JawPlanMinusImage_mm + "\n" +
      "    x2JawPlan_mm: " + x2JawPlan_mm + "\n" +
      "    x2JawPlanMinusImage_mm: " + x2JawPlanMinusImage_mm + "\n" +
      "    y1JawPlan_mm: " + y1JawPlan_mm + "\n" +
      "    y1JawPlanMinusImage_mm: " + y1JawPlanMinusImage_mm + "\n" +
      "    y2JawPlan_mm: " + y2JawPlan_mm + "\n" +
      "    y2JawPlanMinusImage_mm: " + y2JawPlanMinusImage_mm + "\n" +
      "    energyPlan_kev: " + energyPlan_kev + "\n" +
      "    energyPlanMinusImage_kev: " + energyPlanMinusImage_kev + "\n" +
      "    flatteningFilter: " + flatteningFilter + "\n" +
      "    pass: " + pass + "\n"
  }
}

object MetadataCheck extends ProcedureOutput {
  class MetadataCheckTable(tag: Tag) extends Table[MetadataCheck](tag, "metadataCheck") {

    def metadataCheckPK = column[Long]("metadataCheckPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def beamName = column[String]("beamName")
    def gantryAnglePlan_deg = column[Double]("gantryAnglePlan_deg")
    def gantryAnglePlanMinusImage_deg = column[Double]("gantryAnglePlanMinusImage_deg")
    def collimatorAnglePlan_deg = column[Double]("collimatorAnglePlan_deg")
    def collimatorAnglePlanMinusImage_deg = column[Double]("collimatorAnglePlanMinusImage_deg")
    def x1JawPlan_mm = column[Double]("x1JawPlan_mm")
    def x1JawPlanMinusImage_mm = column[Double]("x1JawPlanMinusImage_mm")
    def x2JawPlan_mm = column[Double]("x2JawPlan_mm")
    def x2JawPlanMinusImage_mm = column[Double]("x2JawPlanMinusImage_mm")
    def y1JawPlan_mm = column[Double]("y1JawPlan_mm")
    def y1JawPlanMinusImage_mm = column[Double]("y1JawPlanMinusImage_mm")
    def y2JawPlan_mm = column[Double]("y2JawPlan_mm")
    def y2JawPlanMinusImage_mm = column[Double]("y2JawPlanMinusImage_mm")
    def energyPlan_kev = column[Double]("energyPlan_kev")
    def energyPlanMinusImage_kev = column[Double]("energyPlanMinusImage_kev")
    def flatteningFilter = column[Boolean]("flatteningFilter")
    def pass = column[Boolean]("pass")

    def * = (
      metadataCheckPK.?,
      outputPK,
      beamName,
      gantryAnglePlan_deg,
      gantryAnglePlanMinusImage_deg,
      collimatorAnglePlan_deg,
      collimatorAnglePlanMinusImage_deg,
      x1JawPlan_mm,
      x1JawPlanMinusImage_mm,
      x2JawPlan_mm,
      x2JawPlanMinusImage_mm,
      y1JawPlan_mm,
      y1JawPlanMinusImage_mm,
      y2JawPlan_mm,
      y2JawPlanMinusImage_mm,
      energyPlan_kev,
      energyPlanMinusImage_kev,
      flatteningFilter,
      pass) <> ((MetadataCheck.apply _)tupled, MetadataCheck.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MetadataCheckTable]

  override val topXmlLabel = "MetadataCheck"

  def get(metadataCheckPK: Long): Option[MetadataCheck] = {
    val action = for {
      inst <- MetadataCheck.query if inst.metadataCheckPK === metadataCheckPK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[MetadataCheck] = {
    val action = for {
      inst <- MetadataCheck.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(metadataCheckPK: Long): Int = {
    val q = query.filter(_.metadataCheckPK === metadataCheckPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[MetadataCheck]) = {
    val ops = list.map { imgId => MetadataCheck.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ??? // TODO
  }

  def insertSeq(list: Seq[MetadataCheck]): Unit = {
    val ops = list.map { loc => MetadataCheck.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }
}
