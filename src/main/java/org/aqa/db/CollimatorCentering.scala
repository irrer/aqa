package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

case class CollimatorCentering(
  collimatorCenteringPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  status: String, // termination status
  xCollimatorCenterMinusImageCenter_mm: Double, // X center the collimator minus the center of the image in mm
  yCollimatorCenterMinusImageCenter_mm: Double, // Y center the collimator minus the center of the image in mm
  xCollimatorCenter_mm: Double, // X collimator center in mm
  yCollimatorCenter_mm: Double, // Y collimator center in mm
  north090_mm: Double, // north position of collimator leaf edge for gantry at 90 degrees (Y axis) in mm
  south090_mm: Double, // south position of collimator leaf edge for gantry at 90 degrees (Y axis) in mm
  east090_mm: Double, // east position of collimator leaf edge for gantry at 90 degrees (X axis) in mm
  west090_mm: Double, // west position of collimator leaf edge for gantry at 90 degrees (X axis) in mm
  north270_mm: Double, // north position of collimator leaf edge for gantry at 270 degrees (Y axis) in mm
  south270_mm: Double, // south position of collimator leaf edge for gantry at 270 degrees (Y axis) in mm
  east270_mm: Double, // east position of collimator leaf edge for gantry at 270 degrees (X axis) in mm
  west270_mm: Double // west position of collimator leaf edge for gantry at 270 degrees (X axis) in mm
) {

  def insert: CollimatorCentering = {
    val insertQuery = CollimatorCentering.query returning CollimatorCentering.query.map(_.collimatorCenteringPK) into
      ((collimatorCentering, collimatorCenteringPK) => collimatorCentering.copy(collimatorCenteringPK = Some(collimatorCenteringPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(CollimatorCentering.query.insertOrUpdate(this))

  override def toString: String = {
    "    collimatorCenteringPK: " + collimatorCenteringPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    status: " + status + "\n" +
      "    xCollimatorCenterMinusImageCenter_mm: " + xCollimatorCenterMinusImageCenter_mm + "\n" +
      "    yCollimatorCenterMinusImageCenter_mm: " + yCollimatorCenterMinusImageCenter_mm + "\n" +
      "    xCollimatorCenter_mm: " + xCollimatorCenter_mm + "\n" +
      "    yCollimatorCenter_mm: " + yCollimatorCenter_mm + "\n" +
      "    north090_mm: " + north090_mm + "\n" +
      "    south090_mm: " + south090_mm + "\n" +
      "    east090_mm: " + east090_mm + "\n" +
      "    west090_mm: " + west090_mm + "\n" +
      "    north270_mm: " + north270_mm + "\n" +
      "    south270_mm: " + south270_mm + "\n" +
      "    east270_mm: " + east270_mm + "\n" +
      "    west270_mm: " + west270_mm + "\n"
  }
}

object CollimatorCentering extends ProcedureOutput {
  class CollimatorCenteringTable(tag: Tag) extends Table[CollimatorCentering](tag, "collimatorCentering") {

    def collimatorCenteringPK = column[Long]("collimatorCenteringPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def status = column[String]("status")
    def xCollimatorCenterMinusImageCenter_mm = column[Double]("xCollimatorCenterMinusImageCenter_mm")
    def yCollimatorCenterMinusImageCenter_mm = column[Double]("yCollimatorCenterMinusImageCenter_mm")
    def xCollimatorCenter_mm = column[Double]("xCollimatorCenter_mm")
    def yCollimatorCenter_mm = column[Double]("yCollimatorCenter_mm")
    def north090_mm = column[Double]("north090_mm")
    def south090_mm = column[Double]("south090_mm")
    def east090_mm = column[Double]("east090_mm")
    def west090_mm = column[Double]("west090_mm")
    def north270_mm = column[Double]("north270_mm")
    def south270_mm = column[Double]("south270_mm")
    def east270_mm = column[Double]("east270_mm")
    def west270_mm = column[Double]("west270_mm")

    def * = (
      collimatorCenteringPK.?,
      outputPK,
      status,
      xCollimatorCenterMinusImageCenter_mm,
      yCollimatorCenterMinusImageCenter_mm,
      xCollimatorCenter_mm,
      yCollimatorCenter_mm,
      north090_mm,
      south090_mm,
      east090_mm,
      west090_mm,
      north270_mm,
      south270_mm,
      east270_mm,
      west270_mm) <> ((CollimatorCentering.apply _)tupled, CollimatorCentering.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[CollimatorCenteringTable]

  override val topXmlLabel = "CollimatorCentering"

  def get(collimatorCenteringPK: Long): Option[CollimatorCentering] = {
    val action = for {
      inst <- CollimatorCentering.query if inst.collimatorCenteringPK === collimatorCenteringPK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[CollimatorCentering] = {
    val action = for {
      inst <- CollimatorCentering.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(collimatorCenteringPK: Long): Int = {
    val q = query.filter(_.collimatorCenteringPK === collimatorCenteringPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[CollimatorCentering]) = {
    val ops = list.map { imgId => CollimatorCentering.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ??? // TODO
  }

  def insertSeq(list: Seq[CollimatorCentering]): Unit = {
    val ops = list.map { loc => CollimatorCentering.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }
}
