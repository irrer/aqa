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

  SOPInstanceUID090: String, // UID of 90 degree DICOM image
  SOPInstanceUID270: String, // UID of 270 degree DICOM image

  xCollimatorCenterMinusImageCenter_mm: Double, // X center the collimator minus the center of the image in mm
  yCollimatorCenterMinusImageCenter_mm: Double, // Y center the collimator minus the center of the image in mm

  xCollimatorCenter_mm: Double, // X collimator center in mm
  yCollimatorCenter_mm: Double, // Y collimator center in mm

  X1_090_mm: Double, // X1 position of collimator leaf edge for gantry at 90 degrees (X axis) in mm
  X2_090_mm: Double, // X2 position of collimator leaf edge for gantry at 90 degrees (X axis) in mm
  Y1_090_mm: Double, // Y1 position of collimator leaf edge for gantry at 90 degrees (Y axis) in mm
  Y2_090_mm: Double, // Y2 position of collimator leaf edge for gantry at 90 degrees (Y axis) in mm

  X1_270_mm: Double, // X1 position of collimator leaf edge for gantry at 270 degrees (X axis) in mm
  X2_270_mm: Double, // X2 position of collimator leaf edge for gantry at 270 degrees (X axis) in mm
  Y1_270_mm: Double, // Y1 position of collimator leaf edge for gantry at 270 degrees (Y axis) in mm
  Y2_270_mm: Double //  Y2 position of collimator leaf edge for gantry at 270 degrees (Y axis) in mm
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
      "    SOPInstanceUID090: " + SOPInstanceUID090 + "\n" +
      "    SOPInstanceUID270: " + SOPInstanceUID270 + "\n" +
      "    xCollimatorCenterMinusImageCenter_mm: " + xCollimatorCenterMinusImageCenter_mm + "\n" +
      "    yCollimatorCenterMinusImageCenter_mm: " + yCollimatorCenterMinusImageCenter_mm + "\n" +
      "    xCollimatorCenter_mm: " + xCollimatorCenter_mm + "\n" +
      "    yCollimatorCenter_mm: " + yCollimatorCenter_mm + "\n" +
      "    X1_090_mm: " + X1_090_mm + "\n" +
      "    X2_090_mm: " + X2_090_mm + "\n" +
      "    Y1_090_mm: " + Y1_090_mm + "\n" +
      "    Y2_090_mm: " + Y2_090_mm + "\n" +
      "    X1_270_mm: " + X1_270_mm + "\n" +
      "    X2_270_mm: " + X2_270_mm + "\n" +
      "    Y1_270_mm: " + Y1_270_mm + "\n" +
      "    Y2_270_mm: " + Y2_270_mm + "\n"
  }
}

object CollimatorCentering extends ProcedureOutput {
  class CollimatorCenteringTable(tag: Tag) extends Table[CollimatorCentering](tag, "collimatorCentering") {

    def collimatorCenteringPK = column[Long]("collimatorCenteringPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def status = column[String]("status")
    def SOPInstanceUID090 = column[String]("SOPInstanceUID090")
    def SOPInstanceUID270 = column[String]("SOPInstanceUID270")
    def xCollimatorCenterMinusImageCenter_mm = column[Double]("xCollimatorCenterMinusImageCenter_mm")
    def yCollimatorCenterMinusImageCenter_mm = column[Double]("yCollimatorCenterMinusImageCenter_mm")
    def xCollimatorCenter_mm = column[Double]("xCollimatorCenter_mm")
    def yCollimatorCenter_mm = column[Double]("yCollimatorCenter_mm")
    def X1_090_mm = column[Double]("X1_090_mm")
    def X2_090_mm = column[Double]("X2_090_mm")
    def Y1_090_mm = column[Double]("Y1_090_mm")
    def Y2_090_mm = column[Double]("Y2_090_mm")
    def X1_270_mm = column[Double]("X1_270_mm")
    def X2_270_mm = column[Double]("X2_270_mm")
    def Y1_270_mm = column[Double]("Y1_270_mm")
    def Y2_270_mm = column[Double]("Y2_270_mm")

    def * = (
      collimatorCenteringPK.?,
      outputPK,
      status,
      SOPInstanceUID090,
      SOPInstanceUID270,
      xCollimatorCenterMinusImageCenter_mm,
      yCollimatorCenterMinusImageCenter_mm,
      xCollimatorCenter_mm,
      yCollimatorCenter_mm,
      X1_090_mm,
      X2_090_mm,
      Y1_090_mm,
      Y2_090_mm,
      X1_270_mm,
      X2_270_mm,
      Y1_270_mm,
      Y2_270_mm) <> ((CollimatorCentering.apply _)tupled, CollimatorCentering.unapply _)

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
