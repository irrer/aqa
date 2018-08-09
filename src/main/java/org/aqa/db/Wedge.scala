package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

case class Wedge(
  wedgePK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // SOPInstanceUID of DICOM file.
  beamName: String,
  position_mm: Double, // position in mm from isocenter.  Will be either X for horizontal wedge or Y for vertical wedge.
  radiodensity_hu: Double // radiodensity measured the given position in Hounsfield units
) {

  def insert: Wedge = {
    val insertQuery = Wedge.query returning Wedge.query.map(_.wedgePK) into
      ((Wedge, wedgePK) => Wedge.copy(wedgePK = Some(wedgePK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(Wedge.query.insertOrUpdate(this))

  override def toString: String = ("position: " + position_mm + "    radiodensity:" + radiodensity_hu).trim
}

object Wedge extends ProcedureOutput {
  class WedgeTable(tag: Tag) extends Table[Wedge](tag, "Wedge") {

    def wedgePK = column[Long]("wedgePK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def position_mm = column[Double]("position_mm")
    def radiodensity_hu = column[Double]("radiodensity_hu")

    def * = (
      wedgePK.?,
      outputPK,
      SOPInstanceUID,
      beamName,
      position_mm,
      radiodensity_hu) <> ((Wedge.apply _)tupled, Wedge.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[WedgeTable]

  override val topXmlLabel = "Wedge"

  def get(wedgePK: Long): Option[Wedge] = {
    val action = for {
      inst <- Wedge.query if inst.wedgePK === wedgePK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all wedges for the given output
   */
  def getByOutput(outputPK: Long): Seq[Wedge] = {
    val action = for {
      inst <- Wedge.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(wedgePK: Long): Int = {
    val q = query.filter(_.wedgePK === wedgePK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ???
  }

  def insertSeq(list: Seq[Wedge]): Unit = {
    val ops = list.map { loc => Wedge.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    System.exit(99)
  }
}
