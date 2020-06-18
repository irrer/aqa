package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput
import org.aqa.run.ProcedureStatus

case class LeafPosition(
  leafPositionPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // UID of source image
  beamName: String, // name of beam in plan
  leafIndex: Int, // leaf number starting at 1
  leafPositionIndex: Int, // leaf position number as it moves across the field
  offset_mm: Double, // difference from expected location: measuredEndPosition_mm - expectedEndPosition_mm
  status: String, // termination status
  measuredEndPosition_mm: Double, // measured position of leaf end
  expectedEndPosition_mm: Double, // expected position of leaf end
  measuredMinorSide_mm: Double, // measured position of top side of leaf, or left side if collimator is vertical
  measuredMajorSide_mm: Double // measured position of bottom side of leaf, or right side if collimator is vertical
) {

  def insert: LeafPosition = {
    val insertQuery = LeafPosition.query returning LeafPosition.query.map(_.leafPositionPK) into ((leafPosition, leafPositionPK) => leafPosition.copy(leafPositionPK = Some(leafPositionPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(LeafPosition.query.insertOrUpdate(this))

  override def toString: String = "Beam: " + beamName +
    "  Leaf Index: " + leafIndex.formatted("%2d") +
    "  Leaf Position Index: " + leafPositionIndex.formatted("%2d") +
    "  offset_mm: " + offset_mm.formatted("%9.5f") +
    "  expectedEndPosition_mm: " + expectedEndPosition_mm.formatted("%6.2f") +
    "  measuredEndPosition_mm: " + measuredEndPosition_mm.formatted("%10.5f")

  def pass = status.equalsIgnoreCase(ProcedureStatus.pass.toString)
}

object LeafPosition extends ProcedureOutput {
  class LeafPositionTable(tag: Tag) extends Table[LeafPosition](tag, "leafPosition") {

    def leafPositionPK = column[Long]("leafPositionPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def leafIndex = column[Int]("leafIndex")
    def leafPositionIndex = column[Int]("leafPositionIndex")
    def offset_mm = column[Double]("offset_mm")
    def status = column[String]("status")
    def measuredEndPosition_mm = column[Double]("measuredEndPosition_mm")
    def expectedEndPosition_mm = column[Double]("expectedEndPosition_mm")
    def measuredMinorSide_mm = column[Double]("measuredLowSide_mm")
    def measuredMajorSide_mm = column[Double]("measuredHighSide_mm")

    def * = (
      leafPositionPK.?,
      outputPK,
      SOPInstanceUID,
      beamName,
      leafIndex,
      leafPositionIndex,
      offset_mm,
      status,
      measuredEndPosition_mm,
      expectedEndPosition_mm,
      measuredMinorSide_mm,
      measuredMajorSide_mm) <> ((LeafPosition.apply _)tupled, LeafPosition.unapply _)

    def outputFK = foreignKey("outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[LeafPositionTable]

  override val topXmlLabel = "LeafPosition"

  def get(leafPositionPK: Long): Option[LeafPosition] = {
    val action = for {
      inst <- LeafPosition.query if inst.leafPositionPK === leafPositionPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all LeafPosition for the given output
   */
  def getByOutput(outputPK: Long): Seq[LeafPosition] = {
    val action = for {
      inst <- LeafPosition.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(leafPositionPK: Long): Int = {
    val q = query.filter(_.leafPositionPK === leafPositionPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[LeafPosition] = {
    ???
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[LeafPosition]): Unit = {
    val ops = list.map { loc => LeafPosition.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }
}
