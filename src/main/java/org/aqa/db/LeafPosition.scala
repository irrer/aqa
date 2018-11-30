package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

case class LeafPosition(
  leafPositionPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // UID of source image
  beamName: String, // name of beam in plan
  leafIndex: Int, // leaf number
  offset_mm: Double, // difference from expected location: measured - expected
  measuredEndPosition_mm: Double, // measured position of leaf end
  expectedEndPosition_mm: Double, // expected position of leaf end
  measuredLowSide_mm: Double, // measured position of low side of leaf
  measuredHighSide_mm: Double // measured position of low side of leaf
) {

  def insert: LeafPosition = {
    val insertQuery = LeafPosition.query returning LeafPosition.query.map(_.leafPositionPK) into ((leafPosition, leafPositionPK) => leafPosition.copy(leafPositionPK = Some(leafPositionPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(LeafPosition.query.insertOrUpdate(this))

  override def toString: String = "Beam: " + beamName + "  Leaf: " + leafIndex + "  offset_mm: " + offset_mm
}

object LeafPosition extends ProcedureOutput {
  class LeafPositionTable(tag: Tag) extends Table[LeafPosition](tag, "leafPosition") {

    def leafPositionPK = column[Long]("leafPositionPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def leafIndex = column[Int]("leafIndex")
    def offset_mm = column[Double]("offset_mm")
    def measuredEndPosition_mm = column[Double]("measuredEndPosition_mm")
    def expectedEndPosition_mm = column[Double]("expectedEndPosition_mm")
    def measuredLowSide_mm = column[Double]("measuredLowSide_mm")
    def measuredHighSide_mm = column[Double]("measuredHighSide_mm")

    def * = (
      leafPositionPK.?,
      outputPK,
      SOPInstanceUID,
      beamName,
      leafIndex,
      offset_mm,
      measuredEndPosition_mm,
      expectedEndPosition_mm,
      measuredLowSide_mm,
      measuredHighSide_mm) <> ((LeafPosition.apply _)tupled, LeafPosition.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
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
