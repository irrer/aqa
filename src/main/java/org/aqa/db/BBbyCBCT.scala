package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput
import org.aqa.run.ProcedureStatus
import javax.vecmath.Point3d

case class BBbyCBCT(
  bbByCBCTPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  rtplanSOPInstanceUID: String, // UID of rtplan
  cbctSeriesInstanceUid: String, // series instance UID of CBCT
  offset_mm: Double, // distance between measured CBCT position and expected (plan) location (aka: positioning error)
  status: String, // termination status
  rtplanX_mm: Double, // expected X position in rtplan
  rtplanY_mm: Double, // expected Y position in rtplan
  rtplanZ_mm: Double, // expected Z position in rtplan
  cbctX_mm: Double, // expected X position in cbct
  cbctY_mm: Double, // expected Y position in cbct
  cbctZ_mm: Double // expected Z position in cbct
) {

  def insert: BBbyCBCT = {
    val insertQuery = BBbyCBCT.query returning BBbyCBCT.query.map(_.bbByCBCTPK) into ((bbByCBCT, bbByCBCTPK) => bbByCBCT.copy(bbByCBCTPK = Some(bbByCBCTPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(BBbyCBCT.query.insertOrUpdate(this))

  override def toString: String =
    "bbByCBCTPK : " + bbByCBCTPK +
      "\n    outputPK : " + outputPK +
      "\n    rtplanSOPInstanceUID : " + rtplanSOPInstanceUID +
      "\n    cbctSeriesInstanceUid : " + cbctSeriesInstanceUid +
      "\n    offset_mm : " + Util.fmtDbl(offset_mm) +
      "\n    status : " + status +
      "\n    plan X,Y,Z : " + Util.fmtDbl(rtplanX_mm) + ", " + Util.fmtDbl(rtplanY_mm) + ", " + Util.fmtDbl(rtplanZ_mm) +
      "\n    cbct X,Y,Z : " + Util.fmtDbl(cbctX_mm) + ", " + Util.fmtDbl(cbctY_mm) + ", " + Util.fmtDbl(cbctZ_mm)

  //def pass = status.equalsIgnoreCase(ProcedureStatus.done.toString)

  val rtplan = new Point3d(rtplanX_mm, rtplanY_mm, rtplanZ_mm)

  val cbct = new Point3d(cbctX_mm, cbctY_mm, cbctZ_mm)

  val err = new Point3d(rtplanX_mm - cbctX_mm, rtplanY_mm - cbctY_mm, rtplanZ_mm - cbctZ_mm)
}

object BBbyCBCT extends ProcedureOutput {
  class BBbyCBCTTable(tag: Tag) extends Table[BBbyCBCT](tag, "bbByCBCT") {

    def bbByCBCTPK = column[Long]("bbByCBCTPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def rtplanSOPInstanceUID = column[String]("rtplanSOPInstanceUID")
    def cbctSeriesInstanceUid = column[String]("cbctSeriesInstanceUid")
    def offset_mm = column[Double]("offset_mm")
    def status = column[String]("status")
    def planX_mm = column[Double]("planX_mm")
    def planY_mm = column[Double]("planY_mm")
    def planZ_mm = column[Double]("planZ_mm")
    def cbctX_mm = column[Double]("cbctX_mm")
    def cbctY_mm = column[Double]("cbctY_mm")
    def cbctZ_mm = column[Double]("cbctZ_mm")

    def * = (
      bbByCBCTPK.?,
      outputPK,
      rtplanSOPInstanceUID,
      cbctSeriesInstanceUid,
      offset_mm,
      status,
      planX_mm,
      planY_mm,
      planZ_mm,
      cbctX_mm,
      cbctY_mm,
      cbctZ_mm) <> ((BBbyCBCT.apply _)tupled, BBbyCBCT.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BBbyCBCTTable]

  override val topXmlLabel = "BBbyCBCT"

  def get(bbByCBCTPK: Long): Option[BBbyCBCT] = {
    val action = for {
      inst <- BBbyCBCT.query if inst.bbByCBCTPK === bbByCBCTPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all BBbyCBCT for the given output
   */
  def getByOutput(outputPK: Long): Seq[BBbyCBCT] = {
    val action = for {
      inst <- BBbyCBCT.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(bbByCBCTPK: Long): Int = {
    val q = query.filter(_.bbByCBCTPK === bbByCBCTPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[BBbyCBCT] = {
    ???
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[BBbyCBCT]): Unit = {
    val ops = list.map { loc => BBbyCBCT.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }
}
