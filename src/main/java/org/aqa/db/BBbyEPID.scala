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
import javax.vecmath.Point3d
import java.sql.Timestamp
import java.util.Date
import org.aqa.AngleType
import edu.umro.ScalaUtil.Trace

/**
 * Store the analysis results for one EPID image containing a BB.
 */
case class BBbyEPID(
  bbByEPIDPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  epidSOPInstanceUid: String, // SOP instance UID of EPID image
  offset_mm: Double, // distance between measured EPID position and expected (plan) location (aka: positioning error)
  gantryAngle_deg: Double, // gantry angle in degrees
  status: String, // termination status
  epidImageX_mm: Double, // X position in EPID image
  epidImageY_mm: Double, // Y position in EPID image
  epid3DX_mm: Double, // X position in EPID in 3D plan space
  epid3DY_mm: Double, // Y position in EPID in 3D plan space
  epid3DZ_mm: Double // Z position in EPID in 3D plan space
) {

  def insert: BBbyEPID = {
    val insertQuery = BBbyEPID.query returning BBbyEPID.query.map(_.bbByEPIDPK) into ((bbByEPID, bbByEPIDPK) => bbByEPID.copy(bbByEPIDPK = Some(bbByEPIDPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(BBbyEPID.query.insertOrUpdate(this))

  override def toString: String =
    "bbByEPIDPK : " + bbByEPIDPK +
      "\n    outputPK : " + outputPK +
      "\n    epidSOPInstanceUid : " + epidSOPInstanceUid +
      "\n    offset_mm : " + Util.fmtDbl(offset_mm) +
      "\n    gantryAngle_deg : " + Util.fmtDbl(gantryAngle_deg) +
      "\n    status : " + status +
      "\n    epid image X,Y : " + Util.fmtDbl(epidImageX_mm) + ", " + Util.fmtDbl(epidImageY_mm) +
      "\n    epid 3D X,Y,Z : " + Util.fmtDbl(epid3DX_mm) + ", " + Util.fmtDbl(epid3DY_mm) + ", " + Util.fmtDbl(epid3DZ_mm)

  val epid = new Point3d(epid3DX_mm, epid3DY_mm, epid3DZ_mm)
}

object BBbyEPID extends ProcedureOutput {
  class BBbyEPIDTable(tag: Tag) extends Table[BBbyEPID](tag, "bbByEPID") {

    def bbByEPIDPK = column[Long]("bbByEPIDPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def epidSOPInstanceUid = column[String]("epidSOPInstanceUid")
    def offset_mm = column[Double]("offset_mm")
    def gantryAngle_deg = column[Double]("gantryAngle_deg")
    def status = column[String]("status")
    def epidImageX_mm = column[Double]("epidImageX_mm")
    def epidImageY_mm = column[Double]("epidImageY_mm")
    def epid3DX_mm = column[Double]("epid3DX_mm")
    def epid3DY_mm = column[Double]("epid3DY_mm")
    def epid3DZ_mm = column[Double]("epid3DZ_mm")

    def * = (
      bbByEPIDPK.?,
      outputPK,
      epidSOPInstanceUid,
      offset_mm,
      gantryAngle_deg,
      status,
      epidImageX_mm,
      epidImageY_mm,
      epid3DX_mm,
      epid3DY_mm,
      epid3DZ_mm) <> ((BBbyEPID.apply _)tupled, BBbyEPID.unapply _)

    def outputFK = foreignKey("BBbyEPID_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BBbyEPIDTable]

  override val topXmlLabel = "BBbyEPID"

  def get(bbByEPIDPK: Long): Option[BBbyEPID] = {
    val action = for {
      inst <- BBbyEPID.query if inst.bbByEPIDPK === bbByEPIDPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all BBbyEPID for the given output
   */
  def getByOutput(outputPK: Long): Seq[BBbyEPID] = {
    val action = for {
      inst <- BBbyEPID.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(bbByEPIDPK: Long): Int = {
    val q = query.filter(_.bbByEPIDPK === bbByEPIDPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[BBbyEPID] = {
    ???
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[BBbyEPID]): Unit = {
    val ops = list.map { loc => BBbyEPID.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class BBbyEPIDHistory(date: Date, bbByEPID: BBbyEPID) {
    override def toString = {
      "date: " + date + "    " + bbByEPID
    }
  }

  /**
   * Get the BBbyEPID results that are nearest in time to the given date.
   *
   * @param machinePK: For this machine
   *
   * @param procedurePK: For this procedure
   */
  def history(machinePK: Long, procedurePK: Long) = {
    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK)).map(o => (o.outputPK, o.dataDate))
      bbByEPID <- BBbyEPID.query.filter(c => c.outputPK === output._1)
    } yield ((output._2, bbByEPID))
    //println(sorted.result.statements.mkString("\n    "))
    val result = Db.run(search.result).map(h => new BBbyEPIDHistory(h._1.get, h._2)).sortBy(_.date.getTime)
    result
  }

  /** EPID data and related results. */
  case class DailyDataSetEPID(output: Output, machine: Machine, bbByEPID: BBbyEPID) {
    private val angType = AngleType.classifyAngle(bbByEPID.gantryAngle_deg)
    def isHorz = angType.isDefined && angType.get.toString.equals(AngleType.horizontal.toString)
    def isVert = angType.isDefined && angType.get.toString.equals(AngleType.vertical.toString)
  }

  /**
   * Get all results that were acquired on one day for one institution.
   */
  def getForOneDay(date: Date, institutionPK: Long): Seq[DailyDataSetEPID] = {

    val beginDate = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(date).replaceAll("T.*", "T00:00:00")).getTime)
    val endDate = new Timestamp(beginDate.getTime + (24 * 60 * 60 * 1000))

    val search = for {
      output <- Output.query.filter(o => o.dataDate.isDefined && (o.dataDate >= beginDate) && (o.dataDate < endDate))
      bbByEPID <- BBbyEPID.query.filter(c => (c.outputPK === output.outputPK))
      machine <- Machine.query.filter(m => (m.machinePK === output.machinePK) && (m.institutionPK === institutionPK))
      cbct <- BBbyEPID.query.filter(c => c.bbByEPIDPK === bbByEPID.bbByEPIDPK)
    } yield (output, machine, bbByEPID)

    val seq = Db.run(search.result).map(omc => new DailyDataSetEPID(omc._1, omc._2, omc._3))
    seq
  }

}
