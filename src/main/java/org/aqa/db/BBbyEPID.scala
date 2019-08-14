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
import java.sql.Timestamp
import java.util.Date

case class BBbyEPID(
  bbByEPIDPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  rtplanSOPInstanceUID: String, // UID of rtplan
  epidSeriesInstanceUid: String, // series instance UID of EPID
  offset_mm: Double, // distance between measured EPID position and expected (plan) location (aka: positioning error)
  status: String, // termination status
  rtplanX_mm: Double, // expected X position in rtplan
  rtplanY_mm: Double, // expected Y position in rtplan
  rtplanZ_mm: Double, // expected Z position in rtplan
  epidX_mm: Double, // expected X position in epid
  epidY_mm: Double, // expected Y position in epid
  epidZ_mm: Double // expected Z position in epid
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
      "\n    rtplanSOPInstanceUID : " + rtplanSOPInstanceUID +
      "\n    epidSeriesInstanceUid : " + epidSeriesInstanceUid +
      "\n    offset_mm : " + Util.fmtDbl(offset_mm) +
      "\n    status : " + status +
      "\n    plan X,Y,Z : " + Util.fmtDbl(rtplanX_mm) + ", " + Util.fmtDbl(rtplanY_mm) + ", " + Util.fmtDbl(rtplanZ_mm) +
      "\n    epid X,Y,Z : " + Util.fmtDbl(epidX_mm) + ", " + Util.fmtDbl(epidY_mm) + ", " + Util.fmtDbl(epidZ_mm)

  //def pass = status.equalsIgnoreCase(ProcedureStatus.done.toString)

  val rtplan = new Point3d(rtplanX_mm, rtplanY_mm, rtplanZ_mm)

  val epid = new Point3d(epidX_mm, epidY_mm, epidZ_mm)

  val err = new Point3d(rtplanX_mm - epidX_mm, rtplanY_mm - epidY_mm, rtplanZ_mm - epidZ_mm)
}

object BBbyEPID extends ProcedureOutput {
  class BBbyEPIDTable(tag: Tag) extends Table[BBbyEPID](tag, "bbByEPID") {

    def bbByEPIDPK = column[Long]("bbByEPIDPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def rtplanSOPInstanceUID = column[String]("rtplanSOPInstanceUID")
    def epidSeriesInstanceUid = column[String]("epidSeriesInstanceUid")
    def offset_mm = column[Double]("offset_mm")
    def status = column[String]("status")
    def planX_mm = column[Double]("planX_mm")
    def planY_mm = column[Double]("planY_mm")
    def planZ_mm = column[Double]("planZ_mm")
    def epidX_mm = column[Double]("epidX_mm")
    def epidY_mm = column[Double]("epidY_mm")
    def epidZ_mm = column[Double]("epidZ_mm")

    def * = (
      bbByEPIDPK.?,
      outputPK,
      rtplanSOPInstanceUID,
      epidSeriesInstanceUid,
      offset_mm,
      status,
      planX_mm,
      planY_mm,
      planZ_mm,
      epidX_mm,
      epidY_mm,
      epidZ_mm) <> ((BBbyEPID.apply _)tupled, BBbyEPID.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
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
   * @param limit: Get up to this many sets of results before and after the given date.  If a
   * limit of 10 is given then get up to 10 sets of results that occurred before and up to
   * 10 that occurred at or after.  This means that up to 20 sets of results could be returned.
   * A set of results is all the EPID values recorded from the beams of a single output.
   *
   * @param machinePK: For this machine
   *
   * @param procedurePK: For this procedure
   *
   * @param date: Relative to this date.  If None, then use current date.
   */
  def recentHistory(limit: Int, machinePK: Long, procedurePK: Long, date: Option[Timestamp]) = {

    import java.sql.{ Timestamp, Date, Time }
    import org.joda.time.DateTime
    import org.joda.time.{ DateTime, LocalDate, LocalTime, DateTimeZone }
    import org.joda.time.format._

    implicit def jodaTimeMapping: BaseColumnType[DateTime] = MappedColumnType.base[DateTime, Timestamp](
      dateTime => new Timestamp(dateTime.getMillis),
      timeStamp => new DateTime(timeStamp.getTime))

    val dt = if (date.isDefined) date.get else new Timestamp(Long.MaxValue)

    val before = {
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate < dt)).
          distinct.
          sortBy(_.dataDate.desc).take(limit).
          map(o => (o.outputPK, o.dataDate))
        bbByEPID <- BBbyEPID.query.filter(c => c.outputPK === output._1)
      } yield ((output._2, bbByEPID))

      val sorted = search.distinct.sortBy(_._1.desc)
      //println(sorted.result.statements.mkString("\n    "))
      Db.run(sorted.result)
    }

    def after = {
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate >= dt)).
          distinct.
          sortBy(_.dataDate).take(limit).
          map(o => (o.outputPK, o.dataDate))
        bbByEPID <- BBbyEPID.query.filter(c => c.outputPK === output._1)
      } yield ((output._2, bbByEPID))

      val sorted = search.distinct.sortBy(_._1.asc)
      //println(sorted.result.statements.mkString("\n    "))
      Db.run(sorted.result)
    }

    val all = before ++ after

    val result = all.map(h => new BBbyEPIDHistory(h._1.get, h._2)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    result
  }

}
