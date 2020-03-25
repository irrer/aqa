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
import edu.umro.ScalaUtil.Trace

case class BBbyCBCT(
  bbByCBCTPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  rtplanSOPInstanceUID: String, // UID of RTPLAN
  cbctSeriesInstanceUid: String, // series instance UID of CBCT
  offset_mm: Double, // distance between measured CBCT position and expected (plan) location (aka: positioning error)
  status: String, // termination status
  rtplanX_mm: Double, // expected X position in RTPLAN
  rtplanY_mm: Double, // expected Y position in RTPLAN
  rtplanZ_mm: Double, // expected Z position in RTPLAN
  cbctX_mm: Double, // expected X position in CBCT
  cbctY_mm: Double, // expected Y position in CBCT
  cbctZ_mm: Double // expected Z position in CBCT
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

  case class BBbyCBCTHistory(date: Date, bbByCBCT: BBbyCBCT) {
    override def toString = {
      "date: " + date + "    " + bbByCBCT
    }
  }

  /**
   * Get the BBbyCBCT results that are nearest in time to the given date.
   *
   * @param limit: Get up to this many sets of results before and after the given date.  If a
   * limit of 10 is given then get up to 10 sets of results that occurred before and up to
   * 10 that occurred at or after.  This means that up to 20 sets of results could be returned.
   * A set of results is all the CBCT values recorded from the beams of a single output.
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
        bbByCBCT <- BBbyCBCT.query.filter(c => c.outputPK === output._1)
      } yield ((output._2, bbByCBCT))

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
        bbByCBCT <- BBbyCBCT.query.filter(c => c.outputPK === output._1)
      } yield ((output._2, bbByCBCT))

      val sorted = search.distinct.sortBy(_._1.asc)
      //println(sorted.result.statements.mkString("\n    "))
      Db.run(sorted.result)
    }

    Trace.trace("before.size: " + before.size + "    after.size: " + after.size)

    val all = before ++ after

    val result = all.map(h => new BBbyCBCTHistory(h._1.get, h._2)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    result
  }

  /**
   * Get the procedure PK for the BBbyCBCT procedure.
   */
  def getProcedurePK: Option[Long] = {
    Db.run(query.result.headOption) match {
      case Some(cbct) => {
        Output.get(cbct.outputPK) match {
          case Some(output) => Some(output.procedurePK)
          case _ => None
        }
      }
      case _ => None
    }
  }

  /** CBCT data and related results. */
  case class DailyDataSetCBCT(output: Output, machine: Machine, bbByCBCT: BBbyCBCT);

  /**
   * Get all results that were acquired on one day for one institution.
   */
  def getForOneDay(date: Date, institutionPK: Long): Seq[DailyDataSetCBCT] = {

    val beginDate = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(date).replaceAll("T.*", "T00:00:00")).getTime)
    val endDate = new Timestamp(beginDate.getTime + (24 * 60 * 60 * 1000))

    val search = for {
      output <- Output.query.filter(o => o.dataDate.isDefined && (o.dataDate >= beginDate) && (o.dataDate < endDate))
      bbByCBCT <- BBbyCBCT.query.filter(c => (c.outputPK === output.outputPK))
      machine <- Machine.query.filter(m => (m.machinePK === output.machinePK) && (m.institutionPK === institutionPK))
      cbct <- BBbyCBCT.query.filter(c => c.bbByCBCTPK === bbByCBCT.bbByCBCTPK)
    } yield (output, machine, bbByCBCT)

    val seq = Db.run(search.result).map(omc => new DailyDataSetCBCT(omc._1, omc._2, omc._3))
    seq
  }

}
