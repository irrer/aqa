package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput
import java.util.Date
import java.sql.Timestamp
import edu.umro.ScalaUtil.Trace

/**
 * Encapsulate data from a single VMAT measurement.  Each beam analyzed will have several of these.
 */
case class VMAT(
  vmatPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // UID of source image
  SOPInstanceUIDOpen: String, // UID of open beam
  beamName: String, // name of beam in plan
  beamNameOpen: String, // name of open beam in plan
  averageDose_cu: Double, // average dose value in CU
  averageDoseOpen_cu: Double, // average dose value of open in CU
  beamAverage_pct: Double, // average percent dose value for all VMAT readings for this beam
  top_mm: Double, // top position of planned position of rectangle in mm
  bottom_mm: Double, // bottom position of planned position of rectangle in mm
  left_mm: Double, // left position of planned position of rectangle in mm
  right_mm: Double // right position of planned position of rectangle in mm
) {

  def insert: VMAT = {
    val insertQuery = VMAT.query returning VMAT.query.map(_.vmatPK) into
      ((vmat, vmatPK) => vmat.copy(vmatPK = Some(vmatPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(VMAT.query.insertOrUpdate(this))

  /** Percent of DR-GS over OPEN. */
  def percent = (averageDose_cu / averageDoseOpen_cu) * 100

  /** amount that this percentage differs from the average percent: percent - beamAverage_pct. */
  def diff_pct = percent - beamAverage_pct

  override def toString: String = {
    "    vmatPK: " + vmatPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    left,right: " + left_mm + ", " + right_mm + "\n" +
      "    averageDose_cu: " + Util.fmtDbl(averageDose_cu) + "\n" +
      "    averageDoseOpen_cu: " + Util.fmtDbl(averageDoseOpen_cu) + "\n" +
      "    percent: " + Util.fmtDbl(percent) + "\n" +
      "    diff_pct: " + Util.fmtDbl(diff_pct) + "\n"
  }
}

object VMAT extends ProcedureOutput {
  class VMATTable(tag: Tag) extends Table[VMAT](tag, "vmat") {

    def vmatPK = column[Long]("vmatPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def SOPInstanceUIDOpen = column[String]("SOPInstanceUIDOpen")
    def beamName = column[String]("beamName")
    def beamNameOpen = column[String]("beamNameOpen")
    def averageDose_cu = column[Double]("averageDose_cu")
    def averageDoseOpen_cu = column[Double]("averageDoseOpen_cu")
    def beamAverage_pct = column[Double]("averageDoseOpen_cu")
    def top_mm = column[Double]("top_mm")
    def bottom_mm = column[Double]("bottom_mm")
    def left_mm = column[Double]("left_mm")
    def right_mm = column[Double]("right_mm")

    def * = (
      vmatPK.?,
      outputPK,
      SOPInstanceUID,
      SOPInstanceUIDOpen,
      beamName,
      beamNameOpen,
      averageDose_cu,
      averageDoseOpen_cu,
      beamAverage_pct,
      top_mm,
      bottom_mm,
      left_mm,
      right_mm) <> ((VMAT.apply _)tupled, VMAT.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[VMATTable]

  override val topXmlLabel = "VMAT"

  def get(vmatPK: Long): Option[VMAT] = {
    val action = for {
      inst <- VMAT.query if inst.vmatPK === vmatPK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[VMAT] = {
    val action = for {
      inst <- VMAT.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(vmatPK: Long): Int = {
    val q = query.filter(_.vmatPK === vmatPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[VMAT]) = {
    val ops = list.map { imgId => VMAT.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ??? // TODO
  }

  def insertSeq(list: Seq[VMAT]): Unit = {
    val ops = list.map { loc => VMAT.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class VMATHistory(date: Date, vmat: VMAT) {
    override def toString = {
      "date: " + date + "    " + vmat
    }
  }

  /**
   * Get the VMAT results that are nearest in time to the given date.
   *
   * @param limit: Get up to this many sets of results before and after the given date.  If a
   * limit of 10 is given then get up to 10 sets of results that occurred before and up to
   * 10 that occurred at or after.  This means that up to 20 sets of results could be returned.
   * A set of results is all the VMAT values recorded from the beams of a single output.
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
        vmat <- VMAT.query.filter(c => c.outputPK === output._1)
      } yield ((output._2, vmat))

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
        vmat <- VMAT.query.filter(c => c.outputPK === output._1)
      } yield ((output._2, vmat))

      val sorted = search.distinct.sortBy(_._1.asc)
      //println(sorted.result.statements.mkString("\n    "))
      Db.run(sorted.result)
    }

    val all = before ++ after

    val result = all.map(h => new VMATHistory(h._1.get, h._2)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    result
  }
}
