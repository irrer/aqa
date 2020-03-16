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
  doseMLC_cu: Double, // average dose value in CU
  doseOpen_cu: Double, // average dose value of open in CU
  beamAverage_pct: Double, // average percent dose value for all VMAT readings for this beam
  // The following 4 columns are the top, bottom, left, and right positions of the
  // collimator in mm for this data as specified by the RTPLAN.
  topRtplan_mm: Double,
  bottomRtplan_mm: Double,
  leftRtplan_mm: Double,
  rightRtplan_mm: Double,
  // The following 4 columns are the the top, bottom, left, and right positions of position of
  // rectangle in mm used to take measurements.  This is established by:
  //
  // 1: Extracting the position from the RTPLAN
  // 2: Compensating for central axis shift
  // 3: Shrinking the rectangle by the amount specified in <code>Config.VMATBorderThickness</code> to reduce the effects of edge penumbras
  topAOI_mm: Double,
  bottomAOI_mm: Double,
  leftAOI_mm: Double,
  rightAOI_mm: Double) {

  def insert: VMAT = {
    val insertQuery = VMAT.query returning VMAT.query.map(_.vmatPK) into
      ((vmat, vmatPK) => vmat.copy(vmatPK = Some(vmatPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(VMAT.query.insertOrUpdate(this))

  /** Percent of DR-GS over OPEN. */
  def percent: Double = (doseMLC_cu / doseOpen_cu) * 100

  /** amount that this percentage differs from the average percent: percent - beamAverage_pct. */
  def diff_pct = percent - beamAverage_pct

  override def toString: String = {
    "    vmatPK: " + vmatPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    left,right planned: " + leftRtplan_mm + ", " + rightRtplan_mm + "\n" +
      "    left,right measured: " + leftAOI_mm + ", " + rightAOI_mm + "\n" +
      "    doseMLC_cu: " + Util.fmtDbl(doseMLC_cu) + "\n" +
      "    doseOpen_cu: " + Util.fmtDbl(doseOpen_cu) + "\n" +
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
    def doseMLC_cu = column[Double]("doseMLC_cu")
    def doseOpen_cu = column[Double]("doseOpen_cu")
    def beamAverage_pct = column[Double]("doseOpen_cu")
    def topRtplan_mm = column[Double]("topRtplan_mm")
    def bottomRtplan_mm = column[Double]("bottomRtplan_mm")
    def leftRtplan_mm = column[Double]("leftRtplan_mm")
    def rightRtplan_mm = column[Double]("rightRtplan_mm")
    def topAOI_mm = column[Double]("topAOI_mm")
    def bottomAOI_mm = column[Double]("bottomAOI_mm")
    def leftAOI_mm = column[Double]("leftAOI_mm")
    def rightAOI_mm = column[Double]("rightAOI_mm")

    def * = (
      vmatPK.?,
      outputPK,
      SOPInstanceUID,
      SOPInstanceUIDOpen,
      beamName,
      beamNameOpen,
      doseMLC_cu,
      doseOpen_cu,
      beamAverage_pct,
      topRtplan_mm,
      bottomRtplan_mm,
      leftRtplan_mm,
      rightRtplan_mm,
      topAOI_mm,
      bottomAOI_mm,
      leftAOI_mm,
      rightAOI_mm) <> ((VMAT.apply _)tupled, VMAT.unapply _)

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
