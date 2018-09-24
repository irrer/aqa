package org.aqa.db

import slick.driver.PostgresDriver.api._
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

case class SymmetryAndFlatness(
  symmetryAndFlatnessPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // UID of source image
  beamName: String, // name of beam in plan

  axialSymmetry_mm: Double,
  axialSymmetryBaseline_mm: Double,
  axialSymmetryStatus: String,

  transverseSymmetry_mm: Double,
  transverseSymmetryBaseline_mm: Double,
  transverseSymmetryStatus: String,

  flatness_mm: Double,
  flatnessBaseline_mm: Double,
  flatnessStatus: String) {

  def insert: SymmetryAndFlatness = {
    val insertQuery = SymmetryAndFlatness.query returning SymmetryAndFlatness.query.map(_.symmetryAndFlatnessPK) into
      ((symmetryAndFlatness, symmetryAndFlatnessPK) => symmetryAndFlatness.copy(symmetryAndFlatnessPK = Some(symmetryAndFlatnessPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(SymmetryAndFlatness.query.insertOrUpdate(this))

  override def toString: String = {
    "    symmetryAndFlatnessPK: " + symmetryAndFlatnessPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    axialSymmetry_mm: " + axialSymmetry_mm + "\n" +
      "    axialSymmetryBaseline_mm: " + axialSymmetryBaseline_mm + "\n" +
      "    axialSymmetryStatus: " + axialSymmetryStatus + "\n" +
      "    transverseSymmetry_mm: " + transverseSymmetry_mm + "\n" +
      "    transverseSymmetryBaseline_mm: " + transverseSymmetryBaseline_mm + "\n" +
      "    transverseSymmetryStatus: " + transverseSymmetryStatus + "\n" +
      "    flatness_mm: " + flatness_mm + "\n" +
      "    flatnessBaseline_mm: " + flatnessBaseline_mm + "\n" +
      "    flatnessStatus: " + flatnessStatus + "\n"
  }
}

object SymmetryAndFlatness extends ProcedureOutput {
  class SymmetryAndFlatnessTable(tag: Tag) extends Table[SymmetryAndFlatness](tag, "symmetryAndFlatness") {

    def symmetryAndFlatnessPK = column[Long]("symmetryAndFlatnessPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def axialSymmetry_mm = column[Double]("axialSymmetry_mm")
    def axialSymmetryBaseline_mm = column[Double]("axialSymmetryBaseline_mm")
    def axialSymmetryStatus = column[String]("axialSymmetryStatus")
    def transverseSymmetry_mm = column[Double]("transverseSymmetry_mm")
    def transverseSymmetryBaseline_mm = column[Double]("transverseSymmetryBaseline_mm")
    def transverseSymmetryStatus = column[String]("transverseSymmetryStatus")
    def flatness_mm = column[Double]("flatness_mm")
    def flatnessBaseline_mm = column[Double]("flatnessBaseline_mm")
    def flatnessStatus = column[String]("flatnessStatus")

    def * = (
      symmetryAndFlatnessPK.?,
      outputPK,
      SOPInstanceUID,
      beamName,
      axialSymmetry_mm,
      axialSymmetryBaseline_mm,
      axialSymmetryStatus,
      transverseSymmetry_mm,
      transverseSymmetryBaseline_mm,
      transverseSymmetryStatus,
      flatness_mm,
      flatnessBaseline_mm,
      flatnessStatus) <> ((SymmetryAndFlatness.apply _)tupled, SymmetryAndFlatness.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[SymmetryAndFlatnessTable]

  override val topXmlLabel = "SymmetryAndFlatness"

  def get(symmetryAndFlatnessPK: Long): Option[SymmetryAndFlatness] = {
    val action = for {
      inst <- SymmetryAndFlatness.query if inst.symmetryAndFlatnessPK === symmetryAndFlatnessPK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[SymmetryAndFlatness] = {
    val action = for {
      inst <- SymmetryAndFlatness.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(symmetryAndFlatnessPK: Long): Int = {
    val q = query.filter(_.symmetryAndFlatnessPK === symmetryAndFlatnessPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[SymmetryAndFlatness]) = {
    val ops = list.map { imgId => SymmetryAndFlatness.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ??? // TODO
  }

  def insertSeq(list: Seq[SymmetryAndFlatness]): Unit = {
    val ops = list.map { loc => SymmetryAndFlatness.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class SymmetryAndFlatnessHistory(date: Date, symmetryAndFlatness: SymmetryAndFlatness)

  /**
   * Get the SymmetryAndFlatness results that are nearest in time to the given date, preferring those that have an earlier date.
   *
   * Implementation note: Two DB queries are performed, one for data before and including the time stamp given, and another query for after.
   *
   * @param limit: Only get this many results.  First get all the results that are at or before the given time.  If there
   * are <code>limit</code> or more values, then return those.  Otherwise, also get values after the given time
   *
   * @param machinePK: For this machine
   *
   * @param beamName: For this beam
   *
   * @param procedurePK: For this procedure
   *
   * @param date: Relative to this date.  If None, then use current date.
   */
  def recentHistory(limit: Int, machinePK: Long, procedurePK: Long, beamName: String, date: Option[Timestamp]) = {

    import java.sql.{ Timestamp, Date, Time }
    import org.joda.time.DateTime
    import org.joda.time.{ DateTime, LocalDate, LocalTime, DateTimeZone }
    import org.joda.time.format._

    Trace.trace(limit)
    Trace.trace(machinePK)
    Trace.trace(procedurePK)
    Trace.trace(beamName)
    Trace.trace(date.get)
    implicit def jodaTimeMapping: BaseColumnType[DateTime] = MappedColumnType.base[DateTime, Timestamp](
      dateTime => new Timestamp(dateTime.getMillis),
      timeStamp => new DateTime(timeStamp.getTime))

    Trace.trace
    val ts = if (date.isDefined) date.get else new Timestamp(Long.MaxValue)
    Trace.trace

    val before = {
      Trace.trace
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate.get.toString <= ts.toString)).map(o => (o.outputPK, o.dataDate, o.machinePK))
        symmetryAndFlatness <- SymmetryAndFlatness.query.filter(c => c.outputPK === output._1 && c.beamName === beamName)
      } yield ((output._2, symmetryAndFlatness))

      Trace.trace
      val sorted = search.distinct.sortBy(_._1.desc).take(limit)
      val result = Db.run(sorted.result)
      Trace.trace
      result
    }

    def after = {
      Trace.trace
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate.get.toString > ts.toString)).map(o => (o.outputPK, o.dataDate, o.machinePK))
        symmetryAndFlatness <- SymmetryAndFlatness.query.filter(c => c.outputPK === output._1 && c.beamName === beamName)
      } yield ((output._2, symmetryAndFlatness))
      Trace.trace

      val sorted = search.distinct.sortBy(_._1.asc).take(limit)
      Trace.trace
      val result = Db.run(sorted.result)
      Trace.trace
      result
    }
    Trace.trace

    val all = if (before.size >= limit) before else (before ++ after).take(limit)
    Trace.trace

    // Convert to class and make sure that they are temporally ordered.
    val result = all.map(h => new SymmetryAndFlatnessHistory(h._1.get, h._2)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    Trace.trace
    result
  }
}
