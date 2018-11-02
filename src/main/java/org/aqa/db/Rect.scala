package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput
import java.sql.Timestamp
import java.util.Date

/**
 * Rectangle describing an area in an EPID image and it's average intensity.
 */
case class Rect(
  rectPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // SOPInstanceUID of DICOM file.
  name: String, // identifies what this is related to.  Usually specifies beam name and test.
  intensity_cu: Double, // average intensity for the entire rectangle in CU / square mm
  x_mm: Double, // x in mm from isocenter in isoplane.  0 is center.
  y_mm: Double, // y in mm from isocenter in isoplane.  0 is center.
  width_mm: Double, // width in mm in isoplane.
  height_mm: Double // height in mm in isoplane.
) {

  def insert: Rect = {
    val insertQuery = Rect.query returning Rect.query.map(_.rectPK) into
      ((Rect, rectPK) => Rect.copy(rectPK = Some(rectPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(Rect.query.insertOrUpdate(this))

  override def toString: String = {
    "name: " + name +
      "    intensity_cu:" + intensity_cu +
      "    x_mm:" + x_mm +
      "    y_mm:" + y_mm +
      "    width_mm:" + width_mm +
      "    x_mm:" + x_mm +
      "    height_mm:" + height_mm
  }
}

object Rect extends ProcedureOutput {
  class RectTable(tag: Tag) extends Table[Rect](tag, "Rect") {

    def rectPK = column[Long]("rectPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def name = column[String]("name")
    def intensity_cu = column[Double]("intensity_cu")
    def x_mm = column[Double]("x_mm")
    def y_mm = column[Double]("y_mm")
    def width_mm = column[Double]("width_mm")
    def height_mm = column[Double]("height_mm")

    def * = (
      rectPK.?,
      outputPK,
      SOPInstanceUID,
      name,
      intensity_cu,
      x_mm,
      y_mm,
      width_mm,
      height_mm) <> ((Rect.apply _)tupled, Rect.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[RectTable]

  override val topXmlLabel = "Rect"

  def get(rectPK: Long): Option[Rect] = {
    val action = for {
      inst <- Rect.query if inst.rectPK === rectPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all rects for the given output
   */
  def getByOutput(outputPK: Long): Seq[Rect] = {
    val action = for {
      inst <- Rect.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(rectPK: Long): Int = {
    val q = query.filter(_.rectPK === rectPK)
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

  def insertSeq(list: Seq[Rect]): Unit = {
    val ops = list.map { loc => Rect.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class RectHistory(date: Date, rect: Rect)

  /**
   * Get the Rect results that are nearest in time to the given date, preferring those that have an earlier date.
   *
   * Implementation note: Two DB queries are performed, one for data before and including the time stamp given, and another query for after.
   *
   * @param limit: Only get this many results.  First get all the results that are at or before the given time.  If there
   * are <code>limit</code> or more values, then return those.  Otherwise, also get values after the given time
   *
   * @param machinePK: For this machine
   *
   * @param name: With this name
   *
   * @param procedurePK: For this procedure
   *
   * @param date: Relative to this date.  If None, then use current date.
   */
  def recentHistory(limit: Int, machinePK: Long, procedurePK: Long, name: String, date: Option[Timestamp]) = {

    import java.sql.{ Timestamp, Date, Time }
    import org.joda.time.DateTime
    import org.joda.time.{ DateTime, LocalDate, LocalTime, DateTimeZone }
    import org.joda.time.format._

    implicit def jodaTimeMapping: BaseColumnType[DateTime] = MappedColumnType.base[DateTime, Timestamp](
      dateTime => new Timestamp(dateTime.getMillis),
      timeStamp => new DateTime(timeStamp.getTime))

    val ts = if (date.isDefined) date.get else new Timestamp(Long.MaxValue)

    val before = {
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate.get.toString <= ts.toString)).map(o => (o.outputPK, o.dataDate, o.machinePK))
        symmetryAndFlatness <- Rect.query.filter(c => c.outputPK === output._1 && c.name === name)
      } yield ((output._2, symmetryAndFlatness))

      val sorted = search.distinct.sortBy(_._1.desc).take(limit)
      val result = Db.run(sorted.result)
      result
    }

    def after = {
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate.get.toString > ts.toString)).map(o => (o.outputPK, o.dataDate, o.machinePK))
        symmetryAndFlatness <- Rect.query.filter(c => c.outputPK === output._1 && c.name === name)
      } yield ((output._2, symmetryAndFlatness))

      val sorted = search.distinct.sortBy(_._1.asc).take(limit)
      val result = Db.run(sorted.result)
      result
    }

    val all = if (before.size >= limit) before else (before ++ after).take(limit)

    // Convert to class and make sure that they are temporally ordered.
    val result = all.map(h => new RectHistory(h._1.get, h._2)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    result
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    System.exit(99)
  }
}
