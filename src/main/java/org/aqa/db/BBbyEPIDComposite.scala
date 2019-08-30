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

/**
 * Store the analysis results for a set of EPID images containing a BB.  This is derived from
 * the BBbyEPID values to create an evaluation of the composite results of all images in a set.
 */
case class BBbyEPIDComposite(
  bbByEPIDCompositePK: Option[Long], // primary key
  outputPK: Long, // output primary key
  rtplanSOPInstanceUID: String, // UID of rtplan
  epidSeriesInstanceUID: String, // SOP series instance UID of EPID image
  offset_mm: Double, // distance between measured EPID position and expected (plan) location (aka: positioning error)
  x_mm: Double, // X position in EPID in 3D plan space
  y_mm: Double, // Y position in EPID in 3D plan space
  z_mm: Double // Z position in EPID in 3D plan space
) {

  def insert: BBbyEPIDComposite = {
    val insertQuery = BBbyEPIDComposite.query returning BBbyEPIDComposite.query.map(_.bbByEPIDCompositePK) into ((bbByEPIDComposite, bbByEPIDCompositePK) => bbByEPIDComposite.copy(bbByEPIDCompositePK = Some(bbByEPIDCompositePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(BBbyEPIDComposite.query.insertOrUpdate(this))

  override def toString: String =
    "bbByEPIDCompositePK : " + bbByEPIDCompositePK +
      "\n    outputPK : " + outputPK +
      "\n    rtplanSOPInstanceUID : " + rtplanSOPInstanceUID +
      "\n    epidSeriesInstanceUID : " + epidSeriesInstanceUID +
      "\n    offset_mm : " + Util.fmtDbl(offset_mm) +
      "\n    epid 3D X,Y,Z : " + Util.fmtDbl(x_mm) + ", " + Util.fmtDbl(y_mm) + ", " + Util.fmtDbl(z_mm)

  val epid = new Point3d(x_mm, y_mm, z_mm)
}

object BBbyEPIDComposite extends ProcedureOutput {
  class BBbyEPIDCompositeTable(tag: Tag) extends Table[BBbyEPIDComposite](tag, "bbByEPIDComposite") {

    def bbByEPIDCompositePK = column[Long]("bbByEPIDCompositePK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def rtplanSOPInstanceUID = column[String]("rtplanSOPInstanceUID")
    def epidSeriesInstanceUID = column[String]("epidSeriesInstanceUID")
    def offset_mm = column[Double]("offset_mm")
    def x_mm = column[Double]("x_mm")
    def y_mm = column[Double]("y_mm")
    def z_mm = column[Double]("z_mm")

    def * = (
      bbByEPIDCompositePK.?,
      outputPK,
      rtplanSOPInstanceUID,
      epidSeriesInstanceUID,
      offset_mm,
      x_mm,
      y_mm,
      z_mm) <> ((BBbyEPIDComposite.apply _)tupled, BBbyEPIDComposite.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BBbyEPIDCompositeTable]

  override val topXmlLabel = "BBbyEPIDComposite"

  def get(bbByEPIDCompositePK: Long): Option[BBbyEPIDComposite] = {
    val action = for {
      inst <- BBbyEPIDComposite.query if inst.bbByEPIDCompositePK === bbByEPIDCompositePK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all BBbyEPIDComposite for the given output
   */
  def getByOutput(outputPK: Long): Seq[BBbyEPIDComposite] = {
    val action = for {
      inst <- BBbyEPIDComposite.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(bbByEPIDCompositePK: Long): Int = {
    val q = query.filter(_.bbByEPIDCompositePK === bbByEPIDCompositePK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[BBbyEPIDComposite] = {
    ???
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[BBbyEPIDComposite]): Unit = {
    val ops = list.map { loc => BBbyEPIDComposite.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class BBbyEPIDCompositeHistory(date: Date, bbByEPIDComposite: BBbyEPIDComposite) {
    override def toString = {
      "date: " + date + "    " + bbByEPIDComposite
    }
  }

  /**
   * Get the BBbyEPIDComposite results that are nearest in time to the given date.
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
        bbByEPIDComposite <- BBbyEPIDComposite.query.filter(c => c.outputPK === output._1)
      } yield ((output._2, bbByEPIDComposite))

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
        bbByEPIDComposite <- BBbyEPIDComposite.query.filter(c => c.outputPK === output._1)
      } yield ((output._2, bbByEPIDComposite))

      val sorted = search.distinct.sortBy(_._1.asc)
      //println(sorted.result.statements.mkString("\n    "))
      Db.run(sorted.result)
    }

    val all = before ++ after

    val result = all.map(h => new BBbyEPIDCompositeHistory(h._1.get, h._2)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    result
  }

}
