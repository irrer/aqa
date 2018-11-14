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

case class WedgePoint(
  wedgePointPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // UID of source image
  beamName: String, // name of beam in plan
  value_cu: Double, // value of wedge point in CU : Calibrated Units
  floodValue_cu: Double, // corresponding value of flood field point in CU : Calibrated Units
  percentOfFlood_pct: Double) { // (value_cu * 100) / floodValue_cu

  def insert: WedgePoint = {
    val insertQuery = WedgePoint.query returning WedgePoint.query.map(_.wedgePointPK) into
      ((wedgePoint, wedgePointPK) => wedgePoint.copy(wedgePointPK = Some(wedgePointPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(WedgePoint.query.insertOrUpdate(this))

  override def toString: String = {
    "    wedgePointPK: " + wedgePointPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    value_cu: " + value_cu + "\n" +
      "    floodValue_cu: " + floodValue_cu + "\n" +
      "    percentOfFlood_pct: " + percentOfFlood_pct + "\n"
  }
}

object WedgePoint extends ProcedureOutput {
  class WedgePointTable(tag: Tag) extends Table[WedgePoint](tag, "wedgePoint") {

    def wedgePointPK = column[Long]("wedgePointPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def value_cu = column[Double]("value_cu")
    def floodValue_cu = column[Double]("floodValue_cu")
    def percentOfFlood_pct = column[Double]("percentOfFlood_pct")

    def * = (
      wedgePointPK.?,
      outputPK,
      SOPInstanceUID,
      beamName,
      value_cu,
      floodValue_cu,
      percentOfFlood_pct) <> ((WedgePoint.apply _)tupled, WedgePoint.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[WedgePointTable]

  override val topXmlLabel = "WedgePoint"

  def get(wedgePointPK: Long): Option[WedgePoint] = {
    val action = for {
      inst <- WedgePoint.query if inst.wedgePointPK === wedgePointPK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[WedgePoint] = {
    val action = for {
      inst <- WedgePoint.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(wedgePointPK: Long): Int = {
    val q = query.filter(_.wedgePointPK === wedgePointPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[WedgePoint]) = {
    val ops = list.map { imgId => WedgePoint.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ???
  }

  def insertSeq(list: Seq[WedgePoint]): Unit = {
    val ops = list.map { loc => WedgePoint.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class WedgePointHistory(date: Date, beamName: String, SOPInstanceUID: String, value_cu: Double, floodValue_cu: Double, percentOfFlood_pct: Double)

  /**
   * Get the CenterBeam results that are nearest in time to the given date, preferring those that have an earlier date.
   *
   * @param limit: Only get this many results.  First get all the results that are at or before the given time.  If there
   * are <code>limit</code> or more values, then return those.  Otherwise, also get values after the given time
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

    val ts = if (date.isDefined) date.get else new Timestamp(Long.MaxValue)

    val before = {
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate.get.toString <= ts.toString)).map(o => (o.outputPK, o.dataDate, o.machinePK))
        wedgePoint <- WedgePoint.query.filter(c => c.outputPK === output._1).map(c => (c.beamName, c.SOPInstanceUID, c.value_cu, c.floodValue_cu, c.percentOfFlood_pct))
      } yield ((output._2, wedgePoint._1, wedgePoint._2, wedgePoint._3, wedgePoint._4, wedgePoint._5))

      val sorted = search.distinct.sortBy(_._1.desc).take(limit)
      Db.run(sorted.result)
    }

    def after = {
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate.get.toString > ts.toString)).map(o => (o.outputPK, o.dataDate, o.machinePK))
        wedgePoint <- WedgePoint.query.filter(c => c.outputPK === output._1).map(c => (c.beamName, c.SOPInstanceUID, c.value_cu, c.floodValue_cu, c.percentOfFlood_pct))
      } yield ((output._2, wedgePoint._1, wedgePoint._2, wedgePoint._3, wedgePoint._4, wedgePoint._5))

      val sorted = search.distinct.sortBy(_._1.asc).take(limit)
      Db.run(sorted.result)
    }

    val all = if (before.size >= limit) before else (before ++ after).take(limit)

    val result = all.map(h => new WedgePointHistory(h._1.get, h._2, h._3, h._4, h._5, h._6)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    result
  }
}
