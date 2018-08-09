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

case class CenterDose(
  centerDosePK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // UID of source image
  beamName: String, // name of beam in plan
  dose: Double, // dose value
  units: String) {

  def insert: CenterDose = {
    val insertQuery = CenterDose.query returning CenterDose.query.map(_.centerDosePK) into
      ((centerDose, centerDosePK) => centerDose.copy(centerDosePK = Some(centerDosePK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(CenterDose.query.insertOrUpdate(this))

  override def toString: String = {
    "    centerDosePK: " + centerDosePK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    dose: " + dose + "\n" +
      "    units: " + units + "\n"
  }
}

object CenterDose extends ProcedureOutput {
  class CenterDoseTable(tag: Tag) extends Table[CenterDose](tag, "centerDose") {

    def centerDosePK = column[Long]("centerDosePK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def dose = column[Double]("dose")
    def units = column[String]("units")

    def * = (
      centerDosePK.?,
      outputPK,
      SOPInstanceUID,
      beamName,
      dose,
      units) <> ((CenterDose.apply _)tupled, CenterDose.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[CenterDoseTable]

  override val topXmlLabel = "CenterDose"

  def get(centerDosePK: Long): Option[CenterDose] = {
    val action = for {
      inst <- CenterDose.query if inst.centerDosePK === centerDosePK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[CenterDose] = {
    val action = for {
      inst <- CenterDose.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(centerDosePK: Long): Int = {
    val q = query.filter(_.centerDosePK === centerDosePK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[CenterDose]) = {
    val ops = list.map { imgId => CenterDose.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ??? // TODO
  }

  def insertSeq(list: Seq[CenterDose]): Unit = {
    val ops = list.map { loc => CenterDose.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class CenterDoseHistory(date: Date, beamName: String, dose: Double, SOPInstanceUID: String)

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
   * @param date: For this date
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
        //output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined).map(o => (o.outputPK, o.dataDate, o.machinePK))
        centerDose <- CenterDose.query.filter(c => c.outputPK === output._1).map(c => (c.beamName, c.dose, c.SOPInstanceUID))
      } yield ((output._2, centerDose._1, centerDose._2, centerDose._3))

      val sorted = search.distinct.sortBy(_._1.desc).take(limit)
      Db.run(sorted.result)
    }

    def after = {
      val search = for {
        output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined && (o.dataDate.get.toString > ts.toString)).map(o => (o.outputPK, o.dataDate, o.machinePK))
        //output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK) && o.dataDate.isDefined).map(o => (o.outputPK, o.dataDate, o.machinePK))
        centerDose <- CenterDose.query.filter(c => c.outputPK === output._1).map(c => (c.beamName, c.dose, c.SOPInstanceUID))
      } yield ((output._2, centerDose._1, centerDose._2, centerDose._3))

      val sorted = search.distinct.sortBy(_._1.asc).take(limit)
      Db.run(sorted.result)
    }

    val all = if (before.size >= limit) before else (before ++ after).take(limit)

    val result = all.map(h => new CenterDoseHistory(h._1.get, h._2, h._3, h._4)).sortWith((a, b) => a.date.getTime < b.date.getTime)
    result
  }
}
