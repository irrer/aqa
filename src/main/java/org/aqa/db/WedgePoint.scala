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

case class WedgePoint(
  wedgePointPK: Option[Long], // primary key
  outputPK: Long, // output primary key

  wedgeSOPInstanceUID: String, // UID of wedge source image
  wedgeBeamName: String, // name of wedge beam in plan
  wedgeValue_cu: Double, // value of wedge point in CU : Calibrated Units

  backgroundSOPInstanceUID: String, // UID of background source image
  backgroundBeamName: String, // name of background beam in plan
  backgroundValue_cu: Double, // corresponding value of background field point in CU : Calibrated Units

  percentOfBackground_pct: Double, // (wedgeValue_cu * 100) / backgroundValue_cu
  baselinePercentOfBackground_pct: Double) // baseline for percentOfBackground_pct
  {
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
      "    wedgeSOPInstanceUID: " + wedgeSOPInstanceUID + "\n" +
      "    wedgeBeamName: " + wedgeBeamName + "\n" +
      "    wedgeValue_cu: " + wedgeValue_cu + "\n" +
      "    backgroundSOPInstanceUID: " + backgroundSOPInstanceUID + "\n" +
      "    backgroundBeamName: " + backgroundBeamName + "\n" +
      "    backgroundValue_cu: " + backgroundValue_cu + "\n" +
      "    percentOfBackground_pct: " + percentOfBackground_pct + "\n" +
      "    baselinePercentOfBackground_pct: " + baselinePercentOfBackground_pct + "\n"
  }
}

object WedgePoint extends ProcedureOutput {
  class WedgePointTable(tag: Tag) extends Table[WedgePoint](tag, "wedgePoint") {

    def wedgePointPK = column[Long]("wedgePointPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def wedgeSOPInstanceUID = column[String]("wedgeSOPInstanceUID")
    def wedgeBeamName = column[String]("wedgeBeamName")
    def wedgeValue_cu = column[Double]("wedgeValue_cu")
    def backgroundSOPInstanceUID = column[String]("backgroundSOPInstanceUID")
    def backgroundBeamName = column[String]("backgroundBeamName")
    def backgroundValue_cu = column[Double]("backgroundValue_cu")
    def percentOfBackground_pct = column[Double]("percentOfBackground_pct")
    def baselinePercentOfBackground_pct = column[Double]("baselinePercentOfBackground_pct")

    def * = (
      wedgePointPK.?,
      outputPK,
      wedgeSOPInstanceUID,
      wedgeBeamName,
      wedgeValue_cu,
      backgroundSOPInstanceUID,
      backgroundBeamName,
      backgroundValue_cu,
      percentOfBackground_pct,
      baselinePercentOfBackground_pct) <> ((WedgePoint.apply _)tupled, WedgePoint.unapply _)

    def outputFK = foreignKey("WedgePoint_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
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

  case class WedgePointHistory(date: Date, wedgeBeamName: String, backgroundBeamName: String, percentOfBackground_pct: Double, outputPK: Long)

  /**
   * Get CenterBeam results.
   *
   * @param machinePK: For this machine
   *
   * @param procedurePK: For this procedure
   *
   */
  def recentHistory(limit: Int, machinePK: Long, procedurePK: Long, date: Option[Timestamp]) = {

    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK)).map(o => (o.outputPK, o.dataDate))
      wedgePoint <- WedgePoint.query.
        filter(w => w.outputPK === output._1).
        map(c => (c.wedgeBeamName, c.backgroundBeamName, c.percentOfBackground_pct))
    } yield ((output._2, wedgePoint._1, wedgePoint._2, wedgePoint._3, output._1))

    val result = Db.run(search.result).map(h => new WedgePointHistory(h._1.get, h._2, h._3, h._4, h._5)).sortBy(_.date.getTime)
    result
  }
}
