package org.aqa.db

import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput

import java.sql.Timestamp
import scala.xml.Elem

case class CenterDose(
    centerDosePK: Option[Long], // primary key
    outputPK: Long, // output primary key
    SOPInstanceUID: String, // UID of source image
    beamName: String, // name of beam in plan
    dose: Double, // dose value
    units: String
) {

  def insert: CenterDose = {
    val insertQuery = CenterDose.query returning CenterDose.query.map(_.centerDosePK) into
      ((centerDose, centerDosePK) => centerDose.copy(centerDosePK = Some(centerDosePK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(CenterDose.query.insertOrUpdate(this))

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

    def * = (centerDosePK.?, outputPK, SOPInstanceUID, beamName, dose, units) <> (CenterDose.apply _ tupled, CenterDose.unapply)

    def outputFK = foreignKey("CenterDose_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[CenterDoseTable]

  override val topXmlLabel = "CenterDose"

  def get(centerDosePK: Long): Option[CenterDose] = {
    val action = for {
      inst <- CenterDose.query if inst.centerDosePK === centerDosePK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[CenterDose] = {
    val action = for {
      inst <- CenterDose.query if inst.outputPK === outputPK
    } yield inst
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

  def insert(list: Seq[CenterDose]): Seq[Int] = {
    val ops = list.map { imgId => CenterDose.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    throw new RuntimeException("Insert by elem not implemented.")
  }

  def insertSeq(list: Seq[CenterDose]): Unit = {
    val ops = list.map { loc => CenterDose.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class CenterDoseHistory(output: Output, centerDose: CenterDose) {
    override def toString: String = {
      "date: " + output.dataDate.get + "    " + centerDose
    }

    val date: Timestamp = output.dataDate.get

    val getTime: Long = date.getTime
  }

  /**
    * Get the CenterBeam results.
    *
    * @param machinePK: For this machine
    */
  def history(machinePK: Long): Seq[CenterDoseHistory] = {

    val search = for {
      output <- Output.query.filter(o => o.machinePK === machinePK)
      centerDose <- CenterDose.query.filter(c => c.outputPK === output.outputPK)
    } yield (output, centerDose)

    val result = Db.run(search.result).map(h => CenterDoseHistory(h._1, h._2)).sortBy(_.output.dataDate.get.getTime)
    result
  }
}
