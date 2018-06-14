package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

case class CenterDose(
  centerDosePK: Option[Long], // primary key
  outputPK: Long, // output primary key
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
      "    beamName: " + beamName + "\n" +
      "    dose: " + dose + "\n" +
      "    units: " + units + "\n"
  }
}

object CenterDose extends ProcedureOutput {
  class CenterDoseTable(tag: Tag) extends Table[CenterDose](tag, "centerDose") {

    def centerDosePK = column[Long]("centerDosePK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def beamName = column[String]("beamName")
    def dose = column[Double]("dose")
    def units = column[String]("units")

    def * = (
      centerDosePK.?,
      outputPK,
      beamName,
      dose,
      units) <> ((CenterDose.apply _)tupled, CenterDose.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    //def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)           TODO
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
}
