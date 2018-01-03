package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

case class EPIDCenterCorrection(
  epidCenterCorrectionPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  epidCenterCorrection_mm: Double // distance in mm required to correct image to center
) {

  def insert: EPIDCenterCorrection = {
    val insertQuery = EPIDCenterCorrection.query returning EPIDCenterCorrection.query.map(_.epidCenterCorrectionPK) into
      ((epidCenterCorrection, epidCenterCorrectionPK) => epidCenterCorrection.copy(epidCenterCorrectionPK = Some(epidCenterCorrectionPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(EPIDCenterCorrection.query.insertOrUpdate(this))

  override def toString: String = (epidCenterCorrection_mm.toString).trim
}

object EPIDCenterCorrection extends ProcedureOutput {
  class EPIDCenterCorrectionTable(tag: Tag) extends Table[EPIDCenterCorrection](tag, "epidCenterCorrection") {

    def epidCenterCorrectionPK = column[Long]("epidCenterCorrectionPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def epidCenterCorrection_mm = column[Double]("epidCenterCorrection_mm")

    def * = (
      epidCenterCorrectionPK.?,
      outputPK,
      epidCenterCorrection_mm) <> ((EPIDCenterCorrection.apply _)tupled, EPIDCenterCorrection.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    //def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)           TODO
  }

  val query = TableQuery[EPIDCenterCorrectionTable]

  override val topXmlLabel = "EPIDCenterCorrection"

  def get(epidCenterCorrectionPK: Long): Option[EPIDCenterCorrection] = {
    val action = for {
      inst <- EPIDCenterCorrection.query if inst.epidCenterCorrectionPK === epidCenterCorrectionPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all epidCenterCorrections for the given output
   */
  def getByOutput(outputPK: Long): Seq[EPIDCenterCorrection] = {
    val action = for {
      inst <- EPIDCenterCorrection.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(epidCenterCorrectionPK: Long): Int = {
    val q = query.filter(_.epidCenterCorrectionPK === epidCenterCorrectionPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  private def xmlToList(elem: Elem, outputPK: Long): Seq[EPIDCenterCorrection] = {
    def nodeToEPIDCenterCorrection(ecc: Node): EPIDCenterCorrection = {
      val epidCenterCorrection_mm = ecc.head.text.toDouble
      new EPIDCenterCorrection(None, outputPK, epidCenterCorrection_mm)
    }

    (elem \ topXmlLabel).headOption match {
      case Some(node) => Seq(nodeToEPIDCenterCorrection(node))
      case None => Seq[EPIDCenterCorrection]()
    }
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    toInsert.map(t => t.insertOrUpdate)
    toInsert.size
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    val elem = XML.loadFile(new File("""D:\tmp\aqa\LOC.xml"""))
    val xmlList = xmlToList(elem, 90)
    xmlList.map(loc => println("    outputPK: " + loc.outputPK +
      "     epidCenterCorrection_mm: " + loc.epidCenterCorrection_mm))
    xmlList.map(loc => loc.insertOrUpdate)
    println("EPIDCenterCorrection.main done")
    //println("======== inst: " + get(5))
    //println("======== inst delete: " + delete(5))
  }
}
