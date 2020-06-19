package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput
import org.aqa.webrun.LOCXml

case class DiffBaselineTrans(
  diffBaselineTransPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  section: String, // arbitrary section name.  May be used to associate this section with input data such as UID
  leafIndex: Int, // leaf number
  diffBaselineTrans_mm: Double // difference from baseline trans value in mm
) {

  def insert: DiffBaselineTrans = {
    val insertQuery = DiffBaselineTrans.query returning DiffBaselineTrans.query.map(_.diffBaselineTransPK) into
      ((diffBaselineTrans, diffBaselineTransPK) => diffBaselineTrans.copy(diffBaselineTransPK = Some(diffBaselineTransPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(DiffBaselineTrans.query.insertOrUpdate(this))

  override def toString: String = (diffBaselineTrans_mm.toString).trim
}

object DiffBaselineTrans extends ProcedureOutput {
  class DiffBaselineTransTable(tag: Tag) extends Table[DiffBaselineTrans](tag, "diffBaselineTrans") {

    def diffBaselineTransPK = column[Long]("diffBaselineTransPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def section = column[String]("section")
    def leafIndex = column[Int]("leafIndex")
    def diffBaselineTrans_mm = column[Double]("diffBaselineTrans_mm")

    def * = (
      diffBaselineTransPK.?,
      outputPK,
      section,
      leafIndex,
      diffBaselineTrans_mm) <> ((DiffBaselineTrans.apply _)tupled, DiffBaselineTrans.unapply _)

    def outputFK = foreignKey("DiffBaselineTrans_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DiffBaselineTransTable]

  override val topXmlLabel = "LOCDifferenceFromBaselineTrans"

  def get(diffBaselineTransPK: Long): Option[DiffBaselineTrans] = {
    val action = for {
      inst <- DiffBaselineTrans.query if inst.diffBaselineTransPK === diffBaselineTransPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all diffBaselineTranss for the given output
   */
  def getByOutput(outputPK: Long): Seq[DiffBaselineTrans] = {
    val action = for {
      inst <- DiffBaselineTrans.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(diffBaselineTransPK: Long): Int = {
    val q = query.filter(_.diffBaselineTransPK === diffBaselineTransPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  private def xmlToList(elem: Elem, outputPK: Long): Seq[DiffBaselineTrans] = {
    def leafNodeToLocList(leaf: Node): Seq[DiffBaselineTrans] = {
      val leafIndex = (leaf \ "leafIndex").head.text.toInt
      (leaf \ "Value").map(n => LOCXml.textToDouble(n.text)).zipWithIndex.map(di => new DiffBaselineTrans(None, outputPK, (di._2 + 1).toString, leafIndex, di._1))
    }

    (elem \ topXmlLabel).headOption match {
      case Some(node) => (node \ "Leaf").map(leaf => leafNodeToLocList(leaf)).flatten
      case None => Seq[DiffBaselineTrans]()
    }
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[DiffBaselineTrans]): Unit = {
    val ops = list.map { loc => DiffBaselineTrans.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    System.exit(99)
    //val elem = XML.loadFile(new File("""D:\AQA_Data\data\Chicago_33\TB5x_1\WinstonLutz_1.0_1\2016-12-09T09-50-54-361_134\output_2016-12-09T09-50-54-490\output.xml"""))
    val elem = XML.loadFile(new File("""D:\tmp\aqa\tmp\output.xml"""))
    val xmlList = xmlToList(elem, 134)
    xmlList.map(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     diffBaselineTrans_mm: " + loc.diffBaselineTrans_mm))
    xmlList.map(loc => loc.insertOrUpdate)
    println("DiffBaselineTrans.main done")
    //println("======== inst: " + get(5))
    //println("======== inst delete: " + delete(5))
  }
}
