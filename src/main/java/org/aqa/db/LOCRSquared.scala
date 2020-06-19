package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput
import org.aqa.Logging
import org.aqa.webrun.LOCXml

case class LOCRSquared(
  rSquaredPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  section: String, // arbitrary section name. May be used to associate this section with input data
  // such as UID
  leafIndex: Int, // leaf number
  rSquared_mmsq: Double // R squared value
) {

  def insert: LOCRSquared = {
    val insertQuery = LOCRSquared.query returning LOCRSquared.query.map(_.rSquaredPK) into
      ((rSquared, rSquaredPK) => rSquared.copy(rSquaredPK = Some(rSquaredPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(LOCRSquared.query.insertOrUpdate(this))

  override def toString: String = (rSquared_mmsq.toString).trim
}

object LOCRSquared extends ProcedureOutput with Logging {
  class LOCRSquaredTable(tag: Tag) extends Table[LOCRSquared](tag, "rSquared") {

    def rSquaredPK = column[Long]("rSquaredPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def section = column[String]("section")
    def leafIndex = column[Int]("leafIndex")
    def rSquared_mmsq = column[Double]("rSquared_mmsq")

    def * = (
      rSquaredPK.?,
      outputPK,
      section,
      leafIndex,
      rSquared_mmsq) <> ((LOCRSquared.apply _)tupled, LOCRSquared.unapply _)

    def outputFK = foreignKey("LOCRSquared_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[LOCRSquaredTable]

  override val topXmlLabel = "LOCRSquared"

  def get(rSquaredPK: Long): Option[LOCRSquared] = {
    val action = for {
      inst <- LOCRSquared.query if inst.rSquaredPK === rSquaredPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all rSquareds for the given output
   */
  def getByOutput(outputPK: Long): Seq[LOCRSquared] = {
    val action = for {
      inst <- LOCRSquared.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(rSquaredPK: Long): Int = {
    val q = query.filter(_.rSquaredPK === rSquaredPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  private def xmlToList(elem: Elem, outputPK: Long): Seq[LOCRSquared] = {
    def leafNodeToLocList(leaf: Node): Seq[LOCRSquared] = {
      val leafIndex = (leaf \ "leafIndex").head.text.toInt
      (leaf \ "Value").map(n => LOCXml.textToDouble(n.text)).zipWithIndex.map(di => new LOCRSquared(None, outputPK, (di._2 + 1).toString, leafIndex, di._1))
    }

    val list = (elem \ topXmlLabel).headOption match {
      case Some(node) => (node \ "Leaf").map(leaf => leafNodeToLocList(leaf)).flatten
      case None => Seq[LOCRSquared]()
    }
    logger.info("Number of items constructed: " + list.size)
    list
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[LOCRSquared]): Unit = {
    logger.info("Number of rows to insert: " + list.size)
    val ops = list.map { loc => LOCRSquared.query.insertOrUpdate(loc) }
    Db.perform(ops)
    logger.info("Number of rows inserted: " + list.size)
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    System.exit(99)
    //val elem = XML.loadFile(new File("""D:\AQA_Data\data\Chicago_33\TB5x_1\WinstonLutz_1.0_1\2016-12-09T09-50-54-361_134\output_2016-12-09T09-50-54-490\output.xml"""))
    val elem = XML.loadFile(new File("""D:\tmp\aqa\tmp\output.xml"""))
    val xmlList = xmlToList(elem, 134)
    xmlList.map(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     rSquared_mmsq: " + loc.rSquared_mmsq))
    xmlList.map(loc => loc.insertOrUpdate)
    println("LOCRSquared.main done")
    //println("======== inst: " + get(5))
    //println("======== inst delete: " + delete(5))
  }
}
