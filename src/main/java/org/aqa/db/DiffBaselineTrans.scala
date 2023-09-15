/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.db

import org.aqa.Config
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput
import org.aqa.webrun.LOCXml

import java.io.File
import scala.xml.Elem
import scala.xml.Node
import scala.xml.XML

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

  def insertOrUpdate(): Int = Db.run(DiffBaselineTrans.query.insertOrUpdate(this))

  override def toString: String = "leaf: " + leafIndex.formatted("%2d") + "    section: " + section + "    diffBaselineTrans_mm: " + diffBaselineTrans_mm.toString.trim
}

object DiffBaselineTrans extends ProcedureOutput {
  class DiffBaselineTransTable(tag: Tag) extends Table[DiffBaselineTrans](tag, "diffBaselineTrans") {

    def diffBaselineTransPK = column[Long]("diffBaselineTransPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def section = column[String]("section")
    def leafIndex = column[Int]("leafIndex")
    def diffBaselineTrans_mm = column[Double]("diffBaselineTrans_mm")

    def * = (diffBaselineTransPK.?, outputPK, section, leafIndex, diffBaselineTrans_mm) <> (DiffBaselineTrans.apply _ tupled, DiffBaselineTrans.unapply)

    def outputFK = foreignKey("DiffBaselineTrans_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DiffBaselineTransTable]

  override val topXmlLabel = "LOCDifferenceFromBaselineTrans"

  def get(diffBaselineTransPK: Long): Option[DiffBaselineTrans] = {
    val action = for {
      inst <- DiffBaselineTrans.query if inst.diffBaselineTransPK === diffBaselineTransPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all diffBaselineTrans for the given output
    */
  def getByOutput(outputPK: Long): Seq[DiffBaselineTrans] = {
    val action = for {
      inst <- DiffBaselineTrans.query if inst.outputPK === outputPK
    } yield inst
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
      case Some(node) => (node \ "Leaf").flatMap(leaf => leafNodeToLocList(leaf))
      case None       => Seq[DiffBaselineTrans]()
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
    Config.validate
    DbSetup.init
    System.exit(99)
    val elem = XML.loadFile(new File("""D:\tmp\aqa\tmp\output.xml"""))
    val xmlList = xmlToList(elem, 134)
    xmlList.foreach(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     diffBaselineTrans_mm: " + loc.diffBaselineTrans_mm))
    xmlList.map(loc => loc.insertOrUpdate())
    println("DiffBaselineTrans.main done")
    //println("======== inst: " + get(5))
    //println("======== inst delete: " + delete(5))
  }
}
