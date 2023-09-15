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

case class DiffBaselineOpen(
    diffBaselineOpenPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    section: String, // arbitrary section name.  May be used to associate this section with input data such as UID
    leafIndex: Int, // leaf number
    diffBaselineOpen_mm: Double // difference from baseline open value in mm
) {

  def insert: DiffBaselineOpen = {
    val insertQuery = DiffBaselineOpen.query returning DiffBaselineOpen.query.map(_.diffBaselineOpenPK) into
      ((diffBaselineOpen, diffBaselineOpenPK) => diffBaselineOpen.copy(diffBaselineOpenPK = Some(diffBaselineOpenPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(DiffBaselineOpen.query.insertOrUpdate(this))

  override def toString: String = "leaf: " + leafIndex.formatted("%2d") + "    section: " + section + "    diffBaselineOpen_mm: " + diffBaselineOpen_mm.toString.trim
}

object DiffBaselineOpen extends ProcedureOutput {
  class DiffBaselineOpenTable(tag: Tag) extends Table[DiffBaselineOpen](tag, "diffBaselineOpen") {

    def diffBaselineOpenPK = column[Long]("diffBaselineOpenPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def section = column[String]("section")
    def leafIndex = column[Int]("leafIndex")
    def diffBaselineOpen_mm = column[Double]("diffBaselineOpen_mm")

    def * = (diffBaselineOpenPK.?, outputPK, section, leafIndex, diffBaselineOpen_mm) <> (DiffBaselineOpen.apply _ tupled, DiffBaselineOpen.unapply)

    def outputFK = foreignKey("DiffBaselineOpen_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DiffBaselineOpenTable]

  override val topXmlLabel = "LOCDifferenceFromBaselineOpen"

  def get(diffBaselineOpenPK: Long): Option[DiffBaselineOpen] = {
    val action = for {
      inst <- DiffBaselineOpen.query if inst.diffBaselineOpenPK === diffBaselineOpenPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all diffBaselineOpens for the given output
    */
  def getByOutput(outputPK: Long): Seq[DiffBaselineOpen] = {
    val action = for {
      inst <- DiffBaselineOpen.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list
  }

  def delete(diffBaselineOpenPK: Long): Int = {
    val q = query.filter(_.diffBaselineOpenPK === diffBaselineOpenPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  private def xmlToList(elem: Elem, outputPK: Long): Seq[DiffBaselineOpen] = {
    def leafNodeToLocList(leaf: Node): Seq[DiffBaselineOpen] = {
      val leafIndex = (leaf \ "leafIndex").head.text.toInt
      (leaf \ "Value").map(n => LOCXml.textToDouble(n.text)).zipWithIndex.map(di => new DiffBaselineOpen(None, outputPK, (di._2 + 1).toString, leafIndex, di._1))
    }

    (elem \ topXmlLabel).headOption match {
      case Some(node) => (node \ "Leaf").flatMap(leaf => leafNodeToLocList(leaf))
      case None       => Seq[DiffBaselineOpen]()
    }
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[DiffBaselineOpen]): Unit = {
    val ops = list.map { loc => DiffBaselineOpen.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    Config.validate
    DbSetup.init
    System.exit(99)
    val elem = XML.loadFile(new File("""D:\tmp\aqa\tmp\output.xml"""))
    val xmlList = xmlToList(elem, 134)
    xmlList.foreach(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     diffBaselineOpen_mm: " + loc.diffBaselineOpen_mm))
    xmlList.map(loc => loc.insertOrUpdate())
    println("DiffBaselineOpen.main done")
    //println("======== inst: " + get(5))
    //println("======== inst delete: " + delete(5))
  }
}
