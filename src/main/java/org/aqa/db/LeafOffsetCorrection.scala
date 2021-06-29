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

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput
import org.aqa.webrun.LOCXml

case class LeafOffsetCorrection(
  leafOffsetCorrectionPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  section: String, // arbitrary section name.  May be used to associate this section with input data such as UID
  leafIndex: Int, // leaf number
  correction_mm: Double // maximum leaf gap
) {

  def insert: LeafOffsetCorrection = {
    val insertQuery = LeafOffsetCorrection.query returning LeafOffsetCorrection.query.map(_.leafOffsetCorrectionPK) into
      ((leafOffsetCorrection, leafOffsetCorrectionPK) => leafOffsetCorrection.copy(leafOffsetCorrectionPK = Some(leafOffsetCorrectionPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(LeafOffsetCorrection.query.insertOrUpdate(this))

  override def toString: String = (correction_mm.toString).trim
}

object LeafOffsetCorrection extends ProcedureOutput {
  class LeafOffsetCorrectionTable(tag: Tag) extends Table[LeafOffsetCorrection](tag, "leafOffsetCorrection") {

    def leafOffsetCorrectionPK = column[Long]("leafOffsetCorrectionPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def section = column[String]("section")
    def leafIndex = column[Int]("leafIndex")
    def correction_mm = column[Double]("correction_mm")

    def * = (
      leafOffsetCorrectionPK.?,
      outputPK,
      section,
      leafIndex,
      correction_mm) <> ((LeafOffsetCorrection.apply _)tupled, LeafOffsetCorrection.unapply _)

    def outputFK = foreignKey("LeafOffsetCorrection_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[LeafOffsetCorrectionTable]

  override val topXmlLabel = "LeafOffsetConstancy"

  def get(leafOffsetCorrectionPK: Long): Option[LeafOffsetCorrection] = {
    val action = for {
      inst <- LeafOffsetCorrection.query if inst.leafOffsetCorrectionPK === leafOffsetCorrectionPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all leafOffsetCorrections for the given output
   */
  def getByOutput(outputPK: Long): Seq[LeafOffsetCorrection] = {
    val action = for {
      inst <- LeafOffsetCorrection.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(leafOffsetCorrectionPK: Long): Int = {
    val q = query.filter(_.leafOffsetCorrectionPK === leafOffsetCorrectionPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  private def xmlToList(elem: Elem, outputPK: Long): Seq[LeafOffsetCorrection] = {
    def leafNodeToLocList(leaf: Node): Seq[LeafOffsetCorrection] = {
      val leafIndex = (leaf \ "leafIndex").head.text.toInt
      (leaf \ "Value").map(n => LOCXml.textToDouble(n.text)).zipWithIndex.map(di => new LeafOffsetCorrection(None, outputPK, (di._2 + 1).toString, leafIndex, di._1))
    }

    (elem \ topXmlLabel).headOption match {
      case Some(node) => (node \ "LeafList" \ "Leaf").map(leaf => leafNodeToLocList(leaf)).flatten
      case None => Seq[LeafOffsetCorrection]()
    }
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[LeafOffsetCorrection]): Unit = {
    val ops = list.map { loc => LeafOffsetCorrection.query.insertOrUpdate(loc) }
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
    xmlList.map(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     correction_mm: " + loc.correction_mm))
    xmlList.map(loc => loc.insertOrUpdate)
    println("LeafOffsetCorrection.main done")
    //println("======== inst: " + get(5))
    //println("======== inst delete: " + delete(5))
  }
}
