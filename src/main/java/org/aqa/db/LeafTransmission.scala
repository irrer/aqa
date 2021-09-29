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
import org.aqa.Logging
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput
import org.aqa.webrun.LOCXml

import java.io.File
import scala.xml.Elem
import scala.xml.Node
import scala.xml.XML

case class LeafTransmission(
    leafTransmissionPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    section: String, // arbitrary section name. May be used to associate this section with input data
    // such as UID
    leafIndex: Int, // leaf number
    transmission_fract: Double // transmission fraction
) {

  def insert: LeafTransmission = {
    val insertQuery = LeafTransmission.query returning LeafTransmission.query.map(_.leafTransmissionPK) into ((leafTransmission, leafTransmissionPK) =>
      leafTransmission.copy(leafTransmissionPK = Some(leafTransmissionPK))
    )
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(LeafTransmission.query.insertOrUpdate(this))

  override def toString: String = "leaf: " + leafIndex.formatted("%2d") + "    section: " + section + "    transmission_fract: " + transmission_fract.toString.trim
}

object LeafTransmission extends ProcedureOutput with Logging {
  class LeafTransmissionTable(tag: Tag) extends Table[LeafTransmission](tag, "leafTransmission") {

    def leafTransmissionPK = column[Long]("leafTransmissionPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def section = column[String]("section")
    def leafIndex = column[Int]("leafIndex")
    def transmission_fract = column[Double]("transmission_fract")

    def * = (leafTransmissionPK.?, outputPK, section, leafIndex, transmission_fract) <> (LeafTransmission.apply _ tupled, LeafTransmission.unapply _)

    def outputFK = foreignKey("LeafTransmission_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[LeafTransmissionTable]

  override val topXmlLabel = "LeafTransmission"

  def get(leafTransmissionPK: Long): Option[LeafTransmission] = {
    val action = for {
      inst <- LeafTransmission.query if inst.leafTransmissionPK === leafTransmissionPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all LeafTransmission for the given output
    */
  def getByOutput(outputPK: Long): Seq[LeafTransmission] = {
    val action = for {
      inst <- LeafTransmission.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    logger.info("Number of rows: " + list.size)
    list
  }

  def delete(leafTransmissionPK: Long): Int = {
    val q = query.filter(_.leafTransmissionPK === leafTransmissionPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    val count = Db.run(action)
    logger.info("Number of rows deleted: " + count)
    count
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[LeafTransmission] = {
    def leafNodeToTransList(leaf: Node): Seq[LeafTransmission] = {
      val leafIndex = (leaf \ "leafIndex").head.text.toInt
      (leaf \ "Value").map(n => LOCXml.textToDouble(n.text)).zipWithIndex.map(di => new LeafTransmission(None, outputPK, (di._2 + 1).toString, leafIndex, di._1))
    }

    val list = (elem \ topXmlLabel).headOption match {
      case Some(node) => (node \ "LeafList" \ "Leaf").flatMap(leaf => leafNodeToTransList(leaf))
      case None       => Seq[LeafTransmission]()
    }
    logger.info("Number of rows constructed: " + list.size)

    list
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[LeafTransmission]): Unit = {
    val ops = list.map { loc => LeafTransmission.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    Config.validate
    DbSetup.init

    val lt = get(1000000.toLong)
    println("lt: " + lt)
    System.exit(99)

    val elem = XML.loadFile(new File("""D:\AQA_Data\data\Chicago_33\TB5x_1\WinstonLutz_1.0_1\2016-12-09T09-50-54-361_134\output_2016-12-09T09-50-54-490\output.xml"""))
    val xmlList = xmlToList(elem, 134)
    xmlList.foreach(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     transmission_fract: " + loc.transmission_fract))
    xmlList.map(loc => loc.insertOrUpdate())
    println("LeafTransmission.main done")
  }
}
