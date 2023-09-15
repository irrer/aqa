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

import org.aqa.Logging
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput
import org.aqa.webrun.LOCXml

import scala.xml.Elem
import scala.xml.Node

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

  def insertOrUpdate(): Int = Db.run(LOCRSquared.query.insertOrUpdate(this))

  override def toString: String = "leaf: " + leafIndex.formatted("%2d") + "    section: " + section + "    rSquared_mmsq: " + rSquared_mmsq.toString.trim
}

object LOCRSquared extends ProcedureOutput with Logging {
  class LOCRSquaredTable(tag: Tag) extends Table[LOCRSquared](tag, "rSquared") {

    def rSquaredPK = column[Long]("rSquaredPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def section = column[String]("section")
    def leafIndex = column[Int]("leafIndex")
    def rSquared_mmsq = column[Double]("rSquared_mmsq")

    def * = (rSquaredPK.?, outputPK, section, leafIndex, rSquared_mmsq) <> (LOCRSquared.apply _ tupled, LOCRSquared.unapply )

    def outputFK = foreignKey("LOCRSquared_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[LOCRSquaredTable]

  override val topXmlLabel = "LOCRSquared"

  def get(rSquaredPK: Long): Option[LOCRSquared] = {
    val action = for {
      inst <- LOCRSquared.query if inst.rSquaredPK === rSquaredPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all rSquareds for the given output
    */
  def getByOutput(outputPK: Long): Seq[LOCRSquared] = {
    val action = for {
      inst <- LOCRSquared.query if inst.outputPK === outputPK
    } yield inst
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
      case Some(node) => (node \ "Leaf").flatMap(leaf => leafNodeToLocList(leaf))
      case None       => Seq[LOCRSquared]()
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

}
