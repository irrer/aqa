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

import org.aqa.db.Db.driver.api._
import org.aqa.Logging

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

  def insertOrUpdate(): Int = Db.run(LeafOffsetCorrection.query.insertOrUpdate(this))

  override def toString: String = "leaf: " + leafIndex.formatted("%2d") + "    section: " + section + "    correction_mm: " + correction_mm.toString.trim
}

object LeafOffsetCorrection extends Logging {
  class LeafOffsetCorrectionTable(tag: Tag) extends Table[LeafOffsetCorrection](tag, "leafOffsetCorrection") {

    def leafOffsetCorrectionPK = column[Long]("leafOffsetCorrectionPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def section = column[String]("section")
    def leafIndex = column[Int]("leafIndex")
    private def correction_mm = column[Double]("correction_mm")

    def * = (leafOffsetCorrectionPK.?, outputPK, section, leafIndex, correction_mm) <> (LeafOffsetCorrection.apply _ tupled, LeafOffsetCorrection.unapply)

    def outputFK = foreignKey("LeafOffsetCorrection_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[LeafOffsetCorrectionTable]

  def get(leafOffsetCorrectionPK: Long): Option[LeafOffsetCorrection] = {
    val action = for {
      inst <- LeafOffsetCorrection.query if inst.leafOffsetCorrectionPK === leafOffsetCorrectionPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all leafOffsetCorrections for the given output
    */
  def getByOutput(outputPK: Long): Seq[LeafOffsetCorrection] = {
    val action = for {
      inst <- LeafOffsetCorrection.query if inst.outputPK === outputPK
    } yield inst
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

  def insertSeq(list: Seq[LeafOffsetCorrection]): Unit = {
    list.foreach(_.insertOrUpdate())
  }
}
