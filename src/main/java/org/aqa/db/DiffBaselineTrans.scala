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

object DiffBaselineTrans {
  class DiffBaselineTransTable(tag: Tag) extends Table[DiffBaselineTrans](tag, "diffBaselineTrans") {

    def diffBaselineTransPK = column[Long]("diffBaselineTransPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def section = column[String]("section")
    def leafIndex = column[Int]("leafIndex")
    private def diffBaselineTrans_mm = column[Double]("diffBaselineTrans_mm")

    def * = (diffBaselineTransPK.?, outputPK, section, leafIndex, diffBaselineTrans_mm) <> (DiffBaselineTrans.apply _ tupled, DiffBaselineTrans.unapply)

    def outputFK = foreignKey("DiffBaselineTrans_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DiffBaselineTransTable]

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

  def insertSeq(list: Seq[DiffBaselineTrans]): Unit = {
    list.foreach(_.insertOrUpdate())
  }
}
