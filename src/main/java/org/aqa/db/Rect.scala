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
import java.sql.Timestamp
import java.util.Date
import java.awt.geom.Rectangle2D

/**
 * Rectangle describing an area in an EPID image and it's average intensity.
 */
case class Rect(
  rectPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  SOPInstanceUID: String, // SOPInstanceUID of DICOM file.
  name: String, // identifies what this is related to.  Usually specifies beam name and test.
  intensity_cu: Double, // average intensity for the entire rectangle in CU / square mm
  x_mm: Double, // x in mm from isocenter in isoplane.  0 is center.
  y_mm: Double, // y in mm from isocenter in isoplane.  0 is center.
  width_mm: Double, // width in mm in isoplane.
  height_mm: Double // height in mm in isoplane.
) {

  def insert: Rect = {
    val insertQuery = Rect.query returning Rect.query.map(_.rectPK) into
      ((Rect, rectPK) => Rect.copy(rectPK = Some(rectPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(Rect.query.insertOrUpdate(this))

  override def toString: String = {
    "name: " + name +
      "    intensity_cu:" + intensity_cu +
      "    x_mm:" + x_mm +
      "    y_mm:" + y_mm +
      "    width_mm:" + width_mm +
      "    x_mm:" + x_mm +
      "    height_mm:" + height_mm
  }

  def toRectangle = new Rectangle2D.Double(x_mm, y_mm, width_mm, height_mm)
}

object Rect extends ProcedureOutput {
  class RectTable(tag: Tag) extends Table[Rect](tag, "Rect") {

    def rectPK = column[Long]("rectPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def name = column[String]("name")
    def intensity_cu = column[Double]("intensity_cu")
    def x_mm = column[Double]("x_mm")
    def y_mm = column[Double]("y_mm")
    def width_mm = column[Double]("width_mm")
    def height_mm = column[Double]("height_mm")

    def * = (
      rectPK.?,
      outputPK,
      SOPInstanceUID,
      name,
      intensity_cu,
      x_mm,
      y_mm,
      width_mm,
      height_mm) <> ((Rect.apply _)tupled, Rect.unapply _)

    def outputFK = foreignKey("Rect_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[RectTable]

  override val topXmlLabel = "Rect"

  def get(rectPK: Long): Option[Rect] = {
    val action = for {
      inst <- Rect.query if inst.rectPK === rectPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all rects for the given output
   */
  def getByOutput(outputPK: Long): Seq[Rect] = {
    val action = for {
      inst <- Rect.query if inst.outputPK === outputPK
    } yield (inst)
    val list = Db.run(action.result)
    list
  }

  def delete(rectPK: Long): Int = {
    val q = query.filter(_.rectPK === rectPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ???
  }

  def insertSeq(list: Seq[Rect]): Unit = {
    val ops = list.map { loc => Rect.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    System.exit(99)
  }
}
