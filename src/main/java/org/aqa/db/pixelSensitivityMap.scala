package org.aqa.db

/*
 * Copyright 2024 Regents of the University of Michigan
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

import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput

import scala.xml.Elem

case class pixelSensitivityMap(
    pixelSensitivityMapPK: Option[Long], // primary key
    outputPK: Long, // metadata
    psmType: String // type of PSM

    /*
      TODO
        3.  Calculation steps: These look correct to me. To check, let us standardise on naming:
            WD = whole detector. This is the image directly from the EPID
            FF =  flood field. This is the calibration image automatically applied by the LinAC to create the WD image.
            WD*FF =Raw image. This is the EPID image with the flood field image removed.
            PSM = the alternate calibration image to the flood field that we need for QA applications.
            Raw image/PSM = BR (Beam Response). The BR image is the image that we require to perform our symmetry analysis on.

     */

) {

  def insert: pixelSensitivityMap = {
    val insertQuery = pixelSensitivityMap.query returning pixelSensitivityMap.query.map(_.pixelSensitivityMapPK) into
      ((PSM, pixelSensitivityMapPK) => PSM.copy(pixelSensitivityMapPK = Some(pixelSensitivityMapPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(pixelSensitivityMap.query.insertOrUpdate(this))

  override def toString: String = {
    s"pixelSensitivityMapPK: $pixelSensitivityMapPK    outputPK: $outputPK    psmType: $psmType"
  }
}

//noinspection ScalaWeakerAccess
object pixelSensitivityMap extends ProcedureOutput {
  class PSMTable(tag: Tag) extends Table[pixelSensitivityMap](tag, "PSM") {

    def pixelSensitivityMapPK = column[Long]("pixelSensitivityMapPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def psmType = column[String]("psmType")

    def * = (pixelSensitivityMapPK.?, outputPK, psmType) <> (pixelSensitivityMap.apply _ tupled, pixelSensitivityMap.unapply)

    def outputFK = foreignKey("PSM_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[PSMTable]

  override val topXmlLabel = "pixelSensitivityMap"

  def get(pixelSensitivityMapPK: Long): Option[pixelSensitivityMap] = {
    val action = for {
      inst <- pixelSensitivityMap.query if inst.pixelSensitivityMapPK === pixelSensitivityMapPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[pixelSensitivityMap] = {
    val action = for {
      inst <- pixelSensitivityMap.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(pixelSensitivityMapPK: Long): Int = {
    val q = query.filter(_.pixelSensitivityMapPK === pixelSensitivityMapPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[pixelSensitivityMap]): Seq[Int] = {
    val ops = list.map { imgId => pixelSensitivityMap.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    throw new RuntimeException("Insert by elem not implemented.")
  }

  def insertSeq(list: Seq[pixelSensitivityMap]): Unit = {
    val ops = list.map { loc => pixelSensitivityMap.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

}
