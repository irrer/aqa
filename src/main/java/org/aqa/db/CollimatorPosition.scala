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
import org.aqa.procedures.ProcedureOutput

import scala.xml.Elem

case class CollimatorPosition(
    collimatorPositionPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    status: String, // termination status
    SOPInstanceUID: String, // UID of source image
    beamName: String, // name of beam in plan
    FloodCompensation: Boolean, // true if flood compensation was used
    X1_mm: Double, //  X1 position of collimator  (X axis) in mm
    X2_mm: Double, //  X2 position of collimator  (X axis) in mm
    Y1_mm: Double, //  Y1 of collimator (Y axis) in mm
    Y2_mm: Double, //  Y2 position of collimator (Y axis) in mm
    X1_ExpectedMinusImage_mm: Double, //  X1 plan minus east position of collimator  (X axis) in mm
    X2_ExpectedMinusImage_mm: Double, //  X2 plan minus west position of collimator  (X axis) in mm
    Y1_ExpectedMinusImage_mm: Double, //  Y1 plan minus north position of collimator (Y axis) in mm
    Y2_ExpectedMinusImage_mm: Double, //  Y2 plan minus south position of collimator (Y axis) in mm
    XCollimatorCenterOfRotation_mm: Double, // X position of the collimator's center of rotation
    YCollimatorCenterOfRotation_mm: Double, // Y position of the collimator's center of rotation
    gantryAngle_deg: Double, // gantry angle in degrees
    collimatorAngle_deg: Double // collimator angle in degrees
) {

  def insert: CollimatorPosition = {
    val insertQuery = CollimatorPosition.query returning CollimatorPosition.query.map(_.collimatorPositionPK) into
      ((collimatorPosition, collimatorPositionPK) => collimatorPosition.copy(collimatorPositionPK = Some(collimatorPositionPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(CollimatorPosition.query.insertOrUpdate(this))

  override def toString: String = {
    "    collimatorPositionPK: " + collimatorPositionPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    status: " + status + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    FloodCompensation: " + FloodCompensation + "\n" +
      "    X1_mm: " + X1_mm + "\n" +
      "    X2_mm: " + X2_mm + "\n" +
      "    Y1_mm: " + Y1_mm + "\n" +
      "    Y2_mm: " + Y2_mm + "\n" +
      "    X1_ExpectedMinusImage_mm: " + X1_ExpectedMinusImage_mm + "\n" +
      "    X2_ExpectedMinusImage_mm: " + X2_ExpectedMinusImage_mm + "\n" +
      "    Y1_ExpectedMinusImage_mm: " + Y1_ExpectedMinusImage_mm + "\n" +
      "    Y2_ExpectedMinusImage_mm: " + Y2_ExpectedMinusImage_mm + "\n" +
      "    XCollimatorCenterOfRotation_mm: " + XCollimatorCenterOfRotation_mm + "\n" +
      "    YCollimatorCenterOfRotation_mm: " + YCollimatorCenterOfRotation_mm + "\n" +
      "    gantryAngle_deg: " + gantryAngle_deg + "\n" +
      "    collimatorAngle_deg: " + collimatorAngle_deg + "\n"
  }
}

object CollimatorPosition extends ProcedureOutput {
  class CollimatorPositionTable(tag: Tag) extends Table[CollimatorPosition](tag, "collimatorPosition") {

    def collimatorPositionPK = column[Long]("collimatorPositionPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def status = column[String]("status")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def FloodCompensation = column[Boolean]("FloodCompensation")
    def X1_mm = column[Double]("X1_mm")
    def X2_mm = column[Double]("X2_mm")
    def Y1_mm = column[Double]("Y1_mm")
    def Y2_mm = column[Double]("Y2_mm")
    def X1_ExpectedMinusImage_mm = column[Double]("X1_ExpectedMinusImage_mm")
    def X2_ExpectedMinusImage_mm = column[Double]("X2_ExpectedMinusImage_mm")
    def Y1_ExpectedMinusImage_mm = column[Double]("Y1_ExpectedMinusImage_mm")
    def Y2_ExpectedMinusImage_mm = column[Double]("Y2_ExpectedMinusImage_mm")
    def XCollimatorCenterOfRotation_mm = column[Double]("XCollimatorCenterOfRotation_mm")
    def YCollimatorCenterOfRotation_mm = column[Double]("YCollimatorCenterOfRotation_mm")
    def gantryAngle_deg = column[Double]("gantryAngle_deg")
    def collimatorAngle_deg = column[Double]("collimatorAngle_deg")

    def * =
      (
        collimatorPositionPK.?,
        outputPK,
        status,
        SOPInstanceUID,
        beamName,
        FloodCompensation,
        X1_mm,
        X2_mm,
        Y1_mm,
        Y2_mm,
        X1_ExpectedMinusImage_mm,
        X2_ExpectedMinusImage_mm,
        Y1_ExpectedMinusImage_mm,
        Y2_ExpectedMinusImage_mm,
        XCollimatorCenterOfRotation_mm,
        YCollimatorCenterOfRotation_mm,
        gantryAngle_deg,
        collimatorAngle_deg
      ) <> (CollimatorPosition.apply _ tupled, CollimatorPosition.unapply)

    def outputFK = foreignKey("CollimatorPosition_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[CollimatorPositionTable]

  override val topXmlLabel = "CollimatorPosition"

  def get(collimatorPositionPK: Long): Option[CollimatorPosition] = {
    val action = for {
      inst <- CollimatorPosition.query if inst.collimatorPositionPK === collimatorPositionPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[CollimatorPosition] = {
    val action = for {
      inst <- CollimatorPosition.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(collimatorPositionPK: Long): Int = {
    val q = query.filter(_.collimatorPositionPK === collimatorPositionPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[CollimatorPosition]): Seq[Int] = {
    val ops = list.map { imgId => CollimatorPosition.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    throw new RuntimeException("CollimatorPosition.insert not implemented for Elem data.")
  }

  def insertSeq(list: Seq[CollimatorPosition]): Unit = {
    val ops = list.map { loc => CollimatorPosition.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class ColPosHistory(output: Output, colCent: CollimatorPosition) {}

  /**
    * Get the entire history of collimator position data for the given machine.
    * @param machinePK Machine to get data for.
    * @return List of history items sorted by data date.  For items with the same date, sort by beam name.
    */
  def history(machinePK: Long): Seq[ColPosHistory] = {

    val search = for {
      output <- Output.query.filter(o => o.machinePK === machinePK)
      colCent <- CollimatorPosition.query.filter(w => w.outputPK === output.outputPK)
    } yield (output, colCent)

    val sorted = Db.run(search.result).map(oc => ColPosHistory(oc._1, oc._2)).sortBy(h => h.output.dataDate.get.getTime.formatted("%14d") + h.colCent.beamName)

    sorted
  }

}
