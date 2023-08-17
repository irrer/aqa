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

import java.awt.geom.Point2D
import scala.xml.Elem

case class CollimatorCentering(
    collimatorCenteringPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    status: String, // termination status
    SOPInstanceUID090: String, // UID of 90 degree DICOM image
    SOPInstanceUID270: String, // UID of 270 degree DICOM image
    gantryAngleRounded_deg: Int, // Gantry angle rounded to the nearest multiple of 90 degrees
    beamName090: String, // name of beam with collimator angle 90
    beamName270: String, // name of beam with collimator angle 270
    xCollimatorCenter_mm: Double, // X collimator isoplane center of rotation in mm
    yCollimatorCenter_mm: Double, // Y collimator isoplane center of rotation in mm
    X1_090_mm: Double, // X1 position of collimator leaf edge for gantry at 90 degrees (X axis) in mm
    X2_090_mm: Double, // X2 position of collimator leaf edge for gantry at 90 degrees (X axis) in mm
    Y1_090_mm: Double, // Y1 position of collimator leaf edge for gantry at 90 degrees (Y axis) in mm
    Y2_090_mm: Double, // Y2 position of collimator leaf edge for gantry at 90 degrees (Y axis) in mm
    X1_270_mm: Double, // X1 position of collimator leaf edge for gantry at 270 degrees (X axis) in mm
    X2_270_mm: Double, // X2 position of collimator leaf edge for gantry at 270 degrees (X axis) in mm
    Y1_270_mm: Double, // Y1 position of collimator leaf edge for gantry at 270 degrees (Y axis) in mm
    Y2_270_mm: Double //  Y2 position of collimator leaf edge for gantry at 270 degrees (Y axis) in mm
) {

  def insert: CollimatorCentering = {
    val insertQuery = CollimatorCentering.query returning CollimatorCentering.query.map(_.collimatorCenteringPK) into
      ((collimatorCentering, collimatorCenteringPK) => collimatorCentering.copy(collimatorCenteringPK = Some(collimatorCenteringPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(CollimatorCentering.query.insertOrUpdate(this))

  override def toString: String = {
    "    collimatorCenteringPK: " + collimatorCenteringPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    status: " + status + "\n" +
      "    SOPInstanceUID090: " + SOPInstanceUID090 + "\n" +
      "    SOPInstanceUID270: " + SOPInstanceUID270 + "\n" +
      "    gantryAngleRounded_deg: " + gantryAngleRounded_deg + "\n" +
      "    beamName090: " + beamName090 + "\n" +
      "    beamName270: " + beamName270 + "\n" +
      "    xCollimatorCenter_mm: " + xCollimatorCenter_mm + "\n" +
      "    yCollimatorCenter_mm: " + yCollimatorCenter_mm + "\n" +
      "    X1_090_mm: " + X1_090_mm + "\n" +
      "    X2_090_mm: " + X2_090_mm + "\n" +
      "    Y1_090_mm: " + Y1_090_mm + "\n" +
      "    Y2_090_mm: " + Y2_090_mm + "\n" +
      "    X1_270_mm: " + X1_270_mm + "\n" +
      "    X2_270_mm: " + X2_270_mm + "\n" +
      "    Y1_270_mm: " + Y1_270_mm + "\n" +
      "    Y2_270_mm: " + Y2_270_mm + "\n"
  }

  // collimator center of rotation in mm
  val center = new Point2D.Double(xCollimatorCenter_mm, yCollimatorCenter_mm)
}

object CollimatorCentering extends ProcedureOutput {
  class CollimatorCenteringTable(tag: Tag) extends Table[CollimatorCentering](tag, "collimatorCentering") {

    def collimatorCenteringPK = column[Long]("collimatorCenteringPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def status = column[String]("status")
    def SOPInstanceUID090 = column[String]("SOPInstanceUID090")
    def SOPInstanceUID270 = column[String]("SOPInstanceUID270")
    def gantryAngleRounded_deg = column[Int]("gantryAngleRounded_deg")
    def beamName090 = column[String]("beamName090")
    def beamName270 = column[String]("beamName270")
    def xCollimatorCenter_mm = column[Double]("xCollimatorCenter_mm")
    def yCollimatorCenter_mm = column[Double]("yCollimatorCenter_mm")
    def X1_090_mm = column[Double]("X1_090_mm")
    def X2_090_mm = column[Double]("X2_090_mm")
    def Y1_090_mm = column[Double]("Y1_090_mm")
    def Y2_090_mm = column[Double]("Y2_090_mm")
    def X1_270_mm = column[Double]("X1_270_mm")
    def X2_270_mm = column[Double]("X2_270_mm")
    def Y1_270_mm = column[Double]("Y1_270_mm")
    def Y2_270_mm = column[Double]("Y2_270_mm")

    def * =
      (
        collimatorCenteringPK.?,
        outputPK,
        status,
        SOPInstanceUID090,
        SOPInstanceUID270,
        gantryAngleRounded_deg,
        beamName090,
        beamName270,
        xCollimatorCenter_mm,
        yCollimatorCenter_mm,
        X1_090_mm,
        X2_090_mm,
        Y1_090_mm,
        Y2_090_mm,
        X1_270_mm,
        X2_270_mm,
        Y1_270_mm,
        Y2_270_mm
      ) <> (CollimatorCentering.apply _ tupled, CollimatorCentering.unapply)

    def outputFK = foreignKey("CollimatorCentering_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[CollimatorCenteringTable]

  override val topXmlLabel = "CollimatorCentering"

  def get(collimatorCenteringPK: Long): Option[CollimatorCentering] = {
    val action = for {
      inst <- CollimatorCentering.query if inst.collimatorCenteringPK === collimatorCenteringPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[CollimatorCentering] = {
    val action = for {
      inst <- CollimatorCentering.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(collimatorCenteringPK: Long): Int = {
    val q = query.filter(_.collimatorCenteringPK === collimatorCenteringPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[CollimatorCentering]): Seq[Int] = {
    val ops = list.map { imgId => CollimatorCentering.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    throw new RuntimeException("Collimator Centering insert not defined for Elem data.")
  }

  def insertSeq(list: Seq[CollimatorCentering]): Unit = {
    val ops = list.map { loc => CollimatorCentering.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class ColCentHistory(output: Output, colCent: CollimatorCentering) {}

  /**
    * Get the entire history of collimator centering data for the given machine.
    * @param machinePK Machine to get data for.
    * @param gantryAngle Gantry angle rounded to nearest 90 degrees.
    * @return List of history items sorted by data date.
    */
  def history(machinePK: Long, gantryAngle: Option[Int], procedurePK: Long): Seq[ColCentHistory] = {

    val ga = if (gantryAngle.isDefined) gantryAngle.get else 0

    val search = for {
      output <- Output.valid.filter(o => o.procedurePK === procedurePK)
      colCent <- CollimatorCentering.query.filter(w => (w.outputPK === output.outputPK) && (w.gantryAngleRounded_deg === ga))
    } yield (output, colCent)

    val sorted = Db.run(search.result).map(oc => ColCentHistory(oc._1, oc._2)).sortBy(_.output.dataDate.get.getTime)

    sorted
  }

}
