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

package org.aqa.db

import org.aqa.db.Db.driver.api._
import org.aqa.Logging
import org.aqa.Util

case class PSMBeam(
    psmBeamPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    xCenter_mm: Double, // X coordinate of beam's center in mm
    yCenter_mm: Double, // Y coordinate of beam's center in mm
    SOPInstanceUID: String, // SOPInstanceUID if it is in the DICOM
    beamName: String, // name of beam
    mean_cu: Double, // average value of pixels in CU
    stdDev_cu: Double, // standard deviation of pixels in CU
    top_mm: Option[Double], // top of field edge measurement.  May be None if artifacts made measurement impossible.
    bottom_mm: Option[Double], // bottom of field edge measurement.  May be None if artifacts made measurement impossible.
    left_mm: Option[Double], // left field edge measurement.  May be None if artifacts made measurement impossible.
    right_mm: Option[Double] // right field edge measurement.  May be None if artifacts made measurement impossible.
)  {

  def insert: PSMBeam = {
    val insertQuery = PSMBeam.query returning PSMBeam.query.map(_.psmBeamPK) into
      ((psmBeam, psmBeamPK) => psmBeam.copy(psmBeamPK = Some(psmBeamPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(PSMBeam.query.insertOrUpdate(this))

  override def toString: String = {
    "    psmBeamPK: " + psmBeamPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    xCenter_mm: " + Util.fmtDbl(xCenter_mm) + "\n" +
      "    yCenter_mm: " + Util.fmtDbl(yCenter_mm) + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    beamName: " + beamName + "\n" +
      "    mean_cu: " + Util.fmtDbl(mean_cu) + "\n" +
      "    stdDev_cu: " + Util.fmtDbl(stdDev_cu) + "\n" +
      "    top_mm: " + Util.fmtDbl(top_mm) + "\n" +
      "    bottom_mm: " + Util.fmtDbl(bottom_mm) + "\n" +
      "    left_mm: " + Util.fmtDbl(left_mm) + "\n" +
      "    right_mm: " + Util.fmtDbl(right_mm)
  }

}

object PSMBeam extends Logging {
  class PSMBeamTable(tag: Tag) extends Table[PSMBeam](tag, "psmBeam") {

    def psmBeamPK = column[Long]("psmBeamPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def xCenter_mm = column[Double]("xCenter_mm")

    def yCenter_mm = column[Double]("yCenter_mm")

    def SOPInstanceUID = column[String]("SOPInstanceUID")

    def beamName = column[String]("beamName")

    def mean_cu = column[Double]("mean_cu")

    def stdDev_cu = column[Double]("stdDev_cu")

    def top_mm = column[Option[Double]]("top_mm")

    def bottom_mm = column[Option[Double]]("bottom_mm")

    def left_mm = column[Option[Double]]("left_mm")

    def right_mm = column[Option[Double]]("right_mm")

    def * =
      (
        psmBeamPK.?,
        outputPK,
        xCenter_mm,
        yCenter_mm,
        SOPInstanceUID,
        beamName,
        mean_cu,
        stdDev_cu,
        top_mm,
        bottom_mm,
        left_mm,
        right_mm
      ) <> (PSMBeam.apply _ tupled, PSMBeam.unapply)

    def outputFK = foreignKey("PSMBeam_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[PSMBeamTable]

  def get(psmBeamPK: Long): Option[PSMBeam] = {
    val action = for {
      inst <- PSMBeam.query if inst.psmBeamPK === psmBeamPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[PSMBeam] = {
    val action = for {
      inst <- PSMBeam.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(psmBeamPK: Long): Int = {
    val q = query.filter(_.psmBeamPK === psmBeamPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[PSMBeam]): Seq[Int] = {
    list.map(_.insertOrUpdate())
  }

}
