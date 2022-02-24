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

import scala.xml.Elem

/**
  * Store the analysis results for a single gap skew image.
  */
case class GapSkew(
    gapSkewPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    rtplanSOPInstanceUID: String, // UID of RTPLAN
    rtimageSeriesInstanceUID: String, // SOP series instance UID of EPID image
    rtimageUID: String, // SOP series instance UID of EPID image
    beamName: String, // name of beam
    topLeftX_mm: Double, // X isoplane coordinate center of top left measurement
    topLeftY_mm: Double, // Y isoplane coordinate of top left center measurement
    topRightX_mm: Double, // X isoplane coordinate center of top right measurement
    topRightY_mm: Double, // Y isoplane coordinate of top right center measurement
    topPlannedY_mm: Double, // planned Y for top
    topIsJaw: Boolean, // true if top is a jaw edge, false if it is an MLC edge
    bottomLeftX_mm: Double, // X isoplane coordinate center of bottom left measurement
    bottomLeftY_mm: Double, // Y isoplane coordinate of bottom left center measurement
    bottomRightX_mm: Double, // X isoplane coordinate center of bottom right measurement
    bottomRightY_mm: Double, // Y isoplane coordinate of bottom right center measurement
    bottomPlannedY_mm: Double, // planned Y for bottomtop
    bottomIsJaw: Boolean // true if bottom top is a jaw edge, false if it is an MLC edge
) {

  def insert: GapSkew = {
    val insertQuery = GapSkew.query returning GapSkew.query.map(_.gapSkewPK) into ((gapSkew, gapSkewPK) => gapSkew.copy(gapSkewPK = Some(gapSkewPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(GapSkew.query.insertOrUpdate(this))

  val topDeltaY = topRightY_mm - topLeftY_mm
  val bottomDeltaY = bottomRightY_mm - bottomLeftY_mm

  val topAngle_deg: Double = {
    val topDeltaX = topLeftX_mm - topRightX_mm
    Math.toDegrees(Math.atan(topDeltaY / topDeltaX))
  }

  val bottomAngle_deg: Double = {
    val bottomDeltaX = bottomLeftX_mm - bottomRightX_mm
    Math.toDegrees(Math.atan(bottomDeltaY / bottomDeltaX))
  }

  val averageAngle_deg = (topAngle_deg + bottomAngle_deg) / 2
}

object GapSkew extends ProcedureOutput with Logging {

  class GapSkewTable(tag: Tag) extends Table[GapSkew](tag, "gapSkew") {

    def gapSkewPK = column[Long]("gapSkewPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def rtplanSOPInstanceUID = column[String]("rtplanSOPInstanceUID")

    def rtimageSeriesInstanceUID = column[String]("rtimageSeriesInstanceUID")
    def rtimageUID = column[String]("rtimageUID")
    def beamName = column[String]("beamName")

    def topLeftX_mm = column[Double]("topLeftX_mm")
    def topLeftY_mm = column[Double]("topLeftY_mm")
    def topRightX_mm = column[Double]("topRightX_mm")
    def topRightY_mm = column[Double]("topRightY_mm")
    def topPlannedY_mm = column[Double]("topPlannedY_mm")
    def topIsJaw = column[Boolean]("topIsJaw")

    def bottomLeftX_mm = column[Double]("bottomLeftX_mm")
    def bottomLeftY_mm = column[Double]("bottomLeftY_mm")
    def bottomRightX_mm = column[Double]("bottomRightX_mm")
    def bottomRightY_mm = column[Double]("bottomRightY_mm")
    def bottomPlannedY_mm = column[Double]("bottomPlannedY_mm")
    def bottomIsJaw = column[Boolean]("bottomIsJaw")

    def * =
      (
        gapSkewPK.?,
        outputPK,
        rtplanSOPInstanceUID,
        rtimageSeriesInstanceUID,
        rtimageUID,
        beamName,
        topLeftX_mm,
        topLeftY_mm,
        topRightX_mm,
        topRightY_mm,
        topPlannedY_mm,
        topIsJaw,
        bottomLeftX_mm,
        bottomLeftY_mm,
        bottomRightX_mm,
        bottomRightY_mm,
        bottomPlannedY_mm,
        bottomIsJaw
      ) <> (GapSkew.apply _ tupled, GapSkew.unapply)

    def outputFK = foreignKey("GapSkew_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[GapSkewTable]

  override val topXmlLabel = "GapSkew"

  def get(gapSkewPK: Long): Option[GapSkew] = {
    val action = for {
      inst <- GapSkew.query if inst.gapSkewPK === gapSkewPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all GapSkew for the given output
    */
  def getByOutput(outputPK: Long): Seq[GapSkew] = {
    val action = for {
      inst <- GapSkew.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list
  }

  def delete(gapSkewPK: Long): Int = {
    val q = query.filter(_.gapSkewPK === gapSkewPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[GapSkew] = {
    if (true) throw new RuntimeException("Unsupported function.") // should never be called
    if ((elem == null) || (outputPK == -1)) System.currentTimeMillis // fixes compiler warnings
    Seq[GapSkew]()
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[GapSkew]): Unit = {
    val ops = list.map { loc => GapSkew.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }
}
