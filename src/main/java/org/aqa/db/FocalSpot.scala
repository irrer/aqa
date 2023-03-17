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

/**
  * Store and instance of all of the data for a single image of focal spot.
  */
case class FocalSpot(
    // @formatter:off
    focalSpotPK                : Option[Long], // primary key
    outputPK                   : Long,   // output primary key
    SOPInstanceUID             : String, // UID of RTIMAGE
    gantryAngleRounded_deg     : Int,    // Gantry angle rounded to the nearest multiple of 90 degrees
    collimatorAngleRounded_deg : Int,    // Collimator angle rounded to the nearest multiple of 90 degrees
    beamName                   : String, // name of beam
    KVP                        : Double, // beam energy               : DICOM 0018,0060 : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020030/00180060
    RTImageSID_mm              : Double, // source to imager distance : DICOM 3002,0026 : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020026
    ExposureTime               : Double, // exposure time             : DICOM 0018,1150 : https://dicom.innolitics.com/ciods/rt-image/rt-image/00181150
    //
    topEdge_mm                 : Double, // top edge measurement
    bottomEdge_mm              : Double, // bottom edge measurement
    leftEdge_mm                : Double, // left edge measurement
    rightEdge_mm               : Double, // right edge measurement
    //
    topEdgePlanned_mm          : Double, // planned top edge
    bottomEdgePlanned_mm       : Double, // planned bottom edge
    leftEdgePlanned_mm         : Double, // planned left edge
    rightEdgePlanned_mm        : Double  // planned right edge
    // @formatter:on
) {

  def insert: FocalSpot = {
    val insertQuery = FocalSpot.query returning FocalSpot.query.map(_.focalSpotPK) into
      ((focalSpot, focalSpotPK) => focalSpot.copy(focalSpotPK = Some(focalSpotPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(FocalSpot.query.insertOrUpdate(this))

  override def toString: String = {
    s"""focalSpotPK: $focalSpotPK
       outputPK: $outputPK
       SOPInstanceUID: $SOPInstanceUID
       gantryAngleRounded_deg: $gantryAngleRounded_deg
       collimatorAngleRounded_deg: $collimatorAngleRounded_deg
       beamName: $beamName
       KVP: $KVP
       RTImageSID_mm: $RTImageSID_mm
       ExposureTime: $ExposureTime
       topEdge_mm: $topEdge_mm
       bottomEdge_mm: $bottomEdge_mm
       leftEdge_mm: $leftEdge_mm
       rightEdge_mm: $rightEdge_mm
       topEdgePlanned_mm: $topEdgePlanned_mm
       bottomEdgePlanned_mm: $bottomEdgePlanned_mm
       leftEdgePlanned_mm: $leftEdgePlanned_mm
       rightEdgePlanned_mm: $rightEdgePlanned_mm
   """
  }

}

object FocalSpot extends ProcedureOutput {
  class FocalSpotTable(tag: Tag) extends Table[FocalSpot](tag, "focalSpot") {

    def focalSpotPK = column[Long]("focalSpotPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def gantryAngleRounded_deg = column[Int]("gantryAngleRounded_deg")
    def collimatorAngleRounded_deg = column[Int]("collimatorAngleRounded_deg")
    def beamName = column[String]("beamName")
    def KVP = column[Double]("KVP")
    def RTImageSID_mm = column[Double]("RTImageSID_mm")
    def ExposureTime = column[Double]("ExposureTime")
    def topEdge_mm = column[Double]("topEdge_mm")
    def bottomEdge_mm = column[Double]("bottomEdge_mm")
    def leftEdge_mm = column[Double]("leftEdge_mm")
    def rightEdge_mm = column[Double]("rightEdge_mm")
    def topEdgePlanned_mm = column[Double]("topEdgePlanned_mm")
    def bottomEdgePlanned_mm = column[Double]("bottomEdgePlanned_mm")
    def leftEdgePlanned_mm = column[Double]("leftEdgePlanned_mm")
    def rightEdgePlanned_mm = column[Double]("rightEdgePlanned_mm")

    def * =
      (
        focalSpotPK.?,
        outputPK,
        SOPInstanceUID,
        gantryAngleRounded_deg,
        collimatorAngleRounded_deg,
        beamName,
        KVP,
        RTImageSID_mm,
        ExposureTime,
        topEdge_mm,
        bottomEdge_mm,
        leftEdge_mm,
        rightEdge_mm,
        topEdgePlanned_mm,
        bottomEdgePlanned_mm,
        leftEdgePlanned_mm,
        rightEdgePlanned_mm
      ) <> (FocalSpot.apply _ tupled, FocalSpot.unapply)

    def outputFK = foreignKey("FocalSpot_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[FocalSpotTable]

  override val topXmlLabel = "FocalSpot"

  def get(focalSpotPK: Long): Option[FocalSpot] = {
    val action = for {
      inst <- FocalSpot.query if inst.focalSpotPK === focalSpotPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
    * Get a list of all rows for the given output
    */
  def getByOutput(outputPK: Long): Seq[FocalSpot] = {
    val action = for {
      inst <- FocalSpot.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(focalSpotPK: Long): Int = {
    val q = query.filter(_.focalSpotPK === focalSpotPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[FocalSpot]): Seq[Int] = {
    val ops = list.map { imgId => FocalSpot.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    throw new RuntimeException("Focal Spot insert not defined for Elem data.")
  }

  def insertSeq(list: Seq[FocalSpot]): Unit = {
    val ops = list.map { loc => FocalSpot.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  case class ColCentHistory(output: Output, colCent: FocalSpot) {}

  /**
    * Get the entire history of Focal Spot data for the given machine.
    * @param machinePK Machine to get data for.
    * @param procedurePK For this procedure.
    * @return List of history items sorted by data date.
    */
  def history(machinePK: Long, procedurePK: Long): Seq[ColCentHistory] = {

    val search = for {
      output <- Output.query.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      colCent <- FocalSpot.query.filter(w => (w.outputPK === output.outputPK))
    } yield (output, colCent)

    val sorted = Db.run(search.result).map(oc => ColCentHistory(oc._1, oc._2)).sortBy(_.output.dataDate.get.getTime)

    sorted
  }

}
