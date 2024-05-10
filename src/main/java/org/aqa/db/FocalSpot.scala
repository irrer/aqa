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

/**
 * Store and instance of all of the data for a single image of focal spot.
 */
case class FocalSpot(
                      // @formatter:off
                      focalSpotPK                      : Option[Long], // primary key
                      outputPK                         : Long,    // output primary key
                      SOPInstanceUID                   : String,  // UID of RTIMAGE
                      gantryAngleRounded_deg           : Int,     // Gantry angle rounded to the nearest multiple of 90 degrees
                      collimatorAngleRounded_deg       : Int,     // Collimator angle rounded to the nearest multiple of 90 degrees
                      beamName                         : String,  // name of beam
                      isJaw                            : Boolean, // true if edges are defined by jaw, false if defined by MLC
                      isFFF                            : Boolean, // true if beam is FFF (Flattening Filter Free)
                      KVP_kv                           : Double,  // beam energy in kv              : DICOM 0018,0060 : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020030/00180060
                      RTImageSID_mm                    : Double,  // source to imager distance      : DICOM 3002,0026 : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020026
                      ExposureTime                     : Double,  // exposure time                  : DICOM 0018,1150 : https://dicom.innolitics.com/ciods/rt-image/rt-image/00181150

                      XRayImageReceptorTranslationX_mm : Double,  // XRayImageReceptorTranslation X : DICOM 3002,000d : https://dicom.innolitics.com/ciods/rt-image/rt-image/3002000d
                      XRayImageReceptorTranslationY_mm : Double,  // XRayImageReceptorTranslation Y : DICOM 3002,000d : (same as above)
                      XRayImageReceptorTranslationZ_mm : Double,  // XRayImageReceptorTranslation Z : DICOM 3002,000d : (same as above)

                      topEdge_mm                       : Double,  // top edge measurement
                      bottomEdge_mm                    : Double,  // bottom edge measurement
                      leftEdge_mm                      : Double,  // left edge measurement
                      rightEdge_mm                     : Double,  // right edge measurement

                      topEdgePlanned_mm                : Double,  // planned top edge
                      bottomEdgePlanned_mm             : Double,  // planned bottom edge
                      leftEdgePlanned_mm               : Double,  // planned left edge
                      rightEdgePlanned_mm              : Double   // planned right edge
                      // @formatter:on
                    ) {

  def insert: FocalSpot = {
    val insertQuery = FocalSpot.query returning FocalSpot.query.map(_.focalSpotPK) into
      ((focalSpot, focalSpotPK) => focalSpot.copy(focalSpotPK = Some(focalSpotPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  val centerX: Double = (leftEdge_mm + rightEdge_mm) / 2
  val centerY: Double = (topEdge_mm + bottomEdge_mm) / 2

  val topEdgeError_mm: Double = topEdge_mm - topEdgePlanned_mm
  val bottomEdgeError_mm: Double = bottomEdge_mm - bottomEdgePlanned_mm
  val leftEdgeError_mm: Double = leftEdge_mm - leftEdgePlanned_mm
  val rightEdgeError_mm: Double = rightEdge_mm - rightEdgePlanned_mm

  def insertOrUpdate(): Int = Db.run(FocalSpot.query.insertOrUpdate(this))

  val isMLC: Boolean = !isJaw

  val beamLimiterName: String = if (isJaw) "Jaw" else "MLC"

  val fluenceName: String = if (isFFF) "FFF" else "STD"

  override def toString: String = {
    s"""focalSpotPK: $focalSpotPK
       outputPK: $outputPK
       SOPInstanceUID: $SOPInstanceUID
       gantryAngleRounded_deg: $gantryAngleRounded_deg
       collimatorAngleRounded_deg: $collimatorAngleRounded_deg
       beamName: $beamName
       edge type: $beamLimiterName
       fluence : $fluenceName
       KVP_kv: $KVP_kv
       RTImageSID_mm: $RTImageSID_mm
       ExposureTime: $ExposureTime
       XRayImageReceptorTranslationX_mm: $XRayImageReceptorTranslationX_mm
       XRayImageReceptorTranslationY_mm: $XRayImageReceptorTranslationY_mm
       XRayImageReceptorTranslationZ_mm: $XRayImageReceptorTranslationZ_mm
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

object FocalSpot {
  class FocalSpotTable(tag: Tag) extends Table[FocalSpot](tag, "focalSpot") {

    def focalSpotPK = column[Long]("focalSpotPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def SOPInstanceUID = column[String]("SOPInstanceUID")

    def gantryAngleRounded_deg = column[Int]("gantryAngleRounded_deg")

    def collimatorAngleRounded_deg = column[Int]("collimatorAngleRounded_deg")

    def beamName = column[String]("beamName")

    def isJaw = column[Boolean]("isJaw")

    def isFFF = column[Boolean]("isFFF")

    def KVP_kv = column[Double]("KVP_kv")

    def RTImageSID_mm = column[Double]("RTImageSID_mm")

    def ExposureTime = column[Double]("ExposureTime")

    def XRayImageReceptorTranslationX_mm = column[Double]("XRayImageReceptorTranslationX_mm")

    def XRayImageReceptorTranslationY_mm = column[Double]("XRayImageReceptorTranslationY_mm")

    def XRayImageReceptorTranslationZ_mm = column[Double]("XRayImageReceptorTranslationZ_mm")

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
        isJaw,
        isFFF,
        KVP_kv,
        RTImageSID_mm,
        ExposureTime,
        XRayImageReceptorTranslationX_mm,
        XRayImageReceptorTranslationY_mm,
        XRayImageReceptorTranslationZ_mm,
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
    list.map(_.insertOrUpdate())
  }

  def insertSeq(list: Seq[FocalSpot]): Unit = {
    list.foreach(_.insertOrUpdate())
  }

  case class FocalSpotHistory(output: Output, focalSpot: FocalSpot) {}

  /**
   * Get the entire history of Focal Spot data for the given machine.
   *
   * @param machinePK   Machine to get data for.
   * @param procedurePK For this procedure.
   * @return List of history items sorted by data date.
   */
  def history(machinePK: Long, procedurePK: Long): Seq[FocalSpotHistory] = {

    val search = for {
      output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      colCent <- FocalSpot.query.filter(w => w.outputPK === output.outputPK)
    } yield (output, colCent)

    val sorted = Db.run(search.result).map(oc => FocalSpotHistory(oc._1, oc._2)).sortBy(_.output.dataDate.get.getTime)

    sorted
  }

  /**
   * Get the entire history of Focal Spot data for the given machine.
   *
   * @param machinePK   Machine to get data for.
   * @param procedurePK For this procedure.
   * @param kvp_kv      For this KVP.
   * @return List of history items sorted by data date.
   */
  def history(machinePK: Long, procedurePK: Long, kvp_kv: Double, isFFF: Boolean): Seq[FocalSpotHistory] = {

    val search = for {
      output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      colCent <- FocalSpot.query.filter(fs => (fs.outputPK === output.outputPK) && (fs.KVP_kv === kvp_kv) && (fs.isFFF === isFFF))
    } yield (output, colCent)

    val sorted = Db.run(search.result).map(oc => FocalSpotHistory(oc._1, oc._2)).sortBy(_.output.dataDate.get.getTime)

    sorted
  }

  /**
   * Round the RTImageSID to the nearest 10.
   *
   * @param RTImageSID Raw value from attribute list.
   * @return Value rounded.
   */
  def roundRTImageSID(RTImageSID: Double): Double = (RTImageSID / 10).round * 10.0


}
