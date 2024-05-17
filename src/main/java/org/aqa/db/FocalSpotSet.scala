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

import scala.collection.immutable

/**
 * Store and instance of all of the data for a single image of focal spot.
 */
case class FocalSpotSet(
                         // @formatter:off
      focalSpotSetPK         : Option[Long], // primary key
      outputPK               : Long,    // output primary key
      KVP_kv                 : Double,  // beam energy in KV
      isFFF                  : Boolean, // True if FFF (Flattening Filter Free), false if standard.
      //
      focalSpotAlignmentX_mm : Double,  // (mis) alignment in X axis
      focalSpotAlignmentY_mm : Double,  // (mis) alignment in Y axis
      //
      jaw090PK               : Long,    // Jaw 090 FocalSpot.focalSpotPK public key reference
      jaw270PK               : Long,    // Jaw 270 FocalSpot.focalSpotPK public key reference
      mlc090PK               : Long,    // MLC 090 FocalSpot.focalSpotPK public key reference
      mlc270PK               : Long     // MLC 270 FocalSpot.focalSpotPK public key reference
      // @formatter:on
                       ) {

  def insert: FocalSpotSet = {
    val insertQuery = FocalSpotSet.query returning FocalSpotSet.query.map(_.focalSpotSetPK) into
      ((focalSpotSet, focalSpotSetPK) => focalSpotSet.copy(focalSpotSetPK = Some(focalSpotSetPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  val fluenceName: String = if (isFFF) "FFF" else "STD"

  def insertOrUpdate(): Int = Db.run(FocalSpotSet.query.insertOrUpdate(this))

  val mv: Double = KVP_kv / 1000.0
  val mvText: String = if (mv.round == mv) mv.round.toString else mv.toString

  override def toString: String = {
    s"""focalSpotSetPK       : $focalSpotSetPK
       outputPK              : $outputPK
       MV                    : $mv
       Fluence               : $fluenceName
       focalSpotAlignmentX_mm: $focalSpotAlignmentX_mm
       focalSpotAlignmentY_mm: $focalSpotAlignmentY_mm
   """
  }
}

object FocalSpotSet {
  class FocalSpotSetTable(tag: Tag) extends Table[FocalSpotSet](tag, "focalSpotSet") {

    def focalSpotSetPK = column[Long]("focalSpotSetPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def KVP_kv = column[Double]("KVP_kv")

    def isFFF = column[Boolean]("isFFF")

    def focalSpotAlignmentX_mm = column[Double]("focalSpotAlignmentX_mm")

    def focalSpotAlignmentY_mm = column[Double]("focalSpotAlignmentY_mm")

    def jaw090PK = column[Long]("jaw090PK")

    def jaw270PK = column[Long]("jaw270PK")

    def mlc090PK = column[Long]("mlc090PK")

    def mlc270PK = column[Long]("mlc270PK")

    def * =
      (
        focalSpotSetPK.?,
        outputPK,
        KVP_kv,
        isFFF,
        focalSpotAlignmentX_mm,
        focalSpotAlignmentY_mm,
        jaw090PK,
        jaw270PK,
        mlc090PK,
        mlc270PK
      ) <> (FocalSpotSet.apply _ tupled, FocalSpotSet.unapply)

    def outputFK = foreignKey("FocalSpotSet_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)

    def jaw090FK = foreignKey("FocalSpotSet_jaw090PKPKConstraint", jaw090PK, FocalSpot.query)(_.focalSpotPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)

    def jaw270FK = foreignKey("FocalSpotSet_jaw270PKPKConstraint", jaw270PK, FocalSpot.query)(_.focalSpotPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)

    def mlc090FK = foreignKey("FocalSpotSet_mlc090PKPKConstraint", mlc090PK, FocalSpot.query)(_.focalSpotPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)

    def mlc270FK = foreignKey("FocalSpotSet_mlc270PKPKConstraint", mlc270PK, FocalSpot.query)(_.focalSpotPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[FocalSpotSetTable]

  def get(focalSpotSetPK: Long): Option[FocalSpotSet] = {
    val action = for {
      inst <- FocalSpotSet.query if inst.focalSpotSetPK === focalSpotSetPK
    } yield inst
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[FocalSpotSet] = {
    val action = for {
      inst <- FocalSpotSet.query if inst.outputPK === outputPK
    } yield inst
    Db.run(action.result)
  }

  def delete(focalSpotSetPK: Long): Int = {
    val q = query.filter(_.focalSpotSetPK === focalSpotSetPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  case class FocalSpotSetHistory(output: Output, focalSpotSet: FocalSpotSet) {
    private val focalSpots = FocalSpot.getSet(Set(focalSpotSet.jaw090PK, focalSpotSet.jaw270PK, focalSpotSet.mlc090PK, focalSpotSet.mlc270PK))

  }


  /**
   * Provide static functions that can access the four images.
   */
  object FocalSpotSetHistory {

    private def findFs(fsh: FocalSpotSetHistory, jaw: Boolean, colAngle: Int): FocalSpot = {
      fsh.focalSpots.find(fs => (fs.isJaw == jaw) && fs.collimatorAngleRounded_deg == colAngle).get
    }

    def jaw090(focalSpotSetHistory: FocalSpotSetHistory): FocalSpot = findFs(focalSpotSetHistory, jaw = true, 90)

    def jaw270(focalSpotSetHistory: FocalSpotSetHistory): FocalSpot = findFs(focalSpotSetHistory, jaw = true, 270)

    def mlc090(focalSpotSetHistory: FocalSpotSetHistory): FocalSpot = findFs(focalSpotSetHistory, jaw = false, 90)

    def mlc270(focalSpotSetHistory: FocalSpotSetHistory): FocalSpot = findFs(focalSpotSetHistory, jaw = false, 270)
  }


  /**
   * Get the entire history of Focal Spot data for the given machine.
   *
   * @param machinePK Machine to get data for.
   * @return List of history items sorted by data date.
   */
  def history(machinePK: Long): immutable.Seq[FocalSpotSetHistory] = {

    val search = for {
      output <- Output.valid.filter(o => o.machinePK === machinePK)
      focalSpotSet <- FocalSpotSet.query.filter(w => w.outputPK === output.outputPK)
    } yield (output, focalSpotSet)

    val focalSpotSetList = Db.run(search.result).map(outFocal => FocalSpotSetHistory(outFocal._1, outFocal._2)).sortBy(_.output.dataDate.get.getTime)

    immutable.Seq(focalSpotSetList).flatten
  }

  /**
   * Get the entire history of Focal Spot data for the given machine.
   *
   * @param machinePK   Machine to get data for.
   * @param procedurePK For this procedure.
   * @param kvp_kv      For this KVP only.
   * @param isFFF       True if FFF
   * @return List of history items sorted by data date.
   */
  def history(machinePK: Long, procedurePK: Long, kvp_kv: Double, isFFF: Boolean): Seq[FocalSpotSetHistory] = {

    val search = for {
      output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      focalSpotSet <- FocalSpotSet.query.filter(fs => (fs.outputPK === output.outputPK) && (fs.KVP_kv === kvp_kv) && (fs.isFFF === isFFF))
    } yield (output, focalSpotSet)

    val focalSpotSetList = Db.run(search.result).map(outFocal => FocalSpotSetHistory(outFocal._1, outFocal._2)).sortBy(_.output.dataDate.get.getTime)

    focalSpotSetList
  }

}


