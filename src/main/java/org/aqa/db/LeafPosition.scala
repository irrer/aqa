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
import org.aqa.run.ProcedureStatus

import scala.xml.Elem

case class LeafPosition(
    leafPositionPK: Option[Long], // primary key
    outputPK: Long, // output primary key
    SOPInstanceUID: String, // UID of source image
    beamName: String, // name of beam in plan
    leafIndex: Int, // leaf number starting at 1
    leafPositionIndex: Int, // leaf position number as it moves across the field
    offset_mm: Double, // difference from expected location: measuredEndPosition_mm - expectedEndPosition_mm
    status: String, // termination status
    measuredEndPosition_mm: Double, // measured position of leaf end
    expectedEndPosition_mm: Double, // expected position of leaf end
    measuredMinorSide_mm: Double, // measured position of top side of leaf, or left side if collimator is vertical
    measuredMajorSide_mm: Double // measured position of bottom side of leaf, or right side if collimator is vertical
) {

  def insert: LeafPosition = {
    val insertQuery = LeafPosition.query returning LeafPosition.query.map(_.leafPositionPK) into ((leafPosition, leafPositionPK) => leafPosition.copy(leafPositionPK = Some(leafPositionPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(LeafPosition.query.insertOrUpdate(this))

  override def toString: String =
    "Beam: " + beamName +
      "  Leaf Index: " + leafIndex.formatted("%2d") +
      "  Leaf Position Index: " + leafPositionIndex.formatted("%2d") +
      "  offset_mm: " + offset_mm.formatted("%9.5f") +
      "  expectedEndPosition_mm: " + expectedEndPosition_mm.formatted("%6.2f") +
      "  measuredEndPosition_mm: " + measuredEndPosition_mm.formatted("%10.5f")

  def pass: Boolean = status.equalsIgnoreCase(ProcedureStatus.pass.toString)
}

object LeafPosition extends ProcedureOutput with Logging {
  class LeafPositionTable(tag: Tag) extends Table[LeafPosition](tag, "leafPosition") {

    def leafPositionPK = column[Long]("leafPositionPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def beamName = column[String]("beamName")
    def leafIndex = column[Int]("leafIndex")
    def leafPositionIndex = column[Int]("leafPositionIndex")
    def offset_mm = column[Double]("offset_mm")
    def status = column[String]("status")
    def measuredEndPosition_mm = column[Double]("measuredEndPosition_mm")
    def expectedEndPosition_mm = column[Double]("expectedEndPosition_mm")
    def measuredMinorSide_mm = column[Double]("measuredLowSide_mm")
    def measuredMajorSide_mm = column[Double]("measuredHighSide_mm")

    def * =
      (
        leafPositionPK.?,
        outputPK,
        SOPInstanceUID,
        beamName,
        leafIndex,
        leafPositionIndex,
        offset_mm,
        status,
        measuredEndPosition_mm,
        expectedEndPosition_mm,
        measuredMinorSide_mm,
        measuredMajorSide_mm
      ) <> (LeafPosition.apply _ tupled, LeafPosition.unapply)

    def outputFK = foreignKey("LeafPosition_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[LeafPositionTable]

  override val topXmlLabel = "LeafPosition"

  def get(leafPositionPK: Long): Option[LeafPosition] = {
    val action = for {
      inst <- LeafPosition.query if inst.leafPositionPK === leafPositionPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all LeafPosition for the given output
    */
  def getByOutput(outputPK: Long): Seq[LeafPosition] = {
    val action = for {
      inst <- LeafPosition.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list
  }

  def delete(leafPositionPK: Long): Int = {
    val q = query.filter(_.leafPositionPK === leafPositionPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[LeafPosition] = {
    throw new RuntimeException("Constructing from Elem not supported: " + outputPK + " : " + elem)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    val toInsert = xmlToList(elem, outputPK)
    insertSeq(toInsert)
    toInsert.size
  }

  def insertSeq(list: Seq[LeafPosition]): Unit = {
    val ops = list.map { loc => LeafPosition.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }

  /**
    * Container for a group of LeafPositions associated with a single DICOM image.
    *
    * All data will have the same SOPInstanceUID.
    *
    * @param output Output with which the data is associated.
    * @param leafPosSeq List of leaf positions.
    */
  case class LeafPosHistory(output: Output, leafPosSeq: Seq[LeafPosition]) {
    // Used for sorting instances of this class
    val ordering: String = output.dataDate.get.getTime + "  " + leafPosSeq.head.beamName

    // Facilitate the quick finding a result given leafPositionIndex and leafIndex.
    private val leafPosMap = leafPosSeq.map(lp => ((lp.leafPositionIndex, lp.leafIndex), lp)).toMap

    /**
      * Get the entry corresponding to the leaf's position and index.
      * @param leafPositionIndex Index of horizontal position of leaf.  Currently a number from 1 to 10.  Note: NOT zero relative.
      * @param leafIndex Index of leaf.  Will be either 1 to 36 or 1 to 52 depending on the collimator.  Note: NOT zero relative.
      * @return A leaf position entry.
      */
    def get(leafPositionIndex: Int, leafIndex: Int): Option[LeafPosition] = leafPosMap.get((leafPositionIndex, leafIndex))
  }

  /**
    * Get the entire history of leaf position data for the given machine.
    * @param machinePK Machine to get data for.
    * @return List of history items sorted by data date and then beam name.  The leafPosSeq is not sorted.
    */
  def history(machinePK: Long, procedurePK: Long): Seq[LeafPosHistory] = {

    logger.info("LeafPosition history starting for machine: " + machinePK + " : " + Machine.get(machinePK).get.id)
    val outputList = {
      val search = for {
        output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      } yield output
      val result = Db.run(search.result)
      result
    }

    val outputMap = outputList.map(o => (o.outputPK.get, o)).toMap

    def getLeafPositionsForGroup(outputPKList: Seq[Long]): Seq[LeafPosition] = {
      val outputPKSet = outputPKList.toSet
      val search = for {
        leafPos <- LeafPosition.query.filter(w => w.outputPK.inSet(outputPKSet))
      } yield leafPos
      val list = Db.run(search.result)
      list
    }

    // output PKs put into groups sized small enough to efficiently retrieve values from the database
    val outputGroupList = edu.umro.ScalaUtil.Util.sizedGroups(outputMap.keys.toSeq, 10)

    // all leaf positions for this machine
    val leafPositionList = outputGroupList.flatMap(getLeafPositionsForGroup)

    // @formatter:off
    val sorted =
      leafPositionList
        .groupBy(_.SOPInstanceUID)
        .map(uidLp => LeafPosHistory(outputMap(uidLp._2.head.outputPK), uidLp._2))
        .toSeq
        .sortBy(_.ordering)
    // @formatter:on

    sorted
  }

}
