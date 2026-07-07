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
import org.aqa.Util

import scala.xml.Elem

case class Stakitt(
                    stakittPK: Option[Long], // primary key
                    outputPK: Long, // output primary key
                    SOPInstanceUID: String, // UID of source image
                    beamName: String, // name of beam in plan
                    leafIndex: Int, // leaf number starting at 1
                    leafPositionIndex: Int, // leaf position number as it moves across the field
                    measuredEndPosition_mm: Double, // measured position of leaf end
                    measuredMinorSide_mm: Double, // measured position of top side of leaf, or left side if collimator is vertical
                    measuredMajorSide_mm: Double, // measured position of bottom side of leaf, or right side if collimator is vertical
                    plannedEndPosition_mm: Double, // planned position of leaf end
                    plannedMinorSide_mm: Double, // measured position of top side of leaf, or left side if collimator is vertical
                    plannedMajorSide_mm: Double // measured position of bottom side of leaf, or right side if collimator is vertical
                  ) extends Logging {

  def insert: Stakitt = {
    //  val insertQuery = LeafPosition.query returning LeafPosition.query.map(_.leafPositionPK) into ((leafPosition, leafPositionPK) => leafPosition.copy(leafPositionPK = Some(leafPositionPK)))
    val insertQuery = Stakitt.query returning Stakitt.query.map(_.stakittPK) into ((stakitt, stakittPK) => stakitt.copy(stakittPK = Some(stakittPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  /** Positioning error of leaf end: planned - measured */
  //noinspection ScalaWeakerAccess
  val leafEndOffset_mm: Double = measuredEndPosition_mm - plannedEndPosition_mm

  //noinspection ScalaWeakerAccess
  val minorSideOffset_mm: Double = measuredMinorSide_mm - plannedMinorSide_mm

  //noinspection ScalaWeakerAccess
  val majorSideOffset_mm: Double = measuredMajorSide_mm - plannedMajorSide_mm

  //noinspection ScalaWeakerAccess
  val measuredLeafWidth_mm: Double = measuredMajorSide_mm - measuredMinorSide_mm

  //noinspection ScalaWeakerAccess
  val plannedLeafWidth_mm: Double = plannedMajorSide_mm - plannedMinorSide_mm

  //noinspection ScalaWeakerAccess
  val leafWidthOffset_mm: Double = measuredLeafWidth_mm - plannedLeafWidth_mm

  /** Index of the leaf that, paired with this leaf, forms a gap. */
  val gapPartnerLeafPositionIndex: Int = {
    if ((leafPositionIndex % 2) == 1)
      leafPositionIndex + 1
    else
      leafPositionIndex - 1
  }

  def insertOrUpdate(): Int = Db.run(Stakitt.query.insertOrUpdate(this))

  override def toString: String =
    "Beam: " + beamName +
      "  Leaf Index: " + leafIndex.formatted("%2d") +
      "  Leaf Position Index: " + leafPositionIndex.formatted("%2d") +
      "  leaf end offset_mm: " + Util.fmtDbl(leafEndOffset_mm) +
      "  minor side offset_mm: " + Util.fmtDbl(minorSideOffset_mm) +
      "  major side offset_mm: " + Util.fmtDbl(majorSideOffset_mm) +
      "  measuredEndPosition_mm: " + Util.fmtDbl(measuredEndPosition_mm) +
      "  measuredMinorSide_mm: " + Util.fmtDbl(measuredMinorSide_mm) +
      "  measuredMajorSide_mm: " + Util.fmtDbl(measuredMajorSide_mm) +
      "  plannedEndPosition_mm: " + Util.fmtDbl(plannedEndPosition_mm) +
      "  plannedMinorSide_mm: " + Util.fmtDbl(plannedMinorSide_mm) +
      "  plannedMajorSide_mm: " + Util.fmtDbl(plannedMajorSide_mm)
}

object Stakitt extends Logging {
  class StakittTable(tag: Tag) extends Table[Stakitt](tag, "stakitt") {

    def stakittPK = column[Long]("stakittPK", O.PrimaryKey, O.AutoInc)

    def outputPK = column[Long]("outputPK")

    def SOPInstanceUID = column[String]("SOPInstanceUID")

    def beamName = column[String]("beamName")

    def leafIndex = column[Int]("leafIndex")

    def leafPositionIndex = column[Int]("leafPositionIndex")

    def measuredEndPosition_mm = column[Double]("measuredEndPosition_mm")

    def measuredMinorSide_mm = column[Double]("measuredLowSide_mm")

    def measuredMajorSide_mm = column[Double]("measuredHighSide_mm")

    def plannedEndPosition_mm = column[Double]("plannedEndPosition_mm")

    def plannedMinorSide_mm = column[Double]("plannedMinorSide_mm")

    def plannedMajorSide_mm = column[Double]("plannedMajorSide_mmk")

    def * =
      (
        stakittPK.?,
        outputPK,
        SOPInstanceUID,
        beamName,
        leafIndex,
        leafPositionIndex,
        measuredEndPosition_mm,
        measuredMinorSide_mm,
        measuredMajorSide_mm,
        plannedEndPosition_mm,
        plannedMinorSide_mm,
        plannedMajorSide_mm
      ) <> (Stakitt.apply _ tupled, Stakitt.unapply)

    def outputFK = foreignKey("Stakitt_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[StakittTable]

  def get(stakittPK: Long): Option[Stakitt] = {
    val action = for {
      inst <- Stakitt.query if inst.stakittPK === stakittPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
   * Get a list of all Stakitt for the given output
   */
  def getByOutput(outputPK: Long): Seq[Stakitt] = {
    val action = for {
      inst <- Stakitt.query if inst.outputPK === outputPK
    } yield inst
    val list = Db.run(action.result)
    list
  }

  def delete(stakittPK: Long): Int = {
    val q = query.filter(_.stakittPK === stakittPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def xmlToList(elem: Elem, outputPK: Long): Seq[Stakitt] = {
    throw new RuntimeException("Constructing from Elem not supported: " + outputPK + " : " + elem)
  }

  def insertSeq(list: Seq[Stakitt]): Unit = {
    list.foreach(_.insertOrUpdate())
  }

  /**
   * Container for a group of Stakitts associated with a single DICOM image.
   *
   * All data will have the same SOPInstanceUID.
   *
   * @param output     Output with which the data is associated.
   * @param leafPosSeq List of leaf positions.
   */
  case class LeafPosHistory(output: Output, leafPosSeq: Seq[Stakitt]) extends HasOutput {
    // Used for sorting instances of this class
    val ordering: String = output.dataDate.get.getTime + "  " + leafPosSeq.head.beamName

    // Facilitate the quick finding a result given leafPositionIndex and leafIndex.
    private val leafPosMap: Map[(Int, Int), Stakitt] = leafPosSeq.map(lp => ((lp.leafPositionIndex, lp.leafIndex) -> lp)).toMap

    /** Fast leaf lookup by stakittPK */
    // val pkMap: Map[Long, Stakitt] = leafPosSeq.map(l => l.stakittPK.get -> l).toMap

    /**
     * Get the gap partner for this leaf end.
     *
     * @param stakitt This leaf end is looking for the partner.
     * @return Gap partner.
     */
    def gapPartner(stakitt: Stakitt): Stakitt = {
      val key: (Int, Int) = (stakitt.gapPartnerLeafPositionIndex, stakitt.leafIndex)
      val s = leafPosMap(key)
      s
    }

    /**
     * Get the entry corresponding to the leaf's position and index.
     *
     * @param leafPositionIndex Index of horizontal position of leaf.  Currently, a number from 1 to 10.  Note: NOT zero relative.
     * @param leafIndex         Index of leaf.  Will be either 1 to 36 or 1 to 52 depending on the collimator.  Note: NOT zero relative.
     * @return A leaf position entry.
     */
    def get(leafPositionIndex: Int, leafIndex: Int): Option[Stakitt] = leafPosMap.get((leafPositionIndex, leafIndex))

    override def getOutput: Output = output
  }

  case class LeafPosHistoryGap(leafPosHistory: LeafPosHistory, x1Stakitt: Stakitt, x2Stakitt: Stakitt) extends HasOutput {
    override def getOutput: Output = leafPosHistory.output

    val measuredGap: Double = x2Stakitt.measuredEndPosition_mm - x1Stakitt.measuredEndPosition_mm
    val plannedGap: Double = x2Stakitt.plannedEndPosition_mm - x1Stakitt.plannedEndPosition_mm

    val gapOffset_mm: Double = measuredGap - plannedGap
  }

  def leafPosHistoryToLeafPosGapList(lph: LeafPosHistory): Seq[LeafPosHistoryGap] = {
    lph.leafPosSeq.filter(s => (s.leafPositionIndex % 2) == 1).map(x1 => LeafPosHistoryGap(lph, x1, lph.gapPartner(x1)))
  }

  /**
   * Get the entire history of leaf position data for the given machine.
   *
   * @param machinePK Machine to get data for.
   * @return List of history items sorted by data date and then beam name.  The leafPosSeq is not sorted.
   */
  def history(machinePK: Long, procedurePK: Long): Seq[LeafPosHistory] = {

    logger.info("Stakitt history starting for machine: " + machinePK + " : " + Machine.get(machinePK).get.id)
    val outputList = {
      val search = for {
        output <- Output.valid.filter(o => (o.machinePK === machinePK) && (o.procedurePK === procedurePK))
      } yield output
      val result = Db.run(search.result)
      result
    }

    val outputMap = outputList.map(o => (o.outputPK.get, o)).toMap

    def getStakittsForGroup(outputPKList: Seq[Long]): Seq[Stakitt] = {
      val outputPKSet = outputPKList.toSet
      val search = for {
        leafPos <- Stakitt.query.filter(w => w.outputPK.inSet(outputPKSet))
      } yield leafPos
      val list = Db.run(search.result)
      list
    }

    // output PKs put into groups sized small enough to efficiently retrieve values from the database
    val outputGroupList = edu.umro.ScalaUtil.Util.sizedGroups(outputMap.keys.toSeq, 10)

    // all leaf positions for this machine
    val leafPositionList = outputGroupList.flatMap(getStakittsForGroup)

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
