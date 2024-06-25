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

package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.LeafPosition
import org.aqa.db.Output

/**
  * Generate a CSV for leaf position data.
  */
class LeafPositionCsv(metadataCache: MetadataCache) extends Phase2Csv[LeafPosition.LeafPosHistory](metadataCache: MetadataCache) {

  // abbreviation for the long name
  type LP = LeafPosition

  // abbreviation for the long name
  private type LPH = LeafPosition.LeafPosHistory

  /** Maximum number of leaves that any MLC has.  Machines that have an MLC with fewer leaves will show 'NA' for irrelevant values.*/
  private lazy val leafCount: Int = 52

  // Positions of leaves as specified by RTPLAN.
  private lazy val positionList_mm: Seq[Int] = Seq(-60, -45, -30, -15, 0, 15, 30, 45, 60, 75)

  override val dataName: String = "LeafPosition"

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(metadataCache: MetadataCache, machinePK: Long): Seq[LPH] = {
    val lpSeq = LeafPosition.history(machinePK, metadataCache.phase2ProcedurePK) ++ LeafPosition.history(machinePK, metadataCache.phase3ProcedurePK)
    lpSeq
  }

  override def getOutput(data: LPH): Output = data.output

  override def getSopUidList(data: LPH): Seq[String] = Seq(data.leafPosSeq.head.SOPInstanceUID)

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("")

  /**
    * Get a value with the matching indices.
    * @param lph Data set.
    * @param leafPositionIndex Index of leaf position.
    * @param leafIndex Index of leaf.
    * @param getValue Function to get value
    * @return String version of value.
    */
  def valueOf(lph: LPH, leafPositionIndex: Int, leafIndex: Int, getValue: LP => Double): String = {
    lph.get(leafPositionIndex, leafIndex) match {
      case Some(lp) => getValue(lp).toString
      case _        => "NA"
    }
  }

  private def colMeasuredEnd(leafPositionIndex: Int, leafIndex: Int): CsvCol[LPH] = {
    val header = "P" + positionList_mm(leafPositionIndex) + " L" + (leafIndex + 1) + " measured"
    val doc = "The measured position of the leaf end for " + " RTPLAN Position " + (leafPositionIndex + 1) + " with leaf number " + leafIndex + "."
    val function = (lph: LPH) => valueOf(lph, leafPositionIndex + 1, leafIndex + 1, _.measuredEndPosition_mm)
    CsvCol(header, doc, function)
  }

  private def colExpectedEndPos(leafPositionIndex: Int): CsvCol[LPH] = {
    val header = "P" + positionList_mm(leafPositionIndex) + " expected"
    val doc = "The expected position of the end of leaf at position " + positionList_mm(leafPositionIndex) + " mm when compensated with collimator centering."
    val function = (lph: LPH) => valueOf(lph, leafPositionIndex + 1, leafIndex = 1, _.expectedEndPosition_mm)
    CsvCol(header, doc, function)
  }

  private def colExpectedLeafSide(leafIndex: Int): CsvCol[LPH] = {
    val header = "Side " + (leafIndex + 1) + " measured"
    val doc = "Measured vertical position of top side of leaf " + (leafIndex + 1) + " in mm."
    val function = (lph: LPH) => valueOf(lph, leafPositionIndex = 1, leafIndex + 1, _.measuredMinorSide_mm)
    CsvCol(header, doc, function)
  }

  private def beamCol(): CsvCol[LPH] = {
    CsvCol("Beam Name", "Name of beam", _.leafPosSeq.head.beamName)
  }

  override protected def makeColList: Seq[CsvCol[LPH]] = {
    Seq(beamCol()) ++
      positionList_mm.indices.flatMap(leafPosIndex => (0 until leafCount).map(leafIndex => colMeasuredEnd(leafPosIndex, leafIndex))) ++
      positionList_mm.indices.map(colExpectedEndPos) ++
      (0 until leafCount).map(colExpectedLeafSide)
  }

}
