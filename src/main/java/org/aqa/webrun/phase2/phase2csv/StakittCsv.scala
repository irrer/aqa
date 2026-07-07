/*
 * Copyright 2026 Regents of the University of Michigan
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

import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.Stakitt

class StakittCsv(metadataCache: MetadataCache) extends Phase2Csv[Stakitt.LeafPosHistoryGap](metadataCache: MetadataCache) {

  // abbreviation for the long name
  private type SH = Stakitt.LeafPosHistoryGap

  override val dataName: String = "Stakitt"

  private def opt(d: Option[Double]): String = if (d.isDefined) d.get.toString else "NA"

  override protected def makeColList: Seq[CsvCol[SH]] = {
    Seq(
      // @formatter:off
      CsvCol("Beam Name"                  , "Beam name from RTPLAN."                                          , (sh: SH) => sh.x1Stakitt.beamName),
      CsvCol("Leaf Number"                , "Collimator leaf number, starting at 1."                          , (sh: SH) => sh.x1Stakitt.leafIndex),
      //
      CsvCol("Gap Offset"                 , "Measured Gap Length - Planned Gap Length in mm."                 , (sh: SH) => sh.gapOffset_mm),
      CsvCol("Measured Gap"               , "Measured Gap Length in mm."                                      , (sh: SH) => sh.measuredGap),
      CsvCol("Planned Gap"                , "Planned Gap Length in mm."                                       , (sh: SH) => sh.plannedGap),
      //
      CsvCol("X1 Leaf Pos Number"         , "X1 Horizontal leaf position number, 1 relative"                  , (sh: SH) => sh.x1Stakitt.leafPositionIndex),
      CsvCol("X1 Leaf End Offset"         , "X1 Measured leaf end - planned leaf end, in mm."                 , (sh: SH) => sh.x1Stakitt.leafEndOffset_mm),
      CsvCol("X1 Measured Leaf End"       , "X1 Measured position of leaf end in mm."                         , (sh: SH) => sh.x1Stakitt.measuredEndPosition_mm),
      CsvCol("X1 Planned Leaf End"        , "X1 Planned position of leaf end in mm."                          , (sh: SH) => sh.x1Stakitt.plannedEndPosition_mm),
      CsvCol("X1 Measured Upper Side"     , "X1 Measured position of the upper (Y1) side of the leaf in mm."  , (sh: SH) => sh.x1Stakitt.measuredMinorSide_mm),
      CsvCol("X1 Measured Lower Side"     , "X1 Measured position of the lower (Y2) side of the leaf in mm."  , (sh: SH) => sh.x1Stakitt.measuredMajorSide_mm),
      CsvCol("X1 Planned Upper Side"      , "X1 Planned position of the upper (Y1) side of the leaf in mm."   , (sh: SH) => sh.x1Stakitt.plannedMinorSide_mm),
      CsvCol("X1 Planned Lower Side"      , "X1 Planned position of the lower (Y2) side of the leaf in mm."   , (sh: SH) => sh.x1Stakitt.plannedMajorSide_mm),
      CsvCol("X1 Upper Leaf Side Offset"  , "X1 Measured upper (Y1) leaf side - planned leaf side, in mm."    , (sh: SH) => sh.x1Stakitt.minorSideOffset_mm),
      CsvCol("X1 Lower Leaf Side Offset"  , "X1 Measured lower (Y2) leaf side - planned leaf side, in mm."    , (sh: SH) => sh.x1Stakitt.majorSideOffset_mm),
      CsvCol("X1 Measured Leaf Width"     , "X1 Measured leaf width in mm."                                   , (sh: SH) => sh.x1Stakitt.measuredLeafWidth_mm),
      CsvCol("X1 Planned Leaf Width"      , "X1 Planned leaf width in mm."                                    , (sh: SH) => sh.x1Stakitt.plannedLeafWidth_mm),
      CsvCol( "X1 Leaf Width Offset"      , "X1 Measured Leaf Width - Planned leaf width in mm."              , (sh: SH) => sh.x1Stakitt.measuredLeafWidth_mm - sh.x1Stakitt.plannedLeafWidth_mm )                                    ,
      //
      CsvCol("X2 Leaf Pos Number"         , "X2 Horizontal leaf position number, 1 relative"                  , (sh: SH) => sh.x2Stakitt.leafPositionIndex),
      CsvCol("X2 Leaf End Offset"         , "X2 Measured leaf end - planned leaf end, in mm."                 , (sh: SH) => sh.x2Stakitt.leafEndOffset_mm),
      CsvCol("X2 Measured Leaf End"       , "X2 Measured position of leaf end in mm."                         , (sh: SH) => sh.x2Stakitt.measuredEndPosition_mm),
      CsvCol("X2 Planned Leaf End"        , "X2 Planned position of leaf end in mm."                          , (sh: SH) => sh.x2Stakitt.plannedEndPosition_mm),
      CsvCol("X2 Measured Upper Side"     , "X2 Measured position of the upper (Y1) side of the leaf in mm."  , (sh: SH) => sh.x2Stakitt.measuredMinorSide_mm),
      CsvCol("X2 Measured Lower Side"     , "X2 Measured position of the lower (Y2) side of the leaf in mm."  , (sh: SH) => sh.x2Stakitt.measuredMajorSide_mm),
      CsvCol("X2 Planned Upper Side"      , "X2 Planned position of the upper (Y1) side of the leaf in mm."   , (sh: SH) => sh.x2Stakitt.plannedMinorSide_mm),
      CsvCol("X2 Planned Lower Side"      , "X2 Planned position of the lower (Y2) side of the leaf in mm."   , (sh: SH) => sh.x2Stakitt.plannedMajorSide_mm),
      CsvCol("X2 Upper Leaf Side Offset"  , "X2 Measured upper (Y1) leaf side - planned leaf side, in mm."    , (sh: SH) => sh.x2Stakitt.minorSideOffset_mm),
      CsvCol("X2 Lower Leaf Side Offset"  , "X2 Measured lower (Y2) leaf side - planned leaf side, in mm."    , (sh: SH) => sh.x2Stakitt.majorSideOffset_mm),
      CsvCol("X2 Measured Leaf Width"     , "X2 Measured leaf width in mm."                                   , (sh: SH) => sh.x2Stakitt.measuredLeafWidth_mm),
      CsvCol("X2 Planned Leaf Width"      , "X2 Planned leaf width in mm."                                    , (sh: SH) => sh.x2Stakitt.plannedLeafWidth_mm),
      CsvCol( "X2 Leaf Width Offset"      , "X2 Measured Leaf Width - Planned leaf width in mm."              , (sh: SH) => sh.x2Stakitt.measuredLeafWidth_mm - sh.x2Stakitt.plannedLeafWidth_mm
      )
      // @formatter:on
    )
  }

  /**
   * Get the data for a particular machine.
   *
   * @param machinePK Machine to get data for.
   * @return List of data for the particular machine.
   */
  override protected def getData(metadataCache: MetadataCache, machinePK: Long): Seq[SH] = {
    val procedureList = Seq(Procedure.ProcOfPhase2, Procedure.ProcOfPhase3, Procedure.ProcOfStakitt).flatten

    val leafGapHistoryList = procedureList.flatMap(p => Stakitt.history(machinePK, p.procedurePK.get))

    val gapList = leafGapHistoryList.flatMap(Stakitt.leafPosHistoryToLeafPosGapList)

    gapList
  }

  override def getOutput(data: SH): Output = data.leafPosHistory.output

  override protected def getSopUidList(data: SH): Seq[String] = Seq(data.leafPosHistory.leafPosSeq.head.SOPInstanceUID).distinct

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("")

}
