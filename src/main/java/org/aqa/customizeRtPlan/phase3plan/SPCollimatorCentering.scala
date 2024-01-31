package org.aqa.customizeRtPlan.phase3plan

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

import org.aqa.Config

class SPCollimatorCentering(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Collimator Centering"

  //noinspection SpellCheckingInspection
  override val abbreviation: String = "Col Cntr"

  override def initialSelectionList: Seq[Selection] = {

    val list = beamList.filter(beam => Config.collimatorCenteringPhase3List.contains(beam.beamName))

    if (list.size != 8) throw new RuntimeException(s"Expected to find 8 collimator centering beams in the RTPLAN, but actually found ${list.size}")

    def gantry(angle: Int): Selection = {
      val beamList = list.filter(_.gantryAngle_roundedDeg == angle).sortBy(_.colAngle_roundedDeg)
      Selection(this, s"Gantry $angle", beamList)
    }

    Seq(0, 90, 180, 270).map(gantry)
  }

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

  override def consecutivelyDeliveredBeamSets: Seq[Seq[Beam]] = selectionList.map(_.beamList)

}
