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

class SPWedge(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Wedge"

  override val abbreviation: String = "Wedge"
  private def wedgeSelectionList: Seq[Selection] = {
    def findPair(wedge: Config.WedgeBeam): Option[(Beam, Beam)] = {
      val fg = beamList.find(_.beamName.equals(wedge.wedge))
      val bg = beamList.find(b => wedge.backgroundList.contains(b.beamName))

      if (fg.isDefined && bg.isDefined)
        Some((fg.get, bg.get))
      else
        None
    }

    Config.WedgeBeamList.flatMap(findPair).map(pair => Selection(this, pair._1.beamName, Seq(pair._1, pair._2)))

  }

  override def initialSelectionList: Seq[Selection] = wedgeSelectionList

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

}
