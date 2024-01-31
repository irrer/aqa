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

class SPVmat(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "VMAT"

  override val abbreviation: String = "VMAT"
  private def vmatSelectionList: Seq[Selection] = {
    def findPair(vmat: Config.VMATBeamPair): Option[(Beam, Beam)] = {
      val mlc = beamList.find(_.beamName.equals(vmat.MLC))
      val open = beamList.find(b => vmat.OPEN.contains(b.beamName))

      if (mlc.isDefined && open.isDefined)
        Some((mlc.get, open.get))
      else
        None
    }

    Config.VMATBeamPairList.flatMap(findPair).map(pair => Selection(this, pair._1.beamName, Seq(pair._1, pair._2)))

  }

  override def initialSelectionList: Seq[Selection] = vmatSelectionList

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

}
