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

import org.aqa.Util

class SPCollimatorPosition(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Collimator Position"

  override val abbreviation: String = "Coll Posn"

  /** The beam must describe a rectangle at this this tall and wide. */
  private val minSize_mm = 10.0

  private def colPosSelectionList: Seq[Selection] = {
    val list = beamList.filter(beam => Util.minCenteredFieldBeam(beam.prototypeBeam, minSize_mm))
    list.map(beam => Selection(this, beam.beamName, Seq(beam)))
  }

  override def initialSelectionList: Seq[Selection] = colPosSelectionList

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

}
