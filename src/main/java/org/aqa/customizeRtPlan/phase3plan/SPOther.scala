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

/**
  * Allow user to select beams that are in the RTPLAN but not assigned to a procedure.  This
  * allows the user to add beams for the user's own purposes.
  * @param metaData Machine metadata
  * @param unclaimedBeamList List of beams that no other test uses.
  */
class SPOther(metaData: SPMetaData, unclaimedBeamList: Seq[Beam]) extends SubProcedure(metaData, unclaimedBeamList: Seq[Beam]) {

  override val name = "Other"

  override val abbreviation: String = "Other"
  private def otherSelectionList: Seq[Selection] = {

    def toSelection(beam: Beam): Selection = {
      Selection(this, beam.beamName, Seq(beam))
    }

    unclaimedBeamList.map(toSelection)

  }

  override def initialSelectionList: Seq[Selection] = otherSelectionList

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

}
