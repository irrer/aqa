package org.aqa.customizeRtPlan.phase3plan

import org.aqa.web.WebUtil.FormButton

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

class Phase3TemplateDefault extends Phase3Template {

  private val functionName = "TemplateDefault"

  /** The standard photon energy that all machines are capable of delivering. */
  private val STANDARD_ENERGY = 6

  override def button: FormButton = makeButton("Default", "Select default tests.", s"$functionName()")

  private def hasStandardEnergy(selection: Selection): Boolean = selection.beamList.exists(_.beamEnergy.photonEnergy_MeV.get == STANDARD_ENERGY)

  /** Return true if the selection has at least one FFF beam. */
  private def hasFff(sel: Selection): Boolean = sel.beamList.exists(_.isFFF)

  private def energy(selection: Selection): Double = selection.beamList.head.beamEnergy.photonEnergy_MeV.get

  private def gantry(selection: Selection): Int = selection.beamList.head.gantryAngle_roundedDeg

  private def energySignature(selection: Selection): String = s"${energy(selection)} ${hasFff(selection)}"

  private def minGantry(list: Seq[Selection]): Seq[Selection] = list.sortBy(gantry).take(1)

  override def js(subProcedureList: SubProcedureList): String = {

    def ofType(c: Class[_]) = subProcedureList.subProcedureList.find(sub => sub.getClass.equals(c)).get.selectionList


    // @formatter:off
    val list: Seq[Selection] =
      ofType(classOf[SPCollimatorCentering])                                                                       ++
      ofType(classOf[SPFocalSpot          ]).filter(hasStandardEnergy).filterNot(hasFff).sortBy(gantry).take(1)    ++
      ofType(classOf[SPFocalSpot          ]).filter(hasStandardEnergy).filter(hasFff).sortBy(gantry).take(1)       ++
      ofType(classOf[SPLeafPosition       ]).filter(hasStandardEnergy).filterNot(hasFff).sortBy(gantry).take(1)    ++
      ofType(classOf[SPLeafPosition       ]).filter(hasFff).sortBy(energy).takeRight(1)                            ++
      ofType(classOf[SPVmat               ])                                                                       ++
      ofType(classOf[SPSymFlatConst       ]).groupBy(energySignature).values.flatMap(minGantry)    // one of each photon energy and FFF type. Prefer gantry angle of 0.

    // @formatter:on

    val htmlIdList = list.map(_.htmlId)

    val jsClaus = htmlIdList.map(id => s"""id.localeCompare("$id")""").mkString(" * ")

    s"""
       | function $functionName() {
       |   for (cb = 0; cb < checkboxList.length; cb++) {
       |     var id = checkboxList[cb].getAttribute("id");
       |     var checkIt = ($jsClaus) == 0;
       |     checkboxList[cb].checked = checkIt;
       |   }
       |   phase3ClickHandler(null);
       | }
       |""".stripMargin
  }
}
