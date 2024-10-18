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

import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.Config
import org.aqa.Util

/**
  * Abstraction layer that defines common interface for sub-procedures.
  */
abstract class SubProcedure(val metaData: SPMetaData, beamList: Seq[Beam]) extends Logging {

  /**
    * Name of this sub procedure.
    */
  val name: String

  /**
    * Short name of sub procedure.
    */
  val abbreviation: String

  /**
    * Return the list of all checkboxes.
    * @return List of all selections.
    */
  protected def initialSelectionList: Seq[Selection]

  /**
    * True if this procedure uses collimator centering.
    */
  val usesCollimatorCentering: Boolean = true

  /**
    * True if this procedure uses the flood field.
    */
  val usesFlood: Boolean = false

  private val colCenterBeamList = {
    beamList.filter(beam => Config.collimatorCenteringPhase3List.exists(_.equals(beam.beamName)))
  }

  final def selectionList: Seq[Selection] = {
    def allBeamsSupported(sel: Selection): Boolean =
      sel.beamList.map(beam => metaData.beamEnergyIsSupported(beam.beamEnergy)).reduce(_ && _)

    /** If the sub-process requires collimator centering, then require the collimator centering beams that have the same
      * gantry angle. Do this by adding the appropriate collimator centering beams to the selection's beam list.
      */
    def requireColCent(selection: Selection): Selection = {
      val gaSet = selection.beamList.filter(_.gantryAngleList_deg.distinct.size == 1).map(_.gantryAngle_roundedDeg).distinct.toSet

      val gantryAngleSet: Set[Int] = {
        if (gaSet.nonEmpty)
          gaSet
        else {

          def calcMeanAngle(list: Seq[Double]): Double = {

            val radList = list.map(angle => Math.toRadians(Util.modulo360(angle)))

            val meanSin = radList.map(Math.sin).sum / list.size
            val meanCos = radList.map(Math.cos).sum / list.size

            val angle = Math.atan2(meanSin, meanCos)

            angle
          }

          def meanGantryAngleOfBeam(beam: Beam): Double = {
            val list = DicomUtil.findAllSingle(beam.prototypeBeam, TagByName.GantryAngle).flatMap(_.getDoubleValues)
            calcMeanAngle(list)
          }

          val angleSet = selection.beamList.map(meanGantryAngleOfBeam).map(Util.angleRoundedTo90).toSet
          angleSet
        }
      }

      val colCentBeamList = colCenterBeamList.filter(colCentBeam => gantryAngleSet.contains(colCentBeam.gantryAngle_roundedDeg))

      val newBeamList = (selection.beamList ++ colCentBeamList).groupBy(_.beamName).values.map(_.head).toSeq

      val newSel = selection.copy(beamList = newBeamList)
      newSel
    }

    def requireFlood(selection: Selection): Selection = {
      beamList.find(_.beamName.toLowerCase.contains("flood")) match {
        case Some(floodBeam) =>
          val newBeamList = selection.beamList :+ floodBeam
          selection.copy(beamList = newBeamList)
        case _ =>
          selection
      }
    }

    val list1 = initialSelectionList.filter(allBeamsSupported)

    val list2 =
      if (usesFlood)
        list1.map(requireFlood)
      else
        list1

    val list3 =
      if (usesCollimatorCentering)
        list2.map(requireColCent)
      else
        list2

    // remove selections that have the same exact list of beams (order does not matter).
    def beamsAsText(sel: Selection) = sel.beamList.map(_.beamName).sorted.mkString(" @@@@ ")
    val list4 = list3.groupBy(beamsAsText).map(_._2.head).toSeq

    def sortSignature(sel: Selection): String = {
      val energy = sel.beamList.head.beamEnergy

      val energyText = "%05.1f".format(energy.photonEnergy_MeV.get)
      val fffText = if (energy.isFFF) "1" else "0"
      val gantryText = "%3d".format(sel.beamList.head.gantryAngle_roundedDeg)

      Seq(fffText, energyText, gantryText).mkString(" | ")
    }

    list4.sortBy(sortSignature)
  }

  /**
    * Given the beams from the original plans, return a list of beams that the user can choose from.
    *
    * @return List of beams applicable to this sub-procedure.
    */
  def getBeamList: Seq[Beam]

  /**
    * Determine if this sub-procedure is selected by the user and uses the given beam.
    * @param beam Test this beam.
    * @param valueMap User selections.
    * @return
    */
  final def usesBeam(beam: Beam, valueMap: ValueMapT): Boolean = {
    initialSelectionList.filter(sel => sel.isSelected(valueMap)).flatMap(_.beamList).exists(b => b.beamName.equals(beam.beamName))
  }

  /**
    * Identifier in the HTML for the sub procedure's header.
    * @return HTML id.
    */
  final def headerId: String = Util.textToHtmlId("header_" + name)

  /**
    * Get the list of all beams used by this subProcedure.
    * @return The list of all beams used by this sub-procedure.
    */
  final def distinctBeamList: Seq[Beam] = selectionList.flatMap(_.beamList).groupBy(_.beamName).values.map(_.head).toSeq

  /**
    * Get the sets of beams that must be delivered consecutively.  Most sub procedures do not
    * restrict the beam ordering, so by default they are each put in their own list.
    * @return Sets of beams.
    */
  def consecutivelyDeliveredBeamSets: Seq[Seq[Beam]] = distinctBeamList.map(beam => Seq(beam))

  override def toString: String = {
    s"$name   ColCenter: $usesCollimatorCentering    flood: $usesFlood    Beams: ${getBeamList.map(_.beamName).mkString("  ")}"
  }

  /**
    * Determine if this subProcedure will be run.
    * @param checkedBeamList List of beams selected by user.
    * @return True if active.
    */
  def subIsSelected(checkedBeamList: Seq[Beam]): Boolean = {
    def subSubIsSelected(subSubProcedure: Selection): Boolean = subSubProcedure.beamList.map(beam => checkedBeamList.exists(b => b.beamName.equals(beam.beamName))).reduce(_ && _)

    selectionList.map(subSubIsSelected).reduce(_ || _)
  }

}
