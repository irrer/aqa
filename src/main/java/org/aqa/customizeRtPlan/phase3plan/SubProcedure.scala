package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Logging
import org.aqa.web.WebUtil.ValueMapT
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

  final def selectionList: Seq[Selection] = {
    def allBeamsSupported(sel: Selection): Boolean =
      sel.beamList.map(beam => metaData.beamEnergyIsSupported(beam.beamEnergy)).reduce(_ && _)

    initialSelectionList.filter(allBeamsSupported)
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
}
