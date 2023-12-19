package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.Logging
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.Util

import scala.xml.Elem

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
    * Given a machine, return the list of all checkboxes.
    * @return List of all selections.
    */
  def selectionList: Seq[Selection]

  /**
    * Given a list of beam names, return the HTML that reflects them.
    * @param beamList List of beam names
    * @return HTML snippet.
    */
  def setBeamList(beamList: Seq[Beam]): Elem

  /**
    * Given a list of checkboxes that are checked, return the list of beam names to put in the plan.
    * @param checkboxIdList List of HTML IDs of checkboxes that are checked.
    * @return List of beams.
    */
  def update(checkboxIdList: Seq[String]): Seq[Beam]

  /**
    * Given the beams from the original plans, return a list of beams that the user can choose from.
    *
    * @return List of beams applicable to this sub-procedure.
    */
  def getBeamList: Seq[Beam]

  /**
    * Generate the list of beams to be put in the plan.
    * @param checkboxIdList List of HTML IDs for checkboxes that are checked.
    * @return The list of beams to be put in the plan.
    */
  def generatePlan(checkboxIdList: Seq[String]): Seq[AttributeList]

  /**
    * Determine if this sub-procedure is selected by the user and uses the given beam.
    * @param beam Test this beam.
    * @param valueMap User selections.
    * @return
    */
  final def usesBeam(beam: Beam, valueMap: ValueMapT): Boolean = {
    selectionList.filter(sel => sel.isSelected(valueMap)).flatMap(_.beamList).exists(b => b.beamName.equals(beam.beamName))
  }

  /**
    * Identifier in the HTML for the sub procedure's header.
    * @return HTML id.
    */
  final def headerId: String = Util.textToHtmlId("header_" + name)
}
