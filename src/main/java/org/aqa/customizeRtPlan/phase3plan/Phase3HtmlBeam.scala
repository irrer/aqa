package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Logging
import org.aqa.web.WebUtil._

import scala.xml.Elem

/**
 * Support construction of the beam html.
 */
object Phase3HtmlBeam extends Logging {


  /**
   * Generate the HTML id for the given sub procedure's use of the given beam.
   *
   * @param subProc Sub-procedure.
   * @param beam    Beam.
   * @return HTML id.
   */
  def beamProcId(subProc: SubProcedure, beam: Beam) = s"${subProc.name} :: ${beam.beamName}"

  /* Return true if the user has made any selections of the given sub procedure that use the given beam. */
  /*
  private def subProcIsUsingBeam(subProc: SubProcedure, beam: Beam, checkedSelectionList: Seq[Selection]): Boolean = {
    subProc.selectionList.exists(sel => checkedSelectionList.exists(s => s.htmlId.equals(sel.htmlId)))
  }
  */

  /* Return the HTML attribute style for the given beam's use by a sub-procedure. */
  def subProcBeamStyle(checked: Boolean): String = {
    val color =
      if (checked)
      //noinspection SpellCheckingInspection
        "lightgreen"
      else
        "white"
    val style = s"border-left: 16px solid $color; margin:5px;"
    style
  }


  /** Return an indicator as to whether the sub procedure uses the beam. */
  private def subProcUseOfBeam(subProc: SubProcedure, beam: Beam): Elem = {
    val id = s"${subProc.name} :: ${beam.beamName}"
    <span id={id} style={subProcBeamStyle(false)} title={subProc.name}>
      <span style="margin:6px;">
        {subProc.name}
      </span>
    </span>
  }

  private def beamToHtml(beam: Beam, valueMap: ValueMapT, subProcedureList: SubProcedureList): Elem = {

    val beamSetUse = subProcedureList.subProcedureList.map(sub => subProcUseOfBeam(sub, beam))

    val subProcedureUses: Elem = {
      val usedBy = <td style="margin: 5px;">
        {beamSetUse.map(elem =>
          <div>
            {elem}
          </div>)}
      </td>
      usedBy
    }

    val image: Elem = {
      val imageUrl = subProcedureList.metaData.urlOfExampleImage(beam)
      <div style="margin: 5px;">
        <h4>
          {beam.beamName}
        </h4> <br/>
        <img src={imageUrl} height="100" style="margin: 2px;"/>
      </div>
    }

    <div style="border:1px solid lightgrey;margin: 2px;">
      <table>
        <tr>
          <td>
            {image}
          </td>
          <td>
            {subProcedureUses}
          </td>
        </tr>
      </table>
    </div>
  }

  def selectedBeamsField(subProcedureList: SubProcedureList, selectedBeamCountTag: String): WebPlainText = {
    // @formatter:off
    def toBeamHtml(valueMap :ValueMapT): Elem = {
      <div>
        <div class="row">
          <div class="col-md-2">
            <h3>Beam List</h3>
          </div>
          <div class="col-md-2">
            <h3>Selected: <span id={selectedBeamCountTag}>0</span></h3>
          </div>
        </div>
        <div class="row">
          {subProcedureList.beamList.map(beam => {<div class="col-md-3"> {beamToHtml(beam, valueMap, subProcedureList)} </div>})}
        </div>
      </div>
    }
    // @formatter:on

    new WebPlainText(
      label = "Selected Beams", showLabel = false,
      col = 0, offset = 0,
      toBeamHtml)
  }

}
