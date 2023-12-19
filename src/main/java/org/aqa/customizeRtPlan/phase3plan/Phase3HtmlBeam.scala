package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Logging
import org.aqa.Util

import scala.xml.Elem

/**
 * Support construction of the beam html.
 */
object Phase3HtmlBeam extends Logging {


  /**
   * Generate the HTML id for the given beam.
   *
   * @param beam For this beam.
   * @return Used in HTML attribute id=...
   */
  def beamHtmlId(beam: Beam): String = "Beam_" + beam.beamName

  /**
   * Generate the HTML id for the given sub procedure's use of the given beam.
   *
   * @param subProc Sub-procedure.
   * @param beam    Beam.
   * @return HTML id.
   */
  def beamProcId(subProc: SubProcedure, beam: Beam): String = Util.textToId(s"${subProc.name} :: ${beam.beamName}")

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
    val id = beamProcId(subProc, beam)
    <span id={id} style={subProcBeamStyle(false)} title={subProc.name}></span>
  }

  private def XbeamToHtml(beam: Beam, subProcedureList: SubProcedureList): Elem = {

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
        <img src={imageUrl} height="20" style="margin: 2px;"/>
      </div>
    }

    <div style="border:1px solid lightgrey;margin: 2px;">
      <table>
        <tr>
          <td>
            {image}
          </td>{subProcedureUses}
        </tr>
      </table>
    </div>
  }

  private def beamToHtml(beam: Beam, subProcedureList: SubProcedureList): Elem = {

    val image: Elem = {
      val imageUrl = subProcedureList.metaData.urlOfExampleImage(beam)
      <div style="margin: 5px;">
        <h4>
          {beam.beamName}
        </h4> <br/>
        <img src={imageUrl} height="40" style="margin: 2px;"/>
      </div>
    }

    def toBeamUse(subProcedure: SubProcedure): Elem = {
      <td>
        {subProcUseOfBeam(subProcedure, beam)}
      </td>
    }

    <tr id={beamHtmlId(beam)} style="display:none;">
      <td>
        {image}
      </td>{subProcedureList.subProcedureList.map(toBeamUse)}
    </tr>

  }

  /**
   * Make the HTML table header for the list of beams.
   *
   * @param subProcedureList     General metadata.
   * @param selectedBeamCountTag HTML id attribute that shows number of beams.
   * @return HTML for table header of beam list.
   */
  private def tableHeader(subProcedureList: SubProcedureList, selectedBeamCountTag: String): Elem = {

    val style = "position: sticky; top: 0px; background: lightgrey;"


    def subToHeader(subProcedure: SubProcedure): Elem = {
      <th style={style} title={subProcedure.name} id={subProcedure.headerId}>
        {subProcedure.abbreviation}
      </th>
    }

    <thead style={style}>
      <th style={style}>Beam</th>{subProcedureList.subProcedureList.map(subToHeader)}
    </thead>
  }

  def selectedBeamsField(subProcedureList: SubProcedureList, selectedBeamCountTag: String): Elem = {
    // @formatter:off
    val htmlX = {
      <div>
        <div class="row">
          <div class="col-md-4">
            <h3>Beam List</h3>
          </div>
          <div class="col-md-4">
            <h3>Selected: <span id={selectedBeamCountTag}>0</span></h3>
          </div>
        </div>
        <div class="row">
          {subProcedureList.beamList.map(beam => {
            <div class="col-md-6" id={beamHtmlId(beam)} style="display:none;">
              {beamToHtml(beam, subProcedureList)}
            </div>
          })}
        </div>
      </div>
    }
    // @formatter:on

    val html = {
      <div>
        <div class="row">
          <div class="col-md-4">
            <h3>Beam List</h3>
          </div>
          <div class="col-md-4">
            <h3>Selected:
              <span id={selectedBeamCountTag}>0</span>
            </h3>
          </div>
        </div>

        <div class="row" style="overflow-y:auto; height:800px;">
          <table style="border-collapse: collapse; width: 100%;" class="table table-bordered">
            {tableHeader(subProcedureList, selectedBeamCountTag)}<tbody>
            {subProcedureList.beamList.map(beam => beamToHtml(beam, subProcedureList))}
          </tbody>
          </table>
        </div>
      </div>
    }
    html
  }

}
