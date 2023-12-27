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
    val style = s"border-left: 16px solid $color; margin:5px; line-height: 20%;"
    style
  }

  /** Return an indicator as to whether the sub procedure uses the beam. */
  private def subProcUseOfBeam(subProc: SubProcedure, beam: Beam): Elem = {
    val id = beamProcId(subProc, beam)
    <span id={id} style={subProcBeamStyle(false)} title={subProc.name}></span>
  }

  private def beamToHtml(beam: Beam, subProcedureList: SubProcedureList): Elem = {

    val gantry = s"Gantry ${beam.gantryAngle_roundedDeg}"


    val mv = {
      beam.beamEnergy.photonEnergy_MeV match {
        case Some(e) =>
          "MV:" + {
            if (e.round == e) e.round else Util.fmtDbl(e)
          }
        case _ =>
          ""
      }
    }

    val doseRate = {
      beam.beamEnergy.maxDoseRate_MUperMin match {
        case Some(dr) =>
          "Dose Rate:" + {
            if (dr.round == dr) dr.round else Util.fmtDbl(dr)
          }
        case _ => ""
      }
    }

    val fff = if (beam.isFFF) "FFF" else ""

    val title = s"$gantry    $mv    $doseRate    $fff"

    val image: Elem = {
      val imageUrl = subProcedureList.metaData.urlOfExampleImage(beam)
      <div style="margin: 5px;" title={title}>
        <table>
          <tr>
            <td>
              <img src={imageUrl} height="50" style="margin: 2px;"/>
            </td>
            <td>
              <h4>
                {beam.beamName}
              </h4>{title}
            </td>
          </tr>
        </table>
      </div>
    }

    def toBeamUse(subProcedure: SubProcedure): Elem = {
      <td style="text-align: center;">
        {subProcUseOfBeam(subProcedure, beam)}
      </td>
    }

    <tr id={beamHtmlId(beam)} style="display:none;">
      <td style="text-align: center;">
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

    val style = "position: sticky; top: 0px; background: lightgrey; text-align: center;"

    def subToHeader(subProcedure: SubProcedure): Elem = {
      <th style={style} title={subProcedure.name} id={subProcedure.headerId}>
        {subProcedure.abbreviation}
      </th>
    }

    // @formatter:off
    <thead style={style}>
      <tr style="text-align: center;">
        <th style={style}> </th>{subProcedureList.subProcedureList.map(subToHeader)}
      </tr>
    </thead>
    // @formatter:on
  }

  def selectedBeamsField(subProcedureList: SubProcedureList, selectedBeamCountTag: String): Elem = {

    // @formatter:off
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

        <div class="row" style={Phase3HtmlForm.formBodyStyle}>
          <table style="width: 100%;" class="table table-bordered">
            {tableHeader(subProcedureList, selectedBeamCountTag)}
            <tbody>
              {subProcedureList.beamList.map(beam => beamToHtml(beam, subProcedureList))}
            </tbody>
          </table>
        </div>
      </div>
    }
    // @formatter:on
    html
  }

}
