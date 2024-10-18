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

import org.aqa.Logging
import org.aqa.Util
import org.aqa.customizeRtPlan.phase3plan.Phase3JS.colorBackground
import org.aqa.customizeRtPlan.phase3plan.Phase3JS.colorCaution
import org.aqa.customizeRtPlan.phase3plan.Phase3JS.colorSelected
import org.aqa.customizeRtPlan.phase3plan.Phase3JS.selectedBeamCountTag
import org.aqa.customizeRtPlan.phase3plan.Phase3JS.unusedBeamCountTag
import org.aqa.web.C3Chart
import org.aqa.web.WebUtil

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
  private def beamProcId(subProc: SubProcedure, beam: Beam): String = Util.textToId(s"${beam.beamName} ${subProc.name}")

  /* Return true if the user has made any selections of the given sub procedure that use the given beam. */
  /*
  private def subProcIsUsingBeam(subProc: SubProcedure, beam: Beam, checkedSelectionList: Seq[Selection]): Boolean = {
    subProc.selectionList.exists(sel => checkedSelectionList.exists(s => s.htmlId.equals(sel.htmlId)))
  }
  */

  /* Return the HTML attribute style for the given beam's use by a sub-procedure. */
  private def subProcBeamStyle(checked: Boolean): String = {
    val color =
      if (checked)
        Phase3JS.colorSelected
      else
        Phase3JS.colorBackground

    val style = s"background:$color;"
    style
  }

  def beamIsUsedBySubProcedure(subProc: SubProcedure, beam: Beam, checkedBeamList: Seq[Beam]): Boolean = {

    val checkedBeamNameSet = checkedBeamList.map(_.beamName).toSet

    def selIsActive(sel: Selection): Boolean = {
      val isActive = sel.beamList.map(b => checkedBeamNameSet.contains(b.beamName)).reduce(_ && _)
      val usesBeam = sel.beamList.exists(_.beamName.equals(beam.beamName))

      isActive && usesBeam
    }

    val usedByBeam: Boolean = subProc.selectionList.exists(selIsActive)

    usedByBeam
  }

  /** Return an indicator as to whether the sub procedure uses the beam. */
  private def subProcUseOfBeam(subProc: SubProcedure, beam: Beam, checkedBeamList: Seq[Beam]): Elem = {
    val id = beamProcId(subProc, beam)
    <div id={id} style={subProcBeamStyle(beamIsUsedBySubProcedure(subProc, beam, checkedBeamList))} title={subProc.name}>
      <p>
        {WebUtil.nbsp}
      </p>
      <p>
        {WebUtil.nbsp}
      </p>
    </div>
  }

  private def beamToHtml(beam: Beam, subProcedureList: SubProcedureList, checkedBeamList: Seq[Beam]): Elem = {

    val gantry = {
      def fmt(d: Double): String = {
        if (d == d.round)
          d.round.toString
        else
          d.toString
      }

      val text: String = {
        if (beam.gantryAngleList_deg.distinct.size == 1)
          fmt(beam.gantryAngle_deg)
        else {
          fmt(beam.gantryAngleList_deg.min) + " - " + fmt(beam.gantryAngleList_deg.max)
        }
      }
      s"Gantry $text"
    }

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

    val col = {
      "MLC: " + beam.colAngle_roundedDeg
    }

    val fff = if (beam.isFFF) "FFF" else ""

    val sp = WebUtil.nbsp + WebUtil.nbsp
    val title = s"$gantry $sp $col $sp $mv $sp $doseRate $sp $fff"

    val image: Elem = {
      val imageUrl = subProcedureList.metaData.urlOfExampleImage(beam)
      <div style="margin: 5px;">
        <table>
          <tr>
            <td>
              <img src={imageUrl} height="80" style="margin: 2px;"/>
            </td>
            <td>
              <h4>
                <table>
                  <tr>
                    <td>
                      <input id={beam.beamName + "::" + C3Chart.makeUniqueChartIdTag.replace("Chart", "Beam")} onclick="phase3ClickHandler(this)" class="form-control" name="M10G180C270" type="checkbox" style="margin-right: 8px;"/>
                    </td>
                    <td>
                      {beam.beamName}
                    </td>
                  </tr>
                </table>
              </h4>{title}
            </td>
          </tr>
        </table>
      </div>
    }

    val isUsedByAnyProcedure = subProcedureList.subProcedureList.exists(sp => beamIsUsedBySubProcedure(sp, beam, checkedBeamList))

    def toBeamUse(subProcedure: SubProcedure): Elem = {

      <td style="text-align: center;">
        {subProcUseOfBeam(subProcedure, beam, checkedBeamList)}
      </td>
    }


    <tr id={beam.beamName} style="display:none;">
      <td style="text-align:center; horizontal-align:middle; vertical-align:middle;">
        {image}
      </td>{subProcedureList.subProcedureList.map(toBeamUse)}
    </tr>

  }

  /**
   * Make the HTML table header for the list of beams.
   *
   * @param subProcedureList General metadata.
   * @return HTML for table header of beam list.
   */
  private def tableHeader(subProcedureList: SubProcedureList): Elem = {

    val style = s"position: sticky; top: 0px; background: ${Phase3JS.colorBackground}; text-align: center;"

    def subToHeader(subProcedure: SubProcedure): Elem = {
      <th style={style} title={subProcedure.name + WebUtil.titleNewline + "Number of beams used and number of tests to be performed."} id={subProcedure.headerId}>
        {subProcedure.abbreviation}
      </th>
    }

    val content = {
      // @formatter:off
      <thead style={style}>
        <tr style="text-align: center;">
          <th style={style}> </th>{subProcedureList.subProcedureList.map(subToHeader)}
        </tr>
      </thead>
    // @formatter:on
    }

    content
  }


  /** Generate HTML id for the usage of a given gantry angle. */
  private def gantryAngleUsageId(gantryAngle: Int): String = s"gantryAngle_$gantryAngle"

  /**
   * Indicate which gantry angles are used.
   *
   * @return HTML with id's for each angle.
   */
  private def gantryAngleUsage(): Elem = {
    def beamUsageHtml(angle: Int) = {
      <span id={gantryAngleUsageId(angle)} style="margin-left: 5px;">
      </span>
    }

    val html = {
      /*
      val title = Seq(
        "Collimator Centering is Optional",
        "",
        "The highlighted gantry angles indicate which gantry angles will be",
        "visited by the selected beams.   If highlighted in colorOk, then then",
        "collimator centering is being done at that gantry angle.  If yellow,",
        "then there is a test specified for that angle, but no collimator",
        "centering.",
        "",
        "The user may choose whether collimator centering should be done or not.",
        "Not doing it reduces the number of beams and therefore delivery time.",
        "If collimator centering data is not available, the it will assumed to be",
        "perfect (0, 0).",
        "",
        "Note: If collimator centering is specified only for gantry angle 0, then",
        "those values will be used for all beams.  This is to maintain backwards",
        "compatibility with Phase2.",
      ).mkString(WebUtil.titleNewline)
      */

      val title = Seq(
        "The highlighted gantry angles indicate which gantry angles will be",
        "visited by the selected beams.  For every gantry angle visited,",
        "collimator centering will be done.",
        "",
        "The user may explicitly specify that collimator centering being ",
        "done for all four gantry angles."
      ).mkString(WebUtil.titleNewline)

      // @formatter:off
      <div title={title}>
        {Seq(0, 90, 180, 270).map(beamUsageHtml)}
        <span> {WebUtil.nbsp} </span>
      </div>
      // @formatter:on
    }
    html
  }

  def selectedBeamsField(subProcedureList: SubProcedureList, checkedBeamList: Seq[Beam]): Elem = {
    // @formatter:off
    val html = {
      <div>
        <div class="row">
          <h3>
            <div class="col-md-3" title="List of beams that will be delivered by the plan.">
              Beam List
            </div>
            <div class="col-md-6" title="Number of beams that will be delivered by the plan">
              Selected: <span style="margin-right:40px;" id={selectedBeamCountTag}>0</span>
              Unused: <span id={unusedBeamCountTag} style={s"background:$colorBackground"}>0</span>
            </div>
            <div class="col-md-6">
              {gantryAngleUsage()}
            </div>
          </h3>
        </div>

        <div class="row" style={Phase3HtmlForm.formBodyStyle + " margin-top:10px;"}>
          <table style="width: 100%;" class="table table-bordered">
            {tableHeader(subProcedureList)}
            <tbody>
              {subProcedureList.beamList.map(beam => beamToHtml(beam, subProcedureList, checkedBeamList))}
            </tbody>
          </table>
        </div>
      </div>
    }
    // @formatter:on
    html
  }

  def setBeamUsageBySubProcedure(subProcedureList: SubProcedureList, checkedBeamList: Seq[Beam]): String = {

    def used(subProcedure: SubProcedure, beam: Beam): String = {
      val color =
        if (beamIsUsedBySubProcedure(subProcedure, beam, checkedBeamList))
          colorSelected
        else
          colorBackground
      s"""document.getElementById("${beamProcId(subProcedure, beam)}").style.background = "$color";"""
    }


    def setBeamUse(beam: Beam): String = {
      val usedByAnyBeam = subProcedureList.subProcedureList.exists(sp => beamIsUsedBySubProcedure(sp, beam, checkedBeamList))

      if (usedByAnyBeam)
        subProcedureList.subProcedureList.map(sp => used(sp, beam)).mkString("\n")
      else
        subProcedureList.subProcedureList.map(sp => s"""document.getElementById("${beamProcId(sp, beam)}").style.background = "$colorCaution";""").mkString("\n")
    }

    checkedBeamList.map(setBeamUse).mkString("\n")
  }

}
