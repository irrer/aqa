/*
 * Copyright 2021 Regents of the University of Michigan
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

package org.aqa.webrun.phase2.symmetryAndFlatness

import edu.umro.ScalaUtil.PrettyXML
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.db.PSMBeam
import org.aqa.db.SymmetryAndFlatness
import org.aqa.db.User
import org.aqa.web.ViewOutput
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlAdmin
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil.getValueMap
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.psm.PSMCache
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.io.File
import scala.xml.Elem

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessSubHTML extends Logging {

  def titleDetails = "Click to view graphs and other details"

  def titleImage = "Click to view DICOM metadata"

  /** PK of the SymmetryAndFlatness row to have its baseline changed. */
  private val symFlatPKTag = "symFlatPK"

  /** Indicates that caller is requesting a CSV of the results. */
  val csvTag = "csv"

  /** Indicates the value of the new baseline.  Must be either true or false. */
  private val baselineTag = "baseline"

  /** Indicates which set of data to retrieve for display as a web page. */
  val outputPKTag = "outputPK"

  /** Used to specify the name of a beam in a URL. */
  val beamNameTag = "BeamName"

  /** Used to specify whether PSM should be used.  If true, use PSM.  If false or not specified, then do not use PSM. */
  val withPsmTag = "withPsm"

  /** The CSS styling used to in indicate that this data was */
  val psmStyle = "color:white;background:lightblue; border-left:10px solid lightblue; border-right:10px solid lightblue;"

  private def titleAxialSymmetry =
    "Axial symmetry from top to bottom: (top-bottom)/bottom.  Max percent limit is " + Config.SymmetryPercentLimit

  private def titleTransverseSymmetry =
    "Transverse symmetry from right to left: (right-left)/left.  Max percent limit is " + Config.SymmetryPercentLimit

  private def titleFlatness =
    "Flatness: (max-min)/(max+min).  Max percent limit is " + Config.FlatnessPercentLimit

  private def titleProfileConstancy =
    "Profile Constancy.  Max percent limit is " + Config.ProfileConstancyPercentLimit

  private def tableHead: Elem = {
    <thead>
      <tr>
        <th style="text-align: center;" title={titleDetails}>
          Beam
        </th>
        <th style="text-align: center;" title={titleImage}>
          Image
        </th>
        <th style="text-align: center;">
          Measurement
        </th>
        <th style="text-align: center;">
          Baseline %
        </th>
        <th style="text-align: center;" title="Difference of value from baseline">
          Difference
          <br/>
        </th>
        <th style="text-align: center;" title="Maximim allowed percent deviation from baseline">
          Percent
          <br/>
          Limit
        </th>
        <th style="text-align: center;" title="Measured value">
          Value %
        </th>
      </tr>
    </thead>
  }

  private def PSMTableHead: Elem = {
    <thead>
      <tr>
        <th style="text-align: center;" title={titleDetails}>
          Beam
        </th>
        <th style="text-align: center;" title={titleImage}>
          Image
        </th>
        <th style="text-align: center;">
          Measurement
        </th>
        <th style="text-align: center;">
          Baseline %
        </th>
        <th style="text-align: center;" title="Difference of value from baseline">
          Difference
          <br/>
        </th>
        <th style="text-align: center;" title="Maximim allowed percent deviation from baseline">
          Percent
          <br/>
          Limit
        </th>
        <th style="text-align: center;" title="Measured value">
          Value %
        </th>
      </tr>
    </thead>
  }

  private def pctRounded(pct: Double) = {
    val factor = 1000000000L
    (pct * factor).round.toDouble / factor
  }

  private def detailsColumn(
                             subDir: File,
                             symFlatDataSet: SymmetryAndFlatnessDataSet,
                             isPsm: Boolean
                           ): Elem = {

    val errorClass = {
      if (symFlatDataSet.symmetryAndFlatness.allNoPSMPass(symFlatDataSet.baseline)) "normal" else "danger"
    }
    val detailUrl = WebServer.urlOfResultsFile(
      SymmetryAndFlatnessHTML.beamHtmlFile(subDir, symFlatDataSet.symmetryAndFlatness, symFlatDataSet.symmetryAndFlatness.canDoPSM)
    )
    val pk = symFlatDataSet.symmetryAndFlatness.symmetryAndFlatnessPK.get
    val id = "baseline" + pk
    val baseline = symFlatDataSet.symmetryAndFlatness.isBaseline.toString

    val psmElem = {
      if (isPsm)
        <span>PSM</span>
      else
        <span></span>
    }

    val input =
      if (symFlatDataSet.symmetryAndFlatness.isBaseline) {
          <input value={baseline} type="checkbox" id={id} onclick={"setBaselineState(this, " + pk + ")"} checked={baseline}/>
      } else {
          <input value={baseline} type="checkbox" id={id} onclick={"setBaselineState(this, " + pk + ")"}/>
      }

    val elem = {
      val baselineUrl = ViewOutput.viewOutputUrl(symFlatDataSet.baseline.outputPK)

      <td style={"vertical-align: middle;" + {
        if (isPsm) psmStyle else ""
      }} class={errorClass} rowspan="4">
        <a href={detailUrl} title={titleDetails}>
          {symFlatDataSet.symmetryAndFlatness.beamName}<br>
          {Phase2Util.jawDescription(symFlatDataSet.al, symFlatDataSet.rtplan)}
        </br>{Phase2Util.angleDescription(symFlatDataSet.al)}{psmElem}
        </a> <br></br> <label title="Check to use this beam as a baseline." for={id}>Baseline</label>{input}<br></br>
        <a href={baselineUrl}>View Baseline</a>
      </td>
    }
    elem
  }

  private def imageColumn(symFlatData: SymmetryAndFlatnessDataSet): Elem = {
    val dicomHref = Phase2Util.dicomViewHref(
      symFlatData.al,
      symFlatData.symmetryAndFlatness.beamName,
      symFlatData.output.dir,
      symFlatData.rtplan
    )
    val imgUrl = WebServer.urlOfResultsFile(
      SymmetryAndFlatnessHTML.annotatedImageFile(SymmetryAndFlatnessHTML.makeSubDir(symFlatData.output.dir), symFlatData.symmetryAndFlatness)
    )
    val imgSmall = {
        <img src={imgUrl} width="100"/>
    }
    val ref = {
      <a href={dicomHref}>
        {imgSmall}
      </a>
    }
    <td style="text-align: center;vertical-align: middle;" title={titleImage} rowspan="4">
      {ref}
    </td>
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private val symmetryPercentLimitColumn = {
    val v = Config.SymmetryPercentLimit
    val elem = {
      <td style="text-align: center;">
        {"%5.2f".format(v)}
      </td>
    }
    WebUtil.setPrecisionAttr(elem, v)
  }

  private val flatnessPercentLimitColumn = {
    val v = Config.FlatnessPercentLimit
    val elem = {
      <td style="text-align: center;">
        {"%5.2f".format(Config.FlatnessPercentLimit)}
      </td>
    }
    WebUtil.setPrecisionAttr(elem, v)
  }

  private val profileConstancyPercentLimitColumn = {
    val v = Config.ProfileConstancyPercentLimit
    val elem = {
      <td style="text-align: center;">
        {"%5.2f".format(v)}
      </td>
    }
    WebUtil.setPrecisionAttr(elem, v)
  }

  private def fmtBaselineColumn(baseline: Option[Double]): Elem = {
    if (baseline.isDefined) {
      val v = pctRounded(baseline.get)
      val elem = {
        <td style="text-align: center;" title={"Baseline % : " + "%10.8f".format(baseline.get)}>
          {"%5.3f".format(pctRounded(baseline.get)).trim}
        </td>
      }
      WebUtil.setPrecisionAttr(elem, v)
    } else <td></td>
  }

  private def fmtBaselineColumn(baseline: Double): Elem = {
    fmtBaselineColumn(Some(baseline))
  }

  private def fmtDifferenceColumn(percent: Double, limit: Double): Elem = {
    val errorClass = if (percent.abs > limit.abs) "danger" else "normal"
    val v = pctRounded(percent)
    val elem =
      <td style="text-align: center;" class={errorClass} title={"Difference: " + "%10.8f".format(percent)}>
        {"%5.2f".format(pctRounded(percent)).trim}
      </td>
    WebUtil.setPrecisionAttr(elem, v)
  }

  private def fmtValueColumn(value: Option[Double]): Elem = {
    if (value.isDefined) {
      val v = pctRounded(value.get)
      val elem =
        <td style="text-align: center;" title={"Value % : " + "%10.8f".format(value.get)}>
          {"%5.3f".format(pctRounded(value.get)).trim}
        </td>
      WebUtil.setPrecisionAttr(elem, v)
    } else
      <td></td>
  }

  private def fmtValueColumn(value: Double): Elem = fmtValueColumn(Some(value))

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def makeRow(symFlatData: SymmetryAndFlatnessDataSet, withPsm: Boolean): Seq[Elem] = {

    if ((!withPsm) || symFlatData.symmetryAndFlatness.canDoPSM) {

      val subDir = SymmetryAndFlatnessHTML.makeSubDir(symFlatData.output.dir)


      // @formatter:off
    Seq(
      {
        <tr align="center">
          {detailsColumn(subDir, symFlatData, withPsm)}
          {imageColumn(symFlatData)}
          <td style="text-align: center;" title={titleAxialSymmetry}>Axial Symmetry</td>
          {fmtBaselineColumn(symFlatData.baseline.axialSymmetryNoPSM())}
          {
            val pct = {
              symFlatData.symmetryAndFlatness.axialSymmetryNoPSM() - symFlatData.baseline.axialSymmetryNoPSM()
            }
            fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)}
          { symmetryPercentLimitColumn }
          { fmtValueColumn(symFlatData.symmetryAndFlatness.axialSymmetryNoPSM()) }
        </tr>
      }, {
        <tr>
          <td style="text-align: center;" title={titleTransverseSymmetry}>Transverse Symmetry</td>
          {fmtBaselineColumn(symFlatData.baseline.transverseSymmetryNoPSM())}
          {
            val pct = symFlatData.symmetryAndFlatness.transverseSymmetryNoPSM() - symFlatData.baseline.transverseSymmetryNoPSM()
            fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)
          }
          { symmetryPercentLimitColumn }
          {fmtValueColumn(symFlatData.symmetryAndFlatness.transverseSymmetryNoPSM())}
        </tr>
      }, {
        <tr>
          <td style="text-align: center;" title={titleFlatness}>Flatness</td>
          {fmtBaselineColumn(symFlatData.baseline.flatnessNoPSM())}
          {
            val pct = symFlatData.symmetryAndFlatness.flatnessNoPSM() - symFlatData.baseline.flatnessNoPSM()
            fmtDifferenceColumn(pct, Config.FlatnessPercentLimit)}
          { flatnessPercentLimitColumn }
          {fmtValueColumn(symFlatData.symmetryAndFlatness.flatnessNoPSM())}
        </tr>
      }, {
        <tr>
          <td style="text-align: center;" title={titleProfileConstancy}>Profile Constancy</td>
          { fmtBaselineColumn(symFlatData.baseline.profileConstancyNoPSM( symFlatData.baseline )) }
          {
            val pct = {

              val a = symFlatData.symmetryAndFlatness.profileConstancyNoPSM( symFlatData.baseline  )
              val b = symFlatData.baseline.profileConstancyNoPSM( symFlatData.baseline  )
              a - b
            }

            fmtDifferenceColumn(pct, Config.ProfileConstancyPercentLimit)
          }
          { profileConstancyPercentLimitColumn }
          { fmtValueColumn(symFlatData.symmetryAndFlatness.profileConstancyNoPSM(symFlatData.baseline)) }
        </tr>
      }
    )
    }
    else
      Seq()
    // @formatter:on
  }

  /**
   * Respond to a request for the data nicely formatted in HTML.
   *
   * @param output          Get data for machine referenced by this output.
   * @param symFlatDataList List of data to display
   * @return Formatted report
   */
  def makeContent(output: Output, symFlatDataList: Seq[SymmetryAndFlatnessDataSet]): Elem = {

    // show link to CSV
    val csv: Elem = {
      val url = (new SymmetryAndFlatnessSubHTML).pathOf + "/SymmetryFlatnessAndConstancy.csv?" + csvTag + "=true&" + outputPKTag + "=" + output.outputPK.get
      <h4>
        <a href={url} title="Download all Symmetry and Flatness for this machine as a CSV viewable in a spreadsheet." style="margin:20px;">CSV</a>
      </h4>
    }

    val content = {
      <div>
        <div class="row">
          jjjjjjjjjjjjjjjjj
          <div class="col-md-2 col-md-offset-1">
            <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css"/>{csv}<br/>
          </div>
          <div class="col-md-2">
            {WebUtil.showPrecision}
          </div>
        </div>
        <table class="table table-responsive table-bordered">
          kkkkkkkkkkkkkkkkkkkkk
          {tableHead}{symFlatDataList.flatMap(sfd => Seq(makeRow(sfd, withPsm = false), makeRow(sfd, withPsm = true)))}
        </table>
        <p/>
      </div>
    }
    content
  }

  /**
   * Format an HTML td with a double value.  Add title to show more precision.
   *
   * @param d Value to format.
   * @return td element.
   */
  private def td(d: Double): Elem = {
    val elem =
      <td title={d.toString}>
        {Util.fmtDbl(d)}
      </td>

    WebUtil.setPrecisionAttr(elem, d)
  }


  /**
   * Format an HTML td with a double value.  Add title to show more precision.
   *
   * @param d Value to format.
   * @return td element.
   */
  private def td(d: Option[Double]) = {
    if (d.isDefined) {
      val elem =
        <td title={d.get.toString}>
          {Util.fmtDbl(d.get)}
        </td>

      WebUtil.setPrecisionAttr(elem, d.get)
    }
    else
      <td></td>
  }

  /**
   * Make HTML to show the results of the calculation of the beam data.
   *
   * @param beamData Data to show.
   * @return HTML to display.
   */
  private def resultTable(beamData: SymmetryAndFlatness.SymmetryAndFlatnessHistory, withPsm: Boolean): Elem = {

    val beamHeaderElem: Elem = {
      val psmElem = {
        if (!withPsm)
          <span>
            <p></p>
          </span>
        else {
          val psmOutputPK = beamData.symmetryAndFlatness.psmGridWithFFFrom().get.psmGrid.centerBeam.outputPK
          <p title="Click to view PSM">
            <a href={ViewOutput.viewOutputUrl(psmOutputPK)}>with PSM</a>
          </p>
        }
      }

      val spanAndDiameter = {
        val span_mm = beamData.symmetryAndFlatness.getSpan_mm
        val diameter_mm = if (beamData.symmetryAndFlatness.diameter_mm.isDefined) beamData.symmetryAndFlatness.diameter_mm.get else Config.SymmetryAndFlatnessDiameter_mm
        <div>
          <span title={SymmetryAndFlatnessHTML.spanTitle}>Span:
            {WebUtil.setPrecisionAttr(<span>
            {Util.fmtDbl(span_mm)}
          </span>, span_mm)}
          </span>
          <p title="Diameter of each sample area in mm.">Diameter:
            {WebUtil.setPrecisionAttr(<span>
            {Util.fmtDbl(diameter_mm)}
          </span>, diameter_mm)}
          </p>
        </div>
      }

      <div class="row">
        <div class="col-md-3">
          <h3 style={if (withPsm) psmStyle else ""}>
            {beamData.symmetryAndFlatness.beamName}{psmElem}
          </h3>
        </div>
        <div class="col-md-2 col-md-offset-1" style="margin-top:24px;">
          {WebUtil.showPrecision}
        </div>
        <div class="col-md-3 col-md-offset-1" style="margin-top:24px;margin-left:80px;">
          {spanAndDiameter}
        </div>
      </div>
    }

    val analysisElem =
      Seq(
        td(beamData.symmetryAndFlatness.getTransverseSymmetry(withPsm)),
        td(beamData.symmetryAndFlatness.getAxialSymmetry(withPsm)),
        td(beamData.symmetryAndFlatness.getFlatness(withPsm)),
        td(beamData.symmetryAndFlatness.getConstancy(withPsm, beamData.baseline))
      )

    val baselineElem =
      Seq(
        td(beamData.baseline.getTransverseSymmetry(withPsm)),
        td(beamData.baseline.getAxialSymmetry(withPsm)),
        td(beamData.baseline.getFlatness(withPsm)),
        td(beamData.baseline.getConstancy(withPsm, beamData.baseline))
      )

    <div style="margin:20px;">
      {beamHeaderElem}<center>
      <h3>Results</h3>
    </center>
      <table class="table table-bordered" title={"Results of this analysis and baseline values" + WebUtil.titleNewline + "for comparison.  All values are in percent."}>
        <thead>
          <tr>
            <th>Source</th>
            <th>Transverse Symmetry %</th>
            <th>Axial Symmetry %</th>
            <th>Flatness %</th>
            <th>Profile Constancy %</th>
          </tr>
        </thead>
        <tr>
          <td>Analysis</td>{analysisElem}
        </tr>
        <tr>
          <td>Baseline</td>{baselineElem}
        </tr>
      </table>
    </div>
  }

  /**
   * Make HTML to show the raw beam data.
   *
   * @param beamData Data to show.
   * @return HTML to display.
   */
  private def inputTable(beamData: SymmetryAndFlatness.SymmetryAndFlatnessHistory, hasPSM: Boolean): Elem = {

    val analysis = {
      // @formatter:off
        <tr>
          <td>Analysis</td>
          {td(beamData.symmetryAndFlatness.getTopCu   (hasPSM))}
          {td(beamData.symmetryAndFlatness.getBottomCu(hasPSM))}
          {td(beamData.symmetryAndFlatness.getLeftCu  (hasPSM))}
          {td(beamData.symmetryAndFlatness.getRightCu (hasPSM))}
          {td(beamData.symmetryAndFlatness.getCenterCu(hasPSM))}
        </tr>
    }

    val baseline = {
        // @formatter:off
        <tr>
          <td>Baseline</td>
          {td(beamData.baseline.getTopCu   (hasPSM))}
          {td(beamData.baseline.getBottomCu(hasPSM))}
          {td(beamData.baseline.getLeftCu  (hasPSM))}
          {td(beamData.baseline.getRightCu (hasPSM))}
          {td(beamData.baseline.getCenterCu(hasPSM))}
        </tr>
        // @formatter:on
    }


    // @formatter:off
    <div style="margin:20px;">
      <center>
        <h3>Inputs</h3>
      </center>
      <table class="table table-bordered" title="Input values from this data set and from baseline.">
        <thead>
          <tr>
            <th>Source</th>
            <th>Top CU</th>
            <th>Bottom CU</th>
            <th>Left CU</th>
            <th>Right CU</th>
            <th>Center CU</th>
          </tr>
        </thead>
        {analysis}
        {baseline}
      </table>
    </div>
    // @formatter:on
  }

  /** Optionally show the formulas for sym/flat/const calculations. */
  private val formulas: Elem = {
    val elem = {
      <div>
        <h3>Calculation of Symmetry</h3>
        Symmetry is calculated both axially and transversely as a percentage with:
        <pre>axial = ((top - bottom) / bottom) * 100</pre>
        <pre>transverse = ((right - left ) / left ) * 100</pre>

        <h3>Calculation of Flatness</h3>
        Flatness is calculated over the image as a percentage using the maximum and minimum values of the 5 measured
        circles:
        <pre>(max - min) / (max + min) * 100</pre>

        <h3>Calculation of Constancy</h3>
        Constancy is calculated over the image by dividing each of the outer values by the
        center value then subtracting each from the baseline values, processed similarly:

        <pre>t = (top / center) - (baseline top / baseline center)</pre>
        <pre>b = (bottom / center) - (baseline bottom / baseline center)</pre>
        <pre>l = (left / center) - (baseline left / baseline center)</pre>
        <pre>r = (right / center) - (baseline right / baseline center)</pre>

        The sum of these is taken, multiplied by 100 and averaged:

        <pre>((t + r + b + l) * 100) / 4</pre>
      </div>
    }
    val id = "SymFlatEquations"

    val elem2 = {
      <div class="container" style="border: 1px solid lightgrey;">
        <div style="margin:10px;">
          <button type="button" class="btn btn-info" data-toggle="collapse" data-target={"#" + id}>
            {WebUtil.magnify}
          </button>
          <span style="white-space: pre-line;">
            Click magnifying glass icon to show/hide formulas
          </span>
          <div id={id} class="collapse">
            {elem}
          </div>
        </div>
      </div>
    }

    elem2
  }


  /**
   * Make HTML to show the raw beam data.
   *
   * @param symFlatHistory Data to show.
   * @return HTML to display.
   */
  // @formatter:off
  private def psmCalcTable(symFlatHistory: SymmetryAndFlatness.SymmetryAndFlatnessHistory): Elem = {

    val beamName = symFlatHistory.symmetryAndFlatness.beamName

    val psmGridWithFF = PSMCache.get(symFlatHistory.symmetryAndFlatness).get
    val psmGrid = psmGridWithFF.psmGrid

    val psmTableHead = {
      <thead>
        <tr>
          <th>Source</th>
          <th>Top CU</th>
          <th>Bottom CU</th>
          <th>Left CU</th>
          <th>Right CU</th>
          <th>Center CU</th>
        </tr>
      </thead>
    }

    def psmToTr(index: Int, name: Elem, beamToValue: PSMBeam => Double ) : Elem = {

      <tr>
        <td><b>{index.toString}</b> : {name}</td>
        {td(beamToValue(psmGrid.topBeam   ))}
        {td(beamToValue(psmGrid.bottomBeam))}
        {td(beamToValue(psmGrid.leftBeam  ))}
        {td(beamToValue(psmGrid.rightBeam ))}
        {td(beamToValue(psmGrid.centerBeam))}
      </tr>
    }

    val beamTimesFFOverPSMOverCenter = {

      <tr>
        <td><span><b>4 : PSM</b> : <br>(row <b>3</b> / row <b>3</b> column <b>E Center CU</b> )</br></span> </td>
        {td(psmGridWithFF.normalizedTop)}
        {td(psmGridWithFF.normalizedBottom)}
        {td(psmGridWithFF.normalizedLeft)}
        {td(psmGridWithFF.normalizedRight)}
        {td(psmGridWithFF.normalizedCenter)}
      </tr>
    }

    val beamValues = {
      <tr>
        <td><b>5</b> : <b>{beamName}</b> </td>
        {td(symFlatHistory.symmetryAndFlatness.top_cu   )}
        {td(symFlatHistory.symmetryAndFlatness.bottom_cu)}
        {td(symFlatHistory.symmetryAndFlatness.left_cu  )}
        {td(symFlatHistory.symmetryAndFlatness.right_cu )}
        {td(symFlatHistory.symmetryAndFlatness.center_cu)}
      </tr>
    }

    val beamTimesFF = {
      <tr>
        <td><b>6</b> : <b>{beamName}</b> * <b>FF</b> (row <b>1</b>)</td>
        {td(symFlatHistory.symmetryAndFlatness.top_cu     * psmGrid.topBeam   .getFloodField_cu(psmGridWithFF))}
        {td(symFlatHistory.symmetryAndFlatness.bottom_cu  * psmGrid.bottomBeam.getFloodField_cu(psmGridWithFF))}
        {td(symFlatHistory.symmetryAndFlatness.left_cu    * psmGrid.leftBeam  .getFloodField_cu(psmGridWithFF))}
        {td(symFlatHistory.symmetryAndFlatness.right_cu   * psmGrid.rightBeam .getFloodField_cu(psmGridWithFF))}
        {td(symFlatHistory.symmetryAndFlatness.center_cu  * psmGrid.centerBeam.getFloodField_cu(psmGridWithFF))}
      </tr>
    }

    val beamTimesFFOverPSM = {
      <tr>
        <td><b>7</b> : <b>{beamName}</b> with <b>PSM</b> = ({beamName} * <b>FF</b>) / <b>PSM</b> </td>
        {td(symFlatHistory.symmetryAndFlatness.topWithPSM_cu   ())}
        {td(symFlatHistory.symmetryAndFlatness.bottomWithPSM_cu())}
        {td(symFlatHistory.symmetryAndFlatness.leftWithPSM_cu  ())}
        {td(symFlatHistory.symmetryAndFlatness.rightWithPSM_cu ())}
        {td(symFlatHistory.symmetryAndFlatness.centerWithPSM_cu())}
      </tr>
    }

    {
      // @formatter:off

    <div style="margin:20px;">
      <center>
        <h3>PSM Processing</h3>
      </center>
      <table class="table table-bordered" title="Input values from this data set and from baseline.">

        {psmTableHead}

        {psmToTr(1, <span><b>FF</b> : Flood Field Normalized</span>                                             , (pb: PSMBeam) => pb.getFloodField_cu(psmGridWithFF))}

        {psmToTr(2, <span><b>BR </b> : Beam Response (from 5 PSM beams)</span>                                  , (pb: PSMBeam) => pb.mean_cu)}

        {psmToTr(3, <span><b>FF*BR </b> : row <b>1</b> * row <b>2</b></span>                                    , (pb: PSMBeam) => pb.ffTimesPSM(psmGridWithFF))}

        {/* 4 */ beamTimesFFOverPSMOverCenter}

        {/* 5 */ beamValues}

        {/* 6 */ beamTimesFF}

        {/* 7 */ beamTimesFFOverPSM}
      </table>
    </div>
    }
  }
  // @formatter:on

  /**
   * Format data for just the given output and beam.  Build an HTML response to show it.
   *
   * @param valueMap Parameter list.  Already validated to have an output PK and beam name.
   * @param response Put HTML here.
   */
  private def showBeamData(valueMap: ValueMapT, response: Response): Unit = {
    val outputPK = valueMap(outputPKTag).toLong
    val output = Output.get(outputPK).get
    val machinePK = Output.get(outputPK).get.machinePK.get
    val beamName = valueMap(beamNameTag).replaceAll("%20", " ")
    val hasPsm = valueMap.contains(withPsmTag) && valueMap(withPsmTag).toBoolean

    val symFlat = {
      val list = SymmetryAndFlatness.getByOutput(outputPK)
      list
        .filter(sf =>
          sf.beamName.equalsIgnoreCase(beamName) &&
            (!hasPsm) || sf.canDoPSM && hasPsm
        )
        .head
    }

    val history = SymmetryAndFlatness.history( //
      machinePK = machinePK,
      span_mm = symFlat.span_mm,
      diameter_mm = symFlat.diameter_mm,
      beamName = beamName,
      hasPSM = hasPsm,
      RTImageSID_mm = symFlat.RTImageSID_mm,
      procedurePK = output.procedurePK
    )

    val beamData = history.find(h => h.output.outputPK.get == outputPK).get

    // @formatter:off
    val content = {
      <div class="row">
        <div class="col-md-4 col-md-offset-4">
          {resultTable(beamData, hasPsm)}
          {inputTable(beamData, hasPsm)}
        </div>
        <div class="col-md-8 col-md-offset-2">
          {formulas}
        </div>
        <div class="col-md-6 col-md-offset-3" style="margin-bottom:70px;">
          {if (hasPsm) psmCalcTable(beamData)}
        </div>
      </div>
    }
    // @formatter:on

    val text = WebUtil.specialCharTagsToLiteralXml(PrettyXML.xmlToText(content))
    WebUtil.setResponse(text, response, Status.SUCCESS_OK)
  }

  /**
   * If the user is authorized (must be in same institution or be whitelisted) then
   * change the given baseline to the given value.
   *
   * @param valueMap Contains user, symmetry and flatness PK, and baseline setting.
   * @param response Put response (HTML) here.
   * @return Message indicating what was done.
   */
  private def setBaseline(valueMap: ValueMapT, response: Response): Unit = {
    // Get parameters.  If there is any syntax error then throw an exception.
    val user = WebUtil.getUser(valueMap).get
    val symFlatPK = valueMap(symFlatPKTag).trim.toLong
    val symmetryAndFlatness = SymmetryAndFlatness.get(symFlatPK).get
    val authorized = {
      WebUtil.userIsWhitelisted(valueMap) || {
        val output = Output.get(symmetryAndFlatness.outputPK).get
        User.get(output.userPK.get).get.institutionPK == user.institutionPK
      }
    }

    if (authorized) {
      val baseline = valueMap(baselineTag).trim.toBoolean
      val newSymmetryAndFlatness = symmetryAndFlatness.copy(isBaseline = baseline)
      newSymmetryAndFlatness.insertOrUpdate()
      val elem = <div>Changed SymmetryAndFlatness
        {symFlatPK.toString}
        to
        {baseline.toString}
      </div>
      val text = PrettyXML.xmlToText(elem)
      WebUtil.setResponse(text, response, Status.SUCCESS_OK)
    } else {
      val elem = <p>Not authorized to change baseline.</p>
      val text = PrettyXML.xmlToText(elem)
      WebUtil.setResponse(text, response, Status.CLIENT_ERROR_FORBIDDEN)
    }
  }

  /**
   *
   * @param valueMap Contains parameters indicating which data to process.
   * @param response Put results here.
   */
  private def makeCsv(valueMap: ValueMapT, response: Response): Unit = {
    val output = Output.get(valueMap(outputPKTag).toLong).get
    val csvText = SymmetryAndFlatnessCSV.makeCsvFile(output)
    response.setStatus(Status.SUCCESS_OK)
    response.setEntity(csvText, MediaType.TEXT_CSV)
  }
}

class SymmetryAndFlatnessSubHTML extends Restlet with Logging with SubUrlAdmin {

  /**
   * If the incoming request is for the given handler, then handle it and return true.
   *
   * @param request  User request.
   * @param response Put results and status here.
   */
  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    def has(tag: String) = valueMap.contains(tag)

    val SF = SymmetryAndFlatnessSubHTML

    try {
      0 match {
        case _ if has(SF.csvTag) =>
          SF.makeCsv(valueMap, response)
        case _ if has(SF.outputPKTag) && has(SF.beamNameTag) =>
          SF.showBeamData(valueMap, response)
        case _ if has(SF.outputPKTag) =>
          val sfTable = SymmetryAndFlatnessMainTableHTML()
          sfTable.collectData(valueMap, response)
          // SF.collectData(valueMap, response)
        case _ if has(SF.baselineTag) =>
          SF.setBaseline(valueMap, response)
        case _ =>
          WebUtil.badRequest(response, message = "Invalid request", Status.CLIENT_ERROR_BAD_REQUEST)
      }
    } catch {
      case t: Throwable =>
        val msg =
          "Problem accessing data to display Symmetry, Flatness, and Constancy results.  Parameters: " + valueMap + "\nerror: " + fmtEx(t)
        logger.warn(msg)
        WebUtil.setResponse(msg, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

}
