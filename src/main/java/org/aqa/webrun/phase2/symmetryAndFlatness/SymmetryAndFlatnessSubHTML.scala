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

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.PrettyXML
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Input
import org.aqa.db.Output
import org.aqa.db.SymmetryAndFlatness
import org.aqa.db.User
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlAdmin
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil.getValueMap
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.DicomFile
import org.aqa.db.PSMBeam
import org.aqa.web.ViewOutput
import org.aqa.webrun.psm.PSMGrid
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.io.File
import scala.collection.immutable
import scala.xml.Elem

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessSubHTML extends Logging {

  private def titleDetails = "Click to view graphs and other details"

  private def titleImage = "Click to view DICOM metadata"

  /** PK of the SymmetryAndFlatness row to have its baseline changed. */
  private val symFlatPKTag = "symFlatPK"

  /** Indicates that caller is requesting a CSV of the results. */
  private val csvTag = "csv"

  /** Indicates the value of the new baseline.  Must be either true or false. */
  private val baselineTag = "baseline"

  /** Indicates which set of data to retrieve for display as a web page. */
  private val outputPKTag = "outputPK"

  /** Used to specify the name of a beam in a URL. */
  val beamNameTag = "BeamName"

  /** Used to specify the name of a beam in a URL. */
  val hasPsmTag = "hasPsm"

  private val psmStyle = "color:white;background:lightblue; border-left:10px solid lightblue; border-right:10px solid lightblue;"

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

  private def pctRounded(pct: Double) = {
    val factor = 1000000000L
    (pct * factor).round.toDouble / factor
  }

  private def detailsColumn(
                             subDir: File,
                             symFlatDataSet: SymmetryAndFlatnessDataSet
                           ): Elem = {

    val psmGridOf: Option[PSMGrid] = {
      if (symFlatDataSet.symmetryAndFlatness.psmDataDate.isDefined)
        PSMGrid.get(symFlatDataSet.output.machinePK.get, symFlatDataSet.symmetryAndFlatness.psmDataDate.get)
      else
        None
    }

    val psmGridBaselineOf: Option[PSMGrid] = {
      if (symFlatDataSet.baseline.psmDataDate.isDefined)
        PSMGrid.get(symFlatDataSet.output.machinePK.get, symFlatDataSet.baseline.psmDataDate.get)
      else
        None
    }

    val errorClass = if (symFlatDataSet.symmetryAndFlatness.allPass(symFlatDataSet.baseline, psmGridOf, psmGridBaselineOf)) "normal" else "danger"
    val detailUrl = WebServer.urlOfResultsFile(
      SymmetryAndFlatnessHTML.beamHtmlFile(subDir, symFlatDataSet.symmetryAndFlatness.beamName, symFlatDataSet.symmetryAndFlatness.psmDataDate.isDefined)
    )
    val pk = symFlatDataSet.symmetryAndFlatness.symmetryAndFlatnessPK.get
    val id = "baseline" + pk
    val baseline = symFlatDataSet.symmetryAndFlatness.isBaseline.toString

    val isPsm = symFlatDataSet.symmetryAndFlatness.psmDataDate.isDefined

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
        </a> <br></br> <label title="Check to use this beam as a baseline." for={id}>Baseline</label>{input}
        <br></br>
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
      SymmetryAndFlatnessHTML.annotatedImageFile(
        SymmetryAndFlatnessHTML.makeSubDir(symFlatData.output.dir),
        symFlatData.symmetryAndFlatness.beamName,
        symFlatData.symmetryAndFlatness.psmDataDate.isDefined
      )
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
        {v.formatted("%5.2f")}
      </td>
    }
    WebUtil.setPrecisionAttr(elem, v)
  }

  private val flatnessPercentLimitColumn = {
    val v = Config.FlatnessPercentLimit
    val elem = {
      <td style="text-align: center;">
        {Config.FlatnessPercentLimit.formatted("%5.2f")}
      </td>
    }
    WebUtil.setPrecisionAttr(elem, v)
  }

  private val profileConstancyPercentLimitColumn = {
    val v = Config.ProfileConstancyPercentLimit
    val elem = {
      <td style="text-align: center;">
        {v.formatted("%5.2f")}
      </td>
    }
    WebUtil.setPrecisionAttr(elem, v)
  }

  private def fmtBaselineColumn(baseline: Option[Double]): Elem = {
    if (baseline.isDefined) {
      val v = pctRounded(baseline.get)
      val elem = {
        <td style="text-align: center;" title={"Baseline % : " + baseline.get.formatted("%10.8f")}>
          {pctRounded(baseline.get).formatted("%5.3f").trim}
        </td>
      }
      WebUtil.setPrecisionAttr(elem, v)
    } else <td></td>
  }

  private def fmtDifferenceColumn(percent: Double, limit: Double): Elem = {
    val errorClass = if (percent.abs > limit.abs) "danger" else "normal"
    val v = pctRounded(percent)
    val elem =
      <td style="text-align: center;" class={errorClass} title={"Difference: " + percent.formatted("%10.8f")}>
        {pctRounded(percent).formatted("%5.2f").trim}
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

  private def psmGridOf(sf: SymmetryAndFlatnessDataSet): Option[PSMGrid] = {
    if (sf.symmetryAndFlatness.psmDataDate.isDefined)
      PSMGrid.get(sf.output.machinePK.get, sf.symmetryAndFlatness.psmDataDate.get)
    else
      None
  }

  private def psmGridBaselineOf(sf: SymmetryAndFlatnessDataSet): Option[PSMGrid] = {
    if (sf.baseline.psmDataDate.isDefined)
      PSMGrid.get(sf.output.machinePK.get, sf.baseline.psmDataDate.get)
    else
      None
  }


  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def makeRow(symFlatData: SymmetryAndFlatnessDataSet): Seq[Elem] = {
    val subDir = SymmetryAndFlatnessHTML.makeSubDir(symFlatData.output.dir)


    // @formatter:off
    Seq(
      {
        <tr align="center">
          {detailsColumn(subDir, symFlatData)}
          {imageColumn(symFlatData)}
          <td style="text-align: center;" title={titleAxialSymmetry}>Axial Symmetry</td>
          {fmtBaselineColumn(symFlatData.baseline.axialSymmetry(psmGridOf(symFlatData)))}
          {
            val pct = {
              (symFlatData.symmetryAndFlatness.axialSymmetry(psmGridOf(symFlatData)) , symFlatData.baseline.axialSymmetry(psmGridBaselineOf(symFlatData))) match {
                case (Some(a), Some(b)) => a - b
                case _ => 0.0
              }
            }
            fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)}
          { symmetryPercentLimitColumn }
          { fmtValueColumn(symFlatData.symmetryAndFlatness.axialSymmetry(psmGridOf(symFlatData))) }
        </tr>
      }, {
        <tr>
          <td style="text-align: center;" title={titleTransverseSymmetry}>Transverse Symmetry</td>
          {fmtBaselineColumn(symFlatData.baseline.transverseSymmetry(psmGridOf(symFlatData)))}
          {
            val pct = (symFlatData.symmetryAndFlatness.transverseSymmetry(psmGridOf(symFlatData)) , symFlatData.baseline.transverseSymmetry(psmGridBaselineOf(symFlatData))) match {
              case (Some(a), Some(b)) => a - b
              case _ => 0.0
            }

            fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)
          }
          { symmetryPercentLimitColumn }
          {fmtValueColumn(symFlatData.symmetryAndFlatness.transverseSymmetry(psmGridOf(symFlatData)))}
        </tr>
      }, {
        <tr>
          <td style="text-align: center;" title={titleFlatness}>Flatness</td>
          {fmtBaselineColumn(symFlatData.baseline.flatness(psmGridBaselineOf(symFlatData)))}
          {
            val pct = (symFlatData.symmetryAndFlatness.flatness(psmGridOf(symFlatData)) , symFlatData.baseline.flatness(psmGridBaselineOf(symFlatData))) match {
              case (Some(a), Some(b)) => a - b
              case _ => 0.0
          }
            fmtDifferenceColumn(pct, Config.FlatnessPercentLimit)}
          { flatnessPercentLimitColumn }
          {fmtValueColumn(symFlatData.symmetryAndFlatness.flatness(psmGridBaselineOf(symFlatData)))}
        </tr>
      }, {
        <tr>
          <td style="text-align: center;" title={titleProfileConstancy}>Profile Constancy</td>
          { fmtBaselineColumn(symFlatData.baseline.profileConstancy(psmGridOf(symFlatData), symFlatData.baseline, psmGridBaselineOf(symFlatData) )) }
          {
            val pct = {

              val a = symFlatData.symmetryAndFlatness.profileConstancy( psmGridOf(symFlatData), symFlatData.baseline,psmGridBaselineOf(symFlatData)  )
              val b = symFlatData.baseline.profileConstancy( psmGridBaselineOf(symFlatData), symFlatData.baseline,psmGridBaselineOf(symFlatData)  )

              (a, b) match {
                case (Some(a), Some(b)) => a - b
                case _ => 0.0
              }
            }

            fmtDifferenceColumn(pct, Config.ProfileConstancyPercentLimit)
          }
          { profileConstancyPercentLimitColumn }
          { fmtValueColumn(symFlatData.symmetryAndFlatness.profileConstancy(psmGridOf(symFlatData), symFlatData.baseline, psmGridBaselineOf(symFlatData))) }
        </tr>
      }
    )
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
          <div class="col-md-2 col-md-offset-1">
            <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css"/>{csv}<br/>
          </div>
          <div class="col-md-2">
            {WebUtil.showPrecision}
          </div>
        </div>
        <table class="table table-responsive table-bordered">
          {tableHead}{symFlatDataList.map(sfd => makeRow(sfd))}
        </table>
        <p/>
      </div>
    }
    content
  }

  /**
   * Collect and format the data as HTML and return it.
   *
   * @param valueMap List of parsed parameters.
   * @return
   */
  private def collectData(valueMap: ValueMapT, response: Response): Unit = {
    val output = Output.get(valueMap(outputPKTag).toLong).get
    val dataDate = output.dataDate.get
    val symFlatList = SymmetryAndFlatness.getByOutput(output.outputPK.get)

    // list of all DICOM series referenced by SymmetryFlatness rows
    val dicomSeries = symFlatList
      .flatMap(sf => DicomSeries.getBySopInstanceUID(sf.SOPInstanceUID))
      .groupBy(_.seriesInstanceUID)
      .map(uidDs => uidDs._2.head)

    // list of all AttributeList's referenced by SymmetryFlatness rows
    // Note: This statement takes several seconds to run while the user is waiting for the
    //  page to show.  It would be nice if it were faster.
    val alList: immutable.Iterable[AttributeList] = dicomSeries.flatMap(ds => ds.attributeListList)

    // the RTPLAN for this result, if the RTPLAN can be found
    val rtplanAl: Option[AttributeList] = {
      // If in test mode, then look in the input first.  This will only be defined if in test
      // mode and the RTPLAN is found in the input.
      val testPlan = {
        if (Config.TestMode) {
          val input = Input.get(output.inputPK).get

          // make sure that the files are there
          if (Util.listDirFiles(input.dir).isEmpty) {
            Input.restoreFilesFromDatabase(input.inputPK.get, input.dir)
          }

          val rtplanList = Util.listDirFiles(input.dir).map(f => new DicomFile(f)).flatMap(_.attributeList).filter(Util.isRtplan)
          rtplanList.headOption
        }
        else
          None
      }

      if (testPlan.isDefined)
        testPlan
      else { // either in production mode or the RTPLAN is not in the input
        val rtplanRefList: Iterable[AttributeList] = alList.groupBy(al => Phase2Util.referencedPlanUID(al)).map(_._2.head)
        val rtplanList = rtplanRefList.flatMap(DicomSeries.getRtplan)
        rtplanList.headOption
      }
    }

    /**
     * Make a data set that contains all the relevant information associated with a SymmetryFlatness row.
     *
     * @param sf Basis of data
     * @return Convenient set of data
     */
    def makeDataSet(sf: SymmetryAndFlatness): Option[SymmetryAndFlatnessDataSet] = {
      try {
        logger.info("Making data set for: " + sf)
        val baseline = {
          val bl = SymmetryAndFlatness.getBaseline( //
            output.machinePK.get,
            span_mm = sf.span_mm,
            diameter_mm = sf.diameter_mm,
            RTImageSID_mm = sf.RTImageSID_mm,
            beamName = sf.beamName,
            hasPsm = sf.psmDataDate.isDefined,
            dataDate = dataDate,
            procedurePK = output.procedurePK)
          bl.get.baseline
        }

        val al: Option[AttributeList] = {
          val aa = alList.find(a => Util.sopOfAl(a).equals(sf.SOPInstanceUID))
          if (aa.isEmpty)
            logger.warn(s"Could not find RTIMAGE SOP ${sf.SOPInstanceUID}    Beam Name: ${sf.beamName}")
          aa
        }

        if (al.isDefined && rtplanAl.isDefined)
          Some(SymmetryAndFlatnessDataSet(sf, output, baseline, al.get, rtplanAl.get))
        else
          None
      } catch {
        case t: Throwable =>
          logger.error(s"Unexpected error on $sf : ${fmtEx(t)}")
          None
      }
    }

    val symFlatDataList = symFlatList.flatMap(sf => makeDataSet(sf)).sortBy(_.time)

    val elem = makeContent(output, symFlatDataList)
    val text = PrettyXML.xmlToText(elem)
    WebUtil.setResponse(text, response, Status.SUCCESS_OK)
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
  private def resultTable(beamData: SymmetryAndFlatness.SymmetryAndFlatnessHistory): Elem = {

    val isPsm = beamData.symmetryAndFlatness.psmDataDate.isDefined

    val beamHeaderElem: Elem = {
      val psmElem = {
        if (beamData.symmetryAndFlatness.psmDataDate.isEmpty)
          <span>
            <p></p>
          </span>
        else {
          <p>
            <span>with PSM</span>
          </p>
        }
      }

      <div class="row">
        <div class="col-md-3 col-md-offset-1">
          <h3 style={if (isPsm) psmStyle else ""}>
            {beamData.symmetryAndFlatness.beamName}{psmElem}
          </h3>
        </div>
        <div class="col-md-2 col-md-offset-1">
          {WebUtil.showPrecision}
        </div>
      </div>
    }

    val analysisElem =
      Seq(
        td(beamData.symmetryAndFlatness.transverseSymmetry(beamData.psmGrid)),
        td(beamData.symmetryAndFlatness.axialSymmetry(beamData.psmGrid)),
        td(beamData.symmetryAndFlatness.flatness(beamData.psmGrid)),
        td(beamData.symmetryAndFlatness.profileConstancy(beamData.psmGrid, beamData.baseline, beamData.psmGridBaseline))
      )

    val baselineElem =
      Seq(
        td(beamData.baseline.transverseSymmetry(beamData.psmGrid)),
        td(beamData.baseline.axialSymmetry(beamData.psmGrid)),
        td(beamData.baseline.flatness(beamData.psmGrid)),
        td(beamData.baseline.profileConstancy(beamData.psmGridBaseline, beamData.baseline, beamData.psmGridBaseline))
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
  private def inputTable(beamData: SymmetryAndFlatness.SymmetryAndFlatnessHistory, grid: Option[PSMGrid]): Elem = {

    val analysis = {
      if (grid.isDefined) {
        // @formatter:off
        <tr>
          <td>Analysis</td>
          {td(beamData.symmetryAndFlatness.beamResponseQaTop   (grid.get))}
          {td(beamData.symmetryAndFlatness.beamResponseQaBottom(grid.get))}
          {td(beamData.symmetryAndFlatness.beamResponseQaLeft  (grid.get))}
          {td(beamData.symmetryAndFlatness.beamResponseQaRight (grid.get))}
          {td(beamData.symmetryAndFlatness.beamResponseQaCenter(grid.get))}
        </tr>
        // @formatter:on
      }
      else {

        // @formatter:off
        <tr>
          <td>Analysis</td>
          {td(beamData.symmetryAndFlatness.top_cu   )}
          {td(beamData.symmetryAndFlatness.bottom_cu)}
          {td(beamData.symmetryAndFlatness.left_cu  )}
          {td(beamData.symmetryAndFlatness.right_cu )}
          {td(beamData.symmetryAndFlatness.center_cu)}
        </tr>
        // @formatter:on
      }
    }

    val baseline = {
      if (grid.isDefined) {
        // @formatter:off
        <tr>
          <td>Analysis</td>
          {td(beamData.baseline.beamResponseQaTop   (grid.get))}
          {td(beamData.baseline.beamResponseQaBottom(grid.get))}
          {td(beamData.baseline.beamResponseQaLeft  (grid.get))}
          {td(beamData.baseline.beamResponseQaRight (grid.get))}
          {td(beamData.baseline.beamResponseQaCenter(grid.get))}
        </tr>
        // @formatter:on
      }
      else {

        // @formatter:off
        <tr>
          <td>Analysis</td>
          {td(beamData.baseline.top_cu   )}
          {td(beamData.baseline.bottom_cu)}
          {td(beamData.baseline.left_cu  )}
          {td(beamData.baseline.right_cu )}
          {td(beamData.baseline.center_cu)}
        </tr>
        // @formatter:on
      }
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


  /**
   * Make HTML to show the raw beam data.
   *
   * @param beamData Data to show.
   * @return HTML to display.
   */
  // @formatter:off
  private def psmCalcTable(psmGrid: PSMGrid, beamData: SymmetryAndFlatness.SymmetryAndFlatnessHistory): Elem = {

    def psmToTr(name: String, beamToValue: PSMBeam => Double ) : Elem = {

      <tr>
        <td>{name}</td>
        {td(beamToValue(psmGrid.topBeam   ))}
        {td(beamToValue(psmGrid.bottomBeam))}
        {td(beamToValue(psmGrid.leftBeam  ))}
        {td(beamToValue(psmGrid.rightBeam ))}
        {td(beamToValue(psmGrid.centerBeam))}
      </tr>
    }

    <div style="margin:20px;">
      <center>
        <h3>PSM Processing</h3>
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

        {psmToTr("PSM Flood Field"   , (pb: PSMBeam) => pb.floodField_cu.get)}

        {psmToTr("PSM Whole Detector", (pb: PSMBeam) => pb.wholeDetector_cu.get)}

        {psmToTr("PSM Raw Image"     , (pb: PSMBeam) => pb.rawImage)}

        {psmToTr("PSM Beam Response" , (pb: PSMBeam) => pb.mean_cu)}

        {psmToTr("PSM Beam Response Normalized" , (pb: PSMBeam) => pb.beamResponseNormalized.get)}

        {psmToTr("PSM = Raw / Beam Response" , (pb: PSMBeam) => pb.psm )}

        <tr>
          <td>{beamData.symmetryAndFlatness.beamName} WD(QA)</td>
          {td(beamData.symmetryAndFlatness.top_cu   )}
          {td(beamData.symmetryAndFlatness.bottom_cu)}
          {td(beamData.symmetryAndFlatness.left_cu  )}
          {td(beamData.symmetryAndFlatness.right_cu )}
          {td(beamData.symmetryAndFlatness.center_cu)}
        </tr>

        <tr>
          <td>{beamData.symmetryAndFlatness.beamName} Raw Image(QA) = FF * WD(QA)</td>
          {td(beamData.symmetryAndFlatness.rawImageQaTop   (psmGrid))}
          {td(beamData.symmetryAndFlatness.rawImageQaBottom(psmGrid))}
          {td(beamData.symmetryAndFlatness.rawImageQaLeft  (psmGrid))}
          {td(beamData.symmetryAndFlatness.rawImageQaRight (psmGrid))}
          {td(beamData.symmetryAndFlatness.rawImageQaCenter(psmGrid))}
        </tr>

        <tr>
          <td>{beamData.symmetryAndFlatness.beamName} BR(top) = Raw Image(QA)/PSM</td>
          {td(beamData.symmetryAndFlatness.beamResponseQaTop   (psmGrid))}
          {td(beamData.symmetryAndFlatness.beamResponseQaBottom(psmGrid))}
          {td(beamData.symmetryAndFlatness.beamResponseQaLeft  (psmGrid))}
          {td(beamData.symmetryAndFlatness.beamResponseQaRight (psmGrid))}
          {td(beamData.symmetryAndFlatness.beamResponseQaCenter(psmGrid))}
        </tr>

      </table>
    </div>
  }
  // @formatter:on

  /**
   * Format data for just the given output and beam.  Build an HTML response to show it.
   *
   * @param valueMap Parameter list.  Already validated to have an output PK and beam name.
   * @param response Put HTML here.
   */
  private def beamData(valueMap: ValueMapT, response: Response): Unit = {
    val outputPK = valueMap(outputPKTag).toLong
    val output = Output.get(outputPK).get
    val machinePK = Output.get(outputPK).get.machinePK.get
    val beamName = valueMap(beamNameTag).replaceAll("%20", " ")
    val hasPsm = valueMap.contains(hasPsmTag) && valueMap(hasPsmTag).toBoolean

    val symFlat = {
      val list = SymmetryAndFlatness.getByOutput(outputPK)
      list
        .filter(sf =>
          sf.beamName.equalsIgnoreCase(beamName) &&
            ((sf.psmDataDate.isDefined && hasPsm) || (sf.psmDataDate.isEmpty && (!hasPsm)))
        )
        .head
    }


    val history = SymmetryAndFlatness.history( //
      machinePK = machinePK,
      span_mm = symFlat.span_mm,
      diameter_mm = symFlat.diameter_mm,
      beamName = beamName,
      hasPsm = hasPsm,
      RTImageSID_mm = symFlat.RTImageSID_mm,
      procedurePK = output.procedurePK
    )

    val beamData = history.find(h => h.output.outputPK.get == outputPK).get

    /** If PSM was used, then get the grid of beams. */
    val psmGrid: Option[PSMGrid] = {
      if (beamData.symmetryAndFlatness.psmDataDate.isDefined) {
        val list = PSMBeam.getByMachineAndTime(output.machinePK.get, beamData.symmetryAndFlatness.psmDataDate.get)
        if (list.isEmpty)
          None
        else {
          val grid = PSMGrid.makePSMGrid(list)
          Some(grid)
        }
      }
      else
        None
    }

    // @formatter:off
    val content = {
      <div class="row">
        <div class="col-md-4 col-md-offset-4">
          {resultTable(beamData)}
          {inputTable(beamData, psmGrid)}
        </div>
        <div class="col-md-6 col-md-offset-3" style="margin-bottom:70px;">
          {if (psmGrid.isDefined) psmCalcTable(psmGrid.get, beamData)}
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
          SF.beamData(valueMap, response)
        case _ if has(SF.outputPKTag) =>
          SF.collectData(valueMap, response)
        case _ if has(SF.baselineTag) =>
          SF.setBaseline(valueMap, response)
        case _ =>
          WebUtil.badRequest(response, message = "Invalid request", Status.CLIENT_ERROR_BAD_REQUEST)
      }
    } catch {
      case t: Throwable =>
        val msg =
          "Problem accessing data to display Symmetry, Flatness, and Constancy results.  Parameters: " + valueMap + "\nerror: " + fmtEx(
            t
          )
        logger.warn(msg)
        WebUtil.setResponse(msg, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

}
