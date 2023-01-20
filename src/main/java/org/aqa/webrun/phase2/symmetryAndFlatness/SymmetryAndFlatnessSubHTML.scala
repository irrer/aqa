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
import org.aqa.db.Output
import org.aqa.db.SymmetryAndFlatness
import org.aqa.db.User
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlAdmin
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil.getValueMap
import org.aqa.webrun.phase2.Phase2Util
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

  /** PK of the SymmetryAndFlatness row to have it's baseline changed. */
  private val symFlatPKTag = "symFlatPK"

  /** Indicates that caller is requesting a CSV of the results. */
  private val csvTag = "csv"

  /** Indicates the value of the new baseline.  Must be either true or false. */
  private val baselineTag = "baseline"

  /** Indicates which set of data to retrieve for display as a web page. */
  private val outputPKTag = "outputPK"

  /** Used to specify the name of a beam in an URL. */
  val beamNameTag = "BeamName"

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
    val errorClass = if (symFlatDataSet.symmetryAndFlatness.allPass(symFlatDataSet.baseline)) "normal" else "danger"
    val detailUrl = WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, symFlatDataSet.symmetryAndFlatness.beamName))
    val pk = symFlatDataSet.symmetryAndFlatness.symmetryAndFlatnessPK.get
    val id = "baseline" + pk
    val baseline = symFlatDataSet.symmetryAndFlatness.isBaseline.toString

    val input =
      if (symFlatDataSet.symmetryAndFlatness.isBaseline) {
        <input value={baseline} type="checkbox" id={id} onclick={"setBaselineState(this, " + pk + ")"} checked={baseline}/>
      } else {
        <input value={baseline} type="checkbox" id={id} onclick={"setBaselineState(this, " + pk + ")"}/>
      }

    val elem = {
      <td style="vertical-align: middle;" class={errorClass} rowspan="4">
        <a href={detailUrl} title={titleDetails}>
          {symFlatDataSet.symmetryAndFlatness.beamName}<br/>{Phase2Util.jawDescription(symFlatDataSet.al, symFlatDataSet.rtplan)}<br/>{Phase2Util.angleDescription(symFlatDataSet.al)}
        </a>
        <p></p>
        <label title="Check to use this beam as a baseline." for={id}>Baseline</label>{input}
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
        symFlatData.symmetryAndFlatness.beamName
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
    <td style="text-align: center;">
      {Config.SymmetryPercentLimit.formatted("%5.2f")}
    </td>
  }

  private val flatnessPercentLimitColumn = {
    <td style="text-align: center;">
      {Config.FlatnessPercentLimit.formatted("%5.2f")}
    </td>
  }

  private val profileConstancyPercentLimitColumn = {
    <td style="text-align: center;">
      {Config.ProfileConstancyPercentLimit.formatted("%5.2f")}
    </td>
  }

  private def fmtBaselineColumn(baseline: Double): Elem = {
    <td style="text-align: center;" title={"Baseline % : " + baseline.formatted("%10.8f")}>
      {pctRounded(baseline).formatted("%5.3f").trim}
    </td>
  }

  private def fmtDifferenceColumn(percent: Double, limit: Double): Elem = {
    val errorClass = if (percent.abs > limit.abs) "danger" else "normal"
    <td style="text-align: center;" class={errorClass} title={"Difference: " + percent.formatted("%10.8f")}>
      {pctRounded(percent).formatted("%5.2f").trim}
    </td>
  }

  private def fmtValueColumn(value: Double): Elem = {
    <td style="text-align: center;" title={"Value % : " + value.formatted("%10.8f")}>
      {pctRounded(value).formatted("%5.3f").trim}
    </td>
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
          {fmtBaselineColumn(symFlatData.baseline.axialSymmetry)}
          {
            val pct = symFlatData.symmetryAndFlatness.axialSymmetry - symFlatData.baseline.axialSymmetry
            fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)}
          { symmetryPercentLimitColumn }
          { fmtValueColumn(symFlatData.symmetryAndFlatness.axialSymmetry) }
        </tr>
      }, {
        <tr>
          <td style="text-align: center;" title={titleTransverseSymmetry}>Transverse Symmetry</td>
          {fmtBaselineColumn(symFlatData.baseline.transverseSymmetry)}
          {
            val pct = symFlatData.symmetryAndFlatness.transverseSymmetry - symFlatData.baseline.transverseSymmetry
            fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)
          }
          { symmetryPercentLimitColumn }
          {fmtValueColumn(symFlatData.symmetryAndFlatness.transverseSymmetry)}
        </tr>
      }, {
        <tr>
          <td style="text-align: center;" title={titleFlatness}>Flatness</td>
          {fmtBaselineColumn(symFlatData.baseline.flatness)}
          {
            val pct = symFlatData.symmetryAndFlatness.flatness - symFlatData.baseline.flatness
            fmtDifferenceColumn(pct, Config.FlatnessPercentLimit)}
          { flatnessPercentLimitColumn }
          {fmtValueColumn(symFlatData.symmetryAndFlatness.flatness)}
        </tr>
      }, {
        <tr>
          <td style="text-align: center;" title={titleProfileConstancy}>Profile Constancy</td>
          { fmtBaselineColumn(symFlatData.baseline.profileConstancy(symFlatData.baseline)) }
          {
            val pct = symFlatData.symmetryAndFlatness.profileConstancy(symFlatData.baseline ) - symFlatData.baseline.profileConstancy(symFlatData.baseline)
            fmtDifferenceColumn(pct, Config.ProfileConstancyPercentLimit)
          }
          { profileConstancyPercentLimitColumn }
          { fmtValueColumn(symFlatData.symmetryAndFlatness.profileConstancy(symFlatData.baseline)) }
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
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css"/>{csv}<br/>
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
    val alList: immutable.Iterable[AttributeList] =
      dicomSeries.flatMap(ds => ds.attributeListList)

    // list RTPLANs SOP UIDs referenced by SymmetryFlatness rows
    val rtplanSopList: Seq[String] =
      alList.map(al => Phase2Util.referencedPlanUID(al)).toSeq.distinct

    // list of all distinct RTPLAN DicomSeries referenced by SymmetryFlatness rows
    val rtplanDicomSeriesList: immutable.Iterable[DicomSeries] = rtplanSopList
      .flatMap(rtplanSop => DicomSeries.getBySopInstanceUID(rtplanSop))
      .groupBy(_.seriesInstanceUID)
      .map(_._2.head)

    val rtplanAlList = rtplanDicomSeriesList.flatMap(ds => ds.attributeListList)

    /**
      * Make a data set that contains all the relevant information associated with a SymmetryFlatness row.
      *
      * @param sf Basis of data
      * @return Convenient set of data
      */
    def makeDataSet(sf: SymmetryAndFlatness): Option[SymmetryAndFlatnessDataSet] = {
      try {
        logger.info("Making data set for: " + sf)
        val baseline = SymmetryAndFlatness.getBaseline(output.machinePK.get, sf.beamName, dataDate, output.procedurePK).get.baseline

        val al: Option[AttributeList] = {
          val aa = alList.find(a => Util.sopOfAl(a).equals(sf.SOPInstanceUID))
          if (aa.isEmpty)
            logger.warn(s"Could not find RTIMAGE SOP ${sf.SOPInstanceUID}    Beam Name: ${sf.beamName}")
          aa
        }

        sf.toString

        val rtplanAl: Option[AttributeList] = {
          if (al.isDefined) {
            val refRtplanSop = Phase2Util.referencedPlanUID(al.get)
            val aa = rtplanAlList.find(a => Util.sopOfAl(a).equals(refRtplanSop))
            if (aa.isEmpty)
              logger.warn(s"Could not find RTPLAN SOP $refRtplanSop   Beam name: ${sf.beamName} List of RTPLAN SOPs size: ${rtplanAlList.size} : " + rtplanAlList.map(Util.sopOfAl).mkString("  "))
            aa
          } else
            None
        }

        if (al.isDefined && rtplanAl.isDefined)
          Some(SymmetryAndFlatnessDataSet(sf, output, baseline, al.get, rtplanAl.get))
        else
          None
      } catch {
        case t: Throwable =>
          logger.error("Unexpected error: " + fmtEx(t))
          None
      }
    }

    val symFlatDataList =
      symFlatList.flatMap(sf => makeDataSet(sf)).sortBy(_.time)

    val elem = makeContent(output, symFlatDataList)
    val text = PrettyXML.xmlToText(elem)
    WebUtil.setResponse(text, response, Status.SUCCESS_OK)
  }

  /**
    * Format an HTML td with a double value.  Add title to show more precision.
    * @param d Value to format.
    * @return td element.
    */
  private def td(d: Double) = {
    <td title={d.toString}>{Util.fmtDbl(d)}</td>
  }

  /**
    * Make HTML to show the results of the calculation of the beam data.
    * @param beamData Data to show.
    * @return HTML to display.
    */
  private def resultTable(beamData: SymmetryAndFlatness.SymmetryAndFlatnessHistory): Elem = {

    <div style="margin:20px;">
      <center><h3>Results</h3></center>
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
          <td>Analysis</td>
          {td(beamData.symmetryAndFlatness.transverseSymmetry)}
          {td(beamData.symmetryAndFlatness.axialSymmetry)}
          {td(beamData.symmetryAndFlatness.flatness)}
          {td(beamData.symmetryAndFlatness.profileConstancy(beamData.baseline))}
        </tr>
        <tr>
          <td>Baseline</td>
          {td(beamData.baseline.transverseSymmetry)}
          {td(beamData.baseline.axialSymmetry)}
          {td(beamData.baseline.flatness)}
          {td(beamData.baseline.profileConstancy(beamData.baseline))}
        </tr>
      </table>
    </div>
  }

  /**
    * Make HTML to show the raw beam data.
    * @param beamData Data to show.
    * @return HTML to display.
    */
  private def inputTable(beamData: SymmetryAndFlatness.SymmetryAndFlatnessHistory): Elem = {
    <div style="margin:20px;">
      <center><h3>Inputs</h3></center>
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
        <tr>
          <td>Analysis</td>
          {td(beamData.symmetryAndFlatness.top_cu)}
          {td(beamData.symmetryAndFlatness.bottom_cu)}
          {td(beamData.symmetryAndFlatness.left_cu)}
          {td(beamData.symmetryAndFlatness.right_cu)}
          {td(beamData.symmetryAndFlatness.center_cu)}
        </tr>
        <tr>
          <td>Baseline</td>
          {td(beamData.baseline.top_cu)}
          {td(beamData.baseline.bottom_cu)}
          {td(beamData.baseline.left_cu)}
          {td(beamData.baseline.right_cu)}
          {td(beamData.baseline.center_cu)}
        </tr>
      </table>
    </div>
  }

  /**
    * Format data for the just the given output and beam.  Build an HTML response to show it.
    *
    * @param valueMap Parameter list.  Already validated to have an output PK and beam name.
    * @param response Put HTML here.
    */
  private def beamData(valueMap: ValueMapT, response: Response): Unit = {
    val outputPK = valueMap(outputPKTag).toLong
    val output = Output.get(outputPK).get
    val machinePK = Output.get(outputPK).get.machinePK.get
    val beamName = valueMap(beamNameTag).replaceAll("%20", " ")
    val history = SymmetryAndFlatness.history(machinePK, beamName, output.procedurePK)
    val beamData = history.find(h => h.output.outputPK.get == outputPK).get

    val content = {
      <div class="row">
        <div class="col-md-4 col-md-offset-4">
          {resultTable(beamData)}
          {inputTable(beamData)}
        </div>
      </div>
    }

    val text = PrettyXML.xmlToText(content)
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
        case _ if has(SF.csvTag)                             => SF.makeCsv(valueMap, response)
        case _ if has(SF.outputPKTag) && has(SF.beamNameTag) => SF.beamData(valueMap, response)
        case _ if has(SF.outputPKTag)                        => SF.collectData(valueMap, response)
        case _ if has(SF.baselineTag)                        => SF.setBaseline(valueMap, response)
        case _                                               => WebUtil.badRequest(response, message = "Invalid request", Status.CLIENT_ERROR_BAD_REQUEST)
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
