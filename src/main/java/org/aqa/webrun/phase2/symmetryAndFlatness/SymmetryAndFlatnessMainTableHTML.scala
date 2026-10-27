package org.aqa.webrun.phase2.symmetryAndFlatness

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.PrettyXML
import org.aqa.DicomFile
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.Config
import org.aqa.db.Input
import org.aqa.db.SymmetryAndFlatness
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Output
import org.aqa.web.WebUtil
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Logging
import org.aqa.web.ViewOutput
import org.aqa.web.WebServer
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessSubHTML.titleDetails
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessSubHTML.titleImage
import org.restlet.Response
import org.restlet.data.Status

import java.io.File
import scala.collection.immutable
import scala.xml.Elem

case class SymmetryAndFlatnessMainTableHTML() extends Logging {

  private def titleAxialSymmetry =
    "Axial symmetry from top to bottom: (top-bottom)/bottom.  Max percent limit is " + Config.SymmetryPercentLimit

  private def titleTransverseSymmetry =
    "Transverse symmetry from right to left: (right-left)/left.  Max percent limit is " + Config.SymmetryPercentLimit

  private def titleFlatness =
    "Flatness: (max-min)/(max+min).  Max percent limit is " + Config.FlatnessPercentLimit

  private def titleProfileConstancy =
    "Profile Constancy.  Max percent limit is " + Config.ProfileConstancyPercentLimit

  private def pctRounded(pct: Double) = {
    val factor = 1000000000L
    (pct * factor).round.toDouble / factor
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

  private val symmetryPercentLimitColumn = {
    val v = Config.SymmetryPercentLimit
    val elem = {
      <td style="text-align: center;">
        {"%5.2f".format(v)}
      </td>
    }
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

  private def tableHead: Elem = {
    <thead>
      <tr>
        <th style="text-align: center;" title={SymmetryAndFlatnessSubHTML.titleDetails}>
          Beam
        </th>
        <th style="text-align: center;" title={SymmetryAndFlatnessSubHTML.titleImage}>
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

  /**
   * Generate the HTML for the column containing a link to the beam details.
   *
   * @param subDir         Directory containing just the data for sym-flat.
   * @param symFlatDataSet Data set to display when link is clicked.
   * @param withPsm        If true, use PSM, else no PSM.
   * @return HTML for cell for Beam column in table.
   */
  private def beamColumn(
                          subDir: File,
                          symFlatDataSet: SymmetryAndFlatnessDataSet,
                          withPsm: Boolean
                        ): Elem = {

    val errorClass = {
      val pass: Boolean = {
        if (withPsm)
          symFlatDataSet.symmetryAndFlatness.allWithPSMPass(symFlatDataSet.baseline)
        else
          symFlatDataSet.symmetryAndFlatness.allNoPSMPass(symFlatDataSet.baseline)
      }
      if (pass) "normal" else "danger"
    }
    val detailUrl = WebServer.urlOfResultsFile(
      SymmetryAndFlatnessHTML.beamHtmlFile(subDir, symFlatDataSet.symmetryAndFlatness, withPsm)
    )
    val pk = symFlatDataSet.symmetryAndFlatness.symmetryAndFlatnessPK.get
    val id = "baseline" + pk
    val baseline = symFlatDataSet.symmetryAndFlatness.isBaseline.toString

    val psmElem = {
      if (withPsm)
        <span>PSM</span>
      else
        <span></span>
    }

    val spanElem = {
      val span = symFlatDataSet.symmetryAndFlatness.getSpan_mm
      val e = WebUtil.setPrecisionAttr(<span>
        {Util.fmtDbl(span)}
      </span>, span)
      <p title={SymmetryAndFlatnessHTML.spanTitle}>Span:
        {e}
      </p>
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
        if (withPsm) SymmetryAndFlatnessSubHTML.psmStyle else ""
      }} class={errorClass} rowspan="4">
        <a href={detailUrl} title={titleDetails}>
          {symFlatDataSet.symmetryAndFlatness.beamName}<br>
          {Phase2Util.jawDescription(symFlatDataSet.al, symFlatDataSet.rtplan)}
        </br>{Phase2Util.angleDescription(symFlatDataSet.al)}{psmElem}
        </a>{spanElem}<p>
        <label title="Check to use this beam as a baseline." for={id}>Baseline</label>{input}
      </p>
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
        symFlatData.symmetryAndFlatness
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

  private def makeRow(symFlatData: SymmetryAndFlatnessDataSet): Seq[Elem] = {
    val withPsm = symFlatData.withPSM && symFlatData.symmetryAndFlatness.canDoPSM

    val subDir = SymmetryAndFlatnessHTML.makeSubDir(symFlatData.output.dir)

    // @formatter:off
      Seq(
        {
          <tr align="center">
            {beamColumn(subDir, symFlatData, withPsm)}
            {imageColumn(symFlatData)}
            <td style="text-align: center;" title={titleAxialSymmetry}>Axial Symmetry</td>
            {fmtBaselineColumn(symFlatData.baseline.getAxialSymmetry(withPsm))}
            {
            val pct = {
              symFlatData.symmetryAndFlatness.getAxialSymmetry(withPsm) - symFlatData.baseline.getAxialSymmetry(withPsm)
            }
            fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)}
            { symmetryPercentLimitColumn }
            { fmtValueColumn(symFlatData.symmetryAndFlatness.getAxialSymmetry(withPsm)) }
          </tr>
        }, {
          <tr>
            <td style="text-align: center;" title={titleTransverseSymmetry}>Transverse Symmetry</td>
            {fmtBaselineColumn(symFlatData.baseline.getTransverseSymmetry(withPsm))}
            {
              val pct = symFlatData.symmetryAndFlatness.getTransverseSymmetry(withPsm) - symFlatData.baseline.getTransverseSymmetry(withPsm)
              fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)
            }
            { symmetryPercentLimitColumn }
            {fmtValueColumn(symFlatData.symmetryAndFlatness.getTransverseSymmetry(withPsm))}
          </tr>
        }, {
          <tr>
            <td style="text-align: center;" title={titleFlatness}>Flatness</td>
            {fmtBaselineColumn(symFlatData.baseline.getFlatness(withPsm))}
            {
              val pct = symFlatData.symmetryAndFlatness.getFlatness(withPsm) - symFlatData.baseline.getFlatness(withPsm)
              fmtDifferenceColumn(pct, Config.FlatnessPercentLimit)}
            { flatnessPercentLimitColumn }
            {fmtValueColumn(symFlatData.symmetryAndFlatness.getFlatness(withPsm))}
          </tr>
        }, {
          <tr>
            <td style="text-align: center;" title={titleProfileConstancy}>Profile Constancy</td>
            { fmtBaselineColumn(symFlatData.baseline.getConstancy(withPsm, symFlatData.baseline)) }
            {
            val pct = {
              val a = symFlatData.symmetryAndFlatness.getConstancy(withPsm, symFlatData.baseline)
              val b = symFlatData.baseline.getConstancy(withPsm, symFlatData.baseline)
              a - b
            }

            fmtDifferenceColumn(pct, Config.ProfileConstancyPercentLimit)
            }
            { profileConstancyPercentLimitColumn }
            { fmtValueColumn(symFlatData.symmetryAndFlatness.getConstancy(withPsm, symFlatData.baseline)) }
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
  private def makeContent(output: Output, symFlatDataList: Seq[SymmetryAndFlatnessDataSet]): Elem = {

    // show link to CSV
    val csv: Elem = {
      val url = (new SymmetryAndFlatnessSubHTML).pathOf + "/SymmetryFlatnessAndConstancy.csv?" + SymmetryAndFlatnessSubHTML.csvTag + "=true&" + SymmetryAndFlatnessSubHTML.outputPKTag + "=" + output.outputPK.get
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


  // Get the RTPLAN for this result, if the RTPLAN can be found
  private def getRtplanAl(inputPK: Long, alList: Iterable[AttributeList]): Option[AttributeList] = {
    // If in test mode, then look in the input first.  This will only be defined if in test
    // mode and the RTPLAN is found in the input.
    val testPlan = {
      if (Config.TestMode) {
        val input = Input.get(inputPK).get

        // make sure that the files are there
        if (Util.listDirFiles(input.dir).isEmpty) {
          Input.restoreFilesFromDatabase(input.inputPK.get, input.dir)
        }

        val rtplanList = Util.listDirFiles(input.dir).map(f => new DicomFile(f)).flatMap(_.attributeList).filter(Util.isRtplan)
        rtplanList.headOption
      } else
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
   * Collect and format the data as HTML and return it.
   *
   * @param valueMap List of parsed parameters.
   * @return
   */
  def collectData(valueMap: ValueMapT, response: Response): Unit = {
    val output = Output.get(valueMap(SymmetryAndFlatnessSubHTML.outputPKTag).toLong).get
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
    val rtplanAl = getRtplanAl(output.inputPK, alList)

    /**
     * Get the attribute list of the given sym-flat instance.
     *
     * @param sf For this sym-flat.
     * @return Attribute list, if available.
     */
    def alOf(sf: SymmetryAndFlatness): Option[AttributeList] = {
      val aa = alList.find(a => Util.sopOfAl(a).equals(sf.SOPInstanceUID))
      if (aa.isEmpty)
        logger.warn(s"Could not find RTIMAGE SOP ${sf.SOPInstanceUID}    Beam Name: ${sf.beamName}")
      aa
    }

    /**
     * Make a data set that contains all the relevant information associated with a SymmetryFlatness row.
     *
     * @param sf Basis of data
     * @return Convenient set of data
     */
    def makeDataSet(sf: SymmetryAndFlatness, withPSM: Boolean): Option[SymmetryAndFlatnessDataSet] = {
      try {
        logger.info("Making data set for: " + sf)

        def baseline = {
          val bl = SymmetryAndFlatness.getBaseline( //
            output.machinePK.get,
            span_mm = sf.span_mm,
            diameter_mm = sf.diameter_mm,
            RTImageSID_mm = sf.RTImageSID_mm,
            beamName = sf.beamName,
            hasPSM = withPSM && sf.canDoPSM,
            dataDate = dataDate,
            procedurePK = output.procedurePK
          )
          bl.get.baseline
        }

        // if all this is true, then create a data set:
        //   The attribute list is available
        //   The RTPLAN is available
        //   Either PSM is not requested, or, PSM is requested and can be performed
        (alOf(sf), rtplanAl, (!withPSM) || (withPSM == sf.canDoPSM)) match {
          case (Some(al), Some(rtplan), true) =>
            Some(SymmetryAndFlatnessDataSet(sf, output, baseline, al, rtplan, withPSM))
          case _ =>
            None
        }

      } catch {
        case t: Throwable =>
          logger.error(s"Unexpected error on $sf : ${fmtEx(t)}")
          None
      }
    }

    val symFlatDataList = symFlatList.flatMap(sf => Seq(makeDataSet(sf, withPSM = false), makeDataSet(sf, withPSM = true)).flatten).sortBy(_.time)

    val elem = makeContent(output, symFlatDataList)
    val text = PrettyXML.xmlToText(elem)
    WebUtil.setResponse(text, response, Status.SUCCESS_OK)
  }

}
