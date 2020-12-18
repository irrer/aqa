package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.run.ProcedureStatus
import org.aqa.web.WebServer
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis._
import org.aqa.Config
import org.aqa.Logging

import java.io.File
import scala.collection.Seq
import scala.xml.Elem

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessSubHTML extends Logging {


  private def titleDetails = "Click to view graphs and other details"
  private def titleImage = "Click to view DICOM metadata"
  private def titleAxialSymmetry = "Axial symmetry from top to bottom: (top-bottom)/bottom.  Max percent limit is " + Config.SymmetryPercentLimit
  private def titleTransverseSymmetry = "Transverse symmetry from right to left: (right-left)/left.  Max percent limit is " + Config.SymmetryPercentLimit
  private def titleFlatness = "Flatness: (max-min)/(max+min).  Max percent limit is " + Config.FlatnessPercentLimit
  private def titleProfileConstancy = "Profile Constancy.  Max percent limit is " + Config.ProfileConstancyPercentLimit

  //  private val passImg = { <img title="Passed" src={ Config.passImageUrl } width="30"/> }
  //  private val failImg = { <img title="Failed" src={ Config.failImageUrl } width="30"/> }

  private def tableHead: Elem = {
    <thead>
      <tr>
        <th style="text-align: center;" title={ titleDetails }>
          Beam
        </th>
        <th style="text-align: center;" title={ titleImage }>
          Image
        </th>
        <th style="text-align: center;">
          Measurement
        </th>
        <th style="text-align: center;">
          Baseline %
        </th>
        <th style="text-align: center;" title="Difference of value from baseline">
          Difference<br/>
        </th>
        <th style="text-align: center;" title="Maximim allowed percent deviation from baseline">
          Percent<br/>
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

  private def detailsColumn(subDir: File, beamName: String, runReq: RunReq): Elem = {

    val al = {
      if (beamName.equals(Config.FloodFieldBeamName)) runReq.flood
      else runReq.derivedMap(beamName).al
    }

    val detailUrl = WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, beamName))
    <td style="vertical-align: middle;" title={ titleDetails } rowspan="4"><a href={ detailUrl }>{ beamName }<br/>{ Phase2Util.jawDescription(al, runReq.rtplan) }<br/>{ Phase2Util.angleDescription(al) }</a></td>
  }

  private def imageColumn(subDir: File, beamName: String, extendedData: ExtendedData, runReq: RunReq): Elem = {
    val dicomFile = runReq.rtimageMap(beamName)
    val dicomHref = Phase2Util.dicomViewHref(dicomFile, extendedData, runReq)
    val imgUrl = WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.annotatedImageFile(subDir, beamName))
    val imgSmall = { <img src={ imgUrl } width="100"/> }
    val ref = { <a href={ dicomHref }>{ imgSmall }</a> }
    <td style="text-align: center;vertical-align: middle;" title={ titleImage } rowspan="4">{ ref }</td>
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private val symmetryPercentLimitColumn = {
    <td style="text-align: center;">
      { Config.SymmetryPercentLimit.formatted("%5.2f") }
    </td>
  }

  private val flatnessPercentLimitColumn = {
    <td style="text-align: center;">
      { Config.FlatnessPercentLimit.formatted("%5.2f") }
    </td>
  }

  private val profileConstancyPercentLimitColumn = {
    <td style="text-align: center;">
      { Config.ProfileConstancyPercentLimit.formatted("%5.2f") }
    </td>
  }

  private def classOfStatus(status: ProcedureStatus.Value): String = {
    if (status.toString == ProcedureStatus.pass.toString) "normal" else "danger"
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def axialSymmetryColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    <td style="text-align: center;" title={ titleAxialSymmetry }>
      { result.axialSymmetry.formatted("%14.6f") }
    </td>
  }

  private def axialSymmetryBaselineColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    <td style="text-align: center;">
      { result.axialSymmetryBaseline.formatted("%14.6f") }
    </td>
  }

  private def axialSymmetryBaselinePercentColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    val pct = result.axialSymmetry - result.axialSymmetryBaseline
    <td style="text-align: center;" class={ classOfStatus(result.axialSymmetryStatus) }>
      { pctRounded(pct).formatted("%5.2f") }
    </td>
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def transverseSymmetryColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    <td style="text-align: center;" title={ titleTransverseSymmetry }>
      { result.transverseSymmetry.formatted("%14.6f") }
    </td>
  }

  private def transverseSymmetryBaselineColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    <td style="text-align: center;">
      { result.transverseSymmetryBaseline.formatted("%14.6f") }
    </td>
  }

  private def transverseSymmetryBaselinePercentColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    val pct = result.transverseSymmetry - result.transverseSymmetryBaseline
    <td style="text-align: center;" class={ classOfStatus(result.transverseSymmetryStatus) }>
      { pctRounded(pct).formatted("%5.2f") }
    </td>
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def flatnessColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    <td style="text-align: center;" title={ titleFlatness }>
      { result.flatness.formatted("%14.6f") }
    </td>
  }

  private def flatnessBaselineColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    <td style="text-align: center;">
      { result.flatnessBaseline.formatted("%14.6f") }
    </td>
  }

  private def flatnessBaselinePercentColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    val pct = result.flatness - result.flatnessBaseline
    <td style="text-align: center;" class={ classOfStatus(result.flatnessStatus) }>
      { pctRounded(pct).formatted("%5.2f") }
    </td>
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def profileConstancyColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    <td style="text-align: center;" title={ titleProfileConstancy }>
      { result.profileConstancy.formatted("%14.6f") }
    </td>
  }

  private def profileConstancyBaselineColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    <td style="text-align: center;">
      { result.profileConstancyBaseline.formatted("%14.6f") }
    </td>
  }

  private def profileConstancyBaselinePercentColumn(result: SymmetryAndFlatnessBeamResult): Elem = {
    val pct = result.profileConstancy - result.profileConstancyBaseline
    <td style="text-align: center;" class={ classOfStatus(result.profileConstancyStatus) }>
      { pctRounded(pct).formatted("%5.2f") }
    </td>
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  private def makeRow(subDir: File, extendedData: ExtendedData, resultBaseline: BeamResultBaseline, runReq: RunReq): Seq[Elem] = {
    val result = resultBaseline.result
    Seq(
      {
        <tr align="center">
          { detailsColumn(subDir, result.beamName, runReq) }
          { imageColumn(subDir, result.beamName, extendedData, runReq) }
          <td style="text-align: center;">Axial Symmetry</td>
          { axialSymmetryBaselineColumn(result) }
          { axialSymmetryBaselinePercentColumn(result) }
          { symmetryPercentLimitColumn }
          { axialSymmetryColumn(result) }
        </tr>
      },
      {
        <tr>
          <td style="text-align: center;">Transverse Symmetry</td>
          { transverseSymmetryBaselineColumn(result) }
          { transverseSymmetryBaselinePercentColumn(result) }
          { symmetryPercentLimitColumn }
          { transverseSymmetryColumn(result) }
        </tr>
      },
      {
        <tr>
          <td style="text-align: center;">Flatness</td>
          { flatnessBaselineColumn(result) }
          { flatnessBaselinePercentColumn(result) }
          { flatnessPercentLimitColumn }
          { flatnessColumn(result) }
        </tr>
      },
      {
        <tr>
          <td style="text-align: center;">Profile Constancy</td>
          { profileConstancyBaselineColumn(result) }
          { profileConstancyBaselinePercentColumn(result) }
          { profileConstancyPercentLimitColumn }
          { profileConstancyColumn(result) }
        </tr>
      })
  }

  def makeContent(subDir: File, extendedData: ExtendedData, resultList: List[BeamResultBaseline], runReq: RunReq): Elem = {

    val useAsBaselineButton: Elem = {
      val href = SymmetryAndFlatnessUseAsBaseline.path + "?outputPK=" + extendedData.output.outputPK.get
      val title = "Use the values here as the baseline for future symmetry and flatness analysis"
      val button = {
        <a class="btn btn-primary" href={ href } role="button" title={ title } style="margin:20px;">Use As Baseline</a>
      }
      button
    }

    //val j = resultList.map(br => br.result)

    SymmetryAndFlatnessCSV.makeCsvFile(extendedData, runReq, resultList, subDir)

    val csv: Elem = {
      val file = new File(subDir, SymmetryAndFlatnessCSV.csvFileName)
      <a href={ WebServer.urlOfResultsFile(file) } title="Download Symmetry and Flatness as a CSV viewable in a spreadsheet." style="margin:20px;">CSV</a>
    }

    val content = {
      <div class="col-md-6 col-md-offset-3">
        { useAsBaselineButton }
        { csv }
        <br/>
        <table class="table table-bordered">
          { tableHead }
          { resultList.map(rb => makeRow(subDir, extendedData, rb, runReq: RunReq)) }
        </table>
        <p/>
      </div>

    }
    content
  }

}
