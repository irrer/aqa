package org.aqa.webrun.phase2.symmetryAndFlatness

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.PrettyXML
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Output
import org.aqa.db.SymmetryAndFlatness
import org.aqa.run.ProcedureStatus
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlAdmin
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil.getValueMap
import org.aqa.webrun.phase2.Phase2Util
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.io.File
import scala.collection.Seq
import scala.collection.immutable
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

  private def detailsColumn(subDir: File, symFlatDataSet: SymmetryAndFlatnessDataSet): Elem = {
    val detailUrl = WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, symFlatDataSet.symmetryAndFlatness.beamName))
    <td style="vertical-align: middle;" title={titleDetails} rowspan="4">
      <a href={detailUrl}>
        {symFlatDataSet.symmetryAndFlatness.beamName}<br/>{Phase2Util.jawDescription(symFlatDataSet.al, symFlatDataSet.rtplan)}<br/>{Phase2Util.angleDescription(symFlatDataSet.al)}
      </a>
    </td>
  }

  private def imageColumn(symFlatData: SymmetryAndFlatnessDataSet): Elem = {
    val dicomHref = Phase2Util.dicomViewHref(symFlatData.al, symFlatData.symmetryAndFlatness.beamName, symFlatData.output.dir, symFlatData.rtplan)
    val imgUrl = WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.annotatedImageFile(SymmetryAndFlatnessHTML.makeSubDir(symFlatData.output.dir), symFlatData.symmetryAndFlatness.beamName))
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
    println(symFlatData.symmetryAndFlatness.beamName)
    Seq(
      {
        <tr align="center">
          {detailsColumn(subDir, symFlatData)}
          {imageColumn(symFlatData)}

          <td style="text-align: center;" title={titleAxialSymmetry}>Axial Symmetry</td>
          {fmtBaselineColumn(symFlatData.baseline.axialSymmetry)}
          {
            val pct = symFlatData.symmetryAndFlatness.axialSymmetry - symFlatData.baseline.axialSymmetry
            fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)
          }
          {symmetryPercentLimitColumn}
          {fmtValueColumn(symFlatData.symmetryAndFlatness.axialSymmetry)}
        </tr>
      },
      {
        <tr>
          <td style="text-align: center;" title={titleTransverseSymmetry}>Transverse Symmetry</td>
          {fmtBaselineColumn(symFlatData.baseline.transverseSymmetry)}
          {
            val pct = symFlatData.symmetryAndFlatness.transverseSymmetry - symFlatData.baseline.transverseSymmetry
            fmtDifferenceColumn(pct, Config.SymmetryPercentLimit)
          }
          {symmetryPercentLimitColumn}
          {fmtValueColumn(symFlatData.symmetryAndFlatness.transverseSymmetry)}
        </tr>
      },
      {
        <tr>
          <td style="text-align: center;" title={titleFlatness}>Flatness</td>
          {fmtBaselineColumn(symFlatData.baseline.flatness)}
          {
            val pct = symFlatData.symmetryAndFlatness.flatness - symFlatData.baseline.flatness
            fmtDifferenceColumn(pct, Config.FlatnessPercentLimit)
          }
          {flatnessPercentLimitColumn}
          {fmtValueColumn(symFlatData.symmetryAndFlatness.flatness)}
        </tr>
      },
      {
        <tr>
          <td style="text-align: center;" title={titleProfileConstancy}>Profile Constancy</td>
          {fmtBaselineColumn(symFlatData.baseline.profileConstancy(symFlatData.baseline))}
          {
            val pct = symFlatData.symmetryAndFlatness.profileConstancy(symFlatData.baseline) - symFlatData.baseline.profileConstancy(symFlatData.baseline)
            fmtDifferenceColumn(pct, Config.ProfileConstancyPercentLimit)
          }
          {profileConstancyPercentLimitColumn}
          {fmtValueColumn(symFlatData.symmetryAndFlatness.profileConstancy(symFlatData.baseline))}
        </tr>
      })
  }

  def makeContent(output: Output, symFlatDataList: Seq[SymmetryAndFlatnessDataSet]): Elem = {
    val useAsBaselineButton: Elem = {
      val href = SymmetryAndFlatnessUseAsBaseline.path + "?outputPK=" + output.outputPK.get
      val title = "Use the values here as the baseline for future symmetry and flatness analysis"
      val button = {
        <a class="btn btn-primary" href={href} role="button" title={title} style="margin:20px;">Use As Baseline</a>
      }
      button
    }

    val subDir = SymmetryAndFlatnessHTML.makeSubDir(output.dir)

    // SymmetryAndFlatnessCSV.makeCsvFile(extendedData, runReq, resultList, subDir)  TODO

    val csv: Elem = {
      val file = new File(subDir, SymmetryAndFlatnessCSV.csvFileName)
      <a href={WebServer.urlOfResultsFile(file)} title="Download Symmetry and Flatness as a CSV viewable in a spreadsheet." style="margin:20px;">CSV</a>
    }

    Trace.trace()
    val content = {
      <div>
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css"/>
        {useAsBaselineButton}{csv}<br/>
        <table class="table table-responsive table-bordered">
          {tableHead}{symFlatDataList.map(sfd => makeRow(sfd))}
        </table>
        <p/>
      </div>
    }
    Trace.trace()
    content
  }

  val outputPKTag = "outputPK"

  /**
   * Collect and format the data as HTML and return it.
   *
   * @param valueMap List of parsed parameters.
   * @return
   */
  private def collectData(valueMap: ValueMapT) = {
    val output = Output.get(valueMap(outputPKTag).toLong).get
    val dataDate = output.dataDate.get
    val symFlatList = SymmetryAndFlatness.getByOutput(output.outputPK.get)

    // list of all DICOM series referenced by SymmetryFlatness rows
    val dicomSeries = symFlatList.flatMap(sf => DicomSeries.getBySopInstanceUID(sf.SOPInstanceUID)).groupBy(_.seriesInstanceUID).map(uidDs => uidDs._2.head)

    // list of all AttributeList's referenced by SymmetryFlatness rows
    Trace.trace()
    val alList: immutable.Iterable[AttributeList] = dicomSeries.flatMap(ds => ds.attributeListList)
    Trace.trace()

    // list RTPLANs SOP UIDs referenced by SymmetryFlatness rows
    val rtplanSopList: Seq[String] = alList.map(al => Phase2Util.referencedPlanUID(al)).toSeq.distinct

    // list of all distinct RTPLAN DicomSeries referenced by SymmetryFlatness rows
    val rtplanDicomSeriesList: immutable.Iterable[DicomSeries] = rtplanSopList.flatMap(rtplanSop => DicomSeries.getBySopInstanceUID(rtplanSop)).groupBy(_.seriesInstanceUID).map(_._2.head)

    val rtplanAlList = rtplanDicomSeriesList.flatMap(ds => ds.attributeListList)

    /**
     * Make a data set that contains all the relevant information associated with a SymmetryFlatness row.
     *
     * @param sf Basis of data
     * @return Convenient set of data
     */
    def makeDataSet(sf: SymmetryAndFlatness): Option[SymmetryAndFlatnessDataSet] = {
      try {
        val baseline = SymmetryAndFlatness.getBaseline(output.machinePK.get, sf.beamName, dataDate).get
        val al: AttributeList = {
          val aa = alList.find(a => Util.sopOfAl(a).equals(sf.SOPInstanceUID))
          if (aa.isEmpty) throw new RuntimeException("Could not find RTIMAGE SOP " + sf.SOPInstanceUID)
          aa.get
        }
        val refRtplanSop = Phase2Util.referencedPlanUID(al)
        val rtplanAl = {
          val aa = rtplanAlList.find(a => Util.sopOfAl(a).equals(refRtplanSop))
          if (aa.isEmpty) throw new RuntimeException("Could not find RTPLAN SOP " + refRtplanSop)
          aa.get
        }
        Some(SymmetryAndFlatnessDataSet(sf, output, baseline, al, rtplanAl))
      }
      catch {
        case t =>
          logger.error("Unexpected error: " + fmtEx(t))
          None
      }
    }

    Trace.trace()
    val symFlatDataList = symFlatList.flatMap(sf => makeDataSet(sf)).sortBy(_.time)

    makeContent(output, symFlatDataList)
  }

}

class SymmetryAndFlatnessSubHTML extends Restlet with Logging with SubUrlAdmin {

  /**
   * If the incoming request is for the given handler, then handle it and return true.
   *
   * @param request  User request.
   * @param response Put results and status here.
   * @return True if handled, false if not relevant.
   */
  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      val text = PrettyXML.xmlToText(SymmetryAndFlatnessSubHTML.collectData(valueMap))
      WebUtil.setResponse(text, response, Status.SUCCESS_OK)
    }
    catch {
      case t: Throwable =>
        val msg = "Problem accessing data to display Symmetry, Flatness, and Constancy results.  Parameters: " + valueMap + "\nerror: " + fmtEx(t)
        logger.warn(msg)
        WebUtil.setResponse(msg, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

}
