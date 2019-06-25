package org.aqa.webrun.phase2.leafPosition

import org.aqa.Logging
import scala.xml.Elem
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.RunReq
import java.io.File
import org.aqa.Config
import org.aqa.web.WebServer
import org.aqa.Util
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.run.ProcedureStatus
import org.aqa.IsoImagePlaneTranslator
import org.aqa.db.LeafPosition
import org.aqa.webrun.phase2.symmetryAndFlatness.LeafPositionCSV

object LeafPositionHTML extends Logging {

  private val subDirName = "LeafPosition"

  private val mainHtmlFileName = subDirName + ".html"

  private val imageId = "LeafPosition"
  private val zoomScript = {
    """
    <script>
      $(document).ready(function(){ $('#""" + imageId + """').zoom(); });
    </script>
"""
  }

  /**
   * Make a table showing each leaf position.
   */
  private def resultsTable(resultList: Seq[LeafPosition]): Elem = {
    val posList = resultList.map(_.expectedEndPosition_mm).distinct.sorted
    val leafList = resultList.map(_.leafIndex).distinct.sorted
    def posHeader(pos: Double) = {
      <th>{ Util.fmtDbl(pos) }<br/>mm</th>
    }

    def offsetRow(leafIndex: Int) = {

      def fmtCell(lp: LeafPosition) = {
        val text = Util.fmtDbl(lp.offset_mm)
        if (lp.pass) {
          <td>{ text }</td>
        } else {
          <td class="danger">{ text }</td>
        }
      }
      <tr><td>{ leafIndex.toString }</td>{ resultList.filter(lp => lp.leafIndex == leafIndex).sortBy(_.expectedEndPosition_mm).map(lp => { fmtCell(lp) }) }</tr>
    }

    val header = {
      <thead>
        <tr>
          <th>Leaf<br/>Index</th>
          { posList.map(pos => posHeader(pos)) }
        </tr>
        { leafList.map(leafIndex => offsetRow(leafIndex)) }
      </thead>
    }

    <table class="table table-bordered">
      { header }
    </table>
  }

  /**
   * Generate the details page for the given beam and return URLs to the HTML page and image.  Also write the HTML page to disk.
   */
  private def makeBeamHtml(subDir: File, extendedData: ExtendedData, runReq: RunReq, beamResult: LeafPositionAnalysis.BeamResults): (String, String) = {
    val beamName = beamResult.beamName
    val htmlFileName = Phase2Util.textToId(beamName) + ".html"
    val htmlFile = new File(subDir, htmlFileName)
    val url = WebServer.urlOfResultsFile(htmlFile)
    val maxOffset = beamResult.resultList.map(_.offset_mm).maxBy(_.abs)
    val derived = runReq.derivedMap(beamName)
    val horizontal = Phase2Util.isHorizontal(derived.attributeList)
    val leafWidthList_mm = LeafPositionUtil.getLeafWidthList_mm(
      LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, runReq.rtplan.attributeList.get))

    val translator = new IsoImagePlaneTranslator(derived.attributeList)
    val image = LeafPositionAnnotateImage.annotateImage(beamResult.resultList, horizontal, derived.pixelCorrectedImage, leafWidthList_mm, translator)
    val imageFileName = Phase2Util.textToId(beamName) + ".png"
    val imageFile = new File(subDir, imageFileName)
    Util.writePng(image, imageFile)
    val imageUrl = WebServer.urlOfResultsFile(imageFile)
    val dicomHtmlHref = Phase2Util.dicomViewHref(derived.attributeList, extendedData, runReq)

    val content = {
      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <h2>Beam  <a href={ dicomHtmlHref } title="View DICOM">{ beamName }</a> Max offset: { Util.fmtDbl(maxOffset) } </h2>
        </div>
        <div class="row">
          <div class="col-md-6" id={ imageId }>
            Hover over image to zoom in.  Dashed lines show expected position, solid lines are measured position. All values in mm in the isoplane.<br/>
            <img class="img-responsive" src={ imageUrl }/>
          </div>
          <div class="col-md-6">
            { resultsTable(beamResult.resultList) }
          </div>
        </div>
      </div>
    }

    val html = Phase2Util.wrapSubProcedure(extendedData, content, LeafPositionAnalysis.subProcedureName, beamResult.status, Some(zoomScript), runReq)
    Util.writeFile(htmlFile, html)

    (url, imageUrl)
  }

  private def makeRow(subDir: File, extendedData: ExtendedData, runReq: RunReq, beamResult: LeafPositionAnalysis.BeamResults): Elem = {

    val htmlUrlAndImageUrl = makeBeamHtml(subDir, extendedData, runReq, beamResult)

    val dicomFile = runReq.rtimageMap(beamResult.beamName)
    val dicomImageHref = Phase2Util.dicomViewImageHref(dicomFile.attributeList.get, extendedData, runReq)
    val maxOffsetText = "Max Offset " + Util.fmtDbl(beamResult.resultList.map(_.offset_mm).maxBy(_.abs)) + " mm"

    val clss = if (beamResult.pass) "" else "danger"
    val td = {
      <td class={ clss } align="center" style="vertical-align: middle;" title="Click to view details">
        <div style="margin:20px;">
          <a href={ htmlUrlAndImageUrl._1 }><h4>{ beamResult.beamName }</h4><br/>{ maxOffsetText }<br/><img src={ htmlUrlAndImageUrl._2 } width="160"/></a>
        </div>
      </td>
    }

    td
  }

  private def makeMainHtml(subDir: File, extendedData: ExtendedData, runReq: RunReq, beamResultList: Seq[LeafPositionAnalysis.BeamResults], pass: Boolean): Elem = {

    val tdList = beamResultList.par.map(br => makeRow(subDir, extendedData, runReq, br)).toList
    val groupedTdList = tdList.zipWithIndex.groupBy(_._2 / 4).toList.sortBy(_._1).map(ig => ig._2.map(_._1))
    val csvUrl = LeafPositionCSV.makeCsvFile(extendedData, runReq, beamResultList, subDir)

    val content = {
      <div class="col-md-2 col-md-offset-3">
        <a href={ csvUrl } title="Click to download a spreadsheet of this data">CSV</a>
        <br/>
        <table class="table table-bordered">
          { groupedTdList.map(g => <tr> { g } </tr>) }
        </table>
        <p/>
      </div>
    }
    content
  }

  private def summary(mainHtmlFile: File, maxOffset: Double, pass: Boolean) = {
    val iconImage = if (pass) Config.passImageUrl else Config.failImageUrl
    val elem = {
      <div title="Click for details.">
        <a href={ WebServer.urlOfResultsFile(mainHtmlFile) }>
          Leaf Position max offset:{ Util.fmtDbl(maxOffset) }<br/>
          <img src={ iconImage } height="32"/>
        </a>
      </div>
    }
    elem
  }

  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, beamResultList: Seq[LeafPositionAnalysis.BeamResults], pass: Boolean): Elem = {
    val subDir = new File(extendedData.output.dir, subDirName)
    subDir.mkdirs
    val mainHtmlFile = new File(subDir, mainHtmlFileName)
    val mainHtml = makeMainHtml(subDir, extendedData, runReq, beamResultList, pass)

    val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
    val html = Phase2Util.wrapSubProcedure(extendedData, mainHtml, LeafPositionAnalysis.subProcedureName, status, None, runReq)
    Util.writeFile(mainHtmlFile, html)

    val maxOffset = beamResultList.map(br => br.resultList).flatten.map(_.offset_mm).maxBy(_.abs)
    val sum = summary(mainHtmlFile, maxOffset, pass)
    sum
  }

}
