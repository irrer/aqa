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

object LeafPositionHTML extends Logging {

  private val subDirName = "LeafPosition"

  private val mainHtmlFileName = subDirName + ".html"

  private def csv = "" // TODO

  private def tableHead = {
    <thead>
      <tr>
        <th style="text-align: center;" title="Click to view details"> Beam </th>
        <th style="text-align: center;" title="Largest deviation of measured leaf position from expected.">Max Offset</th>
      </tr>
    </thead>
  }

  private def makeBeamHtml(subDir: File, extendedData: ExtendedData, runReq: RunReq, beamResult: LeafPositionAnalysis.BeamResults): String = {
    val beamName = beamResult.beamName
    val htmlFileName = Phase2Util.beamNameToId(beamName) + ".html"
    val htmlFile = new File(subDir, htmlFileName)
    val url = WebServer.urlOfResultsFile(htmlFile)
    val maxOffset = beamResult.resultList.map(_.offset_mm).maxBy(_.abs)
    val derived = runReq.derivedMap(beamName)
    val horizontal = Phase2Util.isHorizontal(derived.attributeList)
    val leafWidthList_mm = LeafPositionAnalysis.getLeafWidthList_mm(
      LeafPositionAnalysis.leafSides(horizontal, beamName, derived.attributeList, derived.pixelCorrectedImage, runReq.rtplan.attributeList.get))

    val translator = new IsoImagePlaneTranslator(derived.attributeList)
    val image = LeafPositionAnalysisAnnotateImage.annotateImage(beamResult.resultList, horizontal, derived.pixelCorrectedImage, leafWidthList_mm, translator)
    val imageFileName = Phase2Util.beamNameToId(beamName) + ".jpg"
    val imageFile = new File(subDir, imageFileName)
    Util.writePng(image, imageFile)
    val imageUrl = WebServer.urlOfResultsFile(imageFile)

    val content = {
      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <h2>Beam { beamName + " Max offset: " + Util.fmtDbl(maxOffset) } </h2>
        </div>
        <div class="row">
          <div class="col-md-5 col-md-offset-1">
            <img class="img-responsive" src={ imageUrl }/>
          </div>
          <div class="col-md-5 col-md-offset-1">
            Table Table Table // TODO
          </div>
        </div>
      </div>
    }

    url
  }

  private def makeRow(subDir: File, extendedData: ExtendedData, runReq: RunReq, beamResult: LeafPositionAnalysis.BeamResults): Elem = {

    val beamHtmlUrl = makeBeamHtml(subDir, extendedData, runReq, beamResult)

    val dicomFile = runReq.rtimageMap(beamResult.beamName)
    val dicomImageHref = Phase2Util.dicomViewImageHref(dicomFile.attributeList.get, extendedData, runReq)

    <tr align="center">
      <td style="vertical-align: middle;" title="Click to view graphs and other details" rowspan="4">
        <a href={ beamHtmlUrl }>{ beamResult.beamName }<img src={ dicomImageHref } width="128"/></a>
      </td>
      <td align="center">
        { Util.fmtDbl(beamResult.resultList.map(_.offset_mm).maxBy(_.abs)) }
      </td>
    </tr>
  }

  private def makeMainHtml(subDir: File, extendedData: ExtendedData, runReq: RunReq, beamResultList: Seq[LeafPositionAnalysis.BeamResults], pass: Boolean): Elem = {
    val content = {
      <div class="col-md-6 col-md-offset-3">
        { csv }
        <br/>
        <table class="table table-bordered">
          { tableHead }
          { beamResultList.map(br => makeRow(subDir, extendedData, runReq, br)) }
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

  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, beamResultList: Seq[LeafPositionAnalysis.BeamResults], pass: Boolean) = {
    val subDir = new File(extendedData.output.dir, subDirName)
    subDir.mkdirs
    val mainHtmlFile = new File(subDir, mainHtmlFileName)
    val mainHtml = makeMainHtml(subDir, extendedData, runReq, beamResultList, pass)

    val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
    val html = Phase2Util.wrapSubProcedure(extendedData, mainHtml, LeafPositionAnalysis.subProcedureName, status, None, runReq)
    Util.writeFile(mainHtmlFile, html)

    val maxOffset = beamResultList.map(br => br.resultList).flatten.map(_.offset_mm).maxBy(_.abs)
    val sum = summary(mainHtmlFile, maxOffset, pass)
    ???
  }

}