package org.aqa.webrun.psm.html

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.webrun.ExtendedData
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.psm.PSMBeamAnalysisResult
import org.aqa.webrun.psm.PSMDicom
import org.aqa.webrun.psm.PSMGradientAscent

import java.awt.Color
import java.io.File
import scala.xml.Elem

/**
  * Generate HTML page to show all PSM data.
  */

class PSMMainHTML(
    extendedData: ExtendedData,
    rtplan: AttributeList,
    resultList: Seq[PSMBeamAnalysisResult],
    psmGradientAscent: PSMGradientAscent,
    ffAl: AttributeList,
    ffImg: DicomImage,
    wdAl: AttributeList,
    wdImg: DicomImage,
    rawAl: AttributeList,
    rawImg: DicomImage,
    brAl: AttributeList,
    brImg: DicomImage,
    psmAl: AttributeList,
    psmImg: DicomImage
) extends Logging {

  def make(): Unit = {

    val resultHtml = new ResultHtml(extendedData, resultList)

    /*
    val ffHtml = new FFHtml(extendedData, ffAl, ffImg)
    val wdHtml = new WDHtml(extendedData, wdAl, wdImg)
    val rawHtml = new RawHtml(extendedData, rawAl, rawImg)
    val brHtml = new BRHtml(extendedData, brAl, brImg)
    val psmHtml = new PSMHtml(extendedData, psmAl, psmImg)
     */

    val imageHtml = new ImageHTML(
      extendedData: ExtendedData,
      rtplan: AttributeList,
      resultList: Seq[PSMBeamAnalysisResult],
      psmGradientAscent: PSMGradientAscent,
      ffAl: AttributeList,
      ffImg: DicomImage,
      wdAl: AttributeList,
      wdImg: DicomImage,
      rawAl: AttributeList,
      rawImg: DicomImage,
      brAl: AttributeList,
      brImg: DicomImage,
      psmAl: AttributeList,
      psmImg: DicomImage
    )

    val imageStuff = imageHtml.make()

    val content = {
      <div>
        <div class="row">
          <div class="col-md-2 col-md-offset-1" >
            {WebUtil.showPrecision}
          </div>
        </div>
          <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <table class="table responsive table-bordered" style="margin-top:25px;">
              {imageStuff._1}
            </table>
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            {resultHtml.make()}
          </div>
        </div>
      </div>
    }

    val js =
      s"""<script>
         |${imageStuff._2}
         |</script>
         |""".stripMargin

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM", c3 = true, runScript = Some(js))
    val htmlFile = new File(extendedData.output.dir, "display.html")
    Util.writeFile(htmlFile, text)

  }

}

object PSMMainHTML extends Logging {

  /**
    * Make a web page for viewing the RTPLAN and return an HTML snippet to navigate to it.
    * @param extendedData Metadata.
    * @param rtplan For this DICOM RTPLAN.
    * @return HTML reference.
    */
  private def makeRtplanHtml(extendedData: ExtendedData, rtplan: AttributeList): Elem = {
    val htmlFile = new File(extendedData.output.dir, "rtplan.html")
    val content = {
      <div class="row">
        <div class="col-md-10 col-md-offset-1" >
          <a href="display.html">Back to main</a>
          <h3>RTPLAN for PSM</h3>
          <pre style="margin-top: 10px;margin-bottom: 100px;">
            {WebUtil.nl + DicomUtil.attributeListToString(rtplan)}
          </pre>
        </div>
      </div>
    }
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM RTPLAN", runScript = None)
    Util.writeFile(htmlFile, text)

    val reference = {
      <a href={htmlFile.getName}>View RTPLAN</a>
    }
    reference
  }

  /**
    * Make a CSV as part of development.
    * TODO: This should be either:
    *     - be part of the other CSV downloads
    *     - made  better to include metadata.
    * @param extendedData Meta data.
    * @param rtplan DICOM RTPLAN.
    * @param resultList List of results.
    */
  /*
  private def makeQuickCSV(extendedData: ExtendedData, rtplan: AttributeList, resultList: Seq[PSMBeamAnalysisResult]): Unit = {

    val row1 = Seq("Beam Name", "X Center", "Y Center", "Mean CU")

    def toRow(result: PSMBeamAnalysisResult): String = {
      val beam = result.psmBeam
      val list = Seq(beam.beamName, beam.xCenter_mm.toString, beam.yCenter_mm.toString, beam.mean_cu.toString)
      list.mkString(",")
    }

    val text = (row1.mkString(",") +: resultList.map(toRow)).mkString("\n")

    val csvFile = new File(extendedData.output.dir, "PSM.csv")

    Util.writeFile(csvFile, text)
  }
   */

  /**
    * Write all the HTML.
    * @param extendedData Metadata.
    * @param resultList List of results.
    */
  def makeHtml(
      extendedData: ExtendedData,
      rtplan: AttributeList,
      resultList: Seq[PSMBeamAnalysisResult],
      psmGradientAscent: PSMGradientAscent,
      ffAl: AttributeList,
      ffImg: DicomImage,
      wdAl: AttributeList,
      wdImg: DicomImage,
      rawAl: AttributeList,
      rawImg: DicomImage,
      brAl: AttributeList,
      brImg: DicomImage,
      psmAl: AttributeList,
      psmImg: DicomImage
  ): Unit = {

    // makeQuickCSV(extendedData, rtplan, resultList)

    val beamResponseBufferedImage = brImg.toBufferedImage(Color.white)
    val beamResponseFile = new File(extendedData.output.dir, "BeamResponse.png")
    Util.writePng(beamResponseBufferedImage, beamResponseFile)

    val planElem = makeRtplanHtml(extendedData, rtplan)

    val compositeImageHTML = new PSMCompositeImageHTML(extendedData)
    compositeImageHTML.make(resultList)

    val dicomImage = psmGradientAscent.psmInterpolator.normalizedDicomImage

    val dicomFileName = extendedData.machine.id + "_" + Util.timeAsFileName(extendedData.output.dataDate.get) + "_BeamResponse.dcm"

    val dicomFile = new File(extendedData.output.dir, dicomFileName)
    val dicom = PSMDicom.psmToDicom(dicomImage, resultList.head.rtimage, "BeamResponse", "Beam Response normalized")

    DicomUtil.writeAttributeListToFile(dicom, dicomFile, "AQA")
    logger.info("Wrote PSM DICOM file " + dicomFile.getAbsolutePath)

    val beamResponseImageHTML = new PSMBeamResponseHTML(extendedData, dicomImage, psmGradientAscent.getMaxPoint_iso, resultList)
    beamResponseImageHTML.make()

    // TODO
    // val wholeImageHTML = new PSMWholeImageHTML(extendedData, dicomImage, ascent.getMaxPoint_iso, resultList)
    // wholeImageHTML.make()

    val resultHTML = new ResultHtml(extendedData, resultList)

    val content = {
      // <div style="display:flex; align-items:center; justify-content:center; margin-bottom:200px;">
      <div>
        <div class="row">
          <div class="col-md-2">
            {planElem}
          </div>
          <div class="col-md-4">
            <a href={dicomFileName}>Download DICOM version of normalized PSM</a>
          </div>
        </div>
        <div class="row">
          <div class="col-md-5 col-md-offset-1" title="Click for larger image.">
            <a href={compositeImageHTML.htmlFileName}>
              <h4 style="text-align: center;">Mean CU Readings for each beam center</h4>
              <img src={compositeImageHTML.imageFileName} class="img-responsive fit-image" style="margin-right:20px;"/>
            </a>
          </div>
          <div class="col-md-5" title="Click for larger image.">
            <a href={beamResponseImageHTML.htmlFileName}>
              <h4 style="text-align: center;">Contoured Beam Response</h4>
              <img src={beamResponseImageHTML.imageFileName} class="img-responsive fit-image" style="margin-left:20px;"/>
            </a>
          </div>
        </div>

        <div class="row">
          <div class="col-md-10 col-md-offset-1" title="Click for larger image.">
            {resultHTML.make()}
          </div>
        </div>
      </div>
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM", runScript = None)
    val htmlFile = new File(extendedData.output.dir, "display.html")
    Util.writeFile(htmlFile, text)
  }
}
