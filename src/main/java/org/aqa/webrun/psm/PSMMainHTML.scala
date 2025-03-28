package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Logging
import org.aqa.webrun.ExtendedData
import org.aqa.Config
import org.aqa.Util
import org.aqa.web.WebUtil

import java.awt.Color
import java.io.File
import scala.xml.Elem

/**
  * Generate HTML page to show all PSM data.
  */
object PSMMainHTML extends Logging {

  private def fmt(d: Double): String = d.formatted("%8.2f").trim

  /**
    * Annotate the image with the mean CU in the center of the beam.  Use the existing buffered image.
    * @param result For this beam result.
    */
  private def annotateImage(result: PSMBeamAnalysisResult): Unit = {
    val gc = ImageUtil.getGraphics(result.bufferedImage)
    gc.setColor(Color.black)
    val trans = new IsoImagePlaneTranslator(result.rtimage)
    val center_pix = trans.iso2Pix(result.psmBeam.xCenter_mm, result.psmBeam.yCenter_mm)

    val text1 = "Mean CU"
    val text2 = fmt(result.psmBeam.mean_cu)
    val offset = ImageText.getTextDimensions(gc, text1).getHeight / 2
    ImageText.drawTextCenteredAt(gc, center_pix.getX, center_pix.getY - offset, text1)
    ImageText.drawTextCenteredAt(gc, center_pix.getX, center_pix.getY + offset, text2)

    val width = trans.iso2PixDistX(Config.PSMRadius_mm).toInt
    val height = trans.iso2PixDistY(Config.PSMRadius_mm).toInt
    gc.drawOval((center_pix.getX - width / 2).toInt, (center_pix.getY - height / 2).toInt, width, height)
  }

  /**
    * Make a web page for one result.
    * @param extendedData Metadata
    * @param result Result for one RTIMAGE.
    * @param pngFile png file.
    * @param htmlFile html file.
    */
  private def makeRtimageWebPage(extendedData: ExtendedData, result: PSMBeamAnalysisResult, pngFile: File, htmlFile: File): Unit = {

    val content = {
      <div>
        <div style="text-align:center;">
          <a href="../display.html">Back to main</a>
          <h3>{result.psmBeam.beamName}</h3>
          <img src={pngFile.getName} alt="Full DICOM Image" class="center"/>
        </div>
        <pre style="margin-top: 10px;margin-bottom: 100px;">
          {WebUtil.nl + DicomUtil.attributeListToString(result.rtimage)}
        </pre>
      </div>
    }
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM", runScript = None)
    Util.writeFile(htmlFile, text)
  }

  /**
    * Make a web page for one result.
    * @param extendedData Metadata
    * @param result Result for one RTIMAGE.
    * @param pngFile png file.
    * @param beamDir beam directory file.
    * @param htmlLink Points to web page for the result.
    */
  private def makeTableContent(extendedData: ExtendedData, result: PSMBeamAnalysisResult, pngFile: File, beamDir: File, htmlLink: String): Elem = {
    val src = { beamDir.getName + "/" + pngFile.getName }
    <td style="text-align:center;">
      <a href={htmlLink}>
        <h4>{result.psmBeam.beamName + " : " + fmt(result.psmBeam.mean_cu)}</h4>
        <img src={src} width="120" alt="Full DICOM Image" class="center"/>
      </a>
    </td>
  }

  /**
    * Make a web page for one result.
    * @param extendedData metadata.
    * @param result For this result.
    * @return An HTML snippet that shows a thumbnail and links to the page.
    */
  private def resultToHtml(extendedData: ExtendedData, result: PSMBeamAnalysisResult): Elem = {

    val beamDirName = "beams"

    val beamDir = new File(extendedData.output.dir, beamDirName)
    beamDir.mkdirs

    val fileNamePrefix = "Beam_" + FileUtil.replaceInvalidFileNameCharacters(result.psmBeam.beamName, '_')

    val pngFile = new File(beamDir, fileNamePrefix + ".png")
    val htmlFile = new File(beamDir, fileNamePrefix + ".html")
    val htmlLink = beamDirName + "/" + htmlFile.getName

    annotateImage(result)
    Config.applyWatermark(result.bufferedImage)
    Util.writePng(result.bufferedImage, pngFile)

    makeRtimageWebPage(extendedData, result, pngFile, htmlFile)

    val tableContent = makeTableContent(extendedData, result, pngFile, beamDir, htmlLink)

    tableContent
  }

  private def rowToElem(extendedData: ExtendedData, resultRow: Seq[PSMBeamAnalysisResult]): Elem = {
    <tr>
      {resultRow.map(result => resultToHtml(extendedData, result))}
    </tr>
  }

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

    makeQuickCSV(extendedData, rtplan, resultList)

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
            <table class="table responsive table-bordered" style="margin-top:25px;">
              {PSMUtil.layoutSpatiallyPSMResult(resultList.toList).map(row => rowToElem(extendedData, row))}
            </table>
          </div>
        </div>
      </div>
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM", runScript = None)
    val htmlFile = new File(extendedData.output.dir, "display.html")
    Util.writeFile(htmlFile, text)
  }
}
