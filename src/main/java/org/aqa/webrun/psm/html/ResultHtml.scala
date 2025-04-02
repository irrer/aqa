package org.aqa.webrun.psm.html

import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.webrun.psm.PSMUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.psm.PSMBeamAnalysisResult
import org.aqa.Config
import org.aqa.Util
import org.aqa.web.WebUtil

import java.awt.Color
import java.io.File
import scala.xml.Elem

class ResultHtml(extendedData: ExtendedData, resultList: Seq[PSMBeamAnalysisResult]) {

  private def fmt(d: Double): String = "%8.2f".format(d).trim

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

  private def rowToElem(extendedData: ExtendedData, resultRow: Seq[PSMBeamAnalysisResult]): Elem = {
    <tr>
      {resultRow.map(result => resultToHtml(extendedData, result))}
    </tr>
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

  def make(): Elem = {
    <table class="table responsive table-bordered" style="margin-top:25px;" title="Click for larger image and metadata.">
      {PSMUtil.layoutSpatiallyPSMResult(resultList.toList).map(row => rowToElem(extendedData, row))}
    </table>

  }
}
