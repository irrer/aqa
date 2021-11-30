package org.aqa.webrun.LOCBaseline

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Output
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData

import java.io.File
import scala.xml.Elem

object LOCBaselineHtml extends Logging {

  private def writeMetaFile(al: AttributeList, name: String, extendedData: ExtendedData, extraContent: Elem): String = {
    val dir = extendedData.output.dir
    val baseFileName = FileUtil.replaceInvalidFileNameCharacters(name, '_')

    val metaFileName = baseFileName + ".html"
    val htmlMetaFile = new File(dir, metaFileName)
    val content = {
      <div>
        <h3>{name}</h3>
        {extraContent}
        <pre>{WebUtil.nl + DicomUtil.attributeListToString(al)}</pre>
      </div>
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), "LOC " + name)
    Util.writeFile(htmlMetaFile, text)
    metaFileName
  }

  private def showNonImageDicom(al: AttributeList, name: String, extendedData: ExtendedData): Elem = {
    val fileName = writeMetaFile(al, name, extendedData, <span/>)
    <center>
      <a href={fileName}>View {name}<p/></a>
    </center>
  }

  /**
    * Make a little web page for looking at the image DICOM and metadata.
    * @param al DICOM to show.
    * @param name Title name.
    * @param extendedData Related metadata.
    * @return Reference to the web page.
    */
  private def showImageDicom(al: AttributeList, name: String, extendedData: ExtendedData): Elem = {
    val dir = extendedData.output.dir
    val baseFileName = FileUtil.replaceInvalidFileNameCharacters(name, '_')

    def makePng(): String = {
      val fileName = baseFileName + ".png"
      val pngFile = new File(dir, fileName)
      val image = new DicomImage(al).toDeepColorBufferedImage(0.08)
      Util.writePng(image, pngFile)
      fileName
    }

    val pngFileName = makePng()

    val imgOnlyName = writeMetaFile(al, name + " Image", extendedData, <img src={pngFileName} style="margin-top: 20px;margin-bottom: 35px;"/>)

    val imgHtmlWithMetadata = {
      val html = {
        <a href={imgOnlyName}>
          <img width="256" src={pngFileName} style="margin: 35px;"/>
        </a>
      }
      html
    }

    val htmlMetaFileName = writeMetaFile(al, name, extendedData, imgHtmlWithMetadata)

    <center>
      <a href={htmlMetaFileName}>View {name}<p/><img width="200" src={pngFileName}/></a>
    </center>
  }

  private def makeHtml(extendedData: ExtendedData, runReq: LOCBaselineRunReq): Elem = {
    val rtplanDicom = {
      val rtplan = DicomSeries.getRtplan(runReq.baselineOpen)

      if (rtplan.isDefined)
        showNonImageDicom(rtplan.get, "RTPLAN", extendedData)
      else
        <div>RTPLAN not available</div>
    }

    <div>
      <h3>Success</h3>
      Both the open field and transmission files were saved for future LOC testing.
      <div class="row" style="margin-top: 50px;">
        <div class="col-md-3"> {showImageDicom(runReq.baselineOpen, "OPEN", extendedData)} </div>
        <div class="col-md-3"> {showImageDicom(runReq.baselineOpen, "TRANS", extendedData)} </div>
        <div class="col-md-3"> {rtplanDicom} </div>
      </div>
    </div>
  }

  def write(extendedData: ExtendedData, runReq: LOCBaselineRunReq): Unit = {
    Trace.trace()
    val content = makeHtml(extendedData, runReq)
    Trace.trace()
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), "LOC Baseline")
    Trace.trace()
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Trace.trace()
    Util.writeFile(file, text)
    Trace.trace()
  }

}
