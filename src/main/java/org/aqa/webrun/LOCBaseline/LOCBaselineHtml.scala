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

  /**
    * Make a little web page for looking at the DICOM.
    * @param al DICOM to show.
    * @param name Title name.
    * @param extendedData Related metadata.
    * @return Reference to the web page.
    */
  private def showDicom(al: AttributeList, name: String, extendedData: ExtendedData): Elem = {
    val dir = extendedData.output.dir
    val baseFileName = FileUtil.replaceInvalidFileNameCharacters(name, '_')

    val pngFile = new File(dir, baseFileName + ".png")

    val imgHtml = if (al.isImage) {
      val image = new DicomImage(al).toDeepColorBufferedImage(0.008)
      Util.writePng(image, pngFile)
      val html = {
        <a href={pngFile.getName}>
          <img width="256" src={pngFile.getName}/>
        </a>
      }
      html
    } else {
      <span></span>
    }

    val htmlFile = new File(dir, baseFileName + ".html")

    val content = {
      <div>
        <h3>
          {name}
        </h3>{imgHtml}<pre>
        {WebUtil.nl + DicomUtil.attributeListToString(al)}
      </pre>
      </div>
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), "LOC " + name)
    Util.writeFile(htmlFile, text)

    <center>
      <a href={htmlFile.getName}>View {name}<p/><img width="200" src={pngFile.getName}/></a>
    </center>
  }

  private def makeHtml(extendedData: ExtendedData, runReq: LOCBaselineRunReq): Elem = {
    val rtplan = DicomSeries.getRtplan(runReq.baselineOpen)
    <div>
      <h3>Success</h3>
      Both the open field and transmission files were saved for future LOC testing.
      {showDicom(runReq.baselineOpen, "OPEN", extendedData)}
      {showDicom(runReq.baselineOpen, "TRANS", extendedData)}
      {
      if (rtplan.isDefined)
        showDicom(rtplan.get, "RTPLAN", extendedData)
      else
        <div>RTPLAN not available</div>
    }
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
