package org.aqa.webrun.gapSkew

import com.pixelmed.dicom
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData

import java.io.File

case class DicomHtml(extendedData: ExtendedData) {

  def makeDicomContent(al: dicom.AttributeList, title: String, imageFileName: Option[String] = None): String = {
    val baseFileName = title.replaceAll("[^a-zA-Z0-9_]", "_")
    val htmlFileName = "DICOM_" + baseFileName + ".html"
    val dicomFileName = "DICOM_" + baseFileName + ".dcm"

    val imageRef = {
      if (imageFileName.isDefined) {
          <img src={imageFileName.get}/>
      } else <span/>
    }

    def fileOf(name: String) = new File(extendedData.output.dir, name)

    DicomUtil.writeAttributeListToFile(al, fileOf(dicomFileName), "AQA")

    val content = {
      <div>
        <h2>{title}</h2>
        <a href={dicomFileName}>Download DICOM</a>
        <p></p>
        {imageRef}
        <p></p>
        <pre>
          {WebUtil.nl + DicomUtil.attributeListToString(al)}
        </pre>
      </div>
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), title)
    Util.writeFile(fileOf(htmlFileName), text)

    htmlFileName
  }
}
