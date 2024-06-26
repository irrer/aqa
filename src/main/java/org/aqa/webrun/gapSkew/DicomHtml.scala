/*
 * Copyright 2022 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.gapSkew

import com.pixelmed.dicom
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData

import java.io.File

/**
  * Make an html file containing the image (if this is an image) and the metadata.
  * @param extendedData Regarding the output.
  * @param title Name of web page.
  */
case class DicomHtml(extendedData: ExtendedData, title: String) {

  private val baseFileName = title.replaceAll("[^a-zA-Z0-9_]", "_")
  val htmlUrl: String = "DICOM_" + baseFileName + ".html"

  def makeDicomContent(al: dicom.AttributeList, imageFileName: Option[String] = None): Unit = {
    val dicomFileName = "DICOM_" + baseFileName + ".dcm"

    val imageRef = {
      if (imageFileName.isDefined) {
        <img src={imageFileName.get}/>
      } else <span/>
    }

    def fileOf(name: String) = new File(extendedData.output.dir, name)

    Util.writeAttributeListToFile(al, fileOf(dicomFileName))

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
    Util.writeFile(fileOf(htmlUrl), text)
  }
}
