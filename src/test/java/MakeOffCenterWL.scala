import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.UMROGUID
import org.aqa.DicomFile

import java.io.File
import java.util.Date

/*
 * Copyright 2021 Regents of the University of Michigan
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

/**
  * Take a centered Winston Lutz image and make an off-centered one.
  */
object MakeOffCenterWL {

  private def putPixelListInDicom(wlDicom: AttributeList, pixelList: Seq[Float]): Unit = {

    val shortArray = pixelList.map(p => p.round & 0xffff).map(_.toShort).toArray

    wlDicom.remove(TagByName.PixelData)

    val pixelData = new OtherWordAttribute(TagByName.PixelData)
    pixelData.setValues(shortArray)

    wlDicom.put(pixelData)
  }

  private def setMetaData(wlDicom: AttributeList): Unit = {
    def setAttr(tag: AttributeTag, value: String): Unit = {
      val attrList = DicomUtil.findAllSingle(wlDicom, tag)
      attrList.foreach(attr => {
        attr.removeValues()
        attr.addValue(value)
      })
    }

    val date = new Date()

    val dateText = DicomUtil.dicomDateFormat.format(date)
    val timeText = DicomUtil.dicomTimeFormat.format(date)

    val sopUid = UMROGUID.getUID
    val serUid = UMROGUID.getUID
    val forUid = UMROGUID.getUID
    val stdUid = UMROGUID.getUID

    setAttr(TagByName.MediaStorageSOPInstanceUID, sopUid)
    setAttr(TagByName.SOPInstanceUID, sopUid)
    setAttr(TagByName.SeriesInstanceUID, serUid)
    setAttr(TagByName.FrameOfReferenceUID, forUid)
    setAttr(TagByName.StudyInstanceUID, stdUid)

    setAttr(TagByName.InstanceCreationTime, timeText)
    setAttr(TagByName.StudyTime, timeText)
    setAttr(TagByName.SeriesTime, timeText)
    setAttr(TagByName.AcquisitionTime, timeText)
    setAttr(TagByName.ContentTime, timeText)

    setAttr(TagByName.InstanceCreationDate, dateText)
    setAttr(TagByName.StudyDate, dateText)
    setAttr(TagByName.SeriesDate, dateText)
    setAttr(TagByName.AcquisitionDate, dateText)
    setAttr(TagByName.ContentDate, dateText)
  }

  def main(args: Array[String]): Unit = {
    println("starting")

    val wlFile = new File("D:/tmp/wl/offCenter/wl.dcm")
    val wlFile2 = new File("D:/tmp/wl/offCenter/wl2.dcm")
    wlFile2.delete()

    val wlDicom = new DicomFile(wlFile).attributeList.get
    val di = new DicomImage(wlDicom)

    val xShift_pix = 200 // shift this many pixels in X axis
    val yShift_pix = 300 // shift this many pixels in Y axis

    // ------------------------------------------------------------------------

    def doPixel(x: Int, y: Int): Float = {
      val x2 = (x + xShift_pix) % di.width
      val y2 = (y + yShift_pix) % di.height
      val v = di.get(x2, y2)
      v
    }

    def doRow(y: Int) = {
      (0 until di.width).map(x => doPixel(x, y))
    }

    val pixelList = (0 until di.height).flatMap(doRow)

    putPixelListInDicom(wlDicom, pixelList)

    setMetaData(wlDicom)

    DicomUtil.writeAttributeListToFile(wlDicom, wlFile2, "AQA")

    println(s"finished.  Wrote file $wlFile2")
  }

}
