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


import javax.vecmath.Point3d
import org.aqa.DicomFile
import java.io.File
import org.aqa.Util
import com.pixelmed.dicom.SOPClass
import org.aqa.ImageRegistration
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName

/**
 * Compare position of BB in CBCT to RTPLAN isocenter.
 */
object MultMatrix2 {

  val topDir = new File("""D:\tmp\aqa\CBCT\""")

  def getAttrListList = {
    val patientDirs = topDir.listFiles.filter(dir => dir.getName.startsWith("MQATX"))
    val fileList = patientDirs.map(pd => pd.listFiles).flatten.filter(f => f.getName.startsWith("RI") || f.getName.startsWith("RE")).toSeq
    println("Number of files found: " + fileList.size)
    val attrListList = fileList.map(f => new DicomFile(f).attributeList.get)
    attrListList
  }

  private def c2s(d: Double) = d.formatted("%8.2f")
  def p2s(p: Point3d) = c2s(p.getX) + ", " + c2s(p.getY) + ", " + c2s(p.getZ)

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    val all = getAttrListList
    val regList = all.filter(al => Util.isReg(al)).map(al => new ImageRegistration(al))
    val rtimageList = all.filter(al => Util.isRtimage(al))

    def tryRtimage(rtimage: AttributeList): String = {
      val DeviceSerialNumber = rtimage.get(TagFromName.DeviceSerialNumber).getSingleStringValueOrEmptyString

      val DevSerNo = DeviceSerialNumber.format("%6s")
      val frameOfRef = Util.getAttrValue(rtimage, TagFromName.FrameOfReferenceUID).get
      val PatientID = Util.getAttrValue(rtimage, TagFromName.PatientID).get
      val date = Util.standardDateFormat.format(Util.extractDateTimeAndPatientIdFromDicomAl(rtimage)._1.head)
      val planUID = DicomUtil.seqToAttr(rtimage, TagByName.ReferencedRTPlanSequence).
        head.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString

      val isoPoint = new Point3d(rtimage.get(TagByName.IsocenterPosition).getDoubleValues)
      val XRayImageReceptorTranslation = new Point3d(rtimage.get(TagByName.XRayImageReceptorTranslation).getDoubleValues)
      //val reg = regList.find(reg => reg.frameOfRefUID.equals(frameOfRef))

      val okReg = regList.filter(reg => reg.frameOfRefUID.equals(frameOfRef))

      val reg = regList.find(reg => reg.frameOfRefUID.equals(frameOfRef))
      if (reg.isDefined) {
        def p2sT(p: Point3d) = p2s(p) + " --> " + p2s(reg.get.transform(p))
        //    "    planUID: " + planUID.format("%-60s") +

        PatientID +
          "    DevSerNo: " + DevSerNo +
          "    date: " + date +
          "    num reg: " + okReg.size.formatted("%4d") +
          "    isoPoint: " + p2sT(isoPoint) +
          "    XRayImageReceptorTranslation: " + p2s(XRayImageReceptorTranslation)
      } else
        PatientID +
          "    DevSerNo: " + DevSerNo +
          "    date: " + date +
          "    No reg for " + frameOfRef.format("%-60s") +
          "    isoPoint: " + p2s(isoPoint) +
          "    XRayImgRcptrTrans: " + p2s(XRayImageReceptorTranslation)

    }

    val results = rtimageList.map(rtimage => tryRtimage(rtimage)).filterNot(s => s.contains(" DevSerNo: 824327626427 "))
    val passed = results.filter(r => r.contains("isoPoint"))
    val numWithReg = passed.size
    println(results.sorted.mkString("\n"))

    println("rtimageList.size: " + rtimageList.size)
    println("Number of images with    reg: " + numWithReg)
    println("Number of images without reg: " + (results.size - numWithReg))

    println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
  }

}
