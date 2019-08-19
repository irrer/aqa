
import javax.vecmath.Point3d
import org.aqa.DicomFile
import java.io.File
import org.aqa.Util
import com.pixelmed.dicom.SOPClass
import org.aqa.ImageRegistration
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName

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

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    val all = getAttrListList
    val regList = all.filter(al => Util.isModality(al, SOPClass.SpatialRegistrationStorage)).map(al => new ImageRegistration(al))
    val rtimageList = all.filter(al => Util.isModality(al, SOPClass.RTImageStorage))

    def tryRtimage(rtimage: AttributeList): String = {
      val frameOfRef = Util.getAttrValue(rtimage, TagFromName.FrameOfReferenceUID).get
      val PatientID = Util.getAttrValue(rtimage, TagFromName.PatientID).get
      val DevSerNo = rtimage.get(TagFromName.DeviceSerialNumber).getSingleStringValueOrEmptyString.formatted("%16s")
      //val reg = regList.find(reg => reg.frameOfRefUID.equals(frameOfRef))
      val reg = regList.find(reg => reg.sameFrameOfRef(rtimage))
      if (reg.isDefined) {
        val isoPoint = new Point3d(rtimage.get(TagFromName.IsocenterPosition).getDoubleValues)
        val after = reg.get.transform(isoPoint)
        PatientID +
          "    DevSerNo: " + DevSerNo +
          "    isoPoint: " + isoPoint +
          "    transformed: " + after
      } else
        PatientID + "    DevSerNo: " + DevSerNo + "    No reg for " + frameOfRef
    }

    val results = rtimageList.map(rtimage => tryRtimage(rtimage))
    val passed = results.filter(r => r.contains("isoPoint"))
    val numWithReg = passed.size
    println(results.sorted.mkString("\n"))

    println("rtimageList.size: " + rtimageList.size)
    println("Number of images with    reg: " + numWithReg)
    println("Number of images without reg: " + (results.size - numWithReg))

    println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
  }

}