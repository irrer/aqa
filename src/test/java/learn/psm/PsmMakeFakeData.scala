package learn.psm

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.io.File

object PsmMakeFakeData {

  private val dir = new File("""D:\aqa\psm\fakedData\fakePsm""")
  private val outDir = new File(dir, "output")
  Util.deleteFileTreeSafely(outDir)
  outDir.mkdirs()

  private val dateTagList = Seq(
    TagByName.InstanceCreationDate,
    TagByName.StudyDate,
    TagByName.SeriesDate,
    TagByName.AcquisitionDate,
    TagByName.ContentDate,
    TagByName.RTPlanDate,
    TagByName.ReviewDate
  )

  private val fileList = Util.listDirFiles(dir).filter(_.getName.endsWith("DCM"))

  private def changeDates(al: AttributeList, dateText: String): Unit = {

    def changeOneDate(tag: AttributeTag): Unit = {
      val attr = al.get(tag)
      if (attr != null) {
        attr.removeValues()
        attr.addValue(dateText)
      }
    }

    dateTagList.foreach(changeOneDate)
  }

  private def tweakImage(al: AttributeList): Unit = {
    val pix = al.getPixelData.getShortValues
    val col = al.get(TagByName.Columns).getIntegerValues.head
    val start = ((pix.size + col) / 2) - 2
    val finish = ((pix.size + col) / 2) + 2

    var count = 0

    def doPix(i: Int): Short = {
      if ((i < start) || (i > finish))
        pix(i)
      else {
        val v = ((pix(i) ^ 1) & 0xffff).toShort
        count = count + 1
        v
      }
    }

    val pixTweaked = pix.indices.map(doPix).toArray

    val attr = al.get(TagByName.PixelData).asInstanceOf[OtherWordAttribute]
    attr.removeValues()
    attr.setValues(pixTweaked)
  }

  private def changeDSN(al: AttributeList): Unit = {

    def changeIt(attr: Attribute): Unit = {
    if (attr != null) {
      attr.removeValues()
      attr.addValue("3525")
    }
    }

    DicomUtil.findAllSingle(al, TagByName.DeviceSerialNumber).foreach(changeIt)
  }

  private def doFile(file: File): Unit = {
    val al = new AttributeList
    al.read(file)

    val oldHash = if (al.get(TagByName.PixelData) == null) "NA" else Util.imagePixelMD5Hash(al)

    println(s"Doing file  ${file.getName}  hash: $oldHash")

    val dateText =
      if (file.getName.toLowerCase().matches(".*flood.*"))
        "20220101"
      else
        "20220102"

    changeDSN(al)

    changeDates(al, dateText)
    if (al.get(TagByName.PixelData) != null)
      tweakImage(al)

    val newHash = if (al.get(TagByName.PixelData) == null) "NA" else Util.imagePixelMD5Hash(al)
    val outFile = new File(outDir, "T" + file.getName)
    DicomUtil.writeAttributeListToFile(al, outFile, "AQA")
    println(s"Did   file ${outFile.getName}  hash: $newHash\n")
  }

  def main(args: Array[String]): Unit = {
    Trace.trace("Starting ----------------------------------------------------------------------------")

    fileList.foreach(doFile)

    Trace.trace("Done ----------------------------------------------------------------------------")
  }
}
