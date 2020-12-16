
import java.io.File
import org.aqa.DicomFile
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import org.aqa.Util
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.SOPClassDescriptions
import edu.umro.DicomDict.TagByName

object SearchReg {

  def main(args: Array[String]): Unit = {

    println("starting ...")
    case class Reg(file: File) {

      val al = new DicomFile(file).attributeList.get

      def get(tag: AttributeTag): String = {
        val a = al.get(tag)
        if (a == null) ""
        else a.getSingleStringValueOrEmptyString
      }

      val dateTime = get(TagFromName.ContentDate) + "  " + get(TagFromName.ContentTime)

      val mySop = get(TagFromName.SOPInstanceUID)
      val mySeries = get(TagFromName.SeriesInstanceUID)
      val myFrameRef = get(TagFromName.FrameOfReferenceUID)

      val refFrame = DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).map(a => a.getSingleStringValueOrEmptyString).toIndexedSeq
      val refSeries = DicomUtil.findAllSingle(al, TagFromName.SeriesInstanceUID).map(a => a.getSingleStringValueOrEmptyString).toIndexedSeq
      val refSop = DicomUtil.findAllSingle(al, TagFromName.ReferencedSOPInstanceUID).map(a => a.getSingleStringValueOrEmptyString).toIndexedSeq
      val refSopClass = DicomUtil.findAllSingle(al, TagFromName.ReferencedSOPClassUID).map(a => SOPClassDescriptions.getDescriptionFromUID(a.getSingleStringValueOrEmptyString)).toIndexedSeq
      val matrixList = DicomUtil.findAllSingle(al, TagByName.FrameOfReferenceTransformationMatrix).map(a => a.getDoubleValues.mkString("  ")).toIndexedSeq

      override def toString = {
        "file: " + file.getAbsolutePath + dateTime +
          "\n    mySop: " + mySop +
          "    mySeries: " + mySeries +
          "    myFrameRef: " + myFrameRef +
          "\n    refFrame: " + refFrame.distinct.sorted.mkString("  ") +
          "\n    refSeries: " + refSeries.distinct.sorted.mkString("  ") +
          "\n    refSop: " + refSop.distinct.size + " : " + refSop.distinct.sorted.mkString("  ") +
          "\n    refSopClass: " + refSopClass.distinct.sorted.mkString(" | ") +
          "\n    matrixList: " + "\n        " + matrixList.sorted.mkString("\n        ")
      }
    }

    val inDir = new File("""D:\tmp\aqa\CBCT\MQATX1OBIQA2019Q3""")

    val list = inDir.listFiles.filter(f => f.getName.matches("RE.*dcm"))

    def doit(regFile: File) = new Reg(regFile)

    val allReg = list.map(regFile => doit(regFile)).toList

    val series = allReg.groupBy(reg => reg.mySeries).values.map(s => s.sortBy(_.dateTime))

    def matrixOf(series: List[Reg]): String = {
      val uniq = series.map(reg => reg.matrixList).flatten.distinct
      "Unique matrixes: " + uniq.size
    }

    println(series.map(s => s.mkString("\n") + "\n" + matrixOf(s)).mkString("\n\n--------\n\n"))

    println
    println("Number of files: " + list.size)
    println("Number of reg: " + allReg.size)
    println("Number of series: " + series.size)
    println("done.")
  }

}
