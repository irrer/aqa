package learn

import com.pixelmed.dicom.DateTimeAttribute
import com.pixelmed.dicom.AttributeList
import java.io.File
import com.pixelmed.dicom.TagFromName
import java.util.TimeZone
import org.aqa.Util

object DateCheck {

  def check(file: File): Unit = {
    println("\nProcessing file " + file.getAbsolutePath)

    val al = new AttributeList
    al.read(file)

    val date = DateTimeAttribute.getDateFromFormattedString(al, TagFromName.AcquisitionDate, TagFromName.AcquisitionTime)
    val timeTextOrig = al.get(TagFromName.AcquisitionTime).getSingleStringValueOrEmptyString
    val dateTextOrig = al.get(TagFromName.AcquisitionDate).getSingleStringValueOrEmptyString
    println("timeTextOrig: " + timeTextOrig)
    println("dateTextOrig: " + dateTextOrig)

    val gmt = TimeZone.getTimeZone("GMT")
    println("date: " + date)
    println("DateTimeAttribute.getTimeZone: " + DateTimeAttribute.getTimeZone(gmt, date))
    println("DateTimeAttribute.getCurrentTimeZone: " + DateTimeAttribute.getCurrentTimeZone)
    println("TimeZone.getDefault: " + TimeZone.getDefault)
    val tz = TimeZone.getDefault
    println("tz.getRawOffset: " + tz.getRawOffset)
    println("tz.getDSTSavings: " + tz.getDSTSavings)
    val sum = tz.getRawOffset + tz.getDSTSavings
    println("sum: " + sum)

    val adjustedDate = Util.adjustDicomDateByLocalTimeZone(date)
    println("adjustedDate: " + adjustedDate)
  }

  def main(args: Array[String]): Unit = {
    val riFile = new File("""D:\AQA_Data\results\INST___1\MACH__22\BB_by_EPID_0.1_7\2019-10-03T18-43-16-053_1429\RTIMAGE_001.dcm""")

    val fileList = (args.toSeq.map(a => new File(a)) :+ riFile).filter(f => f.canRead)

    fileList.map(f => check(f))
  }
}