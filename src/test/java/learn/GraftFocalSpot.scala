package learn

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import edu.umro.util.UMROGUID
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util

import java.io.File
import java.util.Date

object GraftFocalSpot {

  private def getLargestBeamNumber(phase3rtplan: AttributeList): Int = {
    val attrList = DicomUtil.findAll(phase3rtplan, Set(TagByName.BeamNumber, TagByName.ReferencedBeamNumber))
    attrList.flatMap(_.getIntegerValues).max
  }

  private def setAttrText(attr: Attribute, value: String): Unit = {
    attr.removeValues()
    attr.addValue(value)
  }

  /**
    * Set all attributes with the given tag to the given text.
    * @param al All of attr in here.
    * @param tag Tag.
    * @param value New text value.
    */
  private def setAllText(al: AttributeList, tag: AttributeTag, value: String): Unit = {
    DicomUtil.findAllSingle(al, tag).foreach(attr => setAttrText(attr, value))
  }

  private def changeBeamNumber(attr: Attribute, beamNumberMap: Map[Int, Int]): Unit = {
    val oldNum = attr.getIntegerValues.head
    attr.removeValues()
    attr.addValue(beamNumberMap(oldNum))
  }

  /**
    * Fix all the beam numbers in focal spot so they don't collide with Phase 3
    * @param phase3rtplan Phase3 rtplan
    * @param fsRtplan Focal spot rtplan
    */
  private def changeRtplanBeamNumbers(phase3rtplan: AttributeList, fsRtplan: AttributeList): Map[Int, Int] = {

    val firstNewBeamNumber = getLargestBeamNumber(phase3rtplan) + 1
    Trace.trace("First new beam number: " + firstNewBeamNumber)

    // maps old focal spot beam numbers to new ones.
    val beamNumberMap = {
      val fs = DicomUtil.findAllSingle(fsRtplan, TagByName.ReferencedBeamNumber).flatMap(_.getIntegerValues).distinct.sorted
      fs.zipWithIndex.map(oldNew => (oldNew._1, oldNew._2 + firstNewBeamNumber)).toMap
    }

    DicomUtil.findAll(fsRtplan, Set(TagByName.BeamNumber, TagByName.ReferencedBeamNumber)).foreach(b => changeBeamNumber(b, beamNumberMap))
    beamNumberMap
  }

  /**
    * Fix all the dose references in focal spot to use the ones from Phase 3.
    *
    * @param phase3rtplan Phase3 rtplan
    * @param fsRtplan     Focal spot rtplan
    */
  private def changeTextRef(phase3rtplan: AttributeList, fsRtplan: AttributeList): Unit = {

    def change(tag: AttributeTag): Unit = {

      val value = DicomUtil.findAllSingle(phase3rtplan, tag).head.getSingleStringValueOrEmptyString()

      DicomUtil.findAllSingle(fsRtplan, tag).foreach(attr => setAttrText(attr, value))
    }

    change(TagByName.ReferencedDoseReferenceUID)
    // change(TagByName.ReferencedPrimaryDoseRefUID)
    change(TagByName.DeviceSerialNumber)
    change(TagByName.ManufacturerModelName)
    change(TagByName.TreatmentMachineName)
  }

  private def appendSequence(phase3rtplan: AttributeList, fsRtplan: AttributeList, tag: AttributeTag): Unit = {
    val p3 = phase3rtplan.get(tag).asInstanceOf[SequenceAttribute]

    val list = fsRtplan.get(tag).asInstanceOf[SequenceAttribute]
    (0 until list.getNumberOfItems).foreach(i => p3.addItem(list.getItem(i)))
  }

  private def changeRtimageBeamNumbers(beamNumberMap: Map[Int, Int], fsRtimageList: Seq[AttributeList]): Unit = {
    val list = fsRtimageList.flatMap(rtimage => DicomUtil.findAllSingle(rtimage, TagByName.ReferencedBeamNumber))
    list.foreach(b => changeBeamNumber(b, beamNumberMap))
  }

  /**
    * Change dates and times that vary across images.
    * @param phase3rtimageList p3
    * @param fsRtimageList fs
    */
  private def changeDates(phase3rtimageList: Seq[AttributeList], fsRtimageList: Seq[AttributeList]): Unit = {
    val maxDate = {
      val dateList = phase3rtimageList.flatMap(al => {
        val ic = DicomUtil.getTimeAndDate(al, TagByName.ContentDate, TagByName.ContentTime)
        val ac = DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime)
        Seq(ic, ac).flatten
      })
      dateList.maxBy(_.getTime)
    }

    def setDates(rtimage: AttributeList, index: Int): Unit = {
      val date = new Date(maxDate.getTime + ((index + 1) * 10 * 1000))
      val dateText = DicomUtil.dicomDateFormat.format(date)
      val timeText = DicomUtil.dicomTimeFormat.format(date)

      setAllText(rtimage, TagByName.AcquisitionDate, dateText)
      setAllText(rtimage, TagByName.AcquisitionTime, timeText)

      setAllText(rtimage, TagByName.InstanceCreationDate, dateText)
      setAllText(rtimage, TagByName.InstanceCreationTime, timeText)

      setAllText(rtimage, TagByName.ContentDate, dateText)
      setAllText(rtimage, TagByName.ContentTime, timeText)
    }

    fsRtimageList.zipWithIndex.foreach(ri => setDates(ri._1, ri._2))
  }

  private def changeEachSopInstanceUid(fsRtimageList: Seq[AttributeList]): Unit = {

    def changeSOP(rtimage: AttributeList): Unit = {
      val sopUid = UMROGUID.getUID
      setAllText(rtimage, TagByName.MediaStorageSOPInstanceUID, sopUid)
      setAllText(rtimage, TagByName.SOPInstanceUID, sopUid)
    }

    fsRtimageList.foreach(changeSOP)
  }

  private def changeStaticValues(phase3rtimageList: Seq[AttributeList], fsRtimageList: Seq[AttributeList]): Unit = {

    def copy(tag: AttributeTag): Unit = {
      val value = DicomUtil.findAllSingle(phase3rtimageList.head, tag).head.getSingleStringValueOrEmptyString()
      fsRtimageList.foreach(al => setAllText(al, tag, value))
    }

    copy(TagByName.PatientName)
    copy(TagByName.PatientID)
    copy(TagByName.StudyDate)
    copy(TagByName.StudyTime)
    copy(TagByName.StationName)
    // copy(TagByName.StudyDescription)
    copy(TagByName.OperatorsName)
    copy(TagByName.ManufacturerModelName)
    copy(TagByName.StudyInstanceUID)
    copy(TagByName.SeriesInstanceUID)
    copy(TagByName.DeviceSerialNumber)
    copy(TagByName.SoftwareVersions)
    copy(TagByName.ReferencedSOPInstanceUID)

  }

  /**
    * Add the focal spot beams to Phase 3.
    *
    * This means adding beams to a plan and adding RTIMAGE files o Phase 3 data set.
    *
    */
  private def graft(phase3rtplan: AttributeList, fsRtplan: AttributeList, phase3rtimageList: Seq[AttributeList], fsRtimageList: Seq[AttributeList]): Unit = {
    val beamNumberMap = changeRtplanBeamNumbers(phase3rtplan, fsRtplan)
    changeTextRef(phase3rtplan, fsRtplan)

    appendSequence(phase3rtplan, fsRtplan, TagByName.FractionGroupSequence)
    appendSequence(phase3rtplan, fsRtplan, TagByName.BeamSequence)

    changeRtimageBeamNumbers(beamNumberMap, fsRtimageList)

    changeDates(phase3rtimageList, fsRtimageList)
    changeEachSopInstanceUid(fsRtimageList)
    changeStaticValues(phase3rtimageList, fsRtimageList)
  }

  private def readDicom(file: File): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }

  def main(args: Array[String]): Unit = {

    val inDir = new File("""D:\pf\IntelliJ\ws\aqa\target\Phase3DataSet\output""")

    val alList = Util.listDirFiles(inDir).map(readDicom)

    val phase3rtplan = alList.find(Util.isRtplan).get
    val phase3rtimageList = alList.filter(Util.isRtimage)

    val fsDir = new File("""D:\pf\IntelliJ\ws\aqa\src\test\resources\TestFocalSpot\Millenium_MLC""")
    val fsDicomList = Util.listDirFiles(fsDir).filter(_.getName.toLowerCase.endsWith(".dcm")).map(Util.readDicomFile).map(_.right.get)

    val fsRtplan = fsDicomList.find(Util.isRtplan).get
    val fsRtimageList = fsDicomList.filter(Util.isRtimage)

    graft(phase3rtplan, fsRtplan, phase3rtimageList, fsRtimageList)

    val outDir = new File("""D:\pf\IntelliJ\ws\aqa\target\Phase3DataSet\graft""")
    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs()

    val appName = "Graft"

    DicomUtil.writeAttributeListToFile(phase3rtplan, new File(outDir, "RTPLAN.dcm"), appName)

    fsRtimageList.foreach(rtimage => {
      val name = "RTIMAGE_" + Util.beamNumber(rtimage).formatted("%02d") + ".dcm"
      DicomUtil.writeAttributeListToFile(rtimage, new File(outDir, name), appName)
    })

    println("Wrote files to " + outDir.getAbsolutePath)
  }
}
