package learn

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util

import java.io.File

object GraftFocalSpot {

  private def getLargestBeamNumber(phase3rtplan: AttributeList): Int = {
    val attrList = DicomUtil.findAll(phase3rtplan, Set(TagByName.BeamNumber, TagByName.ReferencedBeamNumber))
    attrList.map(_.getIntegerValues).flatten.max
  }

  /**
    * Fix all the beam numbers in focal spot so they don't collide with Phase 3
    * @param phase3rtplan Phase3 rtplan
    * @param fsRtplan Focal spot rtplan
    */
  private def changeBeamNumbers(phase3rtplan: AttributeList, fsRtplan: AttributeList): Unit = {

    val firstNewBeamNumber = getLargestBeamNumber(phase3rtplan) + 1
    Trace.trace("First new beam number: " + firstNewBeamNumber)

    // maps old focal spot beam numbers to new ones.
    val beamNumberMap = {
      val fs = DicomUtil.findAllSingle(fsRtplan, TagByName.ReferencedBeamNumber).flatMap(_.getIntegerValues).distinct.sorted
      fs.zipWithIndex.map(oldNew => (oldNew._1, oldNew._2 + firstNewBeamNumber)).toMap
    }

    def changeBeamNumber(attr: Attribute): Unit = {
      val oldNum = attr.getIntegerValues.head
      attr.removeValues()
      attr.addValue(beamNumberMap(oldNum))
    }

    DicomUtil.findAll(fsRtplan, Set(TagByName.BeamNumber, TagByName.ReferencedBeamNumber)).foreach(changeBeamNumber)
  }

  /**
    * Fix all the dose references in focal spot to use the ones from Phase 3.
    *
    * @param phase3rtplan Phase3 rtplan
    * @param fsRtplan     Focal spot rtplan
    */
  private def changeTextRef(phase3rtplan: AttributeList, fsRtplan: AttributeList): Unit = {

    def change(tag: AttributeTag): Unit = {

      def setAttr(attr: Attribute, value: String): Unit = {
        attr.removeValues()
        attr.addValue(value)
      }

      val value = DicomUtil.findAllSingle(phase3rtplan, tag).head.getSingleStringValueOrEmptyString()

      DicomUtil.findAllSingle(fsRtplan, tag).map(attr => setAttr(attr, value))
    }

    change(TagByName.ReferencedDoseReferenceUID)
    change(TagByName.ReferencedPrimaryDoseRefUID)
    change(TagByName.DeviceSerialNumber)
    change(TagByName.ManufacturerModelName)
    change(TagByName.TreatmentMachineName)
  }

  private def appendSequence(phase3rtplan: AttributeList, fsRtplan: AttributeList, tag: AttributeTag): Unit = {
    val p3 = phase3rtplan.get(tag).asInstanceOf[SequenceAttribute]

    val list = fsRtplan.get(tag).asInstanceOf[SequenceAttribute]
    (0 until list.getNumberOfItems).map(i => p3.addItem(list.getItem(i)))
  }

  /**
    * Add the focal spot beams to Phase 3.
    *
    * This means adding beams to a plan and adding RTIMAGE files o Phase 3 data set.
    *
    * @param args Ignored
    */
  private def graft(phase3rtplan: AttributeList, fsRtplan: AttributeList, phase3rtimageList: Seq[AttributeList], fsRtimageList: Seq[AttributeList]): Unit = {
    changeBeamNumbers(phase3rtplan, fsRtplan)
    changeTextRef(phase3rtplan, fsRtplan)

    appendSequence(phase3rtplan, fsRtplan, TagByName.FractionGroupSequence)
    appendSequence(phase3rtplan, fsRtplan, TagByName.BeamSequence)
  }

  private def readDicom(file: File): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }

  def main(args: Array[String]): Unit = {

    val phase3rtplan = Util.readDicomFile(new File("""D:\pf\IntelliJ\ws\aqa\src\main\resources\static\rtplan\rtplanPhase3Millenium.dcm""")).right.get
    val phase3rtimageList = ???

    val fsDir = new File("""D:\pf\IntelliJ\ws\aqa\src\test\resources\TestFocalSpot\Millenium_MLC\RTPLAN.Mil.dcm""")
    val fsDicomList = Util.listDirFiles(fsDir).filter(_.getName.endsWith(".dcm")).map(Util.readDicomFile).map(_.right.get)

    val fsRtplan = fsDicomList.find(Util.isRtplan).get
    val fsRtimageList = fsDicomList.filter(Util.isRtimage)

    graft(phase3rtplan, fsRtplan, phase3rtimageList, fsRtimageList)

  }
}
