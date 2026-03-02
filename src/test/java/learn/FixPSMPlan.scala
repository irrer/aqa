package learn

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.DicomFile

import java.io.File
import javax.vecmath.Point2d

/**
  * View image characteristics.  Review coordinate system.
  */
object FixPSMPlan {

  // val dir = new File("D:/tmp/psm/MichaelPSM/AQA_PSM/Data_6xFFF_250317/fake")
  // val dir = new File("D:/tmp/psm/MichaelPSM/AQA_PSM/Data_10x_250317")
  val dir = new File("D:/tmp/psm/MichaelPSM/AQA_PSM/Data_10xFFF_250317")

  def centerOf(attribute: Attribute): Double = {
    val posList = attribute.getDoubleValues
    ((posList.head + posList(1)) / 2).round
  }

  def beamCenter(img: AttributeList): Point2d = {
    val ljp = DicomUtil.findAllSingle(img, TagByName.LeafJawPositions)
    val x = centerOf(ljp.head)
    val y = centerOf(ljp(1))
    new Point2d(x, y)
  }

  def fmt(l: Long): String = "%4d".format(l)

  private case class ImgSpec(img: AttributeList) {
    val ljp = DicomUtil.findAllSingle(img, TagByName.LeafJawPositions)

    val ljpX = ljp.head.getDoubleValues
    val ljpY = ljp(1).getDoubleValues

    private val center = DicomUtil.findAllSingle(img, TagByName.XRayImageReceptorTranslation).head.getDoubleValues

    val xCenter: Long = (center.head).round
    val yCenter: Long = (center(1)).round

    val size: Long = (ljpX(1) - ljpX.head).round

    val BeamNumber: Int = DicomUtil.findAllSingle(img, TagByName.ReferencedBeamNumber).head.getIntegerValues.head

    override def toString: String = {
      s"Img  x: ${fmt(xCenter)}  y: ${fmt(yCenter)}    size: ${fmt(size)}    BeamNumber: ${fmt(BeamNumber)}"
    }
  }

  private case class PlanSpec(plan: AttributeList, beam: AttributeList) {

    val ljp = DicomUtil.findAllSingle(beam, TagByName.LeafJawPositions)

    val ljpX = ljp.head.getDoubleValues
    val ljpY = ljp(1).getDoubleValues

    private val center = DicomUtil.findAllSingle(beam, TagByName.RTImagePosition).head.getDoubleValues

    val xCenter: Long = (center.head).round
    val yCenter: Long = (center(1)).round

    val size: Long = (ljpX(1) - ljpX.head).round

    val BeamNumber: Int = DicomUtil.findAllSingle(beam, TagByName.BeamNumber).head.getIntegerValues.head

    def sameBeam(attr: Attribute): Boolean = attr.getIntegerValues.head == BeamNumber

    val BeamNumberAttrList: Seq[Attribute] = {

      val all: Seq[Attribute] = {
        DicomUtil.findAllSingle(beam, TagByName.ReferencedBeamNumber) ++ //
          DicomUtil.findAllSingle(beam, TagByName.BeamNumber) ++ //
          DicomUtil.findAllSingle(plan, TagByName.ReferencedBeamNumber) ++ //
          DicomUtil.findAllSingle(plan, TagByName.BeamNumber)
      }

      val list = all.filter(sameBeam)
      list
    }

    override def toString: String = {
      val bnText = BeamNumberAttrList.map(_.getIntegerValues.head.toLong).map(fmt).mkString("  ")
      s"Plan x: ${fmt(xCenter)}  y: ${fmt(yCenter)}    size: ${fmt(size)}    BeamNumber: ${fmt(BeamNumber)}    $bnText"
    }
  }

  def main(args: Array[String]): Unit = {

    val newPlanFile = new File(dir, "newRTPLAN.dcm")

    val plan = new DicomFile(new File(dir, "RTPLAN.dcm")).attributeList.get

    val rtimageAlList = {
      val list = FileUtil.listFiles(dir).filter(f => f.getName.startsWith("RI") && f.getName.endsWith(".dcm"))
      list.map(f => new DicomFile(f).attributeList.get)
    }

    val rtimageList = rtimageAlList.map(ImgSpec).sortBy(_.BeamNumber)

    val expectedRtplanSOP = DicomUtil.seqToAttr(rtimageAlList.head, TagByName.ReferencedRTPlanSequence).head.get(TagByName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString

    val planList = {
      val planBeamList = DicomUtil.seqToAttr(plan, TagByName.BeamSequence)
      planBeamList.map(beam => PlanSpec(plan, beam)).sortBy(_.BeamNumber)
    }

    println(rtimageList.mkString("\n"))

    println(planList.mkString("\n"))

    def fixPlan(planSpec: PlanSpec): Unit = {
      val newBeamNumber = rtimageList.find(i => (planSpec.xCenter == i.xCenter) && (planSpec.yCenter == i.yCenter) && (planSpec.size == i.size)).get.BeamNumber

      def setBeamNumber(attr: Attribute): Unit = {
        attr.removeValues()
        attr.addValue(newBeamNumber)
      }

      planSpec.BeamNumberAttrList.foreach(setBeamNumber)
    }

    planList.foreach(fixPlan)

    val MediaStorageSOPInstanceUID = plan.get(TagByName.MediaStorageSOPInstanceUID)
    MediaStorageSOPInstanceUID.removeValues()
    MediaStorageSOPInstanceUID.addValue(expectedRtplanSOP)

    val SOPInstanceUID = plan.get(TagByName.SOPInstanceUID)
    SOPInstanceUID.removeValues()
    SOPInstanceUID.addValue(expectedRtplanSOP)

    DicomUtil.writeAttributeListToFile(plan, newPlanFile, "AQA")

    println(s"Wrote new plan at: $newPlanFile")
  }

}
